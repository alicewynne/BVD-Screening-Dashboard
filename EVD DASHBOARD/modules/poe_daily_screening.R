# ------------------------------------------------------------------------------
# POE DAILY SCREENING DATA
# Reads the shared PoE screening workbook:
#   - "Daily entry"   : one row per PoE per day (from 30 Sep 2026)
#   - "wide_data_OLD" : historic totals for 4 PoEs (before 30 Sep 2026)
#   - "PoE List"      : PoE lookup (type, border country, state, start date)
# Returns one clean long table plus a data quality log.
# ------------------------------------------------------------------------------


# Sharing link to the daily PoE screening workbook
TALLY_URL       <- "https://worldhealthorg-my.sharepoint.com/:x:/g/personal/dagamaa_who_int/IQCe8i3GaDLOR5zPbnPkgvjqAVbCdtltmgI0pabt-LFrRTg?e=1KctJM"

TALLY_FILE      <- "PoE_screening_data.xlsx"
TALLY_SHEET     <- "Daily entry"
POE_LIST_SHEET  <- "PoE List"
HISTORIC_SHEET  <- "wide_data_OLD"

POE_ODK_FORM_PK <- 18951L                  # ODK "PoE Daily Screening Report"
POE_ODK_START   <- as.Date("2026-09-30")   # first day taken from ODK


# Historic data is used for dates before the new daily poe screening workbook was implemented.Only 4 PoEs were entering data before this date: Nimule, Kaya, River Rei and Olikwi.
HISTORIC_CUTOFF <- POE_ODK_START      # OLD file up to 29th Oct

# Historic column prefixes -> PoE names exactly as in the PoE List
HISTORIC_POE_MAP <- c(
  "main_ground_crossing_nimule" = "Nimule",
  "kaya_po_e"                   = "Kaya",
  "olikwi_po_e_nimule"          = "Olikwi",
  "rei_river_port"              = "Rei-river port"
)

NOT_ASSIGNED_LABEL <- "Not yet assigned"

# ---------- page 1 settings (thresholds need to be agreed) ----------
P1_SILENT_DAYS       <- 3        # days without a report before a PoE is flagged
P1_COMPLETENESS_MIN  <- 0.80     # completeness below this is flagged
P1_RATE_PER          <- 10000    # alert rate per this many travellers screened
P1_RATE_MIN_SCREENED <- 500      # minimum weekly screened before the rate is shown
P1_DEFAULT_VIEW      <- "daily"  # default chart view: "daily" or "weekly"
P1_DEFAULT_WEEKS     <- 12       # default date range, in weeks
P1_ROLLING_DAYS      <- 7        # rolling average window on the daily chart
P1_WEEK_START        <- 1        # epi week starts Monday (1)
P1_REFRESH_MS        <- 60 * 60 * 1000   # re-download the workbook every hour

# Reports cover the previous 24 hours, so the reference date is yesterday
p1_reference_date <- function() Sys.Date() - 1


# Default date range: from the first date with data up to the reference date
p1_default_dates <- function(tally) {
  end_date <- p1_reference_date()
  start_date <- if (nrow(tally) > 0) {
    min(tally$date, na.rm = TRUE)
  } else {
    end_date - P1_DEFAULT_WEEKS * 7 + 1   # fallback if there is no data at all
  }
  c(start_date, end_date)
}

P1_COLOURS <- c(
  entry     = "#003b6f",   # dark blue (matches header)
  exit      = "#5b9bd5",   # light blue
  not_split = "#bfbfbf",   # grey: total not split by entry/exit
  alerts    = "#dd4b39",   # red (matches existing palette)
  rolling   = "#f39c12"    # 7-day rolling average line
)


# Chart toolbar: always visible, with a high-resolution PNG download
chart_download_config <- function(p, name) {
  p %>% config(
    displaylogo    = FALSE,
    displayModeBar = TRUE,
    modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d", "toggleSpikelines",
                               "hoverClosestCartesian", "hoverCompareCartesian"),
    toImageButtonOptions = list(
      format   = "png",
      filename = paste0(name, "_", format(Sys.Date(), "%Y-%m-%d")),
      scale    = 3
    )
  )
}

# ---- ODK daily report: fetch (uses the PoE key from .Renviron) ----
fetch_ona_form <- function(form_pk, token = Sys.getenv("POE_ODK_TOKEN")) {
  if (!nzchar(token)) { message("POE_ODK_TOKEN not set in .Renviron"); return(tibble()) }
  recs  <- list(); start <- 0
  repeat {
    page <- fetch_ona_page(token, form_pk, start_at = start, limit = ONA_PAGE_SIZE)
    if (is.null(page) || length(page) == 0) break
    recs <- c(recs, page)
    if (length(page) < ONA_PAGE_SIZE) break
    start <- start + ONA_PAGE_SIZE
  }
  if (length(recs) == 0) return(tibble())
  jsonlite::fromJSON(jsonlite::toJSON(recs, auto_unbox = TRUE, null = "null")) %>%
    jsonlite::flatten() %>%
    as_tibble()
}

# ---- ODK daily report: clean (one row per PoE per day, latest submission wins) ----
clean_poe_odk <- function(raw) {
  if (is.null(raw) || nrow(raw) == 0) return(tibble())
  col <- function(nm) if (nm %in% names(raw)) raw[[nm]] else rep(NA, nrow(raw))
  num <- function(nm) suppressWarnings(as.numeric(col(nm)))
  
  tibble(
    date       = as.Date(substr(as.character(col("grp_poe/reporting_date")), 1, 10)),
    poe        = str_squish(as.character(coalesce(col("grp_poe/poe_label"), col("grp_poe/poe")))),
    entry      = num("grp_screening/entry_screened"),
    exit       = num("grp_screening/exit_screened"),
    total_calc = num("grp_screening/total_screened"),
    total_rep  = num("grp_screening/total_screened_reported"),
    triggered  = num("grp_alerts/alerts_triggered"),
    discarded  = num("grp_alerts/alerts_discarded"),
    bodies     = num("grp_alerts/bodies_moved"),
    submitted  = ymd_hms(col("_submission_time"), quiet = TRUE)
  ) %>%
    filter(!is.na(date), !is.na(poe), poe != "", date >= POE_ODK_START) %>%
    arrange(desc(submitted)) %>%
    distinct(date, poe, .keep_all = TRUE) %>%
    arrange(date, poe)
}

# ---- ODK daily report: match the historic data's columns ----
odk_to_tally <- function(odk) {
  if (is.null(odk) || nrow(odk) == 0) return(NULL)
  odk %>%
    filter(poe != JIA_POE_NAME) %>%            # JIA comes from its own screening form
    transmute(
      date,
      poe,
      entry_screened   = entry,
      exit_screened    = exit,
      total_typed      = coalesce(total_rep, total_calc),
      alerts_triggered = triggered,
      alerts_discarded = discarded,
      bodies_moved     = bodies,
      source           = "ODK daily report"
    )
}

# ---------- small helpers ----------

# Text cleaning: removes non-breaking spaces and turns blanks into NA
tally_clean_text <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "\u00a0", " ")
  x <- str_squish(x)
  x[x %in% c("", "NA")] <- NA_character_
  x
}


# One cell from a readxl list column -> Date
# Handles real Excel dates, Excel serial numbers and day-first text
tally_cell_to_date <- function(v) {
  if (is.null(v) || length(v) == 0) return(as.Date(NA))
  if (inherits(v, c("POSIXct", "POSIXt", "Date"))) return(as.Date(v))
  if (is.numeric(v)) {
    if (is.na(v)) return(as.Date(NA))
    return(as.Date(v, origin = "1899-12-30"))
  }
  s <- tally_clean_text(v)
  if (is.na(s)) return(as.Date(NA))
  d <- suppressWarnings(
    parse_date_time(s, orders = c("dmY", "dbY", "dBY", "Ymd"), quiet = TRUE)
  )
  as.Date(d)
}

tally_col_to_date <- function(x) {
  if (is.null(x)) return(as.Date(character(0)))
  as.Date(vapply(x, function(v) as.numeric(tally_cell_to_date(v)), numeric(1)),
          origin = "1970-01-01")
}

# readxl list column -> numeric (blanks and hidden characters become NA)
tally_col_to_num <- function(x) {
  if (is.null(x)) return(numeric(0))
  vapply(x, function(v) {
    if (is.null(v) || length(v) == 0) return(NA_real_)
    if (is.numeric(v)) return(as.numeric(v))
    suppressWarnings(as.numeric(tally_clean_text(v)))
  }, numeric(1))
}

tally_col_to_chr <- function(x) {
  if (is.null(x)) return(character(0))
  tally_clean_text(vapply(x, function(v) {
    if (is.null(v) || length(v) == 0) return(NA_character_)
    as.character(v)
  }, character(1)))
}

# Find a column by header pattern (case-insensitive); returns NULL if absent
tally_find_col <- function(df, pattern) {
  i <- which(str_detect(tolower(names(df)), pattern))[1]
  if (is.na(i)) NULL else df[[i]]
}

empty_poe_tally <- function() {
  tibble(
    date = as.Date(character(0)), poe = character(0),
    entry_screened = numeric(0), exit_screened = numeric(0),
    total_typed = numeric(0), total_screened = numeric(0),
    not_split_screened = numeric(0), screening_split = logical(0),
    alerts_triggered = numeric(0), alerts_discarded = numeric(0),
    bodies_moved = numeric(0), source = character(0), source_row = integer(0)
  )
}

empty_poe_list <- function() {
  tibble(
    poe = character(0), poe_type = character(0), border_country = character(0),
    state = character(0), county = character(0),
    reporting_start = as.Date(character(0)), is_reporting = logical(0)
  )
}

empty_tally_dq <- function() {
  tibble(source = character(0), source_row = integer(0), date = as.Date(character(0)),
         poe = character(0), issue = character(0))
}

# ---------- readers ----------

read_tally_daily <- function(path = TALLY_FILE) {
  raw <- read_excel(path, sheet = TALLY_SHEET, col_types = "list", .name_repair = "minimal")
  n <- nrow(raw)
  if (n == 0) return(empty_poe_tally())
  
  num_or_na <- function(col) if (is.null(col)) rep(NA_real_, n) else tally_col_to_num(col)
  
  out <- tibble(
    date             = tally_col_to_date(tally_find_col(raw, "^date")),
    poe              = tally_col_to_chr(tally_find_col(raw, "^poe")),
    entry_screened   = num_or_na(tally_find_col(raw, "entry")),
    exit_screened    = num_or_na(tally_find_col(raw, "exit")),
    total_typed      = num_or_na(tally_find_col(raw, "total")),
    alerts_triggered = num_or_na(tally_find_col(raw, "triggered")),
    alerts_discarded = num_or_na(tally_find_col(raw, "discarded")),
    bodies_moved     = num_or_na(tally_find_col(raw, "bod|remains")),
    source           = "Daily entry",
    source_row       = seq_len(n) + 1L   # +1 for the header row = Excel row number
  )
  
  # Drop completely empty rows
  out %>%
    filter(!(is.na(date) & is.na(poe) &
               is.na(entry_screened) & is.na(exit_screened) & is.na(total_typed) &
               is.na(alerts_triggered) & is.na(alerts_discarded) & is.na(bodies_moved)))
}

read_tally_historic <- function(path = TALLY_FILE) {
  if (!HISTORIC_SHEET %in% excel_sheets(path)) return(empty_poe_tally())
  
  raw <- read_excel(path, sheet = HISTORIC_SHEET, col_types = "list", .name_repair = "minimal")
  if (nrow(raw) == 0) return(empty_poe_tally())
  
  dates <- tally_col_to_date(raw[[1]])
  get_num <- function(nm) {
    if (nm %in% names(raw)) tally_col_to_num(raw[[nm]]) else rep(NA_real_, nrow(raw))
  }
  
  imap_dfr(HISTORIC_POE_MAP, function(poe_name, prefix) {
    tibble(
      date             = dates,
      poe              = poe_name,
      entry_screened   = NA_real_,
      exit_screened    = NA_real_,
      total_typed      = get_num(paste0(prefix, "_screened_total")),
      alerts_triggered = get_num(paste0(prefix, "_alerts_triggered")),
      alerts_discarded = get_num(paste0(prefix, "_alerts_discarded")),
      bodies_moved     = NA_real_,          # not collected before 30 Sep 2026
      source           = HISTORIC_SHEET,
      source_row       = seq_len(nrow(raw)) + 1L
    )
  }) %>%
    filter(!is.na(date), date < HISTORIC_CUTOFF) %>%
    # a PoE-day with no figures at all = no report that day
    filter(!(is.na(total_typed) & is.na(alerts_triggered) & is.na(alerts_discarded)))
}

read_poe_list <- function(path = TALLY_FILE) {
  raw <- read_excel(path, sheet = POE_LIST_SHEET, col_types = "list", .name_repair = "minimal")
  if (nrow(raw) == 0) return(empty_poe_list())
  n <- nrow(raw)
  chr_or_na <- function(col) if (is.null(col)) rep(NA_character_, n) else tally_col_to_chr(col)
  
  tibble(
    poe             = chr_or_na(tally_find_col(raw, "^poe$")),
    poe_type        = chr_or_na(tally_find_col(raw, "type")),
    border_country  = chr_or_na(tally_find_col(raw, "border")),
    state           = chr_or_na(tally_find_col(raw, "^state")),
    county          = chr_or_na(tally_find_col(raw, "county")),
    latitude  = {
      col <- tally_find_col(raw, "^lat")
      if (is.null(col)) rep(NA_real_, n) else tally_col_to_num(col)
    },
    longitude = {
      col <- tally_find_col(raw, "^lon|^lng")
      if (is.null(col)) rep(NA_real_, n) else tally_col_to_num(col)
    },
    reporting_start = {
      col <- tally_find_col(raw, "start")
      if (is.null(col)) as.Date(rep(NA, n)) else tally_col_to_date(col)
    }
  ) %>%
    filter(!is.na(poe)) %>%
    distinct(poe, .keep_all = TRUE) %>%
    mutate(
      border_country = coalesce(border_country, NOT_ASSIGNED_LABEL),
      state          = coalesce(state, NOT_ASSIGNED_LABEL),
      county         = coalesce(county, NOT_ASSIGNED_LABEL)
    )
}

# ---------- download from SharePoint ----------

# Turn a SharePoint/OneDrive sharing link into a direct download link
tally_download_url <- function(url) {
  url <- trimws(url)
  if (str_detect(url, "[?&]download=1")) return(url)
  paste0(url, if (str_detect(url, "\\?")) "&" else "?", "download=1")
}


# ---------- main loader ----------

load_poe_tally <- function(path = TALLY_FILE) {
  tryCatch({
    poes <- read_poe_list(path)
    hist <- read_tally_historic(path) %>% filter(date < POE_ODK_START)   # OLD file, up to 6 Oct
    odk <- tryCatch(
      odk_to_tally(clean_poe_odk(fetch_ona_form(POE_ODK_FORM_PK))),
      error = function(e) { message("ODK PoE fetch failed: ", conditionMessage(e)); NULL }
    )
    POE_ODK_CACHE <- "poe_odk_cache.rds"
    if (!is.null(odk) && nrow(odk) > 0) {
      saveRDS(odk, POE_ODK_CACHE)                       # save every successful pull
    } else if (file.exists(POE_ODK_CACHE)) {
      message("Using last successful ODK PoE pull because the latest pull failed.")
      odk <- readRDS(POE_ODK_CACHE)                     # fall back to the last good one
    }
    
    tally <- bind_rows(hist, odk) %>%
      mutate(
        screening_split    = !is.na(entry_screened) & !is.na(exit_screened),
        total_screened     = if_else(screening_split, entry_screened + exit_screened, total_typed),
        not_split_screened = if_else(screening_split, 0, total_screened)
      )
    
    # ---- data quality checks ----
    dq <- bind_rows(
      tally %>% filter(is.na(date)) %>% mutate(issue = "Missing or unreadable date"),
      tally %>% filter(is.na(poe)) %>% mutate(issue = "Missing PoE"),
      tally %>% filter(!is.na(poe), !poe %in% poes$poe) %>%
        mutate(issue = "PoE not in PoE List"),
      tally %>% filter(!is.na(date), date > Sys.Date()) %>% mutate(issue = "Date in the future"),
      tally %>% filter(source == "Daily entry", !is.na(date), date < HISTORIC_CUTOFF) %>%
        mutate(issue = "Date before Daily entry start - check day/month order"),
      tally %>% filter(screening_split, !is.na(total_typed),
                       total_typed != entry_screened + exit_screened) %>%
        mutate(issue = "Total screened does not equal entry + exit"),
      tally %>% filter(is.na(total_screened)) %>% mutate(issue = "No screening figure"),
      tally %>% filter(coalesce(alerts_discarded, 0) > coalesce(alerts_triggered, 0)) %>%
        mutate(issue = "Alerts discarded greater than alerts triggered"),
      tally %>% filter(if_any(c(entry_screened, exit_screened, total_typed,
                                alerts_triggered, alerts_discarded, bodies_moved),
                              ~ !is.na(.x) & .x < 0)) %>%
        mutate(issue = "Negative number")
    )
    
    # Rows that cannot be used are removed after logging
    tally <- tally %>%
      filter(!is.na(date), !is.na(poe), poe %in% poes$poe, date <= Sys.Date(),
             !(source == "Daily entry" & date < HISTORIC_CUTOFF))
    
    # Duplicates: keep the last entered row for each PoE and date
    dups <- tally %>%
      group_by(poe, date) %>%
      filter(n() > 1) %>%
      arrange(source == "Daily entry", source_row, .by_group = TRUE) %>%
      slice(-n()) %>%
      ungroup() %>%
      mutate(issue = "Duplicate PoE and date - earlier row ignored")
    dq <- bind_rows(dq, dups)
    
    tally <- tally %>%
      group_by(poe, date) %>%
      arrange(source == "Daily entry", source_row, .by_group = TRUE) %>%
      slice_tail(n = 1) %>%
      ungroup() %>%
      arrange(date, poe)
    
    # Reporting start: use the PoE List date if given, otherwise the first report received
    first_report <- tally %>% group_by(poe) %>% summarise(first_report = min(date), .groups = "drop")
    poes <- poes %>%
      left_join(first_report, by = "poe") %>%
      mutate(
        reporting_start = coalesce(reporting_start, first_report),
        is_reporting    = !is.na(reporting_start) & reporting_start <= Sys.Date()
      ) %>%
      select(-first_report)
    
    dq <- dq %>%
      select(source, source_row, date, poe, issue) %>%
      arrange(source, source_row)
    
    list(tally = tally, poes = poes, dq = dq, loaded_at = Sys.time(), error = NULL)
  }, error = function(e) {
    message("PoE tally could not be loaded: ", conditionMessage(e))
    list(tally = empty_poe_tally(), poes = empty_poe_list(), dq = empty_tally_dq(),
         loaded_at = Sys.time(), error = conditionMessage(e))
  })
}

# Reporting status for each PoE (used by the reporting tile and the summary table)
#   tally     : full tally (not date-filtered), for last report
#   poes      : selected PoEs from the PoE List
#   rng       : selected date range (2 dates)
#   ref       : reference date (yesterday)
p1_reporting_status <- function(tally, poes, rng, ref) {
  period_end <- min(rng[2], ref)
  
  last_reports <- tally %>%
    filter(poe %in% poes$poe, date <= ref) %>%
    group_by(poe) %>%
    summarise(last_report = max(date), .groups = "drop")
  
  reported_in_range <- tally %>%
    filter(poe %in% poes$poe, date >= rng[1], date <= period_end) %>%
    distinct(poe, date) %>%
    count(poe, name = "days_reported")
  
  poes %>%
    select(poe, is_reporting, reporting_start) %>%
    left_join(last_reports, by = "poe") %>%
    left_join(reported_in_range, by = "poe") %>%
    mutate(
      days_reported = coalesce(days_reported, 0L),
      # days expected = days in the range on or after the PoE's reporting start
      expected_from = pmax(rng[1], reporting_start),
      days_expected = if_else(is_reporting & !is.na(expected_from) & expected_from <= period_end,
                              as.integer(period_end - expected_from) + 1L, 0L),
      completeness  = if_else(days_expected > 0, days_reported / days_expected, NA_real_),
      days_since    = as.integer(ref - last_report),
      reported_ref  = !is.na(last_report) & last_report == ref,
      silent        = is_reporting & (is.na(days_since) | days_since >= P1_SILENT_DAYS)
    ) %>%
    select(-expected_from)
}

# Small helper for tile subtitles: a sub-line and a footer line
p1_tile_subtitle <- function(title, subline = NULL, footer = NULL) {
  tagList(
    tags$div(style = "font-weight: bold; font-size: 15px;", title),
    if (!is.null(subline)) tags$div(style = "font-size: 13px;", subline),
    if (!is.null(footer))  tags$div(style = "font-size: 12px; opacity: 0.85; margin-top: 2px;", footer)
  )
}

p1_fmt <- function(x) format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)


# Number of selected PoEs expected to report on each date
# (reporting PoEs whose reporting start is on or before that date)
p1_expected_by_day <- function(poes, dates) {
  starts <- poes$reporting_start[poes$is_reporting]
  vapply(seq_along(dates), function(i) sum(starts <= dates[i], na.rm = TRUE), integer(1))
}

# One row per day from `from` to `to`, for the selected PoEs.
# Days with no reports are kept (as zeros) so gaps show in the charts.
p1_daily_series <- function(tally, poes, from, to) {
  if (is.na(from) || is.na(to) || from > to) return(tibble())
  dates <- seq(from, to, by = "day")
  
  daily <- tally %>%
    filter(poe %in% poes$poe, date >= from, date <= to) %>%
    group_by(date) %>%
    summarise(
      entry            = sum(entry_screened, na.rm = TRUE),
      exit             = sum(exit_screened, na.rm = TRUE),
      not_split        = sum(not_split_screened, na.rm = TRUE),
      total            = sum(total_screened, na.rm = TRUE),
      alerts_triggered = sum(alerts_triggered, na.rm = TRUE),
      alerts_discarded = sum(alerts_discarded, na.rm = TRUE),
      poes_reported    = n_distinct(poe),
      .groups = "drop"
    )
  
  tibble(date = dates) %>%
    left_join(daily, by = "date") %>%
    mutate(
      across(c(entry, exit, not_split, total, alerts_triggered, alerts_discarded, poes_reported),
             ~ coalesce(as.numeric(.x), 0)),
      poes_expected = p1_expected_by_day(poes, date),
      completeness  = if_else(poes_expected > 0, poes_reported / poes_expected, NA_real_)
    )
}

# Epi weeks
p1_week_start <- function(d) floor_date(d, "week", week_start = P1_WEEK_START)

p1_week_label <- function(week_start) {
  wk <- if (P1_WEEK_START == 1) isoweek(week_start) else epiweek(week_start)
  paste0("W", wk, " (", format(week_start, "%d %b"), ")")
}

# Daily series -> one row per epi week
p1_weekly_series <- function(daily, ref) {
  if (nrow(daily) == 0) return(tibble())
  daily %>%
    mutate(week = p1_week_start(date)) %>%
    group_by(week) %>%
    summarise(
      across(c(entry, exit, not_split, total, alerts_triggered, alerts_discarded,
               poes_reported, poes_expected), sum),
      .groups = "drop"
    ) %>%
    mutate(
      completeness = if_else(poes_expected > 0, poes_reported / poes_expected, NA_real_),
      in_progress  = week + 6 > ref,
      label        = p1_week_label(week)
    )
}

# Builds the series for the current view.
#   daily : selected date range (+ a few extra days before, for the rolling average)
#   weekly: whole epi weeks overlapping the selected range, up to the reference date
p1_chart_series <- function(tally, poes, rng, ref, view) {
  if (view == "weekly") {
    from <- p1_week_start(rng[1])
    to   <- min(p1_week_start(rng[2]) + 6, ref)
    p1_weekly_series(p1_daily_series(tally, poes, from, to), ref)
  } else {
    from <- rng[1]
    to   <- min(rng[2], ref)
    s <- p1_daily_series(tally, poes, from - (P1_ROLLING_DAYS - 1), to)
    if (nrow(s) == 0) return(s)
    s %>%
      mutate(rolling = as.numeric(stats::filter(total, rep(1 / P1_ROLLING_DAYS, P1_ROLLING_DAYS),
                                                sides = 1))) %>%
      filter(date >= from)
  }
}

# Alerts not yet discarded (running total) as of each date in `dates`:
# all alerts triggered minus all alerts discarded, from the start of the data
# up to and including that date, for the selected PoEs.
p1_backlog_at <- function(tally, poes, dates) {
  cum <- tally %>%
    filter(poe %in% poes$poe) %>%
    group_by(date) %>%
    summarise(net = sum(alerts_triggered, na.rm = TRUE) - sum(alerts_discarded, na.rm = TRUE),
              .groups = "drop") %>%
    arrange(date) %>%
    mutate(backlog = cumsum(net))
  
  if (nrow(cum) == 0) return(rep(0, length(dates)))
  idx <- findInterval(as.numeric(dates), as.numeric(cum$date))
  out <- ifelse(idx == 0, 0, cum$backlog[pmax(idx, 1)])
  pmax(out, 0)
}

# Sum that stays NA (shown blank) when nothing was recorded,
# e.g. exit screening for PoEs that only report a total
p1_sum_or_na <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
 
# Coloured status dot + label for the summary table
p1_status_badge <- function(colour, label) {
  paste0('<span style="display:inline-block;width:10px;height:10px;border-radius:50%;',
         'background:', colour, ';margin-right:6px;"></span>', label)
}

JIA_POE_NAME <- "Juba International Airport"   # must match the PoE List exactly

# Which clearance log entries count as "discarded" (assessed, not a suspected case):
# everything except alerts referred for action or marked as meeting the case definition
p1_is_discarded <- function(action, case_def) {
  action   <- tolower(coalesce(as.character(action), ""))
  case_def <- tolower(coalesce(as.character(case_def), ""))
  !str_detect(action, "referred") & case_def != "yes"
}

# JIA individual records (Ona) -> daily tally rows in the same format as the workbook
#   jia           : standardised JIA data (evd_data() in app.r)
#   clearance_log : alert clearance log (alert_clearance$clearance_log() in app.r)
# Screened   = number of screening records per day (entry; JIA screens arrivals)
# Triggered  = records flagged as an active EVD alert, by screening date
# Discarded  = alerts cleared as not a suspected case, by date of clearance
p1_jia_tally <- function(jia, clearance_log = NULL) {
  if (is.null(jia) || nrow(jia) == 0) return(empty_poe_tally())
  
  jia <- jia %>%
    filter(!is.na(submission_date), submission_date <= Sys.Date()) %>%
    jia_add_flags() %>%
    mutate(is_alert = poe_alert)
  
  screened <- jia %>%
    group_by(date = submission_date) %>%
    summarise(entry_screened = n(), alerts_triggered = sum(is_alert), .groups = "drop")
  
  discarded <- tibble(date = as.Date(character(0)), alerts_discarded = integer(0))
  if (!is.null(clearance_log) && nrow(clearance_log) > 0) {
    alert_ids <- jia$screening_id[jia$is_alert]
    discarded <- clearance_log %>%
      filter(screening_id %in% alert_ids,
             p1_is_discarded(clearance_action, case_definition_met)) %>%
      mutate(date = as.Date(substr(as.character(cleared_at), 1, 10))) %>%
      filter(!is.na(date)) %>%
      count(date, name = "alerts_discarded")
  }
  
  full_join(screened, discarded, by = "date") %>%
    transmute(
      date,
      poe                = JIA_POE_NAME,
      entry_screened     = as.numeric(coalesce(entry_screened, 0L)),
      exit_screened      = NA_real_,
      total_typed        = NA_real_,
      total_screened     = entry_screened,
      not_split_screened = 0,
      screening_split    = TRUE,
      alerts_triggered   = as.numeric(coalesce(alerts_triggered, 0L)),
      alerts_discarded   = as.numeric(coalesce(alerts_discarded, 0L)),
      bodies_moved       = NA_real_,
      source             = "JIA (Ona)",
      source_row         = NA_integer_
    ) %>%
    arrange(date)
}

# Replace any JIA rows from the workbook with the Ona-derived rows,
# and set JIA's reporting start from its first record
p1_add_jia <- function(d, jia_rows) {
  if (is.null(jia_rows) || nrow(jia_rows) == 0) return(d)
  
  d$tally <- d$tally %>%
    filter(poe != JIA_POE_NAME) %>%
    bind_rows(jia_rows) %>%
    arrange(date, poe)
  
  first_jia <- min(jia_rows$date)
  d$poes <- d$poes %>%
    mutate(
      reporting_start = if_else(poe == JIA_POE_NAME, coalesce(reporting_start, first_jia), reporting_start),
      is_reporting    = !is.na(reporting_start) & reporting_start <= Sys.Date()
    )
  d
}


# ============================================================
# PAGE 1 SERVER: NATIONAL POE OVERVIEW
# ============================================================

# Download + load in one go (used at refresh)
p1_reload <- function() {
  d <- load_poe_tally(f$path)
  d$downloaded <- f$downloaded
  d
}

# TRUE when the loaded data is older than the refresh interval
p1_needs_refresh <- function(d) {
  age_ms <- as.numeric(difftime(Sys.time(), d$loaded_at, units = "secs")) * 1000
  age_ms >= P1_REFRESH_MS
}

# Work out what should stay selected when a picker's choices change:
# - if everything was selected before, select everything that is now available
# - otherwise keep the selections that are still available
p1_keep_selection <- function(new_choices, current, prev_choices) {
  if (is.null(current) || setequal(current, prev_choices)) return(new_choices)
  intersect(current, new_choices)
}


# A PoE can border more than one country, e.g. "DRC, CAR" or "DRC, Uganda".
# Split those into separate countries for the filter.
p1_split_border <- function(x) {
  lapply(str_split(coalesce(x, ""), "\\s*[,;/]\\s*|\\s+and\\s+"), function(v) {
    v <- str_squish(v)
    v <- v[v != ""]
    if (length(v) == 0) NOT_ASSIGNED_LABEL else v
  })
}

# Sorted list of individual bordering countries (for the filter choices)
p1_border_choices <- function(poes) sort(unique(unlist(p1_split_border(poes$border_country))))

# TRUE for PoEs that border at least one of the selected countries
p1_border_match <- function(border_country, selected) {
  vapply(p1_split_border(border_country), function(v) any(v %in% selected), logical(1))
}

# Apply the geography filters (empty selection = all)
p1_filter_poes <- function(poes, border = NULL, state = NULL, county = NULL, poe = NULL) {
  if (length(border)) poes <- poes[p1_border_match(poes$border_country, border), , drop = FALSE]
  if (length(state))  poes <- poes[poes$state  %in% state,  , drop = FALSE]
  if (length(county)) poes <- poes[poes$county %in% county, , drop = FALSE]
  if (length(poe))    poes <- poes[poes$poe    %in% poe,    , drop = FALSE]
  poes
}


# ---------- map layers ----------
P1_MAP_DIR <- "data"

p1_read_layer <- function(file) {
  path <- file.path(P1_MAP_DIR, file)
  if (file.exists(path)) readRDS(path) else NULL
}

P1_MAP_LAYERS <- list(
  states   = p1_read_layer("ssd_states.rds"),
  counties = p1_read_layer("ssd_counties.rds"),
  drc_hz   = p1_read_layer("drc_border_health_zones.rds")
)

# Pick the column holding the area name, by pattern; falls back to the first column.
# If a label comes out wrong, put the right column name in the matching setting below.
P1_NAME_COLS <- c(states = "", counties = "", drc_hz = "")   # e.g. c(states = "admin1Name", ...)

p1_area_names <- function(layer, which, pattern) {
  if (is.null(layer)) return(character(0))
  cols <- setdiff(names(layer), attr(layer, "sf_column"))
  col  <- P1_NAME_COLS[[which]]
  if (!nzchar(col) || !col %in% cols) {
    hit <- cols[str_detect(tolower(cols), pattern)]
    col <- if (length(hit)) hit[1] else cols[1]
  }
  as.character(sf::st_drop_geometry(layer)[[col]])
}

P1_STATUS_COLOURS <- c("1" = "#dd4b39",   # open alerts
                       "2" = "#f39c12",   # not reported 3+ days
                       "3" = "#1f1f1f",   # reporting, no open alerts
                       "4" = "#9e9e9e")   # not yet reporting




poe_overview_server <- function(input, output, session, jia_data = NULL, clearance_log = NULL, cleared_ids = NULL) {
  
  # ---- 4a. Data: workbook (refreshed from SharePoint) + JIA (from Ona) ----
  
  # Workbook: starts from the data loaded when the app started; reloads when it
  # is older than P1_REFRESH_MS (checked every 5 minutes, and when a session opens).
  p1_workbook <- reactiveVal(poe_tally_initial)
  
  observe({
    invalidateLater(5 * 60 * 1000, session)
    if (p1_needs_refresh(isolate(p1_workbook()))) {
      new_data <- p1_reload()
      if (is.null(new_data$error)) {
        p1_workbook(new_data)
      } else {
        # keep the last good data, but record that the update failed
        old <- isolate(p1_workbook())
        old$downloaded <- FALSE
        old$loaded_at  <- Sys.time()
        p1_workbook(old)
      }
    }
  })
  
  # JIA: daily figures derived from the Ona records (updates with every Ona refresh)
  p1_jia <- reactive({
    if (is.null(jia_data)) return(empty_poe_tally())
    log <- if (is.null(clearance_log)) NULL else clearance_log()
    p1_jia_tally(jia_data(), log)
  })
  
  # Combined data used by everything on page 1
  p1_data <- reactive({
    p1_add_jia(p1_workbook(), p1_jia())
  })
  
  
  # ---- 4b. Cascading filters: bordering country -> state -> county -> PoE ----
  p1_prev <- reactiveValues(
    border = p1_border_choices(poe_tally_initial$poes),
    state  = sort(unique(poe_tally_initial$poes$state)),
    county = sort(unique(poe_tally_initial$poes$county)),
    poe    = sort(poe_tally_initial$poes$poe)
  )
  
  # Update one picker's choices, keeping sensible selections
  p1_update_choices <- function(id, choices) {
    if (!identical(choices, p1_prev[[id]])) {
      selected <- p1_keep_selection(choices, isolate(input[[paste0("p1_", id)]]), p1_prev[[id]])
      updatePickerInput(session, paste0("p1_", id), choices = choices, selected = selected)
      p1_prev[[id]] <- choices
    }
  }
  
  # Bordering country choices follow the PoE List
  observeEvent(p1_data(), {
    p1_update_choices("border", p1_border_choices(p1_data()$poes))
  }, ignoreInit = TRUE)
  
  # State: narrowed by bordering country
  observeEvent(list(input$p1_border, p1_data()), {
    poes <- p1_filter_poes(p1_data()$poes, border = input$p1_border)
    p1_update_choices("state", sort(unique(poes$state)))
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  # County: narrowed by bordering country and state
  observeEvent(list(input$p1_border, input$p1_state, p1_data()), {
    poes <- p1_filter_poes(p1_data()$poes, border = input$p1_border, state = input$p1_state)
    p1_update_choices("county", sort(unique(poes$county)))
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  # PoE: narrowed by bordering country, state and county
  observeEvent(list(input$p1_border, input$p1_state, input$p1_county, p1_data()), {
    poes <- p1_filter_poes(p1_data()$poes, border = input$p1_border,
                           state = input$p1_state, county = input$p1_county)
    p1_update_choices("poe", sort(poes$poe))
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  # Reset filters
  observeEvent(input$p1_reset, {
    poes <- p1_data()$poes
    all_choices <- list(
      border = p1_border_choices(poes),
      state  = sort(unique(poes$state)),
      county = sort(unique(poes$county)),
      poe    = sort(poes$poe)
    )
    for (id in names(all_choices)) {
      updatePickerInput(session, paste0("p1_", id), choices = all_choices[[id]], selected = all_choices[[id]])
      p1_prev[[id]] <- all_choices[[id]]
    }
    dflt <- p1_default_dates(p1_data()$tally)
    updateDateRangeInput(session, "p1_dates", start = dflt[1], end = dflt[2])
    updateRadioGroupButtons(session, "p1_view", selected = P1_DEFAULT_VIEW)
  })
  
  # ---- 4c. Filtered data used by the tiles, charts and table ----
  # Nothing selected in a filter is treated as "all".
  p1_selected_poes <- reactive({
    p1_filter_poes(p1_data()$poes, border = input$p1_border, state = input$p1_state,
                   county = input$p1_county, poe = input$p1_poe)
  })
  
  
  
  p1_date_range <- reactive({
    d <- input$p1_dates
    shiny::validate(shiny::need(length(d) == 2 && !anyNA(d), "Choose a start and end date."))
    sort(as.Date(d))
  })
  
  # Selected PoEs, selected date range
  p1_tally <- reactive({
    rng <- p1_date_range()
    p1_data()$tally %>%
      filter(poe %in% p1_selected_poes()$poe, date >= rng[1], date <= rng[2])
  })
  
  # Selected PoEs, last 7 days up to the reference date (ignores the date filter)
  p1_tally_last7 <- reactive({
    ref <- p1_reference_date()
    p1_data()$tally %>%
      filter(poe %in% p1_selected_poes()$poe, date > ref - 7, date <= ref)
  })
  
  # ---- 4d. "Data as of" line ----
  output$p1_data_as_of <- renderUI({
    d      <- p1_data()
    sel    <- p1_selected_poes()
    ref    <- p1_reference_date()
    latest <- suppressWarnings(max(d$tally$date[d$tally$poe %in% sel$poe]))
    
    parts <- list(
      tags$strong(paste0("Data as of ", format(ref, "%d-%b-%Y"))),
      paste0(" \u00b7 ", sum(sel$is_reporting), " of ", nrow(sel), " selected PoEs currently reporting")
    )
    if (is.finite(latest) && latest < ref) {
      parts <- c(parts, paste0(" \u00b7 Latest report: ", format(latest, "%d-%b-%Y")))
    }
    if (!isTRUE(d$downloaded)) {
      parts <- c(parts, list(tags$span(
        style = "color: #dd4b39; margin-left: 6px;",
        icon("exclamation-triangle"),
        paste0(" Using saved copy loaded ", format(d$loaded_at, "%d-%b %H:%M"),
               " \u2013 SharePoint could not be reached")
      )))
    }
    tags$span(parts)
  })
  
  # ---- 5. Headline tiles ----
  
  # Reporting status for the selected PoEs and date range
  p1_status <- reactive({
    p1_reporting_status(p1_data()$tally, p1_selected_poes(), p1_date_range(), p1_reference_date())
  })
  
  # Rows for the reference date (yesterday) only
  p1_tally_ref <- reactive({
    p1_data()$tally %>%
      filter(poe %in% p1_selected_poes()$poe, date == p1_reference_date())
  })
  
  # C1. Travellers screened
  output$p1_vb_screened <- renderValueBox({
    d   <- p1_tally()
    ref <- p1_tally_ref()
    not_split <- sum(d$not_split_screened, na.rm = TRUE)
    
    subline <- paste0(
      "Entry ", p1_fmt(sum(d$entry_screened, na.rm = TRUE)),
      " \u00b7 Exit ", p1_fmt(sum(d$exit_screened, na.rm = TRUE)),
      if (not_split > 0) paste0(" \u00b7 Not split ", p1_fmt(not_split)) else ""
    )
    
    valueBox(
      value    = p1_fmt(sum(d$total_screened, na.rm = TRUE)),
      subtitle = p1_tile_subtitle(
        "Travellers screened",
        subline,
        paste0("Yesterday: ", p1_fmt(sum(ref$total_screened, na.rm = TRUE)))
      ),
      icon  = icon("users"),
      color = "navy"
    )
  })
  
  # C2. Alerts
  output$p1_vb_alerts <- renderValueBox({
    d   <- p1_tally()
    ref <- p1_tally_ref()
    triggered     <- sum(d$alerts_triggered, na.rm = TRUE)
    discarded     <- sum(d$alerts_discarded, na.rm = TRUE)
    not_discarded <- max(triggered - discarded, 0)
    
    valueBox(
      value    = p1_fmt(triggered),
      subtitle = p1_tile_subtitle(
        "Alerts triggered",
        paste0("Discarded ", p1_fmt(discarded), " \u00b7 Not discarded ", p1_fmt(not_discarded)),
        paste0("Yesterday: ", p1_fmt(sum(ref$alerts_triggered, na.rm = TRUE)), " triggered")
      ),
      icon  = icon("bell"),
      color = if (not_discarded > 0) "red" else "green"
    )
  })
  
  # C3. Bodies moved across the border
  output$p1_vb_bodies <- renderValueBox({
    d   <- p1_tally()
    ref <- p1_tally_ref()
    with_bodies <- d %>% filter(coalesce(bodies_moved, 0) > 0) %>% distinct(poe) %>% nrow()
    
    # Bodies were only recorded from the start of the Daily entry form
    footer <- paste0("Yesterday: ", p1_fmt(sum(ref$bodies_moved, na.rm = TRUE)))
    if (p1_date_range()[1] < HISTORIC_CUTOFF) {
      footer <- paste0(footer, " \u00b7 Recorded from ", format(HISTORIC_CUTOFF, "%d-%b-%Y"))
    }
    
    valueBox(
      value    = p1_fmt(sum(d$bodies_moved, na.rm = TRUE)),
      subtitle = p1_tile_subtitle(
        "Bodies moved across the border",
        paste0("At ", with_bodies, if (with_bodies == 1) " PoE" else " PoEs"),
        footer
      ),
      icon  = icon("dove"),
      color = "light-blue"
    )
  })
  
  # C4. Reporting
  output$p1_vb_reporting <- renderValueBox({
    s <- p1_status() %>% filter(is_reporting)
    n_reporting <- nrow(s)
    n_ref       <- sum(s$reported_ref)
    n_silent    <- sum(s$silent)
    completeness <- if (sum(s$days_expected) > 0) {
      sum(s$days_reported) / sum(s$days_expected)
    } else NA_real_
    
    ok <- !is.na(completeness) && completeness >= P1_COMPLETENESS_MIN && n_silent == 0
    
    valueBox(
      value    = paste0(n_ref, " / ", n_reporting),
      subtitle = p1_tile_subtitle(
        paste0("PoEs reported on ", format(p1_reference_date(), "%d-%b-%Y")),
        paste0("Completeness ",
               if (is.na(completeness)) "n/a" else paste0(round(100 * completeness), "%"),
               " for selected period"),
        paste0("Silent ", P1_SILENT_DAYS, "+ days: ", n_silent,
               if (n_silent == 1) " PoE" else " PoEs")
      ),
      icon  = icon("clipboard-check"),
      color = if (ok) "green" else "yellow"
    )
  })
  
  # ---- 6. Chart: travellers screened over time ----
  
  p1_view <- reactive({
    if (is.null(input$p1_view)) P1_DEFAULT_VIEW else input$p1_view
  })
  
  # Shared by both charts (step 7 reuses it)
  p1_series <- reactive({
    p1_chart_series(p1_data()$tally, p1_selected_poes(), p1_date_range(),
                    p1_reference_date(), p1_view())
  })
  
  output$p1_screened_plot <- renderPlotly({
    s      <- p1_series()
    weekly <- p1_view() == "weekly"
    
    if (nrow(s) == 0 || sum(s$total) == 0) {
      return(empty_plotly("No screening data for the selected PoEs and dates"))
    }
    
    # Faded bars: incomplete reporting (daily) or week in progress (weekly)
    if (weekly) {
      x       <- factor(s$label, levels = s$label)
      faded   <- s$in_progress
      heading <- paste0(s$label, ": ", format(s$week, "%d %b"), " \u2013 ",
                        format(s$week + 6, "%d %b %Y"),
                        ifelse(s$in_progress, " (week in progress)", ""))
      reporting_line <- paste0(
        "Reports received: ",
        ifelse(is.na(s$completeness), "n/a", paste0(round(100 * s$completeness), "%"))
      )
    } else {
      x       <- s$date
      faded   <- !is.na(s$completeness) & s$completeness < P1_COMPLETENESS_MIN
      heading <- format(s$date, "%a %d %b %Y")
      reporting_line <- paste0("PoEs reporting: ", s$poes_reported, " of ", s$poes_expected)
    }
    opacity <- ifelse(faded, 0.4, 1)
    
    hover <- paste0(
      "<b>", heading, "</b>",
      "<br>Entry: ", p1_fmt(s$entry),
      "<br>Exit: ", p1_fmt(s$exit),
      ifelse(s$not_split > 0, paste0("<br>Not split: ", p1_fmt(s$not_split)), ""),
      "<br>Total: ", p1_fmt(s$total),
      "<br>", reporting_line
    )
    
    p <- plot_ly()
    bar_parts <- list(
      list(col = "entry",     name = "Entry screened"),
      list(col = "exit",      name = "Exit screened"),
      list(col = "not_split", name = "Total (not split)")
    )
    for (bp in bar_parts) {
      if (sum(s[[bp$col]]) > 0) {
        p <- p %>% add_bars(
          x = x, y = s[[bp$col]], name = bp$name,
          marker = list(color = P1_COLOURS[[bp$col]], opacity = opacity),
          text = hover, hoverinfo = "text", textposition = "none"
        )
      }
    }
    

    
    if (!weekly) {
      p <- p %>% add_lines(
        x = x, y = s$rolling, name = paste0(P1_ROLLING_DAYS, "-day average"),
        line = list(color = P1_COLOURS[["rolling"]], width = 2),
        hoverinfo = "skip"
      )
    }

    xaxis <- if (weekly) {
      list(title = "Epi week", type = "category", tickangle = -45, automargin = TRUE)
    } else {
      list(title = "", type = "date", tickformat = "%d %b",
           dtick = 7 * 24 * 60 * 60 * 1000, tick0 = as.character(min(s$date)))
    }
    
    annotations <- list()
    shapes      <- list()
    if (weekly) {
      # Vertical total above each bar
      annotations <- lapply(seq_len(nrow(s)), function(i) list(
        x = as.character(x[i]), y = s$total[i], text = p1_fmt(s$total[i]),
        textangle = -90, xanchor = "center", yanchor = "bottom", yshift = 4,
        showarrow = FALSE, font = list(size = 11, color = "#333")
      ))
      # Shade the week in progress, with a small label at the top
      if (any(s$in_progress)) {
        i <- which(s$in_progress)[1] - 1          # position of the week (0-based)
        shapes <- list(list(
          type = "rect", xref = "x", yref = "paper",
          x0 = i - 0.5, x1 = i + 0.5, y0 = 0, y1 = 1,
          fillcolor = "rgba(0,0,0,0.07)", line = list(width = 0), layer = "below"
        ))
        annotations <- c(annotations, list(list(
          x = as.character(x[i + 1]), y = 1, yref = "paper", yanchor = "top",
          text = "week in<br>progress", showarrow = FALSE,
          font = list(size = 10, color = "#666")
        )))
      }
    }
    
    p %>%
      layout(
        barmode = "stack",
        xaxis = xaxis,
        yaxis = if (weekly) {
          list(title = "Travellers screened", tickformat = ",d", range = c(0, max(s$total) * 1.3))
        } else {
          list(title = "Travellers screened", tickformat = ",d", rangemode = "tozero")
        },
        legend = list(orientation = "h", x = 0, y = 1.12),
        annotations = annotations,
        shapes = shapes,
        margin = list(t = 40)
      ) %>%
      chart_download_config("PoE_travellers_screened")  

  })
  
  # ---- 7. Chart: alerts over time ----
  
  output$p1_alerts_plot <- renderPlotly({
    s      <- p1_series()
    weekly <- p1_view() == "weekly"
    ref    <- p1_reference_date()
    
    if (nrow(s) == 0 || sum(s$total) == 0) {
      return(empty_plotly("No screening data for the selected PoEs and dates"))
    }
    
    # Running total of alerts not yet discarded, as of each bar
    as_of   <- if (weekly) pmin(s$week + 6, ref) else s$date
    backlog <- p1_backlog_at(p1_data()$tally, p1_selected_poes(), as_of)
    
    if (weekly) {
      x       <- factor(s$label, levels = s$label)
      faded   <- s$in_progress
      heading <- paste0(s$label, ": ", format(s$week, "%d %b"), " \u2013 ",
                        format(s$week + 6, "%d %b %Y"),
                        ifelse(s$in_progress, " (week in progress)", ""))
      reporting_line <- paste0(
        "Reports received: ",
        ifelse(is.na(s$completeness), "n/a", paste0(round(100 * s$completeness), "%"))
      )
      # Alert rate only for weeks with enough travellers screened
      rate_line <- ifelse(
        s$total >= P1_RATE_MIN_SCREENED,
        paste0("<br>Alerts per ", p1_fmt(P1_RATE_PER), " screened: ",
               formatC(s$alerts_triggered / s$total * P1_RATE_PER, format = "f", digits = 1)),
        paste0("<br>Alert rate not shown (fewer than ", p1_fmt(P1_RATE_MIN_SCREENED), " screened)")
      )
    } else {
      x       <- s$date
      faded   <- !is.na(s$completeness) & s$completeness < P1_COMPLETENESS_MIN
      heading <- format(s$date, "%a %d %b %Y")
      reporting_line <- paste0("PoEs reporting: ", s$poes_reported, " of ", s$poes_expected)
      rate_line <- ""
    }
    
    hover <- paste0(
      "<b>", heading, "</b>",
      "<br>Alerts triggered: ", p1_fmt(s$alerts_triggered),
      "<br>Travellers screened: ", p1_fmt(s$total),
      rate_line,
      "<br>", reporting_line,
      "<br>Not yet discarded (running total): ", p1_fmt(backlog)
    )
    
    y_max <- max(1, s$alerts_triggered) * 1.2
    yaxis <- list(title = "Alerts triggered", tickformat = ",d",
                  range = c(0, y_max), rangemode = "tozero")
    if (y_max <= 10) yaxis$dtick <- 1   # whole numbers only when counts are small
    
    xaxis <- if (weekly) {
      list(title = "Epi week", type = "category", tickangle = -45)
    } else {
      list(title = "", type = "date", tickformat = "%d %b",
           dtick = 7 * 24 * 60 * 60 * 1000, tick0 = as.character(min(s$date)))
    }
    
    annotations <- list()
    if (sum(s$alerts_triggered) == 0) {
      annotations <- c(annotations, list(list(
        text = "No alerts triggered in the selected period",
        xref = "paper", yref = "paper", x = 0.5, y = 0.6,
        showarrow = FALSE, font = list(size = 13, color = "#666")
      )))
    }
    if (weekly && any(s$in_progress)) {
      i <- which(s$in_progress)[1]
      annotations <- c(annotations, list(list(
        x = as.character(x[i]), y = s$alerts_triggered[i], text = "week in progress",
        showarrow = FALSE, yshift = 12, font = list(size = 11, color = "#666")
      )))
    }
    
    plot_ly() %>%
      add_bars(
        x = x, y = s$alerts_triggered, name = "Alerts triggered",
        marker = list(color = P1_COLOURS[["alerts"]]),
        text = if (weekly) ifelse(s$alerts_triggered > 0, p1_fmt(s$alerts_triggered), "") else NULL,
        textposition = if (weekly) "outside" else "none", cliponaxis = FALSE,
        hovertext = hover, hoverinfo = "text"
      ) %>%
      layout(
        xaxis = xaxis,
        yaxis = yaxis,
        showlegend = FALSE,
        annotations = annotations,
        margin = list(t = 20)
      ) %>%
      chart_download_config("PoE_alerts")
  })
  
  
  # ---- Open alerts per PoE (used by the flashing box and the summary table) ----
  # All dates up to the reference date, for the PoEs in the geography filters:
  #   Tally PoEs: alerts triggered - alerts discarded (running total, min 0)
  #   JIA:        PoE alerts not yet reviewed on page 3 (any review outcome closes them)
  p1_open_alerts <- reactive({
    sel <- p1_selected_poes()$poe
    ref <- p1_reference_date()
    
    open_tally <- p1_workbook()$tally %>%
      filter(poe %in% sel, poe != JIA_POE_NAME, date <= ref) %>%
      group_by(poe) %>%
      summarise(
        open       = max(sum(alerts_triggered, na.rm = TRUE) - sum(alerts_discarded, na.rm = TRUE), 0),
        last_alert = suppressWarnings(max(date[coalesce(alerts_triggered, 0) > 0])),
        .groups = "drop"
      ) %>%
      filter(open > 0) %>%
      mutate(where = "tally")
    
    open_jia <- tibble(poe = character(0), open = numeric(0), last_alert = as.Date(character(0)), where = character(0))
    if (JIA_POE_NAME %in% sel && !is.null(jia_data)) {
      j <- jia_data()
      if (!is.null(j) && nrow(j) > 0) {
        cleared <- if (is.null(cleared_ids)) character(0) else cleared_ids()
        jo <- jia_add_flags(j) %>% filter(poe_alert | high_risk, !screening_id %in% cleared)
        if (nrow(jo) > 0) {
          open_jia <- tibble(poe = JIA_POE_NAME, open = nrow(jo),
                             last_alert = max(jo$submission_date, na.rm = TRUE), where = "jia")
        }
      }
    }
    
    bind_rows(open_tally, open_jia)
  })
  
  
  # ---- 8. PoE summary table ----
  
  p1_summary <- reactive({
    status <- p1_status()
    
    # Totals for the selected date range
    period <- p1_tally() %>%
      group_by(poe) %>%
      summarise(
        entry     = p1_sum_or_na(entry_screened),
        exit      = p1_sum_or_na(exit_screened),
        total     = p1_sum_or_na(total_screened),
        triggered = p1_sum_or_na(alerts_triggered),
        discarded = p1_sum_or_na(alerts_discarded),
        bodies    = p1_sum_or_na(bodies_moved),
        .groups = "drop"
      )
    
    # Open alerts, all dates (same figures as the flashing box; drives the red status)
    open_alerts <- p1_open_alerts() %>% select(poe, open_alerts = open)
    
    status %>%
      left_join(period, by = "poe") %>%
      left_join(open_alerts, by = "poe") %>%
      mutate(
        open_alerts   = coalesce(open_alerts, 0),
        flag_alerts   = open_alerts > 0,
        flag_silent   = is_reporting & silent,
        status_rank   = case_when(flag_alerts ~ 1L, !is_reporting ~ 4L, flag_silent ~ 2L, TRUE ~ 3L),
        status_label  = case_when(
          flag_alerts & flag_silent  ~ p1_status_badge("#dd4b39", paste0("Open alerts; not reported ", P1_SILENT_DAYS, "+ days")),
          flag_alerts                ~ p1_status_badge("#dd4b39", "Open alerts"),
          !is_reporting              ~ p1_status_badge("#bfbfbf", "Not yet reporting"),
          flag_silent                ~ p1_status_badge("#f39c12", paste0("Not reported ", P1_SILENT_DAYS, "+ days")),
          TRUE                       ~ p1_status_badge("#00a65a", "OK")
        )
      ) %>%
      arrange(status_rank, desc(open_alerts), desc(coalesce(total, -1)), poe)
  })
  
  output$p1_summary_table <- renderDT({
    s <- p1_summary()
    
    display <- s %>%
      transmute(
        Status                  = status_label,
        PoE                     = poe,
        `Last report`           = if_else(is.na(last_report), "Never", format(last_report, "%d-%b-%Y")),
        `Days since report`     = days_since,
        `Entry screened`        = entry,
        `Exit screened`         = exit,
        `Total screened`        = total,
        `Alerts triggered`      = triggered,
        `Alerts discarded`      = discarded,
        `Open alerts (all dates)` = open_alerts,
        `Bodies moved across the border` = bodies,
        Completeness            = completeness,
        rank                    = status_rank
      )
    
    # Totals row (shown in the table footer)
    rep_rows <- s %>% filter(is_reporting)
    total_completeness <- if (sum(rep_rows$days_expected) > 0) {
      paste0(round(100 * sum(rep_rows$days_reported) / sum(rep_rows$days_expected)), "%")
    } else ""
    footer <- c(
      "", "All selected PoEs", "", "",
      p1_fmt(sum(s$entry, na.rm = TRUE)), p1_fmt(sum(s$exit, na.rm = TRUE)),
      p1_fmt(sum(s$total, na.rm = TRUE)), p1_fmt(sum(s$triggered, na.rm = TRUE)),
      p1_fmt(sum(s$discarded, na.rm = TRUE)), p1_fmt(sum(s$open_alerts, na.rm = TRUE)),
      p1_fmt(sum(s$bodies, na.rm = TRUE)), total_completeness, ""
    )
    sketch <- tags$table(
      class = "display",
      tags$thead(tags$tr(lapply(names(display), function(v) tags$th(v)))),
      tags$tfoot(tags$tr(lapply(footer, function(v) tags$th(v))))
    )
    
    rank_col  <- which(names(display) == "rank") - 1   # DT columns are 0-indexed
    open_col  <- which(names(display) == "Open alerts (all dates)") - 1
    total_col <- which(names(display) == "Total screened") - 1
    
    datatable(
      display,
      container  = sketch,
      rownames   = FALSE,
      escape     = -1,                 # allow the coloured dot in the Status column
      extensions = "Buttons",
      options = list(
        dom        = "Bfrt",
        paging     = FALSE,
        scrollX    = TRUE,
        order      = list(list(rank_col, "asc"), list(open_col, "desc"), list(total_col, "desc")),
        columnDefs = list(list(visible = FALSE, targets = rank_col)),
        buttons    = list(list(
          extend = "csv", text = "Download CSV",
          filename = paste0("PoE_summary_", format(p1_reference_date(), "%Y%m%d")),
          exportOptions = list(columns = ":visible")
        ))
      )
    ) %>%
      formatRound(c("Entry screened", "Exit screened", "Total screened", "Alerts triggered",
                    "Alerts discarded", "Open alerts (all dates)", "Bodies moved across the border"),
                  digits = 0, mark = ",") %>%
      formatPercentage("Completeness", digits = 0) %>%
      formatStyle("Open alerts (all dates)",
                  backgroundColor = styleInterval(0, c("transparent", "#f8d7d3"))) %>%
      formatStyle("Days since report",
                  backgroundColor = styleInterval(P1_SILENT_DAYS - 0.5, c("transparent", "#fdebd0"))) %>%
      formatStyle("Completeness",
                  backgroundColor = styleInterval(P1_COMPLETENESS_MIN - 0.0001, c("#fdebd0", "transparent")))
  }, server = FALSE)
  
  
  # Data quality note under the table
  output$p1_dq_note <- renderUI({
    rng <- p1_date_range()
    dq  <- p1_data()$dq %>%
      filter(is.na(poe) | poe %in% p1_selected_poes()$poe,
             is.na(date) | (date >= rng[1] & date <= rng[2]))
    
    if (nrow(dq) == 0) {
      return(div(class = "p1-dq-note", icon("check"), " No data quality issues in the selected period."))
    }
    
    div(
      class = "p1-dq-note",
      tags$details(
        tags$summary(
          style = "cursor: pointer; color: #b9770e;",
          icon("exclamation-triangle"),
          paste0(" ", nrow(dq), " data quality ", if (nrow(dq) == 1) "issue" else "issues",
                 " in the workbook (click to view)")
        ),
        tags$table(
          class = "table table-condensed", style = "margin-top: 6px; font-size: 12px;",
          tags$thead(tags$tr(tags$th("Sheet"), tags$th("Row"), tags$th("Date"),
                             tags$th("PoE"), tags$th("Issue"))),
          tags$tbody(lapply(seq_len(nrow(dq)), function(i) tags$tr(
            tags$td(dq$source[i]), tags$td(dq$source_row[i]),
            tags$td(if (is.na(dq$date[i])) "" else format(dq$date[i], "%d-%b-%Y")),
            tags$td(coalesce(dq$poe[i], "")), tags$td(dq$issue[i])
          )))
        )
      )
    )
  })
  # ---- 9. Open alerts box (flashes while any alert is open) ----
  # Uses p1_open_alerts() (geography filters apply; date filter ignored)
  output$p1_alert_box <- renderUI({
    open <- p1_open_alerts() %>% filter(open > 0) %>% arrange(desc(last_alert))
    if (nrow(open) == 0) return(NULL)
    
    total <- sum(open$open)
    lines <- lapply(seq_len(nrow(open)), function(i) {
      tags$li(
        tags$b(open$poe[i]), ": ", p1_fmt(open$open[i]),
        if (open$open[i] == 1) " open alert" else " open alerts",
        if (is.finite(open$last_alert[i])) paste0(" (latest alert ", format(open$last_alert[i], "%d-%b-%Y"), ")") else "",
        if (open$where[i] == "jia") " \u2013 review on page 3" else ""
      )
    })
    
    div(
      class = "p1-alert-flash",
      tags$div(style = "font-weight: bold; font-size: 18px;",
               icon("bell"), paste0(" ", p1_fmt(total), if (total == 1) " OPEN ALERT" else " OPEN ALERTS")),
      tags$ul(style = "margin: 6px 0; padding-left: 20px;", lines),
      tags$div(style = "font-size: 13px;",
               "Alerts stay open until closed. PoEs: record the outcome in the daily tally (alerts discarded). ",
               "JIA: review and clear on page 3.")
    )
  })
  
  # ---- 10. Map ----
  
  # Base map: drawn once (boundaries don't change with the filters)
  output$p1_map <- renderLeaflet({
    L <- P1_MAP_LAYERS
    m <- leaflet(options = leafletOptions(minZoom = 5)) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      setView(lng = 30.5, lat = 7.3, zoom = 6)
    
    m <- leaflet(options = leafletOptions(minZoom = 5)) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      setView(lng = 30.5, lat = 7.3, zoom = 6) %>%
      addMapPane("states_pane",   zIndex = 380) %>%   # bottom
      addMapPane("counties_pane", zIndex = 390)       # above states, below PoE circles
    
    if (!is.null(L$counties)) {
      m <- m %>% addPolylines(
        data = sf::st_boundary(L$counties), group = "Counties",
        color = "#9e9e9e", weight = 0.6,
        options = pathOptions(pane = "counties_pane", interactive = FALSE)
      )
    }
    if (!is.null(L$states)) {
      m <- m %>% addPolylines(
        data = sf::st_boundary(L$states), group = "States",
        color = "#003b6f", weight = 1.0, opacity = 0.8,
        options = pathOptions(pane = "states_pane", interactive = FALSE)
      )
    }
    
    m %>%
      addLayersControl(
        overlayGroups = c("States", "Counties"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addLegend(
        position = "bottomright", title = "PoE status",
        colors = unname(P1_STATUS_COLOURS),
        labels = c("Open alerts", paste0("Not reported ", P1_SILENT_DAYS, "+ days"),
                   "Reporting, no open alerts", "Not yet reporting"),
        opacity = 0.9
      )
  })
  
  # PoEs for the map: summary figures + coordinates, following the page filters
  p1_map_points <- reactive({
    coords <- p1_selected_poes() %>% select(poe, latitude, longitude)
    p1_summary() %>%
      left_join(coords, by = "poe") %>%
      mutate(total = coalesce(total, 0))
  })
  
  # PoE circles: redrawn when filters or data change, without resetting the zoom
  # PoE dots: redrawn when filters or data change, without resetting the zoom
  observe({
    pts <- p1_map_points() %>% filter(!is.na(latitude), !is.na(longitude))
    proxy <- leafletProxy("p1_map") %>% clearGroup("PoEs")
    if (nrow(pts) == 0) return()
    
    pts <- pts %>%
      mutate(
        colour = unname(P1_STATUS_COLOURS[as.character(status_rank)]),
        hover  = paste0(
          "<b>", poe, "</b>",
          "<br>Travellers screened: ", if_else(is_reporting, p1_fmt(total), "not yet reporting"),
          "<br>Open alerts: ", p1_fmt(open_alerts)
        )
      )
    
    proxy %>%
      addCircleMarkers(
        data = pts, lng = ~longitude, lat = ~latitude, group = "PoEs",
        radius = 5, color = "#ffffff", weight = 1.5,
        fillColor = ~colour, fillOpacity = 0.9,
        label = lapply(pts$hover, htmltools::HTML),
        labelOptions = labelOptions(direction = "auto", textsize = "12px")
      )
  })


  
  output$p1_map_note <- renderUI({
    pts     <- p1_map_points()
    missing <- pts$poe[is.na(pts$latitude) | is.na(pts$longitude)]
    div(
      class = "p1-dq-note",
      "Hover over a PoE for details. ",
      "Use the layer button (top right) to show or hide state and county boundaries.",
      if (length(missing)) tags$div(
        style = "color: #b9770e; margin-top: 4px;",
        icon("exclamation-triangle"),
        paste0(" Not shown - coordinates not available: ", paste(sort(missing), collapse = ", "))
      )
    )
  })
  
  # Returned so other parts of the app can use page 1 data if needed
  invisible(list(data = p1_data, tally = p1_tally, tally_last7 = p1_tally_last7,
                 poes = p1_selected_poes, dates = p1_date_range))
}
