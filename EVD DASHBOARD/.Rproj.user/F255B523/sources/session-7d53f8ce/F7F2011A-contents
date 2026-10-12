# ============================================================
# JIA SCREENING DETAIL
# ============================================================

# Fever threshold from the national suspected-case definition
JIA_FEVER_THRESHOLD <- 37.8

# Countries currently counted as "affected" (regular expression, lower case).
# Edit if the outbreak spreads, e.g. add "|uganda".
JIA_AFFECTED_COUNTRIES <- "democratic republic of the congo|\\bdrc\\b|\\bcod\\b|congo \\(democratic|dr congo|congo-kinshasa"

JIA_PAST_OUTBREAKS <- tibble::tribble(
  ~country,  ~pattern,  ~outbreak_over,
  "Uganda",  "uganda",  as.Date("2026-06-16")
)

jia_past_outbreak_travel <- function(countries_text, screen_date) {
  txt <- str_to_lower(coalesce(countries_text, ""))
  hit <- rep(FALSE, length(txt))
  for (i in seq_len(nrow(JIA_PAST_OUTBREAKS))) {
    hit <- hit | (str_detect(txt, JIA_PAST_OUTBREAKS$pattern[i]) &
                    !is.na(screen_date) & screen_date <= JIA_PAST_OUTBREAKS$outbreak_over[i])
  }
  hit
}

# Symptoms on the national suspected-case list (used for "3 or more symptoms").
# Each entry: label shown in charts = pattern matched in the symptom text.
JIA_LISTED_SYMPTOMS <- c(
  "Headache"                     = "headache",
  "Loss of appetite"             = "anorexia|appetite",
  "Diarrhoea"                    = "diarr",
  "Vomiting"                     = "vomit",
  "Lethargy / fatigue"           = "lethargy|fatigue|weakness|tired",
  "Stomach / abdominal pain"     = "stomach|abdominal|abdomen",
  "Body pains (muscle / joint)"  = "muscle|joint|body pain|myalgia|aching",
  "Difficulty breathing"         = "breath",
  "Hiccups"                      = "hiccup",
  "Sore throat"                  = "sore throat|throat",
  "Rash"                         = "rash",
  "Difficulty swallowing"        = "swallow"
)

# Unexplained bleeding signs (any one counts)
JIA_BLEEDING_PATTERN <- "bleed|blood|purpura|haemorrhag|hemorrhag|miscarriage|abortion"

# Other symptoms the form may record that are NOT on the national list
# (shown in the symptoms chart, not counted towards the case definition)
JIA_OTHER_SYMPTOMS <- c(
  "Red eyes" = "red[ _]?eye|blood from eye|bleeding into eye"
)

# ---------- helpers ----------

# TRUE where the pattern appears in the (lower-cased) text
jia_has <- function(text, pattern) {
  str_detect(str_to_lower(coalesce(text, "")), pattern)
}

# ---------- main function ----------

# Adds the page 2 indicator columns to the standardised JIA data.
# Needs these columns from standardize_evd_data() in app.r:
#   symptom_text, measured_temperature, fever_reported, symptom_flag_reported,
#   country_departure, countries_visited, affected_area_flag, contact_flag,
#   risk_category_form, missing_followup_flag, plus the exposure flags.
jia_add_flags <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)
  
  # Fallbacks so the function still works before the app.r changes are made
  if (!"fever_reported" %in% names(df))        df$fever_reported <- FALSE
  if (!"symptom_flag_reported" %in% names(df)) df$symptom_flag_reported <- FALSE
  if (!"risk_category_form" %in% names(df))    df$risk_category_form <- df$risk_category
  
  sym_text <- str_to_lower(coalesce(df$symptom_text, ""))
  
  # One TRUE/FALSE column per listed symptom (sym_<n>), for counting and charts
  listed <- map_dfc(JIA_LISTED_SYMPTOMS, ~ str_detect(sym_text, .x))
  names(listed) <- paste0("sym_", seq_along(JIA_LISTED_SYMPTOMS))
  n_listed <- rowSums(as.matrix(listed))
  
  df %>%
    bind_cols(listed) %>%
    mutate(
      # --- travel / epi link ---
      affected_country_flag = jia_has(paste(country_departure, countries_visited), JIA_AFFECTED_COUNTRIES),
      # Affected-area travel: DRC in travel history OR "yes" to the affected-area
      # question. Neither source is reliable on its own, so either counts.
      affected_travel       = affected_country_flag | safe_lgl(affected_area_flag),
      epi_link              = affected_travel | safe_lgl(contact_flag),
      
      # --- fever ---
      fever_measured        = !is.na(measured_temperature) & measured_temperature >= JIA_FEVER_THRESHOLD,
      # self-reported: "yes" to the fever question, or fever ticked in the symptom list
      fever_self_reported   = safe_lgl(fever_reported) | str_detect(sym_text, "fever"),
      fever_any             = fever_measured | fever_self_reported,
      
      # --- symptoms ---
      n_listed_symptoms     = n_listed,
      bleeding              = str_detect(sym_text, JIA_BLEEDING_PATTERN),
      any_specific_symptom  = n_listed > 0 | bleeding |
        str_detect(sym_text, paste(JIA_OTHER_SYMPTOMS, collapse = "|")),
      symptomatic           = fever_any | any_specific_symptom | safe_lgl(symptom_flag_reported),
      symptomatic_unspecified = symptomatic & !fever_any & !any_specific_symptom,
      
      # --- national suspected-case definition (screening data) ---
      # (a) fever + at least one bleeding sign, OR
      # (b) epi link AND (fever OR 3+ listed symptoms OR unexplained bleeding)
      suspected_case        = (fever_any & bleeding) |
        (epi_link & (fever_any | n_listed >= 3 | bleeding)),
      
      # --- exposure (any exposure question answered yes, or affected-area travel) ---
      exposure_reported     = safe_lgl(contact_flag) | safe_lgl(funeral_flag) | safe_lgl(touched_body_flag) |
        safe_lgl(cared_sick_flag) | safe_lgl(health_facility_flag) |
        safe_lgl(body_fluids_flag) | safe_lgl(wildlife_flag) |
        safe_lgl(animal_bite_flag) | affected_travel,
      
      # --- risk classification as recorded by the screener (no auto-upgrade) ---
      high_risk             = risk_category_form == "High risk",
      risk_not_classified   = risk_category_form == "Not classified",
      
      # --- follow-up: host name, phone and address all recorded ---
      followup_complete     = !safe_lgl(missing_followup_flag),
      # --- PoE alert (dashboard alert definition) ---
      # Symptomatic AND (any exposure, contact with a case, DRC travel or high-risk)
      poe_alert             = symptomatic &
        (exposure_reported | affected_travel | safe_lgl(contact_flag) | high_risk)
    )
}


# Labels for the sym_<n> columns, in the same order
jia_symptom_labels <- function() names(JIA_LISTED_SYMPTOMS)


# Default date range for page 2: first JIA record up to today
# (JIA records are timestamped, so today's records are included)
p2_default_dates <- function(df) {
  end_date <- Sys.Date()
  start_date <- if (!is.null(df) && nrow(df) > 0 && any(!is.na(df$submission_date))) {
    min(df$submission_date, na.rm = TRUE)
  } else {
    end_date - 12 * 7 + 1
  }
  c(start_date, end_date)
}



# ============================================================
# PAGE 2 SERVER: JIA SCREENING DETAIL
# ============================================================

P2_FOLLOWUP_GREEN <- 0.90  # follow-up complete: green at or above
P2_FOLLOWUP_AMBER <- 0.75  # amber at or above, red below
P2_DQ_MANY_SYMPTOMS <- 10   # this many different symptoms ticked or more = flagged


# Percentage label; one decimal place below 1% so small numbers don't show as 0%
p2_pct <- function(n, d) {
  if (d <= 0) return("n/a")
  p <- 100 * n / d
  if (p > 0 && p < 1) paste0(formatC(p, format = "f", digits = 1), "%") else paste0(round(p), "%")
}

P2_COLOURS <- c(
  affected     = "#dd4b39",   # red: travelled to an affected area
  not_affected = "#003b6f",   # dark blue: all other travellers
  rolling      = "#f39c12",   # 7-day rolling average
  destination  = "#003b6f",
  other        = "#9db3c8",   # other locations
  not_recorded = "#bfbfbf"    # not recorded / unmapped
)


# ---------- destination cleaning ----------
# Pattern (lower case) -> town shown in the chart. First match wins.
# Juba neighbourhoods are grouped under Juba. Add patterns as new spellings appear.
JIA_DESTINATION_MAP <- c(
  "juba|gudele|gudeli|atlabara|atlbara|tomping|thongpiny|thonping|munuki|kator|jebel|konyo|hai |un house|mangateen|lologo|rock city|nyakuron" = "Juba",
  "wau"      = "Wau",
  "malakal"  = "Malakal",
  "bor\\b"   = "Bor",
  "bentiu"   = "Bentiu",
  "yambio"   = "Yambio",
  "aweil"    = "Aweil",
  "rumbek"   = "Rumbek",
  "torit"    = "Torit",
  "yei"      = "Yei",
  "nimule"   = "Nimule",
  "kuajok|kwajok" = "Kuajok",
  "renk"     = "Renk"
)



# Answers that are not a destination
JIA_DESTINATION_NOT_RECORDED <- "^(yes|no|none|n/?a|nil|south sudan|s\\.?sudan|ss|not recorded|unknown|-|\\.)$"

jia_clean_destination <- function(x) {
  xl  <- str_to_lower(str_squish(coalesce(as.character(x), "")))
  out <- rep(NA_character_, length(xl))
  for (pattern in names(JIA_DESTINATION_MAP)) {
    hit <- is.na(out) & str_detect(xl, pattern)
    out[hit] <- JIA_DESTINATION_MAP[[pattern]]
  }
  not_rec <- is.na(out) & (xl == "" | str_detect(xl, JIA_DESTINATION_NOT_RECORDED))
  out[not_rec] <- "Not recorded"
  out[is.na(out)] <- str_to_title(xl[is.na(out)])
  out
}

# ---------- travel history (country of departure + countries visited) ----------
# Three-letter codes -> country names. Add codes as they appear.
JIA_COUNTRY_CODES <- c(
  afg = "Afghanistan", ago = "Angola", alb = "Albania", and = "Andorra", are = "United Arab Emirates",
  arm = "Armenia", atg = "Antigua and Barbuda", aus = "Australia", aut = "Austria", bdi = "Burundi",
  bel = "Belgium", bgd = "Bangladesh", caf = "Central African Republic", can = "Canada",
  che = "Switzerland", chn = "China", cmr = "Cameroon", cod = "Democratic Republic of the Congo",
  cog = "Republic of the Congo", cub = "Cuba", deu = "Germany", dza = "Algeria", egy = "Egypt",
  eri = "Eritrea", esp = "Spain", eth = "Ethiopia", fra = "France", gbr = "United Kingdom",
  gha = "Ghana", grc = "Greece", idn = "Indonesia", ind = "India", ita = "Italy", jpn = "Japan",
  ken = "Kenya", kor = "South Korea", lbr = "Liberia", mwi = "Malawi", mys = "Malaysia",
  nga = "Nigeria", nld = "Netherlands", npl = "Nepal", pak = "Pakistan", phl = "Philippines",
  pol = "Poland", qat = "Qatar", rus = "Russia", rwa = "Rwanda", sau = "Saudi Arabia", sdn = "Sudan",
  sgp = "Singapore", sle = "Sierra Leone", som = "Somalia", ssd = "South Sudan", tcd = "Chad",
  tha = "Thailand", tur = "Turkey", tza = "Tanzania", uga = "Uganda", ukr = "Ukraine",
  usa = "United States", zaf = "South Africa", zmb = "Zambia", zwe = "Zimbabwe"
)

# Multi-word names are turned into codes first so they are not split apart
JIA_MULTIWORD_COUNTRIES <- c(
  "democratic republic of the congo" = "cod", "republic of the congo" = "cog",
  "central african republic" = "caf", "south sudan" = "ssd", "south africa" = "zaf",
  "united states" = "usa", "united kingdom" = "gbr", "united arab emirates" = "are",
  "saudi arabia" = "sau", "sri lanka" = "lka", "burkina faso" = "bfa", "new zealand" = "nzl",
  "north korea" = "prk", "south korea" = "kor", "ivory coast" = "civ", "cote d'ivoire" = "civ"
)

# One cleaned list of countries per traveller from any country text
jia_split_countries <- function(x) {
  xl <- str_to_lower(coalesce(as.character(x), ""))
  for (nm in names(JIA_MULTIWORD_COUNTRIES)) xl <- str_replace_all(xl, fixed(nm), JIA_MULTIWORD_COUNTRIES[[nm]])
  xl <- str_replace_all(xl, "\\b(missing|none|not recorded|n/?a|unknown)\\b", " ")
  lapply(str_split(str_squish(xl), "[\\s,;|]+"), function(tok) {
    tok <- tok[tok != ""]
    out <- ifelse(tok %in% names(JIA_COUNTRY_CODES), JIA_COUNTRY_CODES[tok],
                  ifelse(nchar(tok) == 3, str_to_upper(tok), str_to_title(tok)))
    unique(unname(out))
  })
}

# Travel history = country of departure + countries visited (last 21 days)
jia_travel_history <- function(departure, visited) {
  dep <- jia_split_countries(departure)
  vis <- jia_split_countries(visited)
  Map(function(a, b) unique(c(a, b)), dep, vis)
}

# Was this country affected when the traveller was screened?
#   DRC: always (ongoing).  Past outbreaks (JIA_PAST_OUTBREAKS): up to the end date.
jia_affected_at_screening <- function(country, screen_date) {
  hit <- country == "Democratic Republic of the Congo"
  for (i in seq_len(nrow(JIA_PAST_OUTBREAKS))) {
    hit <- hit | (country == JIA_PAST_OUTBREAKS$country[i] &
                    !is.na(screen_date) & screen_date <= JIA_PAST_OUTBREAKS$outbreak_over[i])
  }
  hit
}






P2_RISK_COLOURS <- c(
  symptom      = "#f39c12",   # orange: symptoms
  fever        = "#d35400",   # dark orange: measured / reported fever
  bleeding     = "#c0392b",   # red: bleeding signs
  exposure     = "#dd4b39",   # red: exposures
  symptomatic  = "#f39c12",
  exposed      = "#dd4b39",
  high_risk    = "#7b241c"    # dark red
)

# Exposure questions shown in the exposure chart: column = label
JIA_EXPOSURE_LABELS <- c(
  contact_flag         = "Contact with an EVD case",
  funeral_flag         = "Attended a funeral or burial",
  touched_body_flag    = "Touched a dead body",
  cared_sick_flag      = "Cared for a sick person",
  health_facility_flag = "Visited a health facility treating EVD",
  body_fluids_flag     = "Handled blood or body fluids",
  wildlife_flag        = "Handled bushmeat or wild animals",
  animal_bite_flag     = "Bitten or scratched by a wild animal",
  affected_travel      = "Affected-area travel (reported or DRC travel history listed)"
)

# Horizontal bar chart used by the symptom and exposure charts
#   d: tibble with label, n, colour; denom_name / denom: e.g. "symptomatic", 42
p2_hbar <-  function(d, denom_name, denom, screened, name = "JIA_chart") {
  d <- d %>%
    filter(n > 0) %>%
    arrange(n) %>%
    mutate(
      pct_denom = vapply(n, p2_pct, character(1), d = denom),
      pct_all   = vapply(n, p2_pct, character(1), d = screened),
      bar_text  = paste0(p1_fmt(n), " (", pct_denom, ")"),
      hover     = paste0("<b>", label, "</b><br>Travellers: ", p1_fmt(n),
                         "<br>% of ", denom_name, " travellers: ", pct_denom,
                         "<br>% of all travellers screened: ", pct_all)
    )
  d$label <- factor(d$label, levels = d$label)
  
  p2_xaxis <- list(title = "Travellers", tickformat = ",d", range = c(0, max(d$n) * 1.3))
  if (max(d$n) <= 10) p2_xaxis$dtick <- 1   # whole numbers only when counts are small
  
  plot_ly(d, x = ~n, y = ~label, type = "bar", orientation = "h",
          marker = list(color = d$colour),
          text = ~bar_text, textposition = "outside", cliponaxis = FALSE,
          hovertext = ~hover, hoverinfo = "text") %>%
    layout(
      xaxis = p2_xaxis,
      yaxis = list(title = ""),
      margin = list(t = 10, l = 10)
    ) %>%
    config(displaylogo = FALSE)
}


jia_detail_server <- function(input, output, session, jia_data, cleared_ids = NULL) {
  
  # ---- 3a. Data with the agreed flags ----
  p2_all <- reactive({
    df <- jia_data()
    if (is.null(df) || nrow(df) == 0) return(df)
    jia_add_flags(df)
  })
  
  # Affected-area toggle applied (all dates) - used for tile footers
  p2_scope <- reactive({
    df <- p2_all()
    if (is.null(df) || nrow(df) == 0) return(df)
    grp <- if (is.null(input$p2_group)) "all" else input$p2_group
    df <- switch(grp,
                 drc   = filter(df, affected_travel),
                 high  = filter(df, high_risk),
                 df
    )
    df
  })
  
  p2_date_range <- reactive({
    d <- input$p2_dates
    shiny::validate(shiny::need(length(d) == 2 && !anyNA(d), "Choose a start and end date."))
    sort(as.Date(d))
  })
  
  # Selected date range + toggle - used by tiles and charts
  p2_data <- reactive({
    df <- p2_scope()
    if (is.null(df) || nrow(df) == 0) return(df)
    rng <- p2_date_range()
    filter(df, submission_date >= rng[1], submission_date <= rng[2])
  })
  
  p2_last7 <- reactive({
    df <- p2_scope()
    if (is.null(df) || nrow(df) == 0) return(df)
    filter(df, submission_date > Sys.Date() - 7)
  })
  
  p2_last24h <- reactive({
    df <- p2_scope()
    if (is.null(df) || nrow(df) == 0) return(df)
    filter(df, submission_time >= Sys.time() - 24 * 60 * 60)
  })
  
  # ---- 3b. Reset filters ----
  observeEvent(input$p2_reset, {
    dflt <- p2_default_dates(jia_data())
    updateDateRangeInput(session, "p2_dates", start = dflt[1], end = dflt[2])
    updateRadioGroupButtons(session, "p2_group", selected = "all")
    updateRadioGroupButtons(session, "p2_view", selected = P1_DEFAULT_VIEW)
  })
  
  # ---- 3c. "Data as of" line ----
  output$p2_data_as_of <- renderUI({
    df <- p2_all()
    if (is.null(df) || nrow(df) == 0) {
      return(tags$span(icon("exclamation-triangle"), " No JIA records available"))
    }
    latest <- max(df$submission_time, na.rm = TRUE)
    n24    <- sum(df$submission_time >= Sys.time() - 24 * 60 * 60, na.rm = TRUE)
    
    parts <- list(
      tags$strong(paste0("Latest JIA record: ", format(latest, "%d-%b-%Y %H:%M"))),
      paste0(" \u00b7 ", p1_fmt(n24), " records in the last 24 hours")
    )
    grp_labels <- c(drc = "affected-area travel", high = "classified high-risk")
    grp <- input$p2_group
    if (!is.null(grp) && grp != "all") {
      parts <- c(parts, list(tags$span(
        style = "color: #dd4b39; margin-left: 6px;",
        paste0(" \u00b7 Showing ", grp_labels[[grp]], " only")
      )))
    }
    tags$span(parts)
  })

  
  # ---- 3e. Headline tiles ----
  
  # Helper for the count tiles: main = selected period, sub = % of screened,
  # footer = last 7 days
  p2_count_tile <- function(flag, title, icon_name, colour_if_recent, colour_else = "navy") {
    df   <- p2_data()
    last <- p2_last7()
    n     <- if (is.null(df)) 0 else sum(df[[flag]])
    total <- if (is.null(df)) 0 else nrow(df)
    n7    <- if (is.null(last)) 0 else sum(last[[flag]])
    
    valueBox(
      value    = p1_fmt(n),
      subtitle = p1_tile_subtitle(
        title,
        paste0(p2_pct(n, total), " of screened"),
        paste0("Last 7 days: ", p1_fmt(n7))
      ),
      icon  = icon(icon_name),
      color = if (n7 > 0) colour_if_recent else colour_else
    )
  }
  
  output$p2_vb_screened <- renderValueBox({
    df   <- p2_data()
    n    <- if (is.null(df)) 0 else nrow(df)
    n24  <- if (is.null(p2_last24h())) 0 else nrow(p2_last24h())
    n7   <- if (is.null(p2_last7())) 0 else nrow(p2_last7())
    valueBox(
      value    = p1_fmt(n),
      subtitle = p1_tile_subtitle(
        "Travellers screened",
        paste0("Last 24 hours: ", p1_fmt(n24)),
        paste0("Last 7 days: ", p1_fmt(n7))
      ),
      icon  = icon("users"),
      color = "navy"
    )
  })
  
  output$p2_vb_affected <- renderValueBox({
    df   <- p2_data()
    last <- p2_last7()
    n        <- if (is.null(df)) 0 else sum(df$affected_travel)
    n_drc    <- if (is.null(df)) 0 else sum(df$affected_country_flag)
    n_yes    <- if (is.null(df)) 0 else sum(safe_lgl(df$affected_area_flag))
    n_both   <- if (is.null(df)) 0 else sum(df$affected_country_flag & safe_lgl(df$affected_area_flag))
    total    <- if (is.null(df)) 0 else nrow(df)
    n7       <- if (is.null(last)) 0 else sum(last$affected_travel)
    
    valueBox(
      value    = p1_fmt(n),
      subtitle = p1_tile_subtitle(
        "DRC travel or self-reported affected-area travel",
        paste0(p1_fmt(n_yes - n_both), " self-reported only \u00b7 ",
               p1_fmt(n_drc - n_both), " DRC only \u00b7 ",
               p1_fmt(n_both), " both"),
      ),
      icon  = icon("globe-africa"),
      color = if (n7 > 0) "yellow" else "navy"
    )
  })
  
  output$p2_vb_alerts <- renderValueBox({
    df   <- p2_data()
    last <- p2_last7()
    ids  <- if (is.null(cleared_ids)) character(0) else cleared_ids()
    
    if (is.null(df) || nrow(df) == 0) {
      n_alert <- 0; n_disc <- 0; total <- 0; n_high <- 0
    } else {
      is_alert <- df$poe_alert | df$high_risk
      n_alert  <- sum(is_alert)
      n_disc   <- sum(is_alert & as.character(df$screening_id) %in% as.character(ids))
      n_high   <- sum(df$high_risk)
      total    <- nrow(df)
    }
    n_wait <- n_alert - n_disc
    n7     <- if (is.null(last) || nrow(last) == 0) 0 else sum(last$poe_alert | last$high_risk)
    
    valueBox(
      value    = p1_fmt(n_alert),
      subtitle = p1_tile_subtitle(
        "PoE alerts triggered",
        paste0(p1_fmt(n_disc), " discarded \u00b7 ", p1_fmt(n_wait), " awaiting review"),
        paste0(p1_fmt(n_high), " classified as high-risk \u00b7 ", p2_pct(n_alert, total), " of screened \u00b7 last 7 days: ", p1_fmt(n7))
      ),
      icon  = icon("bell"),
      color = if (n_wait > 0) "red" else if (n_alert > 0) "green" else "navy"
    )
  })
  
  output$p2_vb_symptomatic <- renderValueBox({
    df   <- p2_data()
    last <- p2_last7()
    n      <- if (is.null(df)) 0 else sum(df$symptomatic)
    n_unsp <- if (is.null(df)) 0 else sum(df$symptomatic_unspecified)
    total  <- if (is.null(df)) 0 else nrow(df)
    n7     <- if (is.null(last)) 0 else sum(last$symptomatic)
    
    valueBox(
      value    = p1_fmt(n),
      subtitle = p1_tile_subtitle(
        "Symptomatic",
        paste0(p2_pct(n, total), " of screened \u00b7 ", p1_fmt(n_unsp),
               " said yes but no symptoms recorded"),
        paste0("Last 7 days: ", p1_fmt(n7))
      ),
      icon  = icon("thermometer-half"),
      color = if (n7 > 0) "yellow" else "navy"
    )
  })
  
  output$p2_vb_exposure <- renderValueBox({
    p2_count_tile("exposure_reported", "Exposure reported", "exclamation-triangle", "yellow")
  })
  

  output$p2_vb_followup <- renderValueBox({
    df   <- p2_data()
    last <- p2_last7()
    total <- if (is.null(df)) 0 else nrow(df)
    comp  <- if (total > 0) sum(df$followup_complete) / total else NA_real_
    gaps  <- if (is.null(df)) 0 else sum(!df$followup_complete)
    comp7 <- if (!is.null(last) && nrow(last) > 0) sum(last$followup_complete) / nrow(last) else NA_real_
    
    colour <- if (is.na(comp)) "navy" else if (comp >= P2_FOLLOWUP_GREEN) "green" else
      if (comp >= P2_FOLLOWUP_AMBER) "yellow" else "red"
    
    valueBox(
      value    = if (is.na(comp)) "n/a" else paste0(round(100 * comp), "%"),
      subtitle = p1_tile_subtitle(
        "Follow-up details complete",
        paste0(p1_fmt(gaps), " with gaps (host name, phone or address missing)"),
        paste0("Last 7 days: ", if (is.na(comp7)) "n/a" else paste0(round(100 * comp7), "%"))
      ),
      icon  = icon("phone"),
      color = colour
    )
  })
  
  # ---- 4. Section 1: Who is arriving ----
  
  p2_view <- reactive({
    if (is.null(input$p2_view)) P1_DEFAULT_VIEW else input$p2_view
  })
  
  # 4a. Travellers screened over time, split by affected-area travel
  output$p2_screened_plot <- renderPlotly({
    df     <- p2_scope()
    rng    <- p2_date_range()
    weekly <- p2_view() == "weekly"
    today  <- Sys.Date()
    
    if (is.null(df) || nrow(df) == 0) {
      return(empty_plotly("No JIA screening records"))
    }
    
    # Build a complete day-by-day series (days with no records stay as zeros)
    from <- if (weekly) p1_week_start(rng[1]) else rng[1] - (P1_ROLLING_DAYS - 1)
    to   <- if (weekly) min(p1_week_start(rng[2]) + 6, today) else min(rng[2], today)
    if (from > to) return(empty_plotly("No JIA screening records for the selected dates"))
    
    daily <- tibble(date = seq(from, to, by = "day")) %>%
      left_join(
        df %>%
          filter(submission_date >= from, submission_date <= to) %>%
          group_by(date = submission_date) %>%
          summarise(affected = sum(affected_travel), total = n(), .groups = "drop"),
        by = "date"
      ) %>%
      mutate(affected = coalesce(affected, 0L), total = coalesce(total, 0L),
             not_affected = total - affected)
    
    if (weekly) {
      s <- daily %>%
        mutate(week = p1_week_start(date)) %>%
        group_by(week) %>%
        summarise(across(c(affected, not_affected, total), sum), .groups = "drop") %>%
        mutate(in_progress = week + 6 >= today, label = p1_week_label(week))
      x       <- factor(s$label, levels = s$label)
      faded   <- s$in_progress
      heading <- paste0(s$label, ": ", format(s$week, "%d %b"), " \u2013 ",
                        format(s$week + 6, "%d %b %Y"),
                        ifelse(s$in_progress, " (week in progress)", ""))
    } else {
      s <- daily %>%
        mutate(rolling = as.numeric(stats::filter(total, rep(1 / P1_ROLLING_DAYS, P1_ROLLING_DAYS),
                                                  sides = 1))) %>%
        filter(date >= rng[1])
      x       <- s$date
      faded   <- s$date == today
      heading <- paste0(format(s$date, "%a %d %b %Y"), ifelse(faded, " (today, so far)", ""))
    }
    
    if (sum(s$total) == 0) return(empty_plotly("No JIA screening records for the selected dates"))
    
    hover <- paste0(
      "<b>", heading, "</b>",
      "<br>Travellers screened: ", p1_fmt(s$total),
      "<br>Travelled to an affected area: ", p1_fmt(s$affected),
      " (", vapply(seq_len(nrow(s)), function(i) p2_pct(s$affected[i], s$total[i]), character(1)), ")"
    )
    opacity <- ifelse(faded, 0.4, 1)
    
    p <- plot_ly() %>%
      add_bars(x = x, y = s$not_affected, name = "No affected-area travel",
               marker = list(color = P2_COLOURS[["not_affected"]], opacity = opacity),
               text = hover, hoverinfo = "text", textposition = "none") %>%
      add_bars(x = x, y = s$affected, name = "Travelled to an affected area",
               marker = list(color = P2_COLOURS[["affected"]], opacity = opacity),
               text = hover, hoverinfo = "text", textposition = "none")
    
    if (weekly) {
      p <- p %>% add_text(
        x = x, y = s$total, text = p1_fmt(s$total),
        textposition = "top center", textfont = list(size = 11, color = "#333"),
        showlegend = FALSE, hoverinfo = "skip"
      )
    }
    
    if (!weekly) {
      p <- p %>% add_lines(x = x, y = s$rolling, name = paste0(P1_ROLLING_DAYS, "-day average"),
                           line = list(color = P2_COLOURS[["rolling"]], width = 2), hoverinfo = "skip")
    }
    
    xaxis <- if (weekly) {
      list(title = "Epi week", type = "category", tickangle = -45)
    } else {
      list(title = "", type = "date", tickformat = "%d %b",
           dtick = 7 * 24 * 60 * 60 * 1000, tick0 = as.character(min(s$date)))
    }
    
    p %>%
      layout(
        barmode = "stack",
        xaxis = xaxis,
        yaxis = list(title = "Travellers screened", tickformat = ",d", rangemode = "tozero"),
        legend = list(orientation = "h", x = 0, y = 1.12),
        margin = list(t = 20)
      ) %>%
      chart_download_config("JIA_travellers_screened")
  })
  
  # 4b. Final destination in South Sudan (top 10)
  output$p2_destination_plot <- renderPlotly({
    df <- p2_data()
    if (is.null(df) || nrow(df) == 0) {
      return(empty_plotly("No JIA screening records for the selected dates"))
    }
    
    total <- nrow(df)
    d <- df %>%
      mutate(destination = jia_clean_destination(destination_ss)) %>%
      group_by(destination) %>%
      summarise(n = n(), affected = sum(affected_travel), .groups = "drop")
    
    named <- d %>% filter(destination != "Not recorded") %>% arrange(desc(n))
    top   <- head(named, 10)
    other <- tail(named, max(nrow(named) - 10, 0))
    
    bars <- bind_rows(
      top %>% mutate(group = "named"),
      if (nrow(other) > 0) tibble(destination = paste0("Other locations (", nrow(other), ")"),
                                  n = sum(other$n), affected = sum(other$affected), group = "other"),
      d %>% filter(destination == "Not recorded") %>% mutate(group = "not_recorded")
    ) %>%
      mutate(
        colour = case_when(group == "other" ~ P2_COLOURS[["other"]],
                           group == "not_recorded" ~ P2_COLOURS[["not_recorded"]],
                           TRUE ~ P2_COLOURS[["destination"]]),
        hover = paste0("<b>", destination, "</b>",
                       "<br>Travellers: ", p1_fmt(n), " (", vapply(n, p2_pct, character(1), d = total), " of screened)",
                       "<br>Travelled to an affected area: ", p1_fmt(affected))
      )
  
    
    # Top destination at the top; other and not recorded at the bottom
    bars$destination <- factor(bars$destination, levels = rev(bars$destination))
    
    plot_ly(
      bars, x = ~n, y = ~destination, type = "bar", orientation = "h",
      marker = list(color = bars$colour),
      text = ~p1_fmt(n), textposition = "outside",
      hovertext = ~hover, hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = "Travellers screened", tickformat = ",d"),
        yaxis = list(title = ""),
        margin = list(t = 10, l = 10)
      ) %>%
      chart_download_config("JIA_destinations")
  })
  
  
  # ---- 5. Section 2: Risk ----
  
  # 5a. Symptom summary figures (above the symptom chart)
  output$p2_symptom_summary <- renderUI({
    df <- p2_data()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    chip <- function(label, n, highlight = FALSE) {
      tags$div(
        class = "p2-chip",
        style = if (highlight && n > 0) "border-color: #c0392b; background: #f8d7d3;" else NULL,
        tags$b(p1_fmt(n)), " ", label
      )
    }
    
    div(
      class = "p2-summary",
      chip(paste0("symptomatic (", p1_fmt(sum(df$symptomatic_unspecified)), " with no specific symptom recorded)"),
           sum(df$symptomatic)),
      chip(paste0("measured temperature \u2265 ", JIA_FEVER_THRESHOLD, "\u00b0C"), sum(df$fever_measured)),
      chip("reported bleeding", sum(df$bleeding), highlight = TRUE),
      chip("sought medical care", sum(safe_lgl(df$medical_care_flag)))
    )
  })
  
  # 5b. Symptoms reported
  output$p2_symptoms_plot <- renderPlotly({
    df <- p2_data()
    if (is.null(df) || nrow(df) == 0 || sum(df$symptomatic) == 0) {
      return(empty_plotly("No symptomatic travellers for the selected dates"))
    }
    
    sym_text   <- str_to_lower(coalesce(df$symptom_text, ""))
    sym_cols   <- paste0("sym_", seq_along(JIA_LISTED_SYMPTOMS))
    other_rows <- map_dfr(names(JIA_OTHER_SYMPTOMS), ~ tibble(
      label = paste0(.x, " (not on national list)"),
      n = sum(str_detect(sym_text, JIA_OTHER_SYMPTOMS[[.x]])),
      colour = P2_RISK_COLOURS[["symptom"]]
    ))
    
    d <- bind_rows(
      tibble(label = paste0("Measured temperature \u2265 ", JIA_FEVER_THRESHOLD, "\u00b0C"),
             n = sum(df$fever_measured), colour = P2_RISK_COLOURS[["fever"]]),
      tibble(label = "Fever (self-reported)",
             n = sum(df$fever_self_reported), colour = P2_RISK_COLOURS[["fever"]]),
      tibble(label = "Bleeding (any reported)",
             n = sum(df$bleeding), colour = P2_RISK_COLOURS[["bleeding"]]),
      tibble(label = jia_symptom_labels(),
             n = vapply(sym_cols, function(col) sum(df[[col]]), numeric(1)),
             colour = P2_RISK_COLOURS[["symptom"]]),
      tibble(label = "Symptomatic, but no specific symptom recorded",
             n = sum(df$symptomatic_unspecified), colour = "#bfbfbf"),
      other_rows
    )
    
    p2_hbar(d, "symptomatic", sum(df$symptomatic), nrow(df), name = "JIA_symptoms")
  })
  
  # 5c. Exposure types
  output$p2_exposure_plot <- renderPlotly({
    df <- p2_data()
    if (is.null(df) || nrow(df) == 0 || sum(df$exposure_reported) == 0) {
      return(empty_plotly("No exposures reported for the selected dates"))
    }
    
    drc_link <- df$affected_travel          # DRC listed OR self-reported
    cols     <- names(JIA_EXPOSURE_LABELS)
    
    d <- tibble(
      label  = unname(JIA_EXPOSURE_LABELS),
      total  = vapply(cols, function(col) sum(safe_lgl(df[[col]])), numeric(1)),
      linked = vapply(cols, function(col) sum(safe_lgl(df[[col]]) & drc_link), numeric(1))
    ) %>%
      filter(!str_detect(label, regex("affected-area", ignore_case = TRUE))) %>%   # drop the travel bar
      mutate(neither = total - linked) %>%
      filter(total > 0) %>%
      arrange(total)                          # biggest bar ends up at the top
    
    if (nrow(d) == 0) return(empty_plotly("No exposures reported for the selected dates"))
    
    hover <- paste0(
      "<b>", d$label, "</b>",
      "<br>DRC listed or self-reported: ", d$linked, " (", round(100 * d$linked / d$total), "%)",
      "<br>Neither: ", d$neither,
      "<br>Total: ", d$total
    )
    max_x    <- max(d$total)
    screened <- sum(df$exposure_reported)  
    pct      <- 100 * d$total / screened
    pct_txt  <- ifelse(pct < 0.1, "<0.1%", paste0(formatC(pct, format = "f", digits = 1), "%"))
    bar_lab  <- paste0(d$total, " (", pct_txt, ")")
    hover    <- paste0(hover, "<br>", pct_txt, " of ", format(screened, big.mark = ","), " travellers reporting an exposure")
    
    xaxis <- list(title = "Travellers", type = "linear", range = c(0, max_x * 1.3))
    if (max_x <= 10) xaxis$dtick <- 1          # whole-number ticks only when counts are small
    
    plot_ly() %>%
      add_bars(y = d$label, x = d$linked, orientation = "h",
               name = "Self-reported travel to an affected area or DRC travel history", marker = list(color = "#b03a2e"),
               text = hover, hoverinfo = "text", textposition = "none") %>%
      add_bars(y = d$label, x = d$neither, orientation = "h",
               name = "Neither", marker = list(color = "#bdbdbd"),
               text = hover, hoverinfo = "text", textposition = "none") %>%
      add_trace(type = "scatter", mode = "text",
                y = d$label, x = d$total, text = bar_lab,
                textposition = "middle right", textfont = list(size = 12, color = "#333333"),
                hoverinfo = "none", showlegend = FALSE) %>%
      layout(
        barmode = "stack",
        xaxis   = xaxis,
        yaxis   = list(title = "", type = "category",
                       categoryorder = "array", categoryarray = d$label),
        legend  = list(orientation = "h", x = 0, xanchor = "left",
                       y = 1.02, yanchor = "bottom", traceorder = "normal"),
        margin  = list(l = 10, r = 30, t = 80)
      ) %>%
      config(displaylogo = FALSE, toImageButtonOptions = list(filename = "JIA_exposures"))
  })
  
  # 5d. Weekly symptomatic, exposed and high-risk travellers
  output$p2_weekly_risk_plot <- renderPlotly({
    df  <- p2_scope()
    rng <- p2_date_range()
    if (is.null(df) || nrow(df) == 0) return(empty_plotly("No JIA screening records"))
    
    from <- p1_week_start(rng[1])
    to   <- min(p1_week_start(rng[2]) + 6, Sys.Date())
    
    s <- tibble(week = seq(from, p1_week_start(to), by = "week")) %>%
      left_join(
        df %>%
          filter(submission_date >= rng[1], submission_date <= min(rng[2], Sys.Date())) %>%
          mutate(week = p1_week_start(submission_date)) %>%
          group_by(week) %>%
          summarise(screened = n(), symptomatic = sum(symptomatic),
                    exposed = sum(exposure_reported), high_risk = sum(high_risk),
                    .groups = "drop"),
        by = "week"
      ) %>%
      mutate(across(c(screened, symptomatic, exposed, high_risk), ~ coalesce(.x, 0L)),
             in_progress = week + 6 >= Sys.Date(),
             label = p1_week_label(week))
    
    if (sum(s$screened) == 0) return(empty_plotly("No JIA screening records for the selected dates"))
    
    x <- factor(s$label, levels = s$label)
    opacity <- ifelse(s$in_progress, 0.4, 1)
    heading <- paste0("<b>", s$label, ": ", format(s$week, "%d %b"), " \u2013 ",
                      format(s$week + 6, "%d %b %Y"),
                      ifelse(s$in_progress, " (week in progress)", ""), "</b>",
                      "<br>Travellers screened: ", p1_fmt(s$screened))
    series_hover <- function(n, name) {
      paste0(heading, "<br>", name, ": ", p1_fmt(n), " (",
             vapply(seq_along(n), function(i) p2_pct(n[i], s$screened[i]), character(1)),
             " of screened)")
    }
    
    bar_label <- function(n) ifelse(n > 0, p1_fmt(n), "")
    
    y_max <- max(1, s$symptomatic, s$exposed, s$high_risk) * 1.2
    yaxis <- list(title = "Travellers", tickformat = ",d", range = c(0, y_max))
    if (y_max <= 10) yaxis$dtick <- 1
    
    plot_ly() %>%
      add_bars(x = x, y = s$symptomatic, name = "Symptomatic",
               marker = list(color = P2_RISK_COLOURS[["symptomatic"]], opacity = opacity),
               text = bar_label(s$symptomatic), textposition = "outside", cliponaxis = FALSE,
               hovertext = series_hover(s$symptomatic, "Symptomatic"), hoverinfo = "text") %>%
      add_bars(x = x, y = s$exposed, name = "Exposure reported",
               marker = list(color = P2_RISK_COLOURS[["exposed"]], opacity = opacity),
               text = bar_label(s$exposed), textposition = "outside", cliponaxis = FALSE,
               hovertext = series_hover(s$exposed, "Exposure reported"), hoverinfo = "text") %>%
      add_bars(x = x, y = s$high_risk, name = "High-risk",
               marker = list(color = P2_RISK_COLOURS[["high_risk"]], opacity = opacity),
               text = bar_label(s$high_risk), textposition = "outside", cliponaxis = FALSE,
               hovertext = series_hover(s$high_risk, "High-risk"), hoverinfo = "text") %>%
      layout(
        barmode = "group",
        xaxis = list(title = "Epi week", type = "category", tickangle = -45),
        yaxis = yaxis,
        legend = list(orientation = "h", x = 0, y = -0.3),
        margin = list(t = 20)
      ) %>%
      chart_download_config("JIA_weekly_risk")
  })
  
  # ---- 6. Data quality checks ----
  # Patterns found while reviewing the JIA data that suggest answers may be
  # unreliable. Follows the page filters (date range and Show selector).
  output$p2_dq_strip <- renderUI({
    df <- p2_data()
    if (is.null(df) || nrow(df) == 0) {
      return(div(class = "p2-caption", "No JIA screening records for the selected dates."))
    }
    
    lgl <- function(col) if (col %in% names(df)) safe_lgl(df[[col]]) else rep(FALSE, nrow(df))
    sym_text <- str_to_lower(coalesce(df$symptom_text, ""))
    
    # Number of different symptoms ticked (fever + national list + bleeding + red eyes)
    n_symptoms <- as.integer(str_detect(sym_text, "fever")) + df$n_listed_symptoms +
      as.integer(df$bleeding) + as.integer(str_detect(sym_text, JIA_OTHER_SYMPTOMS[["Red eyes"]]))
    
    major_exposures <- lgl("funeral_flag") & lgl("touched_body_flag") &
      lgl("cared_sick_flag") & lgl("body_fluids_flag")
    reported_area <- lgl("affected_area_flag")
    contact       <- lgl("contact_flag")
    drc           <- df$affected_country_flag
    past_outbreak <- jia_past_outbreak_travel(paste(df$country_departure, df$countries_visited),
                                              df$submission_date)
    checks <- list(
      list(n = sum(n_symptoms >= P2_DQ_MANY_SYMPTOMS), denom = sum(df$symptomatic), of = "symptomatic",
           label = paste0(P2_DQ_MANY_SYMPTOMS, " or more symptoms ticked"),
           note  = "Unlikely in a traveller well enough to fly; suggests every option was ticked."),
      list(n = sum(major_exposures), denom = sum(df$exposure_reported), of = "reporting an exposure",
           label = "Yes to all major exposures",
           note  = "Funeral, touched a body, cared for a sick person and body fluids all answered yes."),
      list(n = sum(df$symptomatic_unspecified), denom = sum(df$symptomatic), of = "symptomatic",
           label = "Said yes to symptoms, none recorded",
           note  = "Symptoms question answered yes, but no symptom or fever recorded."),
      list(n = sum(contact & !drc), denom = sum(contact), of = "reporting contact",
           label = "Contact with an EVD case, no affected area travel",
           note  = "Reported contact with a case but no affected country in travel history."),
      list(n = sum(reported_area & !drc & !past_outbreak), denom = sum(reported_area), of = "answering yes",
           label = "Yes to affected-area travel, DRC not listed",
           note  = "Answered yes, but neither DRC nor Uganda (before its outbreak ended on 16 Jun) is in the travel history."),
      list(n = sum(lengths(jia_split_countries(df$countries_visited)) == 0), denom = nrow(df), of = "screened",
           label = "No countries visited recorded",
           note  = "Countries visited in the last 21 days left blank, so DRC travel cannot be checked.")
    )
    
    items <- lapply(checks, function(ch) {
      div(
        class = paste("p2-dq-item", if (ch$n > 0) "amber"),
        tags$b(p1_fmt(ch$n)),
        tags$div(style = "font-weight: bold;", ch$label),
        tags$div(style = "font-size: 12px; color: #555;",
                 paste0(p2_pct(ch$n, ch$denom), " of ", p1_fmt(ch$denom), " ", ch$of, ". ", ch$note))
      )
    })
    
    tagList(
      div(class = "p2-dq-strip", items),
      div(class = "p2-caption", style = "margin-top: 8px;",
          "These checks flag answers that are inconsistent or implausible. They do not change any other figure ",
          "on the page. Individual records can be reviewed on the Restricted Alerts tab.")
    )
  })
  
  
  # Returned so other parts of the app can use page 2 data if needed
  invisible(list(all = p2_all, data = p2_data, last7 = p2_last7))
}


# Plain-language reason a record is awaiting review, for the restricted tab.
# Travel and contact links are labelled by how well they are supported:
#   "DRC travel (listed)"                         - affected country in travel history
#   "affected-area travel (reported only - ...)"  - yes to the question, no affected country listed
#   "contact with EVD case (no affected-area travel - verify)"
jia_review_reason <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(character(0))
  n <- nrow(df)
  lgl <- function(col) if (col %in% names(df)) safe_lgl(df[[col]]) else rep(FALSE, n)
  
  # Comma-separated labels of the TRUE flags in each row
  flag_text <- function(cols_labels) {
    m <- vapply(names(cols_labels), lgl, logical(n))
    m <- matrix(m, nrow = n)
    apply(m, 1, function(r) paste(cols_labels[r], collapse = ", "))
  }
  
  symptom_labels <- c(
    fever_any = "fever",
    setNames(str_to_lower(jia_symptom_labels()), paste0("sym_", seq_along(JIA_LISTED_SYMPTOMS))),
    bleeding = "bleeding"
  )
  other_exposure_labels <- c(
    funeral_flag          = "funeral/burial",
    touched_body_flag     = "touched dead body",
    cared_sick_flag       = "cared for sick person",
    health_facility_flag  = "health facility treating EVD",
    body_fluids_flag      = "blood/body fluids",
    wildlife_flag         = "bushmeat/wild animals",
    animal_bite_flag      = "animal bite"
  )
  
  listed   <- lgl("affected_country_flag")
  reported <- lgl("affected_area_flag")
  contact  <- lgl("contact_flag")
  
  travel_link <- case_when(
    listed            ~ "DRC travel (listed)",
    reported          ~ "affected-area travel (reported only \u2013 no affected country listed)",
    TRUE              ~ ""
  )
  contact_link <- case_when(
    contact & (listed | reported) ~ "contact with EVD case",
    contact                       ~ "contact with EVD case (no affected-area travel \u2013 verify)",
    TRUE                          ~ ""
  )
  
  symptoms <- flag_text(symptom_labels)
  others   <- flag_text(other_exposure_labels)
  links    <- apply(cbind(travel_link, contact_link, others), 1, function(r) paste(r[r != ""], collapse = ", "))
  
  parts <- cbind(
    ifelse(lgl("poe_alert"),
           paste0("PoE alert \u2013 symptoms: ", ifelse(symptoms == "", "reported (not specified)", symptoms),
                  "; link: ", ifelse(links == "", "high-risk classification", links)),
           ""),
    ifelse(lgl("high_risk"), "High-risk (screener)", ""),
    ifelse(lgl("suspected_case"), "Meets suspected-case definition", "")
  )
  apply(parts, 1, function(r) paste(r[r != ""], collapse = " | "))
}


