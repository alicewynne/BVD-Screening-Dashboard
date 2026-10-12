# ============================================================
# SOUTH SUDAN EVD TRAVELLER SCREENING DASHBOARD
# EVD Readiness Situation Room
# One-script Shiny dashboard
# Ona live API pull every 15 minutes, paused during restricted review
# Includes preprocessing/cleaning for inconsistent Ona/ODK entries
# Includes blinking alert banner for suspicious / actionable records
# No maps, no metadata tab, no codebook tab, no personal line list
# ============================================================

Sys.setenv(TZ = "Africa/Juba")

options(shiny.maxRequestSize = 100 * 1024^2)
options(shiny.sanitize.errors = FALSE)

# Shiny stability options
options(shiny.maxRequestSize = 100 * 1024^2)
options(shiny.reactlog = FALSE)
options(shiny.sanitize.errors = FALSE)
options(shiny.fullstacktrace = TRUE)

# ============================================================
# 0. INSTALL AND LOAD PACKAGES
# ============================================================

# packages <- c(
#   "shiny",
#   "shinydashboard",
#   "plotly",
#   "dplyr",
#   "DT",
#   "tidyr",
#   "lubridate",
#   "shinyWidgets",
#   "jsonlite",
#   "httr2",
#   "stringr",
#   "scales",
#   "purrr",
#   "tibble",
#   "readr",
#   "later"
# )
# 
# installed <- rownames(installed.packages())
# 
# for (p in packages) {
#   if (!p %in% installed) {
#     install.packages(p, dependencies = TRUE)
#   }
# }

library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(DT)
library(tidyr)
library(lubridate)
library(shinyWidgets)
library(jsonlite)
library(httr2)
library(stringr)
library(scales)
library(purrr)
library(tibble)
library(readr)
library(later)
library(readxl)
library(sf)
library(leaflet)

# modules for the daily PoE screening data, jia screening and restricted red-flag visitor/person details modulel
source("modules/mod_restricted_redflagsv2.R")
source("modules/mod_alert_clearance.R")
source("modules/poe_daily_screening.R")
source("modules/jia_screening.R")

# ============================================================
# 1. CONFIGURATION
# ============================================================

APP_TITLE <- "South Sudan EVD Traveller Screening Dashboard"

NPHI_LOGO <- "Nphi.png"
MOH_LOGO <- "mohs_logo.png"
WHO_LOGO <- "who_logo.png"

# WHO AFRO Ona Data API configuration.
# The form is hosted on whonghub.org, not the public api.ona.io server.
ONA_API_BASE <- "https://api.whonghub.org/api/v1"

# XLSForm/Ona id_string supplied for the screening form.
# The Ona submissions endpoint uses the numeric form PK, so the code
# resolves this id_string automatically at startup.
ONA_FORM_ID_STRING <- "ebola_screening"

# Verified API token for the private WHO AFRO Ona project.
ONA_API_TOKEN_FALLBACK <- "f1114e6b51d52540126cbd032d9881e96a498c03"

# Verified numeric form ID for id_string "ebola_screening".
# Using it directly avoids unnecessary catalogue lookups at startup.
ONA_FORM_PK_FALLBACK <- 17815L
ONA_FORM_PK_RESOLVED <- 17815L


# ============================================================
# AUTHORIZED HEALTH OFFICERS
# Only these emails can view full red-flag visitor/person details.
# Replace the placeholder emails below with the approved health officer emails.
# This requires authenticated app access so session$user contains the viewer email.
# ============================================================

AUTHORIZED_HEALTH_OFFICER_EMAILS <- c(
  "regmij@who.int",
  "bategerezaa@who.int",
  "themdit.matur@gmail.com",
  "josh2013.lasu@gmail.com",
  "morolawrence@gmail.com",
  "owinog@who.int",
  "wynnea@who.int",
  "abenegoa@who.int",
  "majakdegoup99@gmail.com"
)

AUTHORIZED_HEALTH_OFFICER_EMAILS <- tolower(trimws(AUTHORIZED_HEALTH_OFFICER_EMAILS))


# The form already contains more than 10,000 submissions. Ona permits up to
# 10,000 records per page, so the complete dataset is loaded in two pages.
ONA_LIMIT <- 100000
ONA_PAGE_SIZE <- 10000
ONA_MAX_RECORDS <- 100000

# Local cache lets the UI open quickly while Ona refreshes in the background.
ONA_CACHE_FILE <- "ona_live_cache.rds"
ONA_API_RETRY_SECONDS <- c(0, 2, 5)

# Ona data refresh interval: 15 minutes
REFRESH_INTERVAL_MS <- 900000

# Browser keep-alive interval: 15 minutes
KEEPALIVE_INTERVAL_MS <- 900000

# Delay first background Ona refresh slightly so the dashboard opens from cache first.
# This improves first-screen loading speed without changing the dashboard logic.
INITIAL_BACKGROUND_REFRESH_DELAY_SECONDS <- 8


LOCAL_TZ <- "Africa/Juba"

HIGH_TEMP_THRESHOLD <- 38.0
FEVER_THRESHOLD <- 37.5

# Arrival-date validation:
# If recorded arrival date is much older than the submission date, treat it as a data quality issue
# and exclude it from the arrival pattern chart.
ARRIVAL_BACKWARD_TOLERANCE_DAYS <- 2
ARRIVAL_FORWARD_TOLERANCE_DAYS <- 30

last_refresh_text <- reactiveVal("Not yet refreshed")
last_keepalive_text <- reactiveVal("Session active")

# ============================================================
# 2. COLORS
# ============================================================

status_colors <- c(
  "Cleared" = "#00a65a",
  "Referred / Alert" = "#dd4b39",
  "Pending" = "#f39c12",
  "Other" = "#7f8c8d"
)

risk_colors <- c(
  "Standard" = "#00a65a",
  "Moderate risk" = "#f39c12",
  "High risk" = "#dd4b39",
  "Not classified" = "#7f8c8d"
)

priority_colors <- c(
  "Immediate action" = "#dd4b39",
  "Follow-up required" = "#f39c12",
  "Enhanced monitoring" = "#f39c12",
  "Routine monitoring" = "#00a65a",
  "Reviewed / cleared" = "#00a65a"
)

sex_colors <- c(
  "Male" = "#1f77b4",
  "Female" = "#ff7f0e",
  "Other" = "#9467bd",
  "Missing" = "#7f7f7f"
)

# ============================================================
# 3. HELPER FUNCTIONS
# ============================================================

safe_chr <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x
}

safe_num <- function(x) {
  suppressWarnings(as.numeric(x))
}

safe_lgl <- function(x) {
  x <- as.logical(x)
  x[is.na(x)] <- FALSE
  x
}

normalize_text <- function(x) {
  x <- safe_chr(x)
  x <- str_replace_all(x, "\u00A0", " ")
  x <- str_squish(x)
  x
}

blank_to_na <- function(x) {
  x <- normalize_text(x)
  x[x == ""] <- NA_character_
  x
}

first_existing_col <- function(df, candidates, default = NA_character_) {
  existing <- candidates[candidates %in% names(df)]
  
  if (length(existing) == 0) {
    return(rep(default, nrow(df)))
  }
  
  out <- as.character(df[[existing[1]]])
  
  if (length(existing) > 1) {
    for (nm in existing[-1]) {
      out <- dplyr::coalesce(blank_to_na(out), blank_to_na(df[[nm]]))
    }
  }
  
  out <- as.character(out)
  out[is.na(out)] <- ""
  out
}

parse_date_any <- function(x) {
  x <- safe_chr(x)
  
  suppressWarnings(as.Date(parse_date_time(
    x,
    orders = c(
      "ymd", "dmy", "mdy",
      "ymd HMS", "ymd HM",
      "dmy HMS", "dmy HM",
      "mdy HMS", "mdy HM",
      "Ymd HMS", "Ymd HM"
    ),
    tz = LOCAL_TZ
  )))
}

parse_datetime_any <- function(x) {
  x <- blank_to_na(x)
  
  # Ona timestamps include fractional seconds and colon-formatted offsets,
  # for example: 2026-08-27T18:38:27.223750+00:00.
  # Normalize them before parsing so valid rows are not filtered out later.
  x_normalized <- str_replace(x, "Z$", "+0000")
  x_normalized <- str_replace(
    x_normalized,
    "([+-][0-9]{2}):([0-9]{2})$",
    "\\1\\2"
  )
  x_normalized <- str_replace(
    x_normalized,
    "(\\.[0-9]+)([+-][0-9]{4})$",
    "\\2"
  )
  x_normalized <- str_replace(x_normalized, "\\.[0-9]+$", "")
  
  suppressWarnings(parse_date_time(
    x_normalized,
    orders = c(
      "ymd HMSz", "ymd HMz",
      "ymd HMS", "ymd HM", "ymd",
      "dmy HMS", "dmy HM", "dmy",
      "mdy HMS", "mdy HM", "mdy",
      "ymd_HMS", "ymd_HM",
      "Ymd HMS", "Ymd HM"
    ),
    tz = LOCAL_TZ
  ))
}

yes_flag <- function(x) {
  str_to_lower(normalize_text(x)) %in% c(
    "yes", "y", "1", "true", "positive", "present",
    "done", "checked", "selected", "ok"
  )
}

normalize_country_value <- function(x) {
  x0 <- normalize_text(x)
  xl <- str_to_lower(x0)
  
  out <- case_when(
    xl == "" ~ "Missing",
    
    xl %in% c("uga", "ug", "ugand", "uganda", "ugnda", "ugandan") ~ "Uganda",
    str_detect(xl, "^ugand") ~ "Uganda",
    
    xl %in% c(
      "drc", "dr congo", "congo dr", "cod",
      "congo democratic republic", "democratic republic of congo",
      "democratic republic of the congo",
      "congo (democratic republic of the)"
    ) ~ "Democratic Republic of the Congo",
    str_detect(xl, "democratic republic") & str_detect(xl, "congo") ~ "Democratic Republic of the Congo",
    
    xl %in% c("congo", "republic of congo", "congo republic") ~ "Congo",
    xl %in% c("ssd", "south sudan", "s. sudan") ~ "South Sudan",
    xl %in% c("sdn", "sudan") ~ "Sudan",
    xl %in% c("ken", "kenya") ~ "Kenya",
    xl %in% c("eth", "ethiopia") ~ "Ethiopia",
    xl %in% c("ita", "italy") ~ "Italy",
    xl %in% c("gambia", "the gambia") ~ "Gambia",
    xl %in% c("canadian", "canada") ~ "Canada",
    xl %in% c("eritrea", "eritrean") ~ "Eritrea",
    
    TRUE ~ str_to_title(x0)
  )
  
  out <- str_squish(out)
  out[out == ""] <- "Missing"
  out
}

normalize_country_list <- function(x) {
  x <- safe_chr(x)
  
  map_chr(x, function(one) {
    one <- normalize_text(one)
    
    if (one == "") {
      return("")
    }
    
    parts <- unlist(str_split(one, "\\s*[,;|/]\\s*"))
    parts <- parts[parts != ""]
    parts <- normalize_country_value(parts)
    parts <- unique(parts[parts != "Missing"])
    
    if (length(parts) == 0) {
      ""
    } else {
      paste(parts, collapse = ", ")
    }
  })
}

normalize_city_value <- function(x) {
  x0 <- normalize_text(x)
  xl <- str_to_lower(x0)
  
  out <- case_when(
    xl == "" ~ "Not recorded",
    xl %in% c("kampala", "kamapala", "kampal", "kampaala") ~ "Kampala",
    xl %in% c("entebbe", "entebe", "entebbe airport") ~ "Entebbe",
    xl %in% c("juba", "juba city") ~ "Juba",
    xl %in% c("ugand", "uganda") ~ "Uganda - city not specified",
    xl %in% c("gudele", "gudeli", "gudele 2", "gudelle") ~ "Gudele",
    xl %in% c("atlabara", "atlbara") ~ "Atlabara",
    xl %in% c("thonping", "tomping", "tongping") ~ "Tomping",
    xl %in% c("venice", "venezia") ~ "Venice",
    TRUE ~ str_to_title(x0)
  )
  
  out <- str_squish(out)
  out[out == ""] <- "Not recorded"
  out
}

normalize_transport_value <- function(x) {
  x0 <- normalize_text(x)
  xl <- str_to_lower(x0)
  
  out <- case_when(
    xl == "" ~ "Not recorded",
    xl %in% c("air", "airplane", "plane", "flight", "aircraft") ~ "Air",
    xl %in% c("road", "vehicle", "car", "bus", "truck") ~ "Road",
    xl %in% c("boat", "water", "ship") ~ "Water",
    TRUE ~ str_to_title(x0)
  )
  
  out
}

normalize_airline_value <- function(x) {
  x0 <- normalize_text(x)
  xl <- str_to_lower(x0)
  
  out <- case_when(
    xl == "" ~ "Not recorded",
    str_detect(xl, "uganda") | xl %in% c("ug air", "ug airway", "ug airways") ~ "Uganda Airlines",
    str_detect(xl, "ethiopian") ~ "Ethiopian Airlines",
    str_detect(xl, "kenya") ~ "Kenya Airways",
    TRUE ~ str_to_title(x0)
  )
  
  out
}

normalize_purpose_value <- function(x) {
  x0 <- normalize_text(x)
  xl <- str_to_lower(x0)
  
  out <- case_when(
    xl == "" ~ "Not recorded",
    str_detect(xl, "business|bussiness") ~ "Business",
    str_detect(xl, "official|diplomat|mission") ~ "Official",
    str_detect(xl, "family|visit") ~ "Visit family",
    str_detect(xl, "return|home|comong ho|coming home") ~ "Return home",
    str_detect(xl, "student|study") ~ "Study",
    str_detect(xl, "stay") ~ "Stay",
    TRUE ~ str_to_title(x0)
  )
  
  out
}

shorten_symptom_label <- function(x) {
  x0 <- normalize_text(x)
  x0 <- str_replace_all(x0, "_", " ")
  x0 <- str_replace_all(x0, "/", " / ")
  x0 <- str_squish(x0)
  xl <- str_to_lower(x0)
  
  out <- case_when(
    xl == "" ~ "Not specified",
    str_detect(xl, "severe headache|headache") ~ "Headache",
    str_detect(xl, "muscle pain|myalgia") ~ "Muscle pain",
    str_detect(xl, "weakness|fatigue") ~ "Weakness / fatigue",
    str_detect(xl, "sore throat") ~ "Sore throat",
    str_detect(xl, "vomit") ~ "Vomiting",
    str_detect(xl, "diarr") ~ "Diarrhea",
    str_detect(xl, "stomach|abdominal") ~ "Abdominal pain",
    str_detect(xl, "difficulty breathing|breath") ~ "Breathing difficulty",
    str_detect(xl, "unexplained bleeding|bleeding") ~ "Bleeding",
    str_detect(xl, "skin rash|rash") ~ "Skin rash",
    str_detect(xl, "red eyes|red eye") ~ "Red eyes",
    str_detect(xl, "fever") ~ "Fever",
    TRUE ~ str_to_sentence(x0)
  )
  
  out <- str_squish(out)
  out[out == ""] <- "Not specified"
  out
}

normalize_sex_value <- function(x) {
  x0 <- normalize_text(x)
  xl <- str_to_lower(x0)
  
  out <- case_when(
    xl %in% c("male", "m") ~ "Male",
    xl %in% c("female", "f") ~ "Female",
    xl == "" ~ "Missing",
    TRUE ~ str_to_title(x0)
  )
  
  out
}

normalize_status <- function(x) {
  x <- str_to_upper(normalize_text(x))
  
  out <- case_when(
    str_detect(x, "CLEAR") ~ "Cleared",
    str_detect(x, "REFER|ALERT|HOLD|RISK|ESCALATE|ISOLAT") ~ "Referred / Alert",
    str_detect(x, "PENDING|WAIT") ~ "Pending",
    x == "" ~ "Pending",
    TRUE ~ "Other"
  )
  
  out <- safe_chr(out)
  out[out == ""] <- "Pending"
  out
}

normalize_risk <- function(x) {
  x <- str_to_lower(normalize_text(x))
  
  out <- case_when(
    str_detect(x, "high|alert|refer") ~ "High risk",
    str_detect(x, "moderate|medium") ~ "Moderate risk",
    str_detect(x, "standard|low|clear") ~ "Standard",
    x == "" ~ "Not classified",
    TRUE ~ str_to_title(x)
  )
  
  out <- safe_chr(out)
  out[out == ""] <- "Not classified"
  out
}

extract_selected_multiselect <- function(df, prefixes) {
  if (is.null(df) || nrow(df) == 0) {
    return(character())
  }
  
  escaped <- stringr::str_replace_all(
    prefixes,
    "([\\[\\]{}()+*^$|\\\\.?])",
    "\\\\\\1"
  )
  
  pattern <- paste0("^(", paste(escaped, collapse = "|"), ")")
  cols <- names(df)[str_detect(names(df), pattern)]
  
  if (length(cols) == 0) {
    return(rep("", nrow(df)))
  }
  
  mat <- df[, cols, drop = FALSE]
  
  out <- map_chr(seq_len(nrow(mat)), function(i) {
    vals <- safe_chr(unlist(mat[i, , drop = TRUE]))
    selected <- yes_flag(vals)
    
    if (!any(selected, na.rm = TRUE)) {
      return("")
    }
    
    selected_cols <- cols[selected]
    labels <- selected_cols
    
    for (p in prefixes) {
      labels <- str_remove(labels, fixed(p))
    }
    
    labels <- str_replace_all(labels, "_", " ")
    labels <- str_squish(labels)
    labels <- labels[labels != ""]
    
    paste(unique(labels), collapse = ", ")
  })
  
  out
}

make_age_group <- function(age_years) {
  cut(
    age_years,
    breaks = c(-Inf, 4, 14, 24, 44, 64, Inf),
    labels = c("0-4", "5-14", "15-24", "25-44", "45-64", "65+")
  )
}

fallback_choices <- function(x, fallback = "No data yet") {
  x <- sort(unique(safe_chr(x)))
  x <- x[x != ""]
  if (length(x) == 0) fallback else x
}

safe_date_min <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    as.Date(with_tz(Sys.time(), LOCAL_TZ))
  } else {
    min(x, na.rm = TRUE)
  }
}

safe_date_max <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    as.Date(with_tz(Sys.time(), LOCAL_TZ))
  } else {
    max(x, na.rm = TRUE)
  }
}

safe_epi_choices <- function(df) {
  weeks <- sort(unique(safe_chr(df$epi_week)))
  weeks <- weeks[weeks != ""]
  
  if (length(weeks) == 0) {
    today <- as.Date(with_tz(Sys.time(), LOCAL_TZ))
    weeks <- paste0(year(today), "-W", sprintf("%02d", epiweek(today)))
  }
  
  weeks
}

empty_plotly <- function(message = "No data available") {
  plot_ly(
    type = "scatter",
    mode = "markers",
    x = numeric(0),
    y = numeric(0)
  ) %>%
    layout(
      annotations = list(
        text = message,
        x = 0.5,
        y = 0.5,
        showarrow = FALSE,
        font = list(size = 16)
      ),
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE)
    )
}
empty_evd_data <- function() {
  tibble(
    submission_time = as.POSIXct(character(), tz = LOCAL_TZ),
    submission_date = as.Date(character()),
    submission_hour = as.POSIXct(character(), tz = LOCAL_TZ),
    hour_label = character(),
    epi_week = character(),
    screening_id = character(),
    full_name = character(),
    passport_id = character(),
    phone_number = character(),
    sex = character(),
    age_years = numeric(),
    age_group = factor(character(), levels = c("0-4", "5-14", "15-24", "25-44", "45-64", "65+")),
    nationality = character(),
    occupation = character(),
    residence = character(),
    country_departure = character(),
    city_departure = character(),
    countries_visited = character(),
    arrival_date = as.Date(character()),
    arrival_date_validated = as.Date(character()),
    arrival_date_quality_flag = character(),
    transport_mode = character(),
    vehicle_flight_name = character(),
    flight_number = character(),
    seat_number = character(),
    destination_ss = character(),
    duration_stay = numeric(),
    purpose_travel = character(),
    symptom_text = character(),
    medical_care_flag = logical(),
    measured_temperature = numeric(),
    clearance_status = character(),
    risk_category = character(),
    fever_flag = logical(),
    high_temperature_flag = logical(),
    symptom_alert = logical(),
    exposure_alert = logical(),
    contact_flag = logical(),
    funeral_flag = logical(),
    touched_body_flag = logical(),
    cared_sick_flag = logical(),
    health_facility_flag = logical(),
    body_fluids_flag = logical(),
    wildlife_flag = logical(),
    animal_bite_flag = logical(),
    affected_area_flag = logical(),
    priority_country_flag = logical(),
    core_priority_country_flag = logical(),
    missing_followup_flag = logical(),
    incomplete_form_flag = logical(),
    red_flag = logical(),
    active_evd_alert = logical(),
    followup_required = logical(),
    enhanced_monitoring_only = logical(),
    possible_duplicate_flag = logical(),
    alert_cleared = logical(),
    red_flag_reason = character(),
    action_priority = character(),
    screening_minutes = numeric()
  )
}

# ============================================================
# 4. ONA API PULL
# ============================================================

get_ona_token <- function() {
  # Use the verified credential directly so an old ONA_API_TOKEN environment
  # value cannot override it and cause HTTP 401 during local RStudio runs.
  token <- trimws(ONA_API_TOKEN_FALLBACK)
  token <- str_remove(token, regex("^Token\\s+", ignore_case = TRUE))
  
  if (is.null(token) || !nzchar(token)) {
    message("Ona API token is missing. Set ONA_API_TOKEN before starting the app.")
    return("")
  }
  
  token
}

ona_get_json <- function(url, token, query = list(), context = "Ona API request") {
  last_error <- NULL
  
  for (i in seq_along(ONA_API_RETRY_SECONDS)) {
    if (ONA_API_RETRY_SECONDS[[i]] > 0) {
      Sys.sleep(ONA_API_RETRY_SECONDS[[i]])
    }
    
    response <- tryCatch(
      {
        req <- request(url) %>%
          req_user_agent("South-Sudan-EVD-Readiness-Situation-Room/Ona") %>%
          req_headers(
            Authorization = paste("Token", token),
            Accept = "application/json",
            `Cache-Control` = "no-cache",
            Pragma = "no-cache"
          ) %>%
          req_timeout(45) %>%
          req_error(is_error = function(resp) FALSE)
        
        if (length(query) > 0) {
          req <- do.call(req_url_query, c(list(req), query))
        }
        
        req_perform(req)
      },
      error = function(e) {
        last_error <<- conditionMessage(e)
        NULL
      }
    )
    
    if (is.null(response)) {
      next
    }
    
    status <- resp_status(response)
    
    if (status == 200) {
      parsed <- tryCatch(
        resp_body_json(response, simplifyVector = FALSE),
        error = function(e) {
          last_error <<- paste("Could not parse Ona JSON:", conditionMessage(e))
          NULL
        }
      )
      
      if (!is.null(parsed)) {
        return(parsed)
      }
    }
    
    last_error <- paste0("HTTP ", status, " ", resp_status_desc(response))
    
    if (status %in% c(401, 403, 404)) {
      break
    }
  }
  
  message(context, " failed: ", last_error)
  NULL
}

resolve_ona_form_pk <- function(token, id_string = ONA_FORM_ID_STRING) {
  if (!is.na(ONA_FORM_PK_RESOLVED)) {
    return(ONA_FORM_PK_RESOLVED)
  }
  
  if (!is.na(ONA_FORM_PK_FALLBACK)) {
    ONA_FORM_PK_RESOLVED <<- as.integer(ONA_FORM_PK_FALLBACK)
    return(ONA_FORM_PK_RESOLVED)
  }
  
  target_id <- str_to_lower(normalize_text(id_string))
  
  parse_catalogue <- function(parsed) {
    if (is.null(parsed) || length(parsed) == 0) {
      return(tibble())
    }
    
    if (is.list(parsed) && !is.null(names(parsed)) && "results" %in% names(parsed)) {
      parsed <- parsed$results
    }
    
    tryCatch(
      {
        jsonlite::fromJSON(
          jsonlite::toJSON(parsed, auto_unbox = TRUE, null = "null"),
          flatten = TRUE
        ) %>%
          as_tibble()
      },
      error = function(e) {
        message("Could not parse Ona form catalogue: ", conditionMessage(e))
        tibble()
      }
    )
  }
  
  # First use the forms catalogue, where the numeric identifier is `formid`.
  parsed <- ona_get_json(
    url = paste0(ONA_API_BASE, "/forms"),
    token = token,
    query = list(limit = 10000, cache_bust = as.integer(Sys.time())),
    context = "Ona form lookup"
  )
  
  forms <- parse_catalogue(parsed)
  
  if (nrow(forms) > 0 && all(c("formid", "id_string") %in% names(forms))) {
    exact <- forms %>%
      filter(str_to_lower(normalize_text(.data$id_string)) == target_id)
    
    if (nrow(exact) == 0 && "title" %in% names(forms)) {
      exact <- forms %>%
        filter(str_to_lower(normalize_text(.data$title)) == target_id)
    }
    
    if (nrow(exact) > 0) {
      pk <- suppressWarnings(as.integer(exact$formid[[1]]))
      
      if (!is.na(pk)) {
        ONA_FORM_PK_RESOLVED <<- pk
        message("Resolved Ona form id_string '", id_string, "' to numeric form ID ", pk, ".")
        return(pk)
      }
    }
  }
  
  # Fallback: Ona's /data catalogue also exposes id + id_string.
  endpoints_parsed <- ona_get_json(
    url = paste0(ONA_API_BASE, "/data"),
    token = token,
    query = list(limit = 10000, cache_bust = as.integer(Sys.time())),
    context = "Ona data endpoint lookup"
  )
  
  endpoints <- parse_catalogue(endpoints_parsed)
  
  if (nrow(endpoints) > 0 && all(c("id", "id_string") %in% names(endpoints))) {
    exact <- endpoints %>%
      filter(str_to_lower(normalize_text(.data$id_string)) == target_id)
    
    if (nrow(exact) == 0 && "title" %in% names(endpoints)) {
      exact <- endpoints %>%
        filter(str_to_lower(normalize_text(.data$title)) == target_id)
    }
    
    if (nrow(exact) > 0) {
      pk <- suppressWarnings(as.integer(exact$id[[1]]))
      
      if (!is.na(pk)) {
        ONA_FORM_PK_RESOLVED <<- pk
        message("Resolved Ona form id_string '", id_string, "' to numeric form ID ", pk, " via /data catalogue.")
        return(pk)
      }
    }
  }
  
  message(
    "No accessible Ona form matched id_string '",
    id_string,
    "'. Confirm the form ID string and that the API token has view/download access to the project."
  )
  NA_integer_
}

fetch_ona_page <- function(token, form_pk, start_at = 0, limit = ONA_PAGE_SIZE) {
  parsed <- ona_get_json(
    url = paste0(ONA_API_BASE, "/data/", form_pk, ".json"),
    token = token,
    query = list(
      start = start_at,
      limit = limit,
      cache_bust = as.integer(Sys.time())
    ),
    context = paste0("Ona data page request at start=", start_at, ", limit=", limit)
  )
  
  if (is.null(parsed)) {
    return(NULL)
  }
  
  parsed
}

fetch_ona_data <- function() {
  token <- get_ona_token()
  
  if (!nzchar(token)) {
    return(tibble())
  }
  
  form_pk <- resolve_ona_form_pk(token)
  
  if (is.na(form_pk)) {
    return(tibble())
  }
  
  all_records <- list()
  start_at <- 0
  page_size <- min(ONA_PAGE_SIZE, ONA_LIMIT)
  max_records_to_fetch <- min(ONA_LIMIT, ONA_MAX_RECORDS)
  
  repeat {
    records <- fetch_ona_page(
      token = token,
      form_pk = form_pk,
      start_at = start_at,
      limit = page_size
    )
    
    if (is.null(records) || length(records) == 0) {
      break
    }
    
    all_records <- c(all_records, records)
    
    if (length(all_records) >= max_records_to_fetch) {
      all_records <- all_records[seq_len(max_records_to_fetch)]
      break
    }
    
    if (length(records) < page_size) {
      break
    }
    
    start_at <- start_at + page_size
  }
  
  if (length(all_records) == 0) {
    return(tibble())
  }
  
  df <- tryCatch(
    {
      jsonlite::flatten(
        jsonlite::fromJSON(
          jsonlite::toJSON(all_records, auto_unbox = TRUE, null = "null")
        )
      )
    },
    error = function(e) {
      message("Could not flatten Ona records: ", conditionMessage(e))
      return(tibble())
    }
  )
  
  last_refresh_text(format(with_tz(Sys.time(), LOCAL_TZ), "%d %b %Y %H:%M:%S"))
  
  as_tibble(df)
}

# ============================================================
# 4A. FAST LOCAL CACHE HELPERS
# ============================================================

load_ona_cache <- function(cache_file = ONA_CACHE_FILE) {
  if (!file.exists(cache_file)) {
    return(tibble())
  }
  
  out <- tryCatch(
    readRDS(cache_file),
    error = function(e) {
      message("Could not read local Ona cache: ", conditionMessage(e))
      tibble()
    }
  )
  
  if (is.null(out) || !is.data.frame(out)) {
    return(tibble())
  }
  
  as_tibble(out) %>% mutate(across(everything(), as.character))
}

save_ona_cache <- function(df, cache_file = ONA_CACHE_FILE) {
  if (is.null(df) || nrow(df) == 0) {
    return(invisible(FALSE))
  }
  
  tryCatch(
    {
      saveRDS(df, cache_file)
      TRUE
    },
    error = function(e) {
      message("Could not write local Ona cache: ", conditionMessage(e))
      FALSE
    }
  )
}

# ============================================================
# 4B. RAW ONA DATA FOR RESTRICTED AUTHORIZED MODULE
# Keeps all original ODK/Ona columns for authorized red-flag review.
# ============================================================

read_raw_ona_data <- function() {
  raw <- fetch_ona_data()
  
  if (is.null(raw) || nrow(raw) == 0) {
    return(tibble())
  }
  
  raw %>%
    mutate(across(everything(), as.character))
}

# ============================================================
# 5. STANDARDIZE ONA DATA
# ============================================================

standardize_evd_data <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(empty_evd_data())
  }
  
  df <- df %>%
    mutate(across(everything(), as.character))
  
  start_time <- parse_datetime_any(first_existing_col(df, c("start_time", "start", "Start Time")))
  end_time <- parse_datetime_any(first_existing_col(df, c("end_time", "end", "End Time")))
  
  submission_time <- parse_datetime_any(first_existing_col(
    df,
    c("_submission_time", "Submission Date", "submission_date", "timestamp", "end_time", "end", "End Time")
  ))
  
  submission_date <- as.Date(submission_time, tz = LOCAL_TZ)
  submission_hour <- floor_date(submission_time, "hour")
  hour_label <- format(submission_hour, "%H:%M")
  
  full_name <- first_existing_col(df, c("personal_info/full_name", "Full Name"))
  passport_id <- first_existing_col(df, c("personal_info/passport_id", "Passport/National ID Number"))
  phone_number <- first_existing_col(df, c("personal_info/phone_number", "Phone Number"))
  
  sex_raw <- first_existing_col(df, c("personal_info/sex", "Sex"))
  sex <- normalize_sex_value(sex_raw)
  
  date_of_birth <- parse_date_any(first_existing_col(df, c("personal_info/date_of_birth", "Date of Birth")))
  age_raw <- safe_num(first_existing_col(df, c("personal_info/age", "Age")))
  
  derived_age <- ifelse(
    !is.na(age_raw),
    age_raw,
    ifelse(
      !is.na(date_of_birth),
      floor(as.numeric(Sys.Date() - date_of_birth) / 365.25),
      NA_real_
    )
  )
  
  nationality_raw <- first_existing_col(df, c("personal_info/nationality", "Nationality"))
  occupation_raw <- first_existing_col(df, c("personal_info/occupation", "Occupation"))
  residence_raw <- first_existing_col(df, c("personal_info/current_residence", "Current Residence"))
  
  nationality <- normalize_country_value(nationality_raw)
  occupation <- str_to_title(normalize_text(occupation_raw))
  occupation[occupation == ""] <- "Not recorded"
  residence <- normalize_city_value(residence_raw)
  
  country_departure_raw <- first_existing_col(df, c("travel_info/country", "Country of Departure"))
  city_departure_raw <- first_existing_col(df, c("travel_info/city", "City of Departure"))
  
  country_departure <- normalize_country_value(country_departure_raw)
  city_departure <- normalize_city_value(city_departure_raw)
  
  countries_visited_main <- first_existing_col(
    df,
    c("travel_info/countries", "Countries Visited in Last 21 Days")
  )
  
  countries_visited_binary <- extract_selected_multiselect(
    df,
    c(
      "travel_info/countries/",
      "Countries Visited in Last 21 Days/"
    )
  )
  
  countries_visited <- dplyr::coalesce(
    blank_to_na(countries_visited_main),
    blank_to_na(countries_visited_binary)
  )
  countries_visited[is.na(countries_visited)] <- ""
  countries_visited <- normalize_country_list(countries_visited)
  
  arrival_date <- parse_date_any(first_existing_col(df, c("travel_info/arrival_date", "Date of Arrival")))
  
  arrival_date_quality_flag <- ifelse(
    is.na(arrival_date),
    "Missing arrival date",
    ifelse(
      !is.na(submission_date) &
        (
          arrival_date < submission_date - days(ARRIVAL_BACKWARD_TOLERANCE_DAYS) |
            arrival_date > submission_date + days(ARRIVAL_FORWARD_TOLERANCE_DAYS)
        ),
      "Arrival date outside expected range",
      "Valid arrival date"
    )
  )
  
  arrival_date_validated <- ifelse(
    arrival_date_quality_flag == "Valid arrival date",
    as.character(arrival_date),
    NA_character_
  )
  arrival_date_validated <- as.Date(arrival_date_validated)
  
  transport_mode_raw <- first_existing_col(df, c("travel_info/transport_mode", "Means of Transport"))
  transport_mode <- normalize_transport_value(transport_mode_raw)
  
  vehicle_flight_name_raw <- first_existing_col(
    df,
    c("travel_info/vehicle_flight_name", "Flight Name / Airline", "Flight Name / Airline ")
  )
  vehicle_flight_name <- normalize_airline_value(vehicle_flight_name_raw)
  
  flight_number <- normalize_text(first_existing_col(
    df,
    c("travel_info/vehicle_flight_number", "travel_info/flight_number", "Flight Number")
  ))
  flight_number[flight_number == ""] <- "Not recorded"
  
  seat_number <- str_to_upper(normalize_text(first_existing_col(df, c("travel_info/seat_number", "Seat Number"))))
  seat_number[seat_number == ""] <- "Not recorded"
  
  destination_raw <- first_existing_col(df, c("travel_info/destination_ss", "Final Destination in South Sudan"))
  destination_ss <- normalize_city_value(destination_raw)
  
  duration_stay <- safe_num(first_existing_col(df, c("travel_info/duration_stay", "Duration of Stay in South Sudan")))
  
  purpose_raw <- first_existing_col(df, c("travel_info/purpose_travel", "Purpose of Travel"))
  purpose_travel <- normalize_purpose_value(purpose_raw)
  
  symptom_text_main <- first_existing_col(
    df,
    c(
      "health_screening/symptoms",
      "health_screening/symptom_list",
      "health_screening/symptoms_reported",
      "risk_assessment/symptoms",
      "risk_assessment/symptom_list",
      "Select any symptoms currently present",
      "symptoms",
      "symptom_list"
    )
  )
  
  symptom_text_binary <- extract_selected_multiselect(
    df,
    c(
      "health_screening/symptoms/",
      "risk_assessment/symptoms/",
      "Select any symptoms currently present/"
    )
  )
  
  symptom_text <- dplyr::coalesce(
    blank_to_na(symptom_text_main),
    blank_to_na(symptom_text_binary)
  )
  symptom_text[is.na(symptom_text)] <- ""
  
  medical_care <- first_existing_col(
    df,
    c(
      "health_screening/medical_care",
      "health_screening/seeking_medical_care",
      "health_screening/medical_consultation",
      "risk_assessment/medical_care",
      "Have you sought medical care for these symptoms?",
      "medical_care",
      "seeking_medical_care"
    )
  )
  
  medical_care_flag <- yes_flag(medical_care)
  
  measured_temperature <- safe_num(first_existing_col(df, c("health_screening/temperature", "Temperature", "Measured Temperature")))
  
  fever_question <- first_existing_col(
    df,
    c(
      "health_screening/fever",
      "Do you have a fever (temperature >= 37.5 C)?"
    )
  )
  
  contact_ebola <- first_existing_col(df, c("risk_assessment/contact_ebola", "Contact with confirmed/suspected Ebola patient in last 21 days?"))
  funeral_attendance <- first_existing_col(df, c("risk_assessment/funeral_attendance", "Attended funeral or burial ceremony in last 21 days?"))
  touched_body <- first_existing_col(df, c("risk_assessment/touched_body", "Touched a dead body during burial rituals?"))
  cared_sick <- first_existing_col(df, c("risk_assessment/cared_sick", "Cared for sick person with fever, vomiting, diarrhea, or bleeding?"))
  visited_health_facility <- first_existing_col(df, c("risk_assessment/visited_health_facility", "Visited health facility treating Ebola patients?"))
  body_fluids <- first_existing_col(df, c("risk_assessment/body_fluids", "Handled blood or body fluids without protection?"))
  wild_animals <- first_existing_col(df, c("risk_assessment/wild_animals", "Handled bush meat or wild animals?"))
  animal_bite <- first_existing_col(df, c("risk_assessment/animal_bite", "Been bitten or scratched by a wild animal?"))
  affected_area <- first_existing_col(df, c("risk_assessment/affected_area", "Any history of travel to  Ebola-affected area in last 21 days?"))
  
  symptom_flag_raw <- first_existing_col(df, c("risk_assessment/symptom_flag", "symptom_flag", "Do you currently have any symptoms?"))
  exposure_flag_raw <- first_existing_col(df, c("risk_assessment/exposure_flag", "exposure_flag"))
  
  risk_category <- normalize_risk(first_existing_col(df, c("risk_assessment/risk_category", "risk_category")))
  clearance_status <- normalize_status(first_existing_col(df, c("clearance_status")))
  
  host_name <- first_existing_col(df, c("followup_contact/host_name", "Name of Host/Contact Person in South Sudan"))
  host_phone <- first_existing_col(df, c("followup_contact/host_phone", "Phone Number of Host"))
  host_address <- first_existing_col(df, c("followup_contact/host_address", "Address/Location in South Sudan"))
  declaration <- first_existing_col(df, c("followup_contact/declaration", "I declare that the information provided is true and complete to the best of my knowledge."))
  
  screening_id <- first_existing_col(df, c("screening_id", "participant_id", "_uuid", "_id"))
  
  fever_reported <- safe_lgl(yes_flag(fever_question))
  symptom_flag_reported <- safe_lgl(yes_flag(symptom_flag_raw) | safe_num(symptom_flag_raw) == 1)
  
  fever_flag <- yes_flag(fever_question) |
    str_detect(str_to_lower(symptom_text), "fever") |
    (!is.na(measured_temperature) & measured_temperature >= FEVER_THRESHOLD)
  
  high_temperature_flag <- !is.na(measured_temperature) & measured_temperature >= HIGH_TEMP_THRESHOLD
  
  symptom_alert <- fever_flag |
    high_temperature_flag |
    yes_flag(symptom_flag_raw) |
    safe_num(symptom_flag_raw) == 1 |
    normalize_text(symptom_text) != ""
  
  contact_flag <- yes_flag(contact_ebola)
  funeral_flag <- yes_flag(funeral_attendance)
  touched_body_flag <- yes_flag(touched_body)
  cared_sick_flag <- yes_flag(cared_sick)
  health_facility_flag <- yes_flag(visited_health_facility)
  body_fluids_flag <- yes_flag(body_fluids)
  wildlife_flag <- yes_flag(wild_animals)
  animal_bite_flag <- yes_flag(animal_bite)
  affected_area_flag <- yes_flag(affected_area)
  
  exposure_alert <- contact_flag |
    funeral_flag |
    touched_body_flag |
    cared_sick_flag |
    health_facility_flag |
    body_fluids_flag |
    wildlife_flag |
    animal_bite_flag |
    affected_area_flag |
    yes_flag(exposure_flag_raw) |
    safe_num(exposure_flag_raw) == 1
  
  countries_for_priority <- paste(country_departure, countries_visited, sep = ", ")
  
  core_priority_country_flag <- str_detect(
    str_to_lower(countries_for_priority),
    "uganda|democratic republic of the congo|drc|congo \\(democratic"
  )
  
  priority_country_flag <- str_detect(
    str_to_lower(countries_for_priority),
    "uganda|democratic republic of the congo|drc|congo|sudan|kenya|ethiopia"
  )
  
  missing_identity_flag <- normalize_text(full_name) == "" | normalize_text(passport_id) == ""
  missing_followup_flag <- normalize_text(host_name) == "" | normalize_text(host_phone) == "" | normalize_text(host_address) == ""
  
  incomplete_form_flag <- missing_identity_flag |
    normalize_text(phone_number) == "" |
    nationality == "Missing" |
    country_departure == "Missing" |
    is.na(arrival_date_validated) |
    transport_mode == "Not recorded" |
    normalize_text(declaration) == ""
  
  fever_flag <- safe_lgl(fever_flag)
  high_temperature_flag <- safe_lgl(high_temperature_flag)
  symptom_alert <- safe_lgl(symptom_alert)
  exposure_alert <- safe_lgl(exposure_alert)
  priority_country_flag <- safe_lgl(priority_country_flag)
  core_priority_country_flag <- safe_lgl(core_priority_country_flag)
  missing_followup_flag <- safe_lgl(missing_followup_flag)
  incomplete_form_flag <- safe_lgl(incomplete_form_flag)
  medical_care_flag <- safe_lgl(medical_care_flag)
  
  contact_flag <- safe_lgl(contact_flag)
  funeral_flag <- safe_lgl(funeral_flag)
  touched_body_flag <- safe_lgl(touched_body_flag)
  cared_sick_flag <- safe_lgl(cared_sick_flag)
  health_facility_flag <- safe_lgl(health_facility_flag)
  body_fluids_flag <- safe_lgl(body_fluids_flag)
  wildlife_flag <- safe_lgl(wildlife_flag)
  animal_bite_flag <- safe_lgl(animal_bite_flag)
  affected_area_flag <- safe_lgl(affected_area_flag)
  
  risk_category_form <- safe_chr(risk_category)
  
  risk_category <- ifelse(
    risk_category == "Not classified" & (symptom_alert | exposure_alert),
    "High risk",
    risk_category
  )
  
  risk_category <- safe_chr(risk_category)
  risk_category[risk_category == ""] <- "Not classified"
  
  clearance_status <- ifelse(
    clearance_status == "Pending" & !(symptom_alert | exposure_alert | risk_category == "High risk"),
    "Cleared",
    clearance_status
  )
  
  clearance_status <- ifelse(
    symptom_alert | exposure_alert | risk_category == "High risk",
    "Referred / Alert",
    clearance_status
  )
  
  clearance_status <- safe_chr(clearance_status)
  clearance_status[clearance_status == ""] <- "Pending"
  
  # ----------------------------------------------------------
  # WATCH DESK ALERT CLASSIFICATION UPDATE
  # Travel history alone is NOT an active EVD alert.
  # Active EVD alert = compatible symptoms/fever PLUS exposure or priority EVD travel context,
  # or any record explicitly classified as high risk with clinical/exposure signal.
  # Exposure without symptoms is a follow-up/monitoring signal, not an active EVD alert.
  # ----------------------------------------------------------
  
  active_evd_alert <- (
    symptom_alert &
      (
        exposure_alert |
          affected_area_flag |
          contact_flag |
          core_priority_country_flag |
          risk_category == "High risk"
      )
  ) |
    (high_temperature_flag & exposure_alert)
  
  followup_required <- (!symptom_alert & exposure_alert) |
    missing_followup_flag |
    incomplete_form_flag
  
  enhanced_monitoring_only <- priority_country_flag &
    !symptom_alert &
    !exposure_alert &
    !risk_category %in% c("High risk", "Moderate risk")
  
  active_evd_alert <- safe_lgl(active_evd_alert)
  followup_required <- safe_lgl(followup_required)
  enhanced_monitoring_only <- safe_lgl(enhanced_monitoring_only)
  
  red_flag <- active_evd_alert
  red_flag <- safe_lgl(red_flag)
  
  red_flag_reason <- pmap_chr(
    list(
      clearance_status,
      risk_category,
      symptom_alert,
      fever_flag,
      high_temperature_flag,
      exposure_alert,
      missing_followup_flag,
      incomplete_form_flag
    ),
    function(status, risk, symptom, fever, high_temp, exposure, missing_followup, incomplete_form) {
      
      status <- safe_chr(status)
      risk <- safe_chr(risk)
      
      if (length(status) == 0 || is.na(status) || status == "") {
        status <- "Pending"
      }
      
      if (length(risk) == 0 || is.na(risk) || risk == "") {
        risk <- "Not classified"
      }
      
      symptom <- isTRUE(symptom)
      fever <- isTRUE(fever)
      high_temp <- isTRUE(high_temp)
      exposure <- isTRUE(exposure)
      missing_followup <- isTRUE(missing_followup)
      incomplete_form <- isTRUE(incomplete_form)
      
      reasons <- c(
        if (status != "Cleared") "Not cleared / requires review" else NA_character_,
        if (risk %in% c("High risk", "Moderate risk")) risk else NA_character_,
        if (symptom) "Symptom alert" else NA_character_,
        if (fever) "Fever or temperature alert" else NA_character_,
        if (high_temp) paste0("Temperature >= ", HIGH_TEMP_THRESHOLD, " C") else NA_character_,
        if (exposure) "Exposure history alert" else NA_character_,
        if (missing_followup) "Missing host/follow-up information" else NA_character_,
        if (incomplete_form) "Incomplete screening record" else NA_character_
      )
      
      reasons <- reasons[!is.na(reasons)]
      
      if (length(reasons) == 0) {
        "No red flag"
      } else {
        paste(unique(reasons), collapse = "; ")
      }
    }
  )
  
  screening_minutes <- as.numeric(difftime(end_time, start_time, units = "mins"))
  screening_minutes[is.na(screening_minutes) | screening_minutes < 0 | screening_minutes > 240] <- NA_real_
  
  action_priority <- case_when(
    active_evd_alert ~ "Immediate action",
    followup_required ~ "Follow-up required",
    enhanced_monitoring_only | core_priority_country_flag | priority_country_flag | risk_category == "Moderate risk" ~ "Enhanced monitoring",
    TRUE ~ "Routine monitoring"
  )
  
  out <- tibble(
    submission_time = submission_time,
    submission_date = submission_date,
    submission_hour = submission_hour,
    hour_label = hour_label,
    epi_week = paste0(year(submission_date), "-W", sprintf("%02d", epiweek(submission_date))),
    screening_id = safe_chr(screening_id),
    full_name = normalize_text(full_name),
    passport_id = normalize_text(passport_id),
    phone_number = normalize_text(phone_number),
    
    sex = sex,
    age_years = derived_age,
    age_group = make_age_group(derived_age),
    nationality = nationality,
    occupation = occupation,
    residence = residence,
    
    country_departure = country_departure,
    city_departure = city_departure,
    countries_visited = countries_visited,
    arrival_date = arrival_date,
    arrival_date_validated = arrival_date_validated,
    arrival_date_quality_flag = arrival_date_quality_flag,
    
    transport_mode = transport_mode,
    vehicle_flight_name = vehicle_flight_name,
    flight_number = flight_number,
    seat_number = seat_number,
    destination_ss = destination_ss,
    duration_stay = duration_stay,
    purpose_travel = purpose_travel,
    
    symptom_text = safe_chr(symptom_text),
    medical_care_flag = medical_care_flag,
    measured_temperature = measured_temperature,
    clearance_status = clearance_status,
    risk_category = risk_category,
    risk_category_form = risk_category_form,
    fever_reported = fever_reported,
    symptom_flag_reported = symptom_flag_reported,
    fever_flag = fever_flag,
    high_temperature_flag = high_temperature_flag,
    symptom_alert = symptom_alert,
    exposure_alert = exposure_alert,
    contact_flag = contact_flag,
    funeral_flag = funeral_flag,
    touched_body_flag = touched_body_flag,
    cared_sick_flag = cared_sick_flag,
    health_facility_flag = health_facility_flag,
    body_fluids_flag = body_fluids_flag,
    wildlife_flag = wildlife_flag,
    animal_bite_flag = animal_bite_flag,
    affected_area_flag = affected_area_flag,
    priority_country_flag = priority_country_flag,
    core_priority_country_flag = core_priority_country_flag,
    missing_followup_flag = missing_followup_flag,
    incomplete_form_flag = incomplete_form_flag,
    red_flag = red_flag,
    active_evd_alert = active_evd_alert,
    followup_required = followup_required,
    enhanced_monitoring_only = enhanced_monitoring_only,
    possible_duplicate_flag = FALSE,
    red_flag_reason = red_flag_reason,
    action_priority = action_priority,
    screening_minutes = screening_minutes
  ) %>%
    filter(!is.na(submission_time)) %>%
    arrange(desc(submission_time))
  
  out
}

read_screening_data <- function() {
  raw <- fetch_ona_data()
  standardize_evd_data(raw)
}

# ============================================================
# 6. INITIAL DATA
# ============================================================

# Start from the last successful cache. On the first deployment, when no cache
# exists yet, complete one live Ona pull before building the UI so the dashboard
# does not open with empty value boxes and filters.
initial_raw_data <- load_ona_cache()
initial_live_fetch_succeeded <- FALSE

if (is.null(initial_raw_data) || nrow(initial_raw_data) == 0) {
  initial_live_data <- fetch_ona_data()
  
  if (!is.null(initial_live_data) && nrow(initial_live_data) > 0) {
    initial_raw_data <- initial_live_data %>%
      mutate(across(everything(), as.character))
    save_ona_cache(initial_raw_data)
    initial_live_fetch_succeeded <- TRUE
  }
}

initial_data <- standardize_evd_data(initial_raw_data)

INITIAL_SERVER_REFRESH_DELAY_SECONDS <- if (
  isTRUE(initial_live_fetch_succeeded)
) {
  REFRESH_INTERVAL_MS / 1000
} else {
  INITIAL_BACKGROUND_REFRESH_DELAY_SECONDS
}


# PoE daily screening tally
tally_file <- fetch_tally_file()
poe_tally_initial <- load_poe_tally(tally_file$path)
poe_tally_initial$downloaded <- tally_file$downloaded
poe_tally_initial <- p1_add_jia(poe_tally_initial, p1_jia_tally(initial_data))

# ============================================================
# 7. UI
# ============================================================

ui <- dashboardPage(
  title = APP_TITLE,
  
  dashboardHeader(
    title = tags$div(
      style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
      tags$div(
        style = "display: flex; align-items: center; gap: 10px;",
        tags$img(src = MOH_LOGO, height = "50px"),
        tags$img(src = NPHI_LOGO, height = "50px")
      ),
      tags$h1(
        APP_TITLE,
        style = "flex-grow: 1; text-align: center; font-weight: bold; color: white; margin: 0; font-size: 24px;"
      ),
      tags$img(src = WHO_LOGO, height = "50px")
    ),
    titleWidth = "100%"
  ),
  
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    tags$head(tags$script(HTML(paste0("
      setInterval(function() {
        Shiny.setInputValue('browser_keepalive', new Date().getTime(), {priority: 'event'});
      }, ", KEEPALIVE_INTERVAL_MS, ");
    ")))),
    
    
    tags$head(tags$style(HTML("
      .skin-blue .main-header .navbar { background-color: #003b6f; }
      .skin-blue .main-header .logo { background-color: #003b6f; color: white; }
      .skin-blue .main-header .logo:hover { background-color: #003b6f; }
      
            /* Page tabs: large, clearly clickable buttons */
      #main_tabs { border-bottom: 3px solid #003b6f; padding: 8px 8px 0; background: #f4f6f9; }
      #main_tabs > li { margin-bottom: 0; }
      #main_tabs > li > a {
        background: #003b6f; color: #fff !important;
        font-weight: bold; font-size: 16px;
        padding: 12px 24px; margin-right: 8px;
        border: none; border-radius: 6px 6px 0 0;
      }
      #main_tabs > li > a i { color: #fff; }
      #main_tabs > li > a:hover { background: #0093D5; }
      #main_tabs > li.active > a,
      #main_tabs > li.active > a:hover,
      #main_tabs > li.active > a:focus {
        background: #f39c12; color: #fff !important; border: none;
      }
      .nav-tabs-custom > .nav-tabs > li.active { border-top-color: transparent; }

      .content-wrapper, .right-side { background-color: #f4f6f9; }

      @media (max-width: 1200px) {
        .readiness-grid {
          grid-template-columns: repeat(2, 1fr);
        }
      }
    "))),
    
    fluidRow(
      tabBox(
        id = "main_tabs",
        width = 12,
        side = "left",
        
        # tabPanel(
        #   title = tagList(icon("exclamation-triangle"), "EVD Readiness Situation Room"),
        #   
        #   div(
        #     class = "tab-pointer-banner",
        #     span(class = "arrow", HTML("&#8593;")),
        #     "Click the blue tabs above for more insights."
        #   ),
        #   
        #   uiOutput("critical_alert_banner"),
        #   
        #   fluidRow(
        #     box(
        #       title = tagList(icon("bullhorn"), "Strategic Public Health Brief"),
        #       width = 12,
        #       status = "danger",
        #       solidHeader = TRUE,
        #       uiOutput("strategic_brief")
        #     )
        #   ),
        #   
        #   fluidRow(
        #     valueBoxOutput("vb_24h_screened", width = 3),
        #     valueBoxOutput("vb_24h_immediate", width = 3),
        #     valueBoxOutput("vb_24h_core_priority", width = 3),
        #     valueBoxOutput("vb_high_risk_first_tab", width = 3)
        #   ),
        #   
        #   fluidRow(
        #     valueBoxOutput("vb_selected_red_flags", width = 6),
        #     valueBoxOutput("vb_selected_temperature", width = 6)
        #   ),
        #   
        #   fluidRow(
        #     valueBoxOutput("vb_selected_contact_alerts", width = 4),
        #     valueBoxOutput("vb_selected_funeral_alerts", width = 4),
        #     valueBoxOutput("vb_selected_health_facility_alerts", width = 4)
        #   ),
        #   
        #   fluidRow(
        #     valueBoxOutput("vb_active_evd_alerts", width = 6),
        #     valueBoxOutput("vb_reviewed_cleared", width = 6)
        #   ),
        #   
        #   fluidRow(
        #     valueBoxOutput("vb_symptom_percentage", width = 4),
        #     valueBoxOutput("vb_fever_reporting", width = 4),
        #     valueBoxOutput("vb_high_risk_percentage", width = 4)
        #   ),
        #   
        #   fluidRow(
        #     box(
        #       title = tagList(icon("line-chart"), "Daily Screening and Alert Trend"),
        #       width = 6,
        #       status = "info",
        #       solidHeader = TRUE,
        #       plotlyOutput("last24_trend_plot", height = "460px")
        #     ),
        #     box(
        #       title = tagList(icon("line-chart"), "Daily Public Health Signal Trend"),
        #       width = 6,
        #       status = "danger",
        #       solidHeader = TRUE,
        #       plotlyOutput("hourly_signal_plot", height = "460px")
        #     )
        #   ),
        #   
        #   fluidRow(
        #     box(
        #       title = tagList(icon("tasks"), "Immediate Public Health Action Matrix"),
        #       width = 12,
        #       status = "danger",
        #       solidHeader = TRUE,
        #       DTOutput("action_matrix"),
        #       style = "overflow-x: auto;"
        #     )
        #   ),
        #   
        #   fluidRow(
        #     box(
        #       title = tagList(icon("stethoscope"), "Symptom Screening Indicators"),
        #       width = 6,
        #       status = "warning",
        #       solidHeader = TRUE,
        #       plotlyOutput("first_tab_symptom_indicators", height = "350px")
        #     ),
        #     box(
        #       title = tagList(icon("warning"), "Exposure Risk Indicators"),
        #       width = 6,
        #       status = "danger",
        #       solidHeader = TRUE,
        #       plotlyOutput("first_tab_exposure_indicators", height = "350px")
        #     )
        #   ),
        #   
        #   fluidRow(
        #     box(
        #       title = tagList(icon("bar-chart"), "Most Common Symptoms Reported"),
        #       width = 6,
        #       status = "warning",
        #       solidHeader = TRUE,
        #       plotlyOutput("common_symptoms_plot", height = "350px")
        #     ),
        #     box(
        #       title = tagList(icon("line-chart"), "Daily Symptom and Exposure Signal"),
        #       width = 6,
        #       status = "danger",
        #       solidHeader = TRUE,
        #       plotlyOutput("hourly_symptom_exposure_plot", height = "460px")
        #     )
        #   ),
        #   
        #   fluidRow(
        #     box(
        #       title = tagList(icon("warning"), "Public Health Alert Drivers"),
        #       width = 6,
        #       status = "danger",
        #       solidHeader = TRUE,
        #       plotlyOutput("alert_drivers_plot", height = "350px")
        #     ),
        #     box(
        #       title = tagList(icon("globe"), "Priority Travel Signal: DRC / Uganda and Other Priority Travel"),
        #       width = 6,
        #       status = "warning",
        #       solidHeader = TRUE,
        #       plotlyOutput("priority_travel_plot", height = "350px")
        #     )
        #   )
        # ),
        tabPanel(
          title = tagList(icon("globe-africa"), "1. National PoE Overview"),
          value = "poe_overview",
          
          # ---- Page 1 styles ----
          tags$style(HTML("
            .p1-filter-bar {
              position: sticky; top: 0; z-index: 900;
              background: #f4f6f9; border-bottom: 1px solid #d2d6de;
              padding: 10px 15px 4px; margin: 0 -10px 15px;
            }
            .p1-filter-bar .form-group { margin-bottom: 6px; }
            .p1-filter-bar label { font-size: 13px; margin-bottom: 2px; }
            .p1-filter-footer {
              display: flex; flex-wrap: wrap; gap: 8px 20px;
              justify-content: space-between; align-items: center;
              font-size: 13px; color: #444; padding: 2px 0 4px;
            }
            .p1-filter-footer .btn-group-container-sw { display: inline-block; }
            .p1-dq-note { font-size: 12px; color: #666; margin-top: 8px; }
            @keyframes p1flash {
              0%, 100% { background: #c0392b; box-shadow: none; }
              50%      { background: #e74c3c; box-shadow: 0 0 14px #e74c3c; }
            }
            .p1-alert-flash {
              animation: p1flash 1.2s ease-in-out infinite;
              color: #fff; border-radius: 4px; padding: 10px 15px; margin-bottom: 15px;
            }
          ")),
          
          # ---- A. Filter bar (sticky) ----
          div(
            class = "p1-filter-bar",
            fluidRow(
              column(
                2,
                pickerInput(
                  inputId = "p1_border",
                  label = "Bordering country",
                  choices = p1_border_choices(poe_tally_initial$poes),
                  selected = p1_border_choices(poe_tally_initial$poes),
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE, `selected-text-format` = "count > 2")
                )
              ),
              column(
                2,
                pickerInput(
                  inputId = "p1_state",
                  label = "State",
                  choices = sort(unique(poe_tally_initial$poes$state)),
                  selected = sort(unique(poe_tally_initial$poes$state)),
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE, `selected-text-format` = "count > 2")
                )
              ),
              column(
                2,
                pickerInput(
                  inputId = "p1_county",
                  label = "County",
                  choices = sort(unique(poe_tally_initial$poes$county)),
                  selected = sort(unique(poe_tally_initial$poes$county)),
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE, `live-search` = TRUE,
                                 `selected-text-format` = "count > 2")
                )
              ),
              column(
                3,
                pickerInput(
                  inputId = "p1_poe",
                  label = "PoE",
                  choices = sort(poe_tally_initial$poes$poe),
                  selected = sort(poe_tally_initial$poes$poe),
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE, `live-search` = TRUE,
                                 `selected-text-format` = "count > 2")
                )
              ),
              column(
                3,
                dateRangeInput(
                  inputId = "p1_dates",
                  label = "Date range",
                  start = p1_default_dates(poe_tally_initial$tally)[1],
                  end = p1_default_dates(poe_tally_initial$tally)[2],
                  format = "dd-M-yyyy",
                  weekstart = 1
                )
              )
            ),
            div(
              class = "p1-filter-footer",
              uiOutput("p1_data_as_of", inline = TRUE),
              div(
                style = "display: flex; align-items: center; gap: 16px;",
                tags$span("Chart view:"),
                radioGroupButtons(
                  inputId = "p1_view",
                  label = NULL,
                  choices = c("Daily" = "daily", "Weekly" = "weekly"),
                  selected = P1_DEFAULT_VIEW,
                  size = "xs"
                ),
                actionLink("p1_reset", "Reset filters", icon = icon("undo"))
              )
            )
          ),
          
          # ---- B. Open alerts box (flashes while any alert is open) ----
          uiOutput("p1_alert_box"),
          
          # ---- C. Headline tiles ----
          fluidRow(
            valueBoxOutput("p1_vb_screened", width = 3),
            valueBoxOutput("p1_vb_alerts", width = 3),
            valueBoxOutput("p1_vb_bodies", width = 3),
            valueBoxOutput("p1_vb_reporting", width = 3)
          ),
          
          # ---- D. Charts and map ----
          fluidRow(
            box(
              title = tagList(icon("bar-chart"), "Travellers screened over time"),
              width = 12, status = "primary", solidHeader = TRUE,
              plotlyOutput("p1_screened_plot", height = "400px")
            )
          ),
          fluidRow(
            box(
              title = tagList(icon("bell"), "Alerts over time"),
              width = 6, status = "danger", solidHeader = TRUE,
              plotlyOutput("p1_alerts_plot", height = "500px")
            ),
            box(
              title = tagList(icon("map"), "PoE map"),
              width = 6, status = "primary", solidHeader = TRUE,
              leafletOutput("p1_map", height = "460px"),
              uiOutput("p1_map_note")
            )
          ),
          
          # ---- E. PoE summary table ----
          fluidRow(
            box(
              title = tagList(icon("table"), "PoE summary"),
              width = 12, status = "primary", solidHeader = TRUE,
              DTOutput("p1_summary_table"),
              uiOutput("p1_dq_note"),
              style = "overflow-x: auto;"
            )
          )
        ),
        
        tabPanel(
          title = tagList(icon("plane-arrival"), "2. JIA Screening Detail"),
          value = "jia_detail",
          
          # ---- Page 2 styles (filter bar reuses the page 1 classes) ----
          tags$style(HTML("
            .p2-section {
              background: #003b6f; color: #fff; font-weight: bold; font-size: 16px;
              padding: 8px 15px; margin: 10px 0 12px; border-radius: 3px;
            }
            .p2-summary { display: flex; flex-wrap: wrap; gap: 8px; margin-bottom: 8px; }
            .p2-summary .p2-chip {
              background: #f4f6f9; border: 1px solid #d2d6de; border-radius: 3px;
              padding: 4px 10px; font-size: 13px;
            }
            .p2-summary .p2-chip b { font-size: 15px; }
            .p2-caption { font-size: 12px; color: #666; margin-top: 4px; }
            .p2-dq-strip { display: flex; flex-wrap: wrap; gap: 10px; }
            .p2-dq-item {
              flex: 1 1 180px; background: #f4f6f9; border-left: 4px solid #00a65a;
              padding: 8px 10px; font-size: 13px;
            }
            .p2-dq-item.amber { border-left-color: #f39c12; }
            .p2-dq-item b { display: block; font-size: 18px; }
          ")),
          
          # ---- A. Filter bar (sticky) ----
          div(
            class = "p1-filter-bar",
            fluidRow(
              column(
                4,
                dateRangeInput(
                  inputId = "p2_dates",
                  label = "Date range",
                  start = p2_default_dates(initial_data)[1],
                  end = p2_default_dates(initial_data)[2],
                  format = "dd-M-yyyy",
                  weekstart = 1
                )
              ),
              column(
                5,
                radioGroupButtons(
                  inputId = "p2_group",
                  label = "Show",
                  choices = c(
                    "All travellers"       = "all",
                    "Affected-area travel" = "drc",
                    "Classified high-risk" = "high"
                  ),
                  selected = "all",
                  size = "sm"
                )
              )
            ),
            div(
              class = "p1-filter-footer",
              uiOutput("p2_data_as_of", inline = TRUE),
              div(
                style = "display: flex; align-items: center; gap: 16px;",
                tags$span("Chart view:"),
                radioGroupButtons(
                  inputId = "p2_view",
                  label = NULL,
                  choices = c("Daily" = "daily", "Weekly" = "weekly"),
                  selected = P1_DEFAULT_VIEW,
                  size = "xs"
                ),
                actionLink("p2_reset", "Reset filters", icon = icon("undo"))
              )
            )
          ),
          
          # ---- B. Headline tiles ----
          fluidRow(
            valueBoxOutput("p2_vb_screened", width = 3),
            valueBoxOutput("p2_vb_affected", width = 3),
            valueBoxOutput("p2_vb_symptomatic", width = 3),
            valueBoxOutput("p2_vb_exposure", width = 3)
          ),
          fluidRow(
            valueBoxOutput("p2_vb_alerts", width = 6),
            valueBoxOutput("p2_vb_followup", width = 6)
          ),
          
          # ---- Section 1: Who is arriving ----
          div(class = "p2-section", icon("plane-arrival"), " Who is arriving"),
          fluidRow(
            box(
              title = tagList(icon("bar-chart"), "Travellers screened over time"),
              width = 6, status = "primary", solidHeader = TRUE,
              plotlyOutput("p2_screened_plot", height = "400px")
            ),
            box(
              title = tagList(icon("map-marker"), "Final destination in South Sudan (top 10)"),
              width = 6, status = "primary", solidHeader = TRUE,
              plotlyOutput("p2_destination_plot", height = "400px")
            )
          ),
          
          # ---- Section 2: Risk ----
          div(class = "p2-section", icon("stethoscope"), " Risk"),
          fluidRow(
            box(
              title = tagList(icon("thermometer-half"), "Symptoms reported"),
              width = 6, status = "warning", solidHeader = TRUE,
              uiOutput("p2_symptom_summary"),
              plotlyOutput("p2_symptoms_plot", height = "380px"),
              div(class = "p2-caption", "Travellers may report more than one symptom.")
            ),
            box(
              title = tagList(icon("exclamation-triangle"), "Exposure types"),
              width = 6, status = "danger", solidHeader = TRUE,
              uiOutput("p2_exposure_subtitle"),
              # plotlyOutput("p2_exposure_plot", height = "380px")
              plotlyOutput("p2_exposure_plot", height = "420px"),
              tags$div(class = "p1-dq-note",
                       "Travellers may report more than one exposure."),
              # div(class = "p2-caption",
              #     "Contact answers are largely unverified: most travellers reporting contact have no DRC ",
              #     "travel in their history. Use Show: Affected-area travel to see contacts with a travel link.")
            )
          ),
          fluidRow(
            box(
              title = tagList(icon("calendar"), "Weekly symptomatic, exposed and high-risk travellers"),
              width = 12, status = "warning", solidHeader = TRUE,
              plotlyOutput("p2_weekly_risk_plot", height = "350px")
            )
          ),
          
          # ---- Section 3: Data quality ----
          div(class = "p2-section", icon("clipboard-check"), " Data quality"),
          fluidRow(
            box(
              title = tagList(icon("check-square"), "Data quality checks"),
              width = 12, status = "warning", solidHeader = TRUE,
              uiOutput("p2_dq_strip")
            )
          )
        ),
        
        mod_restricted_redflags_ui("restricted_redflags")
      )
    )
  )
)

# ============================================================
# 8. SERVER
# ============================================================

server <- function(input, output, session) {
  
  
  `%||%` <- function(x, y) { if (is.null(x)) y else x }
  
  session$allowReconnect(TRUE)
  
  observeEvent(input$browser_keepalive, {
    last_keepalive_text(format(with_tz(Sys.time(), LOCAL_TZ), "%H:%M:%S"))
  })
  
  # ----------------------------------------------------------
  # SINGLE LIVE ONA API PULL
  # Pull raw Ona data once per refresh cycle, then derive
  # both the cleaned dashboard data and restricted raw data
  # from the same API response. This prevents the dashboard
  # from falling behind due to duplicate API calls.
  # ----------------------------------------------------------
  
  raw_ona_cache <- reactiveVal(initial_raw_data)
  refresh_in_progress <- reactiveVal(FALSE)
  
  raw_ona_data <- reactive({
    raw_ona_cache()
  })
  
  # Stop Ona refreshes once this browser session has ended
  session_active <- TRUE
  ona_refresh_handle <- NULL
  
  session$onSessionEnded(function() {
    session_active <<- FALSE
    if (is.function(ona_refresh_handle)) ona_refresh_handle()   # cancel the pending timer
  })
  
  refresh_ona_cache <- function() {
    # Session closed (tab closed, reloaded or disconnected): do nothing
    if (!session_active || session$isClosed()) return(invisible(FALSE))
    
    if (isTRUE(isolate(refresh_in_progress()))) {
      return(invisible(FALSE))
    }
    
    cached <- isolate(raw_ona_cache())
    active_tab <- isolate(input$main_tabs %||% "")
    
    # Do not interrupt health officers while reviewing/clearing alert records.
    if (identical(active_tab, "Restricted View Alerts and Signal Details") &&
        !is.null(cached) && nrow(cached) > 0) {
      message("Auto-refresh paused while restricted alert review tab is active.")
      return(invisible(FALSE))
    }
    
    refresh_in_progress(TRUE)
    on.exit({
      if (session_active && !session$isClosed()) refresh_in_progress(FALSE)
    }, add = TRUE)
    
    raw <- fetch_ona_data()
    
    # The download can take a while; the session may have closed in the meantime
    if (!session_active || session$isClosed()) return(invisible(FALSE))
    
    if (!is.null(raw) && nrow(raw) > 0) {
      raw <- raw %>% mutate(across(everything(), as.character))
      raw_ona_cache(raw)
      save_ona_cache(raw)
      return(invisible(TRUE))
    }
    
    if (!is.null(cached) && nrow(cached) > 0) {
      message("Using last successful Ona API pull because the latest pull failed.")
      return(invisible(FALSE))
    }
    
    invisible(FALSE)
  }
  
  schedule_ona_refresh <- function(delay_seconds) {
    if (!session_active) return(invisible(NULL))
    ona_refresh_handle <<- later::later(function() {
      if (!session_active) return(invisible(NULL))
      refresh_ona_cache()
      schedule_ona_refresh(REFRESH_INTERVAL_MS / 1000)
    }, delay = delay_seconds)
  }

  # Turn raw ODK submissions into one row per PoE per day
  clean_poe_odk <- function(raw) {
    if (is.null(raw) || nrow(raw) == 0) return(tibble())
    col <- function(nm) if (nm %in% names(raw)) raw[[nm]] else rep(NA, nrow(raw))
    num <- function(nm) suppressWarnings(as.numeric(col(nm)))
    
    # second alerts column (discarded) – found by pattern so the exact name doesn't matter
    disc_nm <- setdiff(grep("^grp_alerts/al", names(raw), value = TRUE), "grp_alerts/alerts_triggered")[1]
    
    tibble(
      date       = as.Date(substr(as.character(col("grp_poe/reporting_date")), 1, 10)),
      poe        = str_squish(as.character(col("grp_poe/poe"))),
      entry      = num("grp_screening/entry_screened"),
      exit       = num("grp_screening/exit_screened"),
      total_calc = num("grp_screening/total_screened"),
      total_rep  = num("grp_screening/total_screened_reported"),
      triggered  = num("grp_alerts/alerts_triggered"),
      discarded  = if (!is.na(disc_nm)) suppressWarnings(as.numeric(raw[[disc_nm]])) else NA_real_,
      bodies     = num("grp_alerts/bodies_moved"),
      notes      = as.character(col("notes")),
      submitted  = ymd_hms(col("_submission_time"), quiet = TRUE)
    ) %>%
      mutate(
        entry_exit = if_else(is.na(entry) & is.na(exit), NA_real_, coalesce(entry, 0) + coalesce(exit, 0)),
        total      = coalesce(total_calc, entry_exit, total_rep)
      ) %>%
      filter(!is.na(date), !is.na(poe), poe != "", date >= POE_ODK_START) %>%
      arrange(desc(submitted)) %>%                 # latest submission first…
      distinct(date, poe, .keep_all = TRUE) %>%    # …so a correction replaces the earlier one
      arrange(date, poe) %>%
      select(date, poe, entry, exit, total, triggered, discarded, bodies, notes, submitted)
  }
 
  
  # Open the dashboard from cache immediately, then refresh Ona after the UI
  # has been sent to the browser. This keeps first paint fast and avoids a
  # one-minute blank load while the live API is responding.
  session$onFlushed(function() {
    schedule_ona_refresh(INITIAL_SERVER_REFRESH_DELAY_SECONDS)
  }, once = TRUE)
  
  evd_data <- reactive({
    raw <- raw_ona_data()
    
    if (is.null(raw) || nrow(raw) == 0) {
      return(empty_evd_data())
    }
    
    standardize_evd_data(raw)
  })
  
  # ----------------------------------------------------------
  # ALERT CLEARANCE / FOLLOW-UP MODULE
  # Health officers can mark alert records as followed up.
  # Those records are removed from the active situation-room
  # alert counts and restricted active alert table.
  # ----------------------------------------------------------
  
  alert_clearance <- mod_alert_clearance_server(
    id = "alert_clearance",
    clearance_file = "alert_clearance_log.csv"
  )
  
  cleared_alert_ids <- reactive({
    alert_clearance$cleared_ids()
  })
  
  # Page 1: National PoE overview
  poe_overview_server(
    input, output, session,
    jia_data      = evd_data,
    clearance_log = alert_clearance$clearance_log,
    cleared_ids   = cleared_alert_ids
  )
  
  # Page 2: JIA Screening Detail
  jia_detail_server(input, output, session, jia_data = evd_data, cleared_ids = cleared_alert_ids)
 
  mod_restricted_redflags_server(
    id = "restricted_redflags",
    evd_data = evd_data,
    raw_kobo_data = raw_ona_data,
    authorized_emails = AUTHORIZED_HEALTH_OFFICER_EMAILS,
    refresh_interval_ms = REFRESH_INTERVAL_MS,
    cleared_ids = cleared_alert_ids,
    mark_alerts_cleared = alert_clearance$mark_alerts_cleared,
    restore_alerts = alert_clearance$restore_alerts,
    clearance_log = alert_clearance$clearance_log
  )
 
}

shinyApp(ui, server)
