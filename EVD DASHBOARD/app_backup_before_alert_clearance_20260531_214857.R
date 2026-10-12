# ============================================================
# SOUTH SUDAN EVD TRAVELER SCREENING DASHBOARD
# EVD Readiness Situation Room
# One-script Shiny dashboard
# Kobo live API pull every 2 minutes
# Auto-rotating tabs every 2 minutes
# Includes preprocessing/cleaning for inconsistent Kobo entries
# Includes blinking alert banner for suspicious / actionable records
# No maps, no metadata tab, no codebook tab, no personal line list
# ============================================================

Sys.setenv(TZ = "Africa/Juba")

# ============================================================
# 0. INSTALL AND LOAD PACKAGES
# ============================================================

packages <- c(
  "shiny",
  "shinydashboard",
  "plotly",
  "dplyr",
  "DT",
  "tidyr",
  "lubridate",
  "shinyWidgets",
  "jsonlite",
  "httr2",
  "stringr",
  "scales",
  "purrr",
  "tibble",
  "readr"
)

installed <- rownames(installed.packages())

for (p in packages) {
  if (!p %in% installed) {
    install.packages(p, dependencies = TRUE)
  }
}

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

# Restricted red-flag visitor/person details module
source("modules/mod_restricted_redflags.R")
source("modules/mod_alert_clearance.R")

# ============================================================
# 1. CONFIGURATION
# ============================================================

APP_TITLE <- "South Sudan EVD Traveler Screening Dashboard"

NPHI_LOGO <- "Nphi.png"
MOH_LOGO <- "mohs_logo.png"
WHO_LOGO <- "who_logo.png"

KOBO_SERVER <- "https://eu.kobotoolbox.org"
KOBO_ASSET_UID <- "awZDh36aJGwdXaWrWrtyHg"

KOBO_API_TOKEN_FALLBACK <- "5ed301da977d2525105c7a7f8f2dd5e2c209e6b9"


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
  "owinog@who.int"
)

AUTHORIZED_HEALTH_OFFICER_EMAILS <- tolower(trimws(AUTHORIZED_HEALTH_OFFICER_EMAILS))


KOBO_LIMIT <- 5000

# Kobo data refresh interval: 2 minutes
REFRESH_INTERVAL_MS <- 120000

# Browser keep-alive interval
KEEPALIVE_INTERVAL_MS <- 60000

# Auto-rotate dashboard tabs every 3 minutes
TAB_ROTATION_INTERVAL_MS <- 180000

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
  "Enhanced monitoring" = "#f39c12",
  "Routine monitoring" = "#00a65a"
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
  x <- safe_chr(x)
  
  suppressWarnings(parse_date_time(
    x,
    orders = c(
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
  plot_ly() %>%
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
    red_flag_reason = character(),
    action_priority = character(),
    screening_minutes = numeric()
  )
}

# ============================================================
# 4. KOBO API PULL
# ============================================================

fetch_kobo_data <- function() {
  token <- Sys.getenv("KOBO_API_TOKEN")
  
  if (is.null(token) || !nzchar(token) || token == "PASTE_YOUR_KOBO_TOKEN_HERE") {
    token <- KOBO_API_TOKEN_FALLBACK
  }
  
  if (is.null(token) || !nzchar(token)) {
    message("Kobo API token is missing.")
    return(tibble())
  }
  
  api_url <- paste0(
    KOBO_SERVER,
    "/api/v2/assets/",
    KOBO_ASSET_UID,
    "/data.json"
  )
  
  request_url <- paste0(
    api_url,
    "?format=json",
    "&limit=", KOBO_LIMIT,
    "&sort={\"_id\":-1}",
    "&cache_bust=", as.integer(Sys.time())
  )
  
  req <- request(request_url) %>%
    req_user_agent("South-Sudan-EVD-Readiness-Situation-Room/Cleaned") %>%
    req_headers(
      Authorization = paste("Token", token),
      Accept = "application/json",
      `Cache-Control` = "no-cache",
      Pragma = "no-cache"
    )
  
  response <- tryCatch(
    req_perform(req),
    error = function(e) {
      message("Kobo API request failed: ", conditionMessage(e))
      return(NULL)
    }
  )
  
  if (is.null(response)) {
    return(tibble())
  }
  
  parsed <- tryCatch(
    resp_body_json(response, simplifyVector = FALSE),
    error = function(e) {
      message("Could not parse Kobo API JSON: ", conditionMessage(e))
      return(NULL)
    }
  )
  
  if (is.null(parsed)) {
    return(tibble())
  }
  
  records <- parsed$results
  
  if (is.null(records) || length(records) == 0) {
    return(tibble())
  }
  
  df <- tryCatch(
    {
      jsonlite::flatten(
        jsonlite::fromJSON(
          jsonlite::toJSON(records, auto_unbox = TRUE)
        )
      )
    },
    error = function(e) {
      message("Could not flatten Kobo records: ", conditionMessage(e))
      return(tibble())
    }
  )
  
  last_refresh_text(format(with_tz(Sys.time(), LOCAL_TZ), "%d %b %Y %H:%M:%S"))
  
  as_tibble(df)
}

# ============================================================
# 4B. RAW KOBO DATA FOR RESTRICTED AUTHORIZED MODULE
# Keeps all original ODK/Kobo columns for authorized red-flag review.
# ============================================================

read_raw_kobo_data <- function() {
  raw <- fetch_kobo_data()
  
  if (is.null(raw) || nrow(raw) == 0) {
    return(tibble())
  }
  
  raw %>%
    mutate(across(everything(), as.character))
}

# ============================================================
# 5. STANDARDIZE KOBO DATA
# ============================================================

standardize_evd_data <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(empty_evd_data())
  }
  
  df <- df %>%
    mutate(across(everything(), as.character))
  
  start_time <- parse_datetime_any(first_existing_col(df, c("start_time", "Start Time")))
  end_time <- parse_datetime_any(first_existing_col(df, c("end_time", "End Time")))
  
  submission_time <- parse_datetime_any(first_existing_col(
    df,
    c("_submission_time", "Submission Date", "submission_date", "timestamp", "end_time", "End Time")
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
  
  flight_number <- normalize_text(first_existing_col(df, c("travel_info/flight_number", "Flight Number")))
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
  
  # Priority travel alone is NOT counted as a red flag.
  # Priority travel is an enhanced monitoring signal.
  red_flag <- clearance_status != "Cleared" |
    risk_category %in% c("High risk", "Moderate risk") |
    symptom_alert |
    exposure_alert |
    missing_followup_flag |
    incomplete_form_flag
  
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
    exposure_alert | symptom_alert | risk_category == "High risk" | clearance_status == "Referred / Alert" ~ "Immediate action",
    core_priority_country_flag | priority_country_flag | missing_followup_flag | incomplete_form_flag | risk_category == "Moderate risk" ~ "Enhanced monitoring",
    TRUE ~ "Routine monitoring"
  )
  
  out <- tibble(
    submission_time = submission_time,
    submission_date = submission_date,
    submission_hour = submission_hour,
    hour_label = hour_label,
    epi_week = paste0(year(submission_date), "-W", sprintf("%02d", epiweek(submission_date))),
    screening_id = safe_chr(screening_id),
    
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
    red_flag_reason = red_flag_reason,
    action_priority = action_priority,
    screening_minutes = screening_minutes
  ) %>%
    filter(!is.na(submission_time)) %>%
    arrange(desc(submission_time))
  
  out
}

read_screening_data <- function() {
  raw <- fetch_kobo_data()
  standardize_evd_data(raw)
}

# ============================================================
# 6. INITIAL DATA
# ============================================================

initial_data <- read_screening_data()

epi_choices_initial <- safe_epi_choices(initial_data)
date_start_initial <- safe_date_min(initial_data$submission_date)
date_end_initial <- safe_date_max(initial_data$submission_date)

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
        tags$img(src = NPHI_LOGO, height = "50px"),
        tags$img(src = MOH_LOGO, height = "50px")
      ),
      tags$h1(
        APP_TITLE,
        style = "flex-grow: 1; text-align: center; font-weight: bold; color: white; margin: 0; font-size: 24px;"
      ),
      tags$img(src = WHO_LOGO, height = "50px")
    ),
    titleWidth = "100%"
  ),
  
  dashboardSidebar(
    width = 230,
    
    tags$head(tags$script(HTML(paste0("
      setInterval(function() {
        Shiny.setInputValue('browser_keepalive', new Date().getTime(), {priority: 'event'});
      }, ", KEEPALIVE_INTERVAL_MS, ");

      $(document).on('shiny:connected', function() {
        var tabRotationTimer = null;
        var tabRotationInterval = ", TAB_ROTATION_INTERVAL_MS, ";

        var getDashboardTabs = function() {
          return $('.nav-tabs-custom .nav-tabs li:visible a[data-toggle=\"tab\"]');
        };

        var scheduleTabRotation = function() {
          if (tabRotationTimer !== null) {
            clearTimeout(tabRotationTimer);
          }
          tabRotationTimer = setTimeout(rotateTabs, tabRotationInterval);
        };

        var rotateTabs = function() {
          var tabs = getDashboardTabs();
          if (tabs.length <= 1) {
            scheduleTabRotation();
            return;
          }

          var activeIndex = -1;
          tabs.each(function(i) {
            if ($(this).parent().hasClass('active')) {
              activeIndex = i;
            }
          });

          var nextIndex = activeIndex + 1;
          if (nextIndex >= tabs.length || activeIndex < 0) {
            nextIndex = 0;
          }

          var currentPane = $('.tab-pane.active');
          currentPane.fadeOut(250, function() {
            tabs.eq(nextIndex).tab('show');
            $('.tab-pane.active').hide().fadeIn(450);
            scheduleTabRotation();
          });
        };

        $(document).on('click', '.nav-tabs-custom .nav-tabs li a[data-toggle=\"tab\"]', function() {
          scheduleTabRotation();
        });

        scheduleTabRotation();
      });
    ")))),
    
    tags$head(tags$style(HTML("
      .skin-blue .main-header .navbar { background-color: #003b6f; }
      .skin-blue .main-header .logo { background-color: #003b6f; color: white; }
      .skin-blue .main-header .logo:hover { background-color: #003b6f; }

      .skin-blue .main-sidebar { background-color: #0093D5; }
      .skin-blue .main-sidebar .sidebar-menu > li > a { color: white; }
      .skin-blue .main-sidebar .sidebar-menu > li > a:hover { background-color: #005b9f; color: white; }
      .skin-blue .main-sidebar .sidebar-menu .active > a { background-color: #002a4d; color: white; }

      .content-wrapper, .right-side { background-color: #f4f6f9; }

      .skin-blue .content .small-box {
        min-height: 112px;
        border-radius: 10px;
        box-shadow: 0 2px 8px rgba(0,0,0,.16);
      }

      .skin-blue .content .small-box .inner {
        display: flex;
        flex-direction: column;
        justify-content: center;
        min-height: 100px;
      }

      .small-box h3 {
        font-size: 30px !important;
        font-weight: 800 !important;
      }

      .small-box p {
        font-size: 13px !important;
        font-weight: 700 !important;
      }

      .box {
        border-radius: 10px;
        box-shadow: 0 2px 8px rgba(0,0,0,.14);
        border-top: 0;
      }

      .box .box-header .box-title {
        text-align: center;
        width: 100%;
        display: block;
        font-size: 17px;
        font-weight: bold;
      }

      .box.box-solid.box-primary > .box-header { background: #003b6f; }
      .box.box-solid.box-info > .box-header { background: #0093D5; }
      .box.box-solid.box-warning > .box-header { background: #f39c12; }
      .box.box-solid.box-danger > .box-header { background: #dd4b39; }
      .box.box-solid.box-success > .box-header { background: #00a65a; }

      .sidebar .form-group {
        color: white;
        font-weight: bold;
      }

      .bootstrap-select .dropdown-toggle {
        font-size: 12px;
      }

      .irs-bar, .irs-bar-edge, .irs-line {
        background-color: #ff6600 !important;
      }

      .irs-grid, .irs-grid-text, .irs-min, .irs-max {
        display: none !important;
      }

      .nav-tabs-custom { background: #ffffff !important; }

      .nav-tabs-custom > .nav-tabs {
        background-color: #ffffff !important;
        border-bottom: 1px solid #d2d6de !important;
        padding-left: 0 !important;
      }

      .nav-tabs-custom > .nav-tabs > li {
        margin-bottom: -1px !important;
        margin-right: 5px !important;
        border-top: 4px solid transparent !important;
      }

      .nav-tabs-custom > .nav-tabs > li > a {
        background-color: #003b6f !important;
        color: #ffffff !important;
        font-weight: bold !important;
        font-size: 15px !important;
        border: 1px solid #003b6f !important;
        border-radius: 4px 4px 0 0 !important;
        margin-right: 0 !important;
        padding: 10px 14px !important;
      }

      .nav-tabs-custom > .nav-tabs > li.active {
        border-top: 4px solid #ff6600 !important;
      }

      .nav-tabs-custom > .nav-tabs > li.active > a {
        background-color: #003b6f !important;
        color: #ffffff !important;
        border-bottom-color: #ffffff !important;
        padding-top: 6px !important;
      }

      .nav-tabs-custom > .tab-content { background: #ffffff !important; }

      .tab-pointer-banner {
        background: #eaf6ff;
        border: 1px solid #9bd4f5;
        color: #003b6f;
        border-radius: 10px;
        padding: 10px 14px;
        margin-bottom: 12px;
        font-weight: 800;
        font-size: 14px;
        box-shadow: 0 1px 5px rgba(0,0,0,.10);
      }

      .tab-pointer-banner .arrow {
        font-size: 22px;
        color: #ff6600;
        margin-right: 8px;
        animation: arrowPulse 1.2s infinite;
      }

      @keyframes arrowPulse {
        0% { opacity: .4; transform: translateY(4px); }
        50% { opacity: 1; transform: translateY(-2px); }
        100% { opacity: .4; transform: translateY(4px); }
      }

      .readiness-panel {
        background: linear-gradient(135deg, #ffffff 0%, #f7fbff 100%);
        border-radius: 14px;
        box-shadow: 0 3px 12px rgba(0,0,0,.12);
        padding: 18px;
        border-left: 8px solid #003b6f;
      }

      .readiness-panel.alert-now {
        border-left-color: #dd4b39;
      }

      .readiness-panel.monitoring {
        border-left-color: #f39c12;
      }

      .readiness-header {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 12px;
        margin-bottom: 12px;
      }

      .readiness-title {
        font-size: 20px;
        font-weight: 800;
        color: #003b6f;
        margin: 0;
      }

      .readiness-badge {
        padding: 8px 14px;
        border-radius: 999px;
        font-size: 13px;
        font-weight: 800;
        color: #ffffff;
        white-space: nowrap;
      }

      .badge-alert { background: #dd4b39; }
      .badge-elevated { background: #f39c12; }
      .badge-routine { background: #00a65a; }

      .readiness-grid {
        display: grid;
        grid-template-columns: repeat(4, 1fr);
        gap: 12px;
        margin-top: 12px;
      }

      .readiness-card {
        background: #ffffff;
        border-radius: 10px;
        padding: 12px;
        box-shadow: 0 1px 5px rgba(0,0,0,.10);
        min-height: 92px;
      }

      .readiness-card .label {
        font-size: 12px;
        text-transform: uppercase;
        letter-spacing: .5px;
        color: #6b7280;
        font-weight: 800;
        display: block;
        margin-bottom: 6px;
      }

      .readiness-card .value {
        font-size: 18px;
        color: #111827;
        font-weight: 800;
        line-height: 1.3;
      }

      .readiness-card .note {
        font-size: 12px;
        color: #374151;
        line-height: 1.35;
        margin-top: 4px;
      }

      .readiness-action {
        background: #003b6f;
        color: #ffffff;
        border-radius: 10px;
        padding: 12px 14px;
        margin-top: 12px;
        font-size: 14px;
        line-height: 1.5;
      }

      .readiness-action strong {
        color: #ffffff;
      }

      .critical-alert-banner {
        background: #dd4b39;
        color: #ffffff;
        border-radius: 14px;
        padding: 18px 22px;
        margin-bottom: 15px;
        text-align: center;
        box-shadow: 0 4px 16px rgba(221,75,57,.45);
        border: 4px solid #ffffff;
        animation: blinkAlert 1s infinite;
      }

      .critical-alert-title {
        font-size: 30px;
        font-weight: 900;
        line-height: 1.2;
        text-transform: uppercase;
        margin-bottom: 6px;
      }

      .critical-alert-subtitle {
        font-size: 18px;
        font-weight: 800;
        line-height: 1.4;
      }

      @keyframes blinkAlert {
        0% { background: #dd4b39; transform: scale(1); }
        50% { background: #9b111e; transform: scale(1.012); }
        100% { background: #dd4b39; transform: scale(1); }
      }

      table.dataTable thead th, table.dataTable tbody td {
        white-space: normal;
        text-align: center;
        word-wrap: break-word;
        word-break: break-word;
        padding: 6px 10px;
      }

      table.dataTable thead th {
        background-color: #003b6f;
        color: white;
        font-size: 11px;
      }

      table.dataTable tbody td {
        font-size: 11px;
      }

      table.dataTable {
        table-layout: auto;
        width: 100% !important;
      }

      @media (max-width: 1200px) {
        .readiness-grid {
          grid-template-columns: repeat(2, 1fr);
        }
      }
    "))),
    
    pickerInput(
      inputId = "risk_filter",
      label = "Risk Category:",
      choices = fallback_choices(initial_data$risk_category),
      selected = fallback_choices(initial_data$risk_category),
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    ),
    
    pickerInput(
      inputId = "status_filter",
      label = "Clearance Status:",
      choices = fallback_choices(initial_data$clearance_status),
      selected = fallback_choices(initial_data$clearance_status),
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    ),
    
    pickerInput(
      inputId = "priority_filter",
      label = "Action Priority:",
      choices = fallback_choices(initial_data$action_priority),
      selected = fallback_choices(initial_data$action_priority),
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    ),
    
    pickerInput(
      inputId = "country_filter",
      label = "Country of Departure:",
      choices = fallback_choices(initial_data$country_departure),
      selected = fallback_choices(initial_data$country_departure),
      multiple = TRUE,
      options = list(`actions-box` = TRUE, `live-search` = TRUE)
    ),
    
    pickerInput(
      inputId = "transport_filter",
      label = "Transport Mode:",
      choices = fallback_choices(initial_data$transport_mode),
      selected = fallback_choices(initial_data$transport_mode),
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    ),
    
    sliderTextInput(
      inputId = "epi_week_filter",
      label = "Epi Week Range:",
      choices = epi_choices_initial,
      selected = range(epi_choices_initial),
      grid = FALSE
    ),
    
    dateRangeInput(
      "date_filter",
      "Date Range:",
      start = date_start_initial,
      end = date_end_initial
    )
  ),
  
  dashboardBody(
    fluidRow(
      tabBox(
        id = "main_tabs",
        width = 12,
        side = "left",
        
        tabPanel(
          title = tagList(icon("exclamation-triangle"), "EVD Readiness Situation Room"),
          
          div(
            class = "tab-pointer-banner",
            span(class = "arrow", HTML("&#8593;")),
            "Click the blue tabs above for more insights. The dashboard also rotates automatically every 3 minutes."
          ),
          
          uiOutput("critical_alert_banner"),
          
          fluidRow(
            box(
              title = tagList(icon("bullhorn"), "Strategic Public Health Brief"),
              width = 12,
              status = "danger",
              solidHeader = TRUE,
              uiOutput("strategic_brief")
            )
          ),
          
          fluidRow(
            valueBoxOutput("vb_2h_screened", width = 3),
            valueBoxOutput("vb_2h_immediate", width = 3),
            valueBoxOutput("vb_2h_symptoms", width = 3),
            valueBoxOutput("vb_2h_exposures", width = 3)
          ),
          
          fluidRow(
            valueBoxOutput("vb_24h_screened", width = 3),
            valueBoxOutput("vb_24h_immediate", width = 3),
            valueBoxOutput("vb_24h_core_priority", width = 3),
            valueBoxOutput("vb_high_risk_first_tab", width = 3)
          ),
          
          fluidRow(
            valueBoxOutput("vb_selected_red_flags", width = 3),
            valueBoxOutput("vb_selected_temperature", width = 3),
            valueBoxOutput("vb_selected_followup_gaps", width = 3),
            valueBoxOutput("vb_selected_clearance_concerns", width = 3)
          ),
          
          fluidRow(
            valueBoxOutput("vb_selected_contact_alerts", width = 3),
            valueBoxOutput("vb_selected_funeral_alerts", width = 3),
            valueBoxOutput("vb_selected_health_facility_alerts", width = 3),
            valueBoxOutput("vb_followup_ready_first_tab", width = 3)
          ),
          
          fluidRow(
            valueBoxOutput("vb_symptom_percentage", width = 3),
            valueBoxOutput("vb_fever_reporting", width = 3),
            valueBoxOutput("vb_medical_care", width = 3),
            valueBoxOutput("vb_high_risk_percentage", width = 3)
          ),
          
          fluidRow(
            box(
              title = tagList(icon("line-chart"), "Daily Screening and Alert Trend"),
              width = 6,
              status = "info",
              solidHeader = TRUE,
              plotlyOutput("last24_trend_plot", height = "350px")
            ),
            box(
              title = tagList(icon("line-chart"), "Daily Public Health Signal Trend"),
              width = 6,
              status = "danger",
              solidHeader = TRUE,
              plotlyOutput("hourly_signal_plot", height = "350px")
            )
          ),
          
          fluidRow(
            box(
              title = tagList(icon("tasks"), "Immediate Public Health Action Matrix"),
              width = 12,
              status = "danger",
              solidHeader = TRUE,
              DTOutput("action_matrix"),
              style = "overflow-x: auto;"
            )
          ),
          
          fluidRow(
            box(
              title = tagList(icon("stethoscope"), "Symptom Screening Indicators"),
              width = 6,
              status = "warning",
              solidHeader = TRUE,
              plotlyOutput("first_tab_symptom_indicators", height = "350px")
            ),
            box(
              title = tagList(icon("warning"), "Exposure Risk Indicators"),
              width = 6,
              status = "danger",
              solidHeader = TRUE,
              plotlyOutput("first_tab_exposure_indicators", height = "350px")
            )
          ),
          
          fluidRow(
            box(
              title = tagList(icon("bar-chart"), "Most Common Symptoms Reported"),
              width = 6,
              status = "warning",
              solidHeader = TRUE,
              plotlyOutput("common_symptoms_plot", height = "350px")
            ),
            box(
              title = tagList(icon("line-chart"), "Daily Symptom and Exposure Signal"),
              width = 6,
              status = "danger",
              solidHeader = TRUE,
              plotlyOutput("hourly_symptom_exposure_plot", height = "350px")
            )
          ),
          
          fluidRow(
            box(
              title = tagList(icon("warning"), "Public Health Alert Drivers"),
              width = 6,
              status = "danger",
              solidHeader = TRUE,
              plotlyOutput("alert_drivers_plot", height = "350px")
            ),
            box(
              title = tagList(icon("globe"), "Priority Travel Signal: DRC / Uganda and Other Priority Travel"),
              width = 6,
              status = "warning",
              solidHeader = TRUE,
              plotlyOutput("priority_travel_plot", height = "350px")
            )
          )
        ),
        
        tabPanel(
          title = tagList(icon("plane"), "Screening and Travel Flow"),
          
          div(
            class = "tab-pointer-banner",
            span(class = "arrow", HTML("&#8593;")),
            "Use the tabs above to move across readiness, travel flow, and exposure/follow-up insights."
          ),
          
          fluidRow(
            valueBoxOutput("vb_unique_departure_countries", width = 3),
            valueBoxOutput("vb_unique_destinations", width = 3),
            valueBoxOutput("vb_transport_modes", width = 3),
            valueBoxOutput("vb_median_screening_time", width = 3)
          ),
          
          fluidRow(
            box(
              title = tagList(icon("line-chart"), "Daily Screening Intensity"),
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              plotlyOutput("hourly_intensity_plot", height = "340px")
            )
          ),
          
          fluidRow(
            box(
              title = tagList(icon("bar-chart"), "Screening Volume by Country of Departure"),
              width = 6,
              status = "primary",
              solidHeader = TRUE,
              plotlyOutput("country_volume_plot", height = "360px")
            ),
            box(
              title = tagList(icon("truck"), "Screening Volume by Transport Mode"),
              width = 6,
              status = "info",
              solidHeader = TRUE,
              plotlyOutput("transport_plot", height = "360px")
            )
          ),
          
          fluidRow(
            box(
              title = tagList(icon("location-arrow"), "Final Destination in South Sudan"),
              width = 6,
              status = "success",
              solidHeader = TRUE,
              plotlyOutput("destination_plot", height = "360px")
            ),
            box(
              title = tagList(icon("calendar"), "Validated Arrival Pattern by Date"),
              width = 6,
              status = "warning",
              solidHeader = TRUE,
              plotlyOutput("arrival_pattern_plot", height = "360px")
            )
          ),
          
          fluidRow(
            box(
              title = tagList(icon("table"), "Operational Route Summary"),
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              DTOutput("route_summary_table"),
              style = "overflow-x: auto;"
            )
          )
        ),
        
        tabPanel(
          title = tagList(icon("stethoscope"), "Risk, Exposure and Follow-up"),
          
          div(
            class = "tab-pointer-banner",
            span(class = "arrow", HTML("&#8593;")),
            "Click the tabs above to return to the main readiness view or travel-flow insights."
          ),
          
          fluidRow(
            valueBoxOutput("vb_body_fluids_alerts", width = 3),
            valueBoxOutput("vb_wildlife_alerts", width = 3),
            valueBoxOutput("vb_animal_bite_alerts", width = 3),
            valueBoxOutput("vb_affected_area_alerts", width = 3)
          ),
          
          fluidRow(
            box(
              title = tagList(icon("thermometer-half"), "Symptom and Temperature Screening"),
              width = 6,
              status = "warning",
              solidHeader = TRUE,
              plotlyOutput("symptom_plot", height = "380px")
            ),
            box(
              title = tagList(icon("warning"), "Exposure Risk Profile"),
              width = 6,
              status = "danger",
              solidHeader = TRUE,
              plotlyOutput("exposure_plot", height = "380px")
            )
          ),
          
          fluidRow(
            box(
              title = tagList(icon("pie-chart"), "Risk Classification"),
              width = 6,
              status = "danger",
              solidHeader = TRUE,
              plotlyOutput("risk_plot", height = "350px")
            ),
            box(
              title = tagList(icon("phone"), "Follow-up Readiness"),
              width = 6,
              status = "primary",
              solidHeader = TRUE,
              plotlyOutput("followup_plot", height = "350px")
            )
          ),
          
          fluidRow(
            box(
              title = tagList(icon("users"), "Demographic Profile for Operational Planning"),
              width = 6,
              status = "info",
              solidHeader = TRUE,
              plotlyOutput("demographic_plot", height = "350px")
            ),
            box(
              title = tagList(icon("table"), "Follow-up Gaps by Country and Transport"),
              width = 6,
              status = "danger",
              solidHeader = TRUE,
              DTOutput("followup_gap_table"),
              style = "overflow-x: auto;"
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
  
  session$allowReconnect(TRUE)
  
  observeEvent(input$browser_keepalive, {
    last_keepalive_text(format(with_tz(Sys.time(), LOCAL_TZ), "%H:%M:%S"))
  })
  
  evd_data <- reactive({
    invalidateLater(REFRESH_INTERVAL_MS, session)
    read_screening_data()
  })
  
  raw_kobo_data <- reactive({
    invalidateLater(REFRESH_INTERVAL_MS, session)
    read_raw_kobo_data()
  })
  
  mod_restricted_redflags_server(
    id = "restricted_redflags",
    evd_data = evd_data,
    raw_kobo_data = raw_kobo_data,
    authorized_emails = AUTHORIZED_HEALTH_OFFICER_EMAILS,
    refresh_interval_ms = REFRESH_INTERVAL_MS
  )
  
  observeEvent(evd_data(), {
    df <- evd_data()
    
    updatePickerInput(session, "risk_filter", choices = fallback_choices(df$risk_category), selected = fallback_choices(df$risk_category))
    updatePickerInput(session, "status_filter", choices = fallback_choices(df$clearance_status), selected = fallback_choices(df$clearance_status))
    updatePickerInput(session, "priority_filter", choices = fallback_choices(df$action_priority), selected = fallback_choices(df$action_priority))
    updatePickerInput(session, "country_filter", choices = fallback_choices(df$country_departure), selected = fallback_choices(df$country_departure))
    updatePickerInput(session, "transport_filter", choices = fallback_choices(df$transport_mode), selected = fallback_choices(df$transport_mode))
    
    epi_choices <- safe_epi_choices(df)
    
    updateSliderTextInput(
      session,
      "epi_week_filter",
      choices = epi_choices,
      selected = range(epi_choices)
    )
    
    date_min <- safe_date_min(df$submission_date)
    date_max <- safe_date_max(df$submission_date)
    
    updateDateRangeInput(
      session,
      "date_filter",
      start = date_min,
      end = date_max,
      min = date_min,
      max = date_max
    )
  }, ignoreInit = FALSE)
  
  filtered_data <- reactive({
    df <- evd_data()
    
    if (nrow(df) == 0) {
      return(df)
    }
    
    if (!is.null(input$date_filter)) {
      df <- df %>%
        filter(
          submission_date >= input$date_filter[1],
          submission_date <= input$date_filter[2]
        )
    }
    
    if (!is.null(input$epi_week_filter) && length(input$epi_week_filter) == 2) {
      weeks <- sort(unique(df$epi_week))
      selected_weeks <- weeks[weeks >= input$epi_week_filter[1] & weeks <= input$epi_week_filter[2]]
      df <- df %>% filter(epi_week %in% selected_weeks)
    }
    
    if (!is.null(input$risk_filter) && length(input$risk_filter) > 0 && !"No data yet" %in% input$risk_filter) {
      df <- df %>% filter(risk_category %in% input$risk_filter)
    }
    
    if (!is.null(input$status_filter) && length(input$status_filter) > 0 && !"No data yet" %in% input$status_filter) {
      df <- df %>% filter(clearance_status %in% input$status_filter)
    }
    
    if (!is.null(input$priority_filter) && length(input$priority_filter) > 0 && !"No data yet" %in% input$priority_filter) {
      df <- df %>% filter(action_priority %in% input$priority_filter)
    }
    
    if (!is.null(input$country_filter) && length(input$country_filter) > 0 && !"No data yet" %in% input$country_filter) {
      df <- df %>% filter(country_departure %in% input$country_filter)
    }
    
    if (!is.null(input$transport_filter) && length(input$transport_filter) > 0 && !"No data yet" %in% input$transport_filter) {
      df <- df %>% filter(transport_mode %in% input$transport_filter)
    }
    
    df
  })
  
  recent_2h_data <- reactive({
    df <- evd_data()
    if (nrow(df) == 0) return(df)
    now <- with_tz(Sys.time(), LOCAL_TZ)
    df %>% filter(submission_time >= now - hours(2))
  })
  
  recent_24h_data <- reactive({
    df <- evd_data()
    if (nrow(df) == 0) return(df)
    now <- with_tz(Sys.time(), LOCAL_TZ)
    df %>% filter(submission_time >= now - hours(24))
  })
  
  suspicious_data <- reactive({
    df <- evd_data()
    if (nrow(df) == 0) return(df)
    
    df %>%
      filter(
        action_priority == "Immediate action" |
          red_flag |
          symptom_alert |
          exposure_alert |
          risk_category == "High risk" |
          clearance_status != "Cleared"
      )
  })
  
  # ==========================================================
  # CRITICAL BLINKING ALERT BANNER
  # ==========================================================
  
  output$critical_alert_banner <- renderUI({
    sus <- suspicious_data()
    
    if (nrow(sus) == 0) {
      return(NULL)
    }
    
    immediate_n <- sum(sus$action_priority == "Immediate action", na.rm = TRUE)
    symptom_n <- sum(sus$symptom_alert, na.rm = TRUE)
    exposure_n <- sum(sus$exposure_alert, na.rm = TRUE)
    high_risk_n <- sum(sus$risk_category == "High risk", na.rm = TRUE)
    review_n <- sum(sus$clearance_status != "Cleared", na.rm = TRUE)
    
    div(
      class = "critical-alert-banner",
      div(class = "critical-alert-title", "CAUTION ALERT"),
      div(
        class = "critical-alert-subtitle",
        paste0(
          "Potential suspicious or actionable record detected: ",
          immediate_n, " immediate action; ",
          symptom_n, " symptom alerts; ",
          exposure_n, " exposure alerts; ",
          high_risk_n, " high-risk; ",
          review_n, " requiring review. ",
          "Please monitor, follow up, and take action according to EVD screening SOPs."
        )
      )
    )
  })
  
  # ==========================================================
  # STRATEGIC BRIEF
  # ==========================================================
  
  output$strategic_brief <- renderUI({
    df <- filtered_data()
    d2 <- recent_2h_data()
    d24 <- recent_24h_data()
    
    if (nrow(df) == 0) {
      return(div(
        class = "readiness-panel alert-now",
        div(
          class = "readiness-header",
          h4(class = "readiness-title", "Current Readiness"),
          span(class = "readiness-badge badge-alert", "No data signal")
        ),
        div(
          class = "readiness-action",
          strong("Operational note: "),
          "No traveler screening records are currently available for the selected period and filters. Confirm Kobo submissions and API connectivity."
        )
      ))
    }
    
    selected_total <- nrow(df)
    selected_immediate <- sum(df$action_priority == "Immediate action", na.rm = TRUE)
    selected_red <- sum(df$red_flag, na.rm = TRUE)
    selected_exposure <- sum(df$exposure_alert, na.rm = TRUE)
    selected_symptoms <- sum(df$symptom_alert, na.rm = TRUE)
    selected_core_travel <- sum(df$core_priority_country_flag, na.rm = TRUE)
    selected_high_risk <- sum(df$risk_category == "High risk", na.rm = TRUE)
    selected_missing <- sum(df$missing_followup_flag, na.rm = TRUE)
    selected_not_cleared <- sum(df$clearance_status != "Cleared", na.rm = TRUE)
    selected_temp <- sum(df$high_temperature_flag, na.rm = TRUE)
    selected_followup_ready <- sum(!df$missing_followup_flag, na.rm = TRUE)
    
    last2_immediate <- sum(d2$action_priority == "Immediate action", na.rm = TRUE)
    last2_exposure <- sum(d2$exposure_alert, na.rm = TRUE)
    last2_symptoms <- sum(d2$symptom_alert, na.rm = TRUE)
    last2_core <- sum(d2$core_priority_country_flag, na.rm = TRUE)
    
    last24_immediate <- sum(d24$action_priority == "Immediate action", na.rm = TRUE)
    last24_core_travel <- sum(d24$core_priority_country_flag, na.rm = TRUE)
    last24_high_risk <- sum(d24$risk_category == "High risk", na.rm = TRUE)
    last24_missing <- sum(d24$missing_followup_flag, na.rm = TRUE)
    
    red_pct <- percent(selected_red / max(selected_total, 1), accuracy = 0.1)
    high_risk_pct <- percent(selected_high_risk / max(selected_total, 1), accuracy = 0.1)
    symptom_pct <- percent(selected_symptoms / max(selected_total, 1), accuracy = 0.1)
    exposure_pct <- percent(selected_exposure / max(selected_total, 1), accuracy = 0.1)
    followup_ready_pct <- percent(selected_followup_ready / max(selected_total, 1), accuracy = 0.1)
    
    top_alert_country <- df %>%
      filter(red_flag) %>%
      count(country_departure, name = "n") %>%
      arrange(desc(n)) %>%
      slice_head(n = 1)
    
    top_alert_country_text <- ifelse(
      nrow(top_alert_country) == 0,
      "No active red-flag concentration",
      paste0(top_alert_country$country_departure[1], " (", top_alert_country$n[1], " red-flag records)")
    )
    
    top_priority_country <- df %>%
      filter(core_priority_country_flag | priority_country_flag) %>%
      count(country_departure, name = "n") %>%
      arrange(desc(n)) %>%
      slice_head(n = 1)
    
    top_priority_country_text <- ifelse(
      nrow(top_priority_country) == 0,
      "No priority travel concentration",
      paste0(top_priority_country$country_departure[1], " (", top_priority_country$n[1], " priority travel records)")
    )
    
    top_exposure <- tibble(
      exposure = c(
        "Contact with EVD case",
        "Funeral attendance",
        "Touched body",
        "Cared for sick person",
        "Health facility exposure",
        "Body fluids exposure",
        "Wildlife/bushmeat exposure",
        "Animal bite",
        "Affected area travel"
      ),
      records = c(
        sum(df$contact_flag, na.rm = TRUE),
        sum(df$funeral_flag, na.rm = TRUE),
        sum(df$touched_body_flag, na.rm = TRUE),
        sum(df$cared_sick_flag, na.rm = TRUE),
        sum(df$health_facility_flag, na.rm = TRUE),
        sum(df$body_fluids_flag, na.rm = TRUE),
        sum(df$wildlife_flag, na.rm = TRUE),
        sum(df$animal_bite_flag, na.rm = TRUE),
        sum(df$affected_area_flag, na.rm = TRUE)
      )
    ) %>%
      arrange(desc(records)) %>%
      filter(records > 0) %>%
      slice_head(n = 1)
    
    top_exposure_text <- ifelse(
      nrow(top_exposure) == 0,
      "No reported exposure category is currently dominant",
      paste0(top_exposure$exposure[1], " (", top_exposure$records[1], " records)")
    )
    
    if (last2_immediate > 0 || last2_exposure > 0 || last2_symptoms > 0) {
      badge_class <- "readiness-badge badge-alert"
      panel_class <- "readiness-panel alert-now"
      readiness_label <- "Immediate verification required"
      recommended_action <- "Prioritize immediate verification of recent records, review exposure history, confirm contact details, and ensure referral/isolation pathway readiness."
    } else if (last24_immediate > 0 || last24_core_travel > 0 || last24_high_risk > 0) {
      badge_class <- "readiness-badge badge-elevated"
      panel_class <- "readiness-panel monitoring"
      readiness_label <- "Enhanced border monitoring"
      recommended_action <- "Maintain enhanced monitoring for travelers from or through DRC/Uganda, validate host contacts, and prepare targeted follow-up."
    } else {
      badge_class <- "readiness-badge badge-routine"
      panel_class <- "readiness-panel"
      readiness_label <- "Routine monitoring"
      recommended_action <- "Maintain routine screening, live monitoring, and readiness checks."
    }
    
    div(
      class = panel_class,
      div(
        class = "readiness-header",
        h4(class = "readiness-title", "Current Readiness"),
        span(class = badge_class, readiness_label)
      ),
      
      div(
        class = "readiness-grid",
        div(
          class = "readiness-card",
          span(class = "label", "Current screening signal"),
          div(class = "value", paste0(comma(selected_total), " screened")),
          div(class = "note", paste0(comma(selected_red), " red flags (", red_pct, ")"))
        ),
        div(
          class = "readiness-card",
          span(class = "label", "Clinical signal"),
          div(class = "value", paste0(comma(selected_symptoms), " symptomatic")),
          div(class = "note", paste0(symptom_pct, " reporting symptoms; ", comma(selected_temp), " high temperature alerts"))
        ),
        div(
          class = "readiness-card",
          span(class = "label", "Exposure signal"),
          div(class = "value", paste0(comma(selected_exposure), " exposure alerts")),
          div(class = "note", paste0(exposure_pct, " of screened travelers"))
        ),
        div(
          class = "readiness-card",
          span(class = "label", "High-risk classification"),
          div(class = "value", paste0(comma(selected_high_risk), " high-risk")),
          div(class = "note", paste0(high_risk_pct, " of screened travelers"))
        ),
        div(
          class = "readiness-card",
          span(class = "label", "Priority travel"),
          div(class = "value", paste0(comma(selected_core_travel), " DRC/Uganda")),
          div(class = "note", top_priority_country_text)
        ),
        div(
          class = "readiness-card",
          span(class = "label", "Clearance review"),
          div(class = "value", paste0(comma(selected_not_cleared), " not cleared")),
          div(class = "note", "Records requiring review or follow-up")
        ),
        div(
          class = "readiness-card",
          span(class = "label", "Follow-up readiness"),
          div(class = "value", followup_ready_pct),
          div(class = "note", paste0(comma(selected_missing), " records with follow-up gaps"))
        ),
        div(
          class = "readiness-card",
          span(class = "label", "Risk concentration"),
          div(class = "value", top_alert_country_text),
          div(class = "note", paste0("Dominant exposure: ", top_exposure_text))
        )
      ),
      
      div(
        class = "readiness-action",
        strong("Last 2 hours: "),
        paste0(
          nrow(d2), " screened; ",
          last2_immediate, " immediate action; ",
          last2_symptoms, " symptom alerts; ",
          last2_exposure, " exposure alerts; ",
          last2_core, " DRC/Uganda priority travel records. "
        ),
        br(),
        strong("Last 24 hours: "),
        paste0(
          nrow(d24), " screened; ",
          last24_immediate, " immediate action; ",
          last24_high_risk, " high-risk travelers; ",
          last24_core_travel, " DRC/Uganda priority travel; ",
          last24_missing, " follow-up gaps. "
        ),
        br(),
        strong("Recommended action: "),
        recommended_action
      )
    )
  })
  
  # ==========================================================
  # VALUE BOXES - FIRST TAB
  # ==========================================================
  
  output$vb_2h_screened <- renderValueBox({
    d <- recent_2h_data()
    valueBox(comma(nrow(d)), "Last 2 Hours: Screened", icon = icon("clock-o"), color = "aqua")
  })
  
  output$vb_2h_immediate <- renderValueBox({
    d <- recent_2h_data()
    n <- sum(d$action_priority == "Immediate action", na.rm = TRUE)
    valueBox(comma(n), "Last 2 Hours: Immediate Action", icon = icon("exclamation-triangle"), color = "red")
  })
  
  output$vb_2h_symptoms <- renderValueBox({
    d <- recent_2h_data()
    n <- sum(d$symptom_alert, na.rm = TRUE)
    valueBox(comma(n), "Last 2 Hours: Symptom Alerts", icon = icon("stethoscope"), color = "yellow")
  })
  
  output$vb_2h_exposures <- renderValueBox({
    d <- recent_2h_data()
    n <- sum(d$exposure_alert, na.rm = TRUE)
    valueBox(comma(n), "Last 2 Hours: Exposure Alerts", icon = icon("user-md"), color = "red")
  })
  
  output$vb_24h_screened <- renderValueBox({
    d <- recent_24h_data()
    valueBox(comma(nrow(d)), "Last 24 Hours: Screened", icon = icon("calendar"), color = "blue")
  })
  
  output$vb_24h_immediate <- renderValueBox({
    d <- recent_24h_data()
    n <- sum(d$action_priority == "Immediate action", na.rm = TRUE)
    valueBox(comma(n), "Last 24 Hours: Immediate Action", icon = icon("warning"), color = "red")
  })
  
  output$vb_24h_core_priority <- renderValueBox({
    d <- recent_24h_data()
    n <- sum(d$core_priority_country_flag, na.rm = TRUE)
    valueBox(comma(n), "Last 24 Hours: DRC/Uganda Travel", icon = icon("globe"), color = "yellow")
  })
  
  output$vb_high_risk_first_tab <- renderValueBox({
    df <- filtered_data()
    n <- sum(df$risk_category == "High risk", na.rm = TRUE)
    pct <- percent(n / max(nrow(df), 1), accuracy = 0.1)
    valueBox(paste0(comma(n), " | ", pct), "High-Risk Travelers", icon = icon("exclamation-circle"), color = "red")
  })
  
  output$vb_selected_red_flags <- renderValueBox({
    df <- filtered_data()
    n <- sum(df$red_flag, na.rm = TRUE)
    pct <- percent(n / max(nrow(df), 1), accuracy = 0.1)
    valueBox(paste0(comma(n), " | ", pct), "Selected Period: Red Flags", icon = icon("flag"), color = "red")
  })
  
  output$vb_selected_temperature <- renderValueBox({
    df <- filtered_data()
    n <- sum(df$high_temperature_flag, na.rm = TRUE)
    valueBox(comma(n), paste0("Temperature >= ", HIGH_TEMP_THRESHOLD, " C"), icon = icon("thermometer-full"), color = "red")
  })
  
  output$vb_selected_followup_gaps <- renderValueBox({
    df <- filtered_data()
    n <- sum(df$missing_followup_flag, na.rm = TRUE)
    valueBox(comma(n), "Follow-up Information Gaps", icon = icon("phone"), color = "yellow")
  })
  
  output$vb_selected_clearance_concerns <- renderValueBox({
    df <- filtered_data()
    n <- sum(df$clearance_status != "Cleared", na.rm = TRUE)
    valueBox(comma(n), "Not Cleared / Requires Review", icon = icon("check-square-o"), color = "red")
  })
  
  output$vb_selected_contact_alerts <- renderValueBox({
    df <- filtered_data()
    n <- sum(df$contact_flag, na.rm = TRUE)
    valueBox(comma(n), "Contact with EVD Case", icon = icon("user-md"), color = "red")
  })
  
  output$vb_selected_funeral_alerts <- renderValueBox({
    df <- filtered_data()
    n <- sum(df$funeral_flag, na.rm = TRUE)
    valueBox(comma(n), "Funeral Attendance Risk", icon = icon("users"), color = "red")
  })
  
  output$vb_selected_health_facility_alerts <- renderValueBox({
    df <- filtered_data()
    n <- sum(df$health_facility_flag, na.rm = TRUE)
    valueBox(comma(n), "Health Facility Exposure", icon = icon("hospital-o"), color = "yellow")
  })
  
  output$vb_followup_ready_first_tab <- renderValueBox({
    df <- filtered_data()
    ready <- sum(!df$missing_followup_flag, na.rm = TRUE)
    pct <- percent(ready / max(nrow(df), 1), accuracy = 0.1)
    valueBox(paste0(comma(ready), " | ", pct), "Follow-up Info Complete", icon = icon("phone-square"), color = "green")
  })
  
  output$vb_symptom_percentage <- renderValueBox({
    df <- filtered_data()
    n <- sum(df$symptom_alert, na.rm = TRUE)
    pct <- percent(n / max(nrow(df), 1), accuracy = 0.1)
    valueBox(pct, "Percentage Reporting Symptoms", icon = icon("percent"), color = "yellow")
  })
  
  output$vb_fever_reporting <- renderValueBox({
    df <- filtered_data()
    n <- sum(df$fever_flag, na.rm = TRUE)
    valueBox(comma(n), "Travelers Reporting Fever", icon = icon("thermometer-half"), color = "yellow")
  })
  
  output$vb_medical_care <- renderValueBox({
    df <- filtered_data()
    n <- sum(df$medical_care_flag, na.rm = TRUE)
    valueBox(comma(n), "Travelers Seeking Medical Care", icon = icon("hospital-o"), color = "aqua")
  })
  
  output$vb_high_risk_percentage <- renderValueBox({
    df <- filtered_data()
    n <- sum(df$risk_category == "High risk", na.rm = TRUE)
    pct <- percent(n / max(nrow(df), 1), accuracy = 0.1)
    valueBox(pct, "Percentage High-Risk Travelers", icon = icon("exclamation-circle"), color = "red")
  })
  
  # ==========================================================
  # VALUE BOXES - SCREENING AND TRAVEL FLOW
  # ==========================================================
  
  output$vb_unique_departure_countries <- renderValueBox({
    df <- filtered_data()
    n <- n_distinct(df$country_departure[df$country_departure != "Missing"])
    valueBox(comma(n), "Departure Countries", icon = icon("globe"), color = "blue")
  })
  
  output$vb_unique_destinations <- renderValueBox({
    df <- filtered_data()
    n <- n_distinct(df$destination_ss[df$destination_ss != "Not recorded"])
    valueBox(comma(n), "Destinations in South Sudan", icon = icon("location-arrow"), color = "green")
  })
  
  output$vb_transport_modes <- renderValueBox({
    df <- filtered_data()
    n <- n_distinct(df$transport_mode[df$transport_mode != "Not recorded"])
    valueBox(comma(n), "Transport Modes Captured", icon = icon("truck"), color = "aqua")
  })
  
  output$vb_median_screening_time <- renderValueBox({
    df <- filtered_data()
    med <- suppressWarnings(median(df$screening_minutes, na.rm = TRUE))
    if (is.na(med) || is.infinite(med)) med <- 0
    valueBox(paste0(round(med, 1), " min"), "Median Screening Time", icon = icon("tachometer"), color = "aqua")
  })
  
  # ==========================================================
  # VALUE BOXES - RISK, EXPOSURE, FOLLOW-UP
  # ==========================================================
  
  output$vb_body_fluids_alerts <- renderValueBox({
    df <- filtered_data()
    n <- sum(df$body_fluids_flag, na.rm = TRUE)
    valueBox(comma(n), "Body Fluid Exposure", icon = icon("tint"), color = "red")
  })
  
  output$vb_wildlife_alerts <- renderValueBox({
    df <- filtered_data()
    n <- sum(df$wildlife_flag, na.rm = TRUE)
    valueBox(comma(n), "Wildlife / Bushmeat Exposure", icon = icon("paw"), color = "yellow")
  })
  
  output$vb_animal_bite_alerts <- renderValueBox({
    df <- filtered_data()
    n <- sum(df$animal_bite_flag, na.rm = TRUE)
    valueBox(comma(n), "Animal Bite Exposure", icon = icon("exclamation"), color = "yellow")
  })
  
  output$vb_affected_area_alerts <- renderValueBox({
    df <- filtered_data()
    n <- sum(df$affected_area_flag, na.rm = TRUE)
    valueBox(comma(n), "Affected Area Travel", icon = icon("map-marker"), color = "red")
  })
  
  # ==========================================================
  # TABLES
  # ==========================================================
  
  output$action_matrix <- renderDT({
    df <- filtered_data()
    
    d <- df %>%
      group_by(action_priority, country_departure, city_departure, transport_mode) %>%
      summarise(
        `Total Screened` = n(),
        `Immediate Action` = sum(action_priority == "Immediate action", na.rm = TRUE),
        `Enhanced Monitoring` = sum(action_priority == "Enhanced monitoring", na.rm = TRUE),
        `Red Flags` = sum(red_flag, na.rm = TRUE),
        `High Risk` = sum(risk_category == "High risk", na.rm = TRUE),
        `Symptom Alerts` = sum(symptom_alert, na.rm = TRUE),
        `Exposure Alerts` = sum(exposure_alert, na.rm = TRUE),
        `DRC/Uganda Travel` = sum(core_priority_country_flag, na.rm = TRUE),
        `Priority Travel` = sum(priority_country_flag, na.rm = TRUE),
        `Follow-up Gaps` = sum(missing_followup_flag, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(
        factor(action_priority, levels = c("Immediate action", "Enhanced monitoring", "Routine monitoring")),
        desc(`Immediate Action`),
        desc(`Red Flags`),
        desc(`Exposure Alerts`),
        desc(`Symptom Alerts`)
      ) %>%
      rename(
        `Action Priority` = action_priority,
        `Country of Departure` = country_departure,
        `City` = city_departure,
        `Transport` = transport_mode
      )
    
    datatable(
      d,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE),
      class = "stripe hover compact"
    )
  })
  
  output$route_summary_table <- renderDT({
    df <- filtered_data()
    
    d <- df %>%
      group_by(country_departure, city_departure, transport_mode, vehicle_flight_name, flight_number, destination_ss) %>%
      summarise(
        `Total Screened` = n(),
        `Cleared` = sum(clearance_status == "Cleared", na.rm = TRUE),
        `Immediate Action` = sum(action_priority == "Immediate action", na.rm = TRUE),
        `Enhanced Monitoring` = sum(action_priority == "Enhanced monitoring", na.rm = TRUE),
        `Red Flags` = sum(red_flag, na.rm = TRUE),
        `DRC/Uganda Travel` = sum(core_priority_country_flag, na.rm = TRUE),
        `Invalid / Old Arrival Date` = sum(arrival_date_quality_flag == "Arrival date outside expected range", na.rm = TRUE),
        `Median Screening Time` = round(median(screening_minutes, na.rm = TRUE), 1),
        .groups = "drop"
      ) %>%
      mutate(`Median Screening Time` = ifelse(is.na(`Median Screening Time`) | is.infinite(`Median Screening Time`), NA, `Median Screening Time`)) %>%
      arrange(desc(`Immediate Action`), desc(`Red Flags`), desc(`Enhanced Monitoring`), desc(`Total Screened`)) %>%
      rename(
        `Country of Departure` = country_departure,
        `City` = city_departure,
        `Transport` = transport_mode,
        `Airline / Vehicle` = vehicle_flight_name,
        `Flight Number` = flight_number,
        `Destination in South Sudan` = destination_ss
      )
    
    datatable(
      d,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE),
      class = "stripe hover compact"
    )
  })
  
  output$followup_gap_table <- renderDT({
    df <- filtered_data()
    
    d <- df %>%
      group_by(country_departure, transport_mode, vehicle_flight_name) %>%
      summarise(
        `Total Screened` = n(),
        `Follow-up Gaps` = sum(missing_followup_flag, na.rm = TRUE),
        `Immediate Action` = sum(action_priority == "Immediate action", na.rm = TRUE),
        `Enhanced Monitoring` = sum(action_priority == "Enhanced monitoring", na.rm = TRUE),
        `Red Flags` = sum(red_flag, na.rm = TRUE),
        `Exposure Alerts` = sum(exposure_alert, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        `Follow-up Gap %` = percent(`Follow-up Gaps` / pmax(`Total Screened`, 1), accuracy = 0.1)
      ) %>%
      arrange(desc(`Follow-up Gaps`), desc(`Immediate Action`), desc(`Exposure Alerts`)) %>%
      rename(
        `Country of Departure` = country_departure,
        `Transport` = transport_mode,
        `Airline / Vehicle` = vehicle_flight_name
      )
    
    datatable(
      d,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE),
      class = "stripe hover compact"
    )
  })
  
  # ==========================================================
  # FIRST TAB LINE GRAPHS AND CHARTS
  # ==========================================================
  
  output$last24_trend_plot <- renderPlotly({
    d24 <- recent_24h_data()
    
    if (nrow(d24) == 0) {
      return(empty_plotly("No records in the last 24 hours"))
    }
    
    d <- d24 %>%
      mutate(day_label = format(submission_date, "%d %b %Y")) %>%
      group_by(submission_date, day_label) %>%
      summarise(
        Screened = n(),
        `Immediate action` = sum(action_priority == "Immediate action", na.rm = TRUE),
        `Enhanced monitoring` = sum(action_priority == "Enhanced monitoring", na.rm = TRUE),
        `High risk` = sum(risk_category == "High risk", na.rm = TRUE),
        `Red flags` = sum(red_flag, na.rm = TRUE),
        `Symptom alerts` = sum(symptom_alert, na.rm = TRUE),
        `Exposure alerts` = sum(exposure_alert, na.rm = TRUE),
        `DRC/Uganda travel` = sum(core_priority_country_flag, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(submission_date) %>%
      pivot_longer(
        cols = c(Screened, `Immediate action`, `Enhanced monitoring`, `High risk`, `Red flags`, `Symptom alerts`, `Exposure alerts`, `DRC/Uganda travel`),
        names_to = "indicator",
        values_to = "records"
      )
    
    plot_ly(
      d,
      x = ~day_label,
      y = ~records,
      color = ~indicator,
      type = "scatter",
      mode = "lines+markers+text",
      text = ~records,
      textposition = "top center",
      hovertemplate = "%{x}<br>%{y} records<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Date", categoryorder = "array", categoryarray = unique(d$day_label)),
        yaxis = list(title = "Records"),
        legend = list(orientation = "h", x = 0.02, y = -0.25)
      )
  })
  
  output$hourly_signal_plot <- renderPlotly({
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      return(empty_plotly("No records available"))
    }
    
    d <- df %>%
      mutate(day_label = format(submission_date, "%d %b %Y")) %>%
      group_by(submission_date, day_label) %>%
      summarise(
        `Red flags` = sum(red_flag, na.rm = TRUE),
        `High risk` = sum(risk_category == "High risk", na.rm = TRUE),
        `Not cleared/review` = sum(clearance_status != "Cleared", na.rm = TRUE),
        `Follow-up gaps` = sum(missing_followup_flag, na.rm = TRUE),
        `Temperature alerts` = sum(high_temperature_flag, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(submission_date) %>%
      pivot_longer(
        cols = c(`Red flags`, `High risk`, `Not cleared/review`, `Follow-up gaps`, `Temperature alerts`),
        names_to = "indicator",
        values_to = "records"
      )
    
    plot_ly(
      d,
      x = ~day_label,
      y = ~records,
      color = ~indicator,
      type = "scatter",
      mode = "lines+markers+text",
      text = ~records,
      textposition = "top center",
      hovertemplate = "%{x}<br>%{y} records<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Date", categoryorder = "array", categoryarray = unique(d$day_label)),
        yaxis = list(title = "Records"),
        legend = list(orientation = "h", x = 0.02, y = -0.25)
      )
  })
  
  output$first_tab_symptom_indicators <- renderPlotly({
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      return(empty_plotly())
    }
    
    d <- tibble(
      Indicator = c(
        "Travelers reporting symptoms",
        "Travelers reporting fever",
        paste0("Travelers with temperature >= ", HIGH_TEMP_THRESHOLD, " C"),
        "Travelers seeking medical care"
      ),
      Records = c(
        sum(df$symptom_alert, na.rm = TRUE),
        sum(df$fever_flag, na.rm = TRUE),
        sum(df$high_temperature_flag, na.rm = TRUE),
        sum(df$medical_care_flag, na.rm = TRUE)
      )
    ) %>%
      arrange(Records)
    
    plot_ly(
      d,
      x = ~Records,
      y = ~Indicator,
      type = "bar",
      orientation = "h",
      text = ~Records,
      textposition = "auto",
      marker = list(color = "#f39c12")
    ) %>%
      layout(
        xaxis = list(title = "Travelers"),
        yaxis = list(title = "")
      )
  })
  
  output$first_tab_exposure_indicators <- renderPlotly({
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      return(empty_plotly())
    }
    
    d <- tibble(
      Indicator = c(
        "Contact with Ebola case",
        "Funeral attendance",
        "Wild animals / bushmeat exposure",
        "Visited health facility",
        "Affected area travel",
        "High-risk travelers"
      ),
      Records = c(
        sum(df$contact_flag, na.rm = TRUE),
        sum(df$funeral_flag, na.rm = TRUE),
        sum(df$wildlife_flag, na.rm = TRUE),
        sum(df$health_facility_flag, na.rm = TRUE),
        sum(df$affected_area_flag, na.rm = TRUE),
        sum(df$risk_category == "High risk", na.rm = TRUE)
      )
    ) %>%
      arrange(Records)
    
    plot_ly(
      d,
      x = ~Records,
      y = ~Indicator,
      type = "bar",
      orientation = "h",
      text = ~Records,
      textposition = "auto",
      marker = list(color = "#dd4b39")
    ) %>%
      layout(
        xaxis = list(title = "Travelers"),
        yaxis = list(title = "")
      )
  })
  
  output$common_symptoms_plot <- renderPlotly({
    df <- filtered_data()
    
    if (nrow(df) == 0 || all(normalize_text(df$symptom_text) == "")) {
      return(empty_plotly("There were no specific potential symptoms of EVD reported."))
    }
    
    d <- df %>%
      select(symptom_text) %>%
      mutate(
        symptom_text = str_replace_all(symptom_text, ";", ","),
        symptom_text = str_replace_all(symptom_text, "\\|", ",")
      ) %>%
      separate_rows(symptom_text, sep = ",") %>%
      mutate(symptom_text = str_trim(str_to_title(symptom_text))) %>%
      filter(symptom_text != "", symptom_text != "No", symptom_text != "None") %>%
      count(symptom_text, name = "Records") %>%
      arrange(desc(Records)) %>%
      slice_head(n = 10) %>%
      arrange(Records)
    
    if (nrow(d) == 0) {
      return(empty_plotly("There were no specific potential symptoms of EVD reported."))
    }
    
    plot_ly(
      d,
      x = ~Records,
      y = ~symptom_text,
      type = "bar",
      orientation = "h",
      text = ~Records,
      textposition = "auto",
      marker = list(color = "#f39c12")
    ) %>%
      layout(
        xaxis = list(title = "Travelers"),
        yaxis = list(title = "")
      )
  })
  
  output$hourly_symptom_exposure_plot <- renderPlotly({
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      return(empty_plotly("No records available"))
    }
    
    d <- df %>%
      mutate(day_label = format(submission_date, "%d %b %Y")) %>%
      group_by(submission_date, day_label) %>%
      summarise(
        `Symptoms` = sum(symptom_alert, na.rm = TRUE),
        `Fever / temperature` = sum(fever_flag, na.rm = TRUE),
        `High temperature` = sum(high_temperature_flag, na.rm = TRUE),
        `Exposure history` = sum(exposure_alert, na.rm = TRUE),
        `Ebola case contact` = sum(contact_flag, na.rm = TRUE),
        `Affected area travel` = sum(affected_area_flag, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(submission_date) %>%
      pivot_longer(
        cols = c(
          `Symptoms`,
          `Fever / temperature`,
          `High temperature`,
          `Exposure history`,
          `Ebola case contact`,
          `Affected area travel`
        ),
        names_to = "indicator",
        values_to = "records"
      )
    
    plot_ly(
      d,
      x = ~day_label,
      y = ~records,
      color = ~indicator,
      type = "scatter",
      mode = "lines+markers+text",
      text = ~records,
      textposition = "top center",
      hovertemplate = "%{x}<br>%{y} records<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Date", categoryorder = "array", categoryarray = unique(d$day_label)),
        yaxis = list(title = "Records"),
        legend = list(orientation = "h", x = 0.02, y = -0.25)
      )
  })
  
  output$alert_drivers_plot <- renderPlotly({
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      return(empty_plotly())
    }
    
    d <- tibble(
      Driver = c(
        "Exposure history",
        "Symptom alert",
        "DRC/Uganda travel history",
        "Priority travel history",
        "Follow-up information gap",
        paste0("Temperature >= ", HIGH_TEMP_THRESHOLD, " C")
      ),
      Records = c(
        sum(df$exposure_alert, na.rm = TRUE),
        sum(df$symptom_alert, na.rm = TRUE),
        sum(df$core_priority_country_flag, na.rm = TRUE),
        sum(df$priority_country_flag, na.rm = TRUE),
        sum(df$missing_followup_flag, na.rm = TRUE),
        sum(df$high_temperature_flag, na.rm = TRUE)
      )
    ) %>%
      arrange(Records)
    
    plot_ly(
      d,
      x = ~Records,
      y = ~Driver,
      type = "bar",
      orientation = "h",
      text = ~Records,
      textposition = "auto",
      marker = list(color = "#dd4b39")
    ) %>%
      layout(xaxis = list(title = "Records"), yaxis = list(title = ""))
  })
  
  output$priority_travel_plot <- renderPlotly({
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      return(empty_plotly())
    }
    
    d <- tibble(
      Category = c(
        "DRC/Uganda priority travel",
        "Other priority travel",
        "No priority travel signal"
      ),
      Records = c(
        sum(df$core_priority_country_flag, na.rm = TRUE),
        sum(df$priority_country_flag & !df$core_priority_country_flag, na.rm = TRUE),
        sum(!df$priority_country_flag, na.rm = TRUE)
      )
    )
    
    plot_ly(
      d,
      labels = ~Category,
      values = ~Records,
      type = "pie",
      hole = 0.55,
      textinfo = "label+percent",
      marker = list(colors = c("#dd4b39", "#f39c12", "#00a65a"))
    ) %>%
      layout(showlegend = FALSE)
  })
  
  # ==========================================================
  # SCREENING AND TRAVEL FLOW PLOTS
  # ==========================================================
  
  output$hourly_intensity_plot <- renderPlotly({
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      return(empty_plotly())
    }
    
    d <- df %>%
      mutate(day_label = format(submission_date, "%d %b %Y")) %>%
      count(submission_date, day_label, name = "records") %>%
      arrange(submission_date)
    
    plot_ly(
      d,
      x = ~day_label,
      y = ~records,
      type = "scatter",
      mode = "lines+markers+text",
      text = ~records,
      textposition = "top center",
      line = list(color = "#003b6f", width = 3),
      marker = list(color = "#0093D5", size = 8),
      fill = "tozeroy",
      hovertemplate = "%{x}<br>%{y} screenings<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Date", categoryorder = "array", categoryarray = unique(d$day_label)),
        yaxis = list(title = "Screenings")
      )
  })
  
  output$country_volume_plot <- renderPlotly({
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      return(empty_plotly())
    }
    
    d <- df %>%
      count(country_departure, name = "records") %>%
      arrange(desc(records)) %>%
      slice_head(n = 12) %>%
      arrange(records)
    
    plot_ly(
      d,
      x = ~records,
      y = ~country_departure,
      type = "bar",
      orientation = "h",
      text = ~records,
      textposition = "auto",
      marker = list(color = "#003b6f")
    ) %>%
      layout(xaxis = list(title = "Screened travelers"), yaxis = list(title = ""))
  })
  
  output$transport_plot <- renderPlotly({
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      return(empty_plotly())
    }
    
    d <- df %>%
      count(transport_mode, name = "records") %>%
      arrange(desc(records))
    
    plot_ly(
      d,
      x = ~transport_mode,
      y = ~records,
      type = "bar",
      text = ~records,
      textposition = "auto",
      marker = list(color = "#0093D5")
    ) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Screened travelers"))
  })
  
  output$destination_plot <- renderPlotly({
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      return(empty_plotly())
    }
    
    d <- df %>%
      count(destination_ss, name = "records") %>%
      arrange(desc(records)) %>%
      slice_head(n = 12) %>%
      arrange(records)
    
    plot_ly(
      d,
      x = ~records,
      y = ~destination_ss,
      type = "bar",
      orientation = "h",
      text = ~records,
      textposition = "auto",
      marker = list(color = "#00a65a")
    ) %>%
      layout(xaxis = list(title = "Screened travelers"), yaxis = list(title = ""))
  })
  
  output$arrival_pattern_plot <- renderPlotly({
    df <- filtered_data() %>%
      filter(!is.na(arrival_date_validated))
    
    if (nrow(df) == 0) {
      return(empty_plotly("No valid arrival date data available. Some records may have older or inconsistent arrival dates and should be checked."))
    }
    
    d <- df %>%
      count(arrival_date_validated, name = "records")
    
    plot_ly(
      d,
      x = ~arrival_date_validated,
      y = ~records,
      type = "scatter",
      mode = "lines+markers",
      line = list(color = "#f39c12", width = 3),
      marker = list(color = "#f39c12", size = 8),
      fill = "tozeroy",
      hovertemplate = "%{x}<br>%{y} validated arrivals<extra></extra>"
    ) %>%
      layout(xaxis = list(title = "Validated arrival date"), yaxis = list(title = "Arrivals"))
  })
  
  # ==========================================================
  # RISK, EXPOSURE AND FOLLOW-UP PLOTS
  # ==========================================================
  
  output$symptom_plot <- renderPlotly({
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      return(empty_plotly())
    }
    
    d <- tibble(
      Indicator = c(
        "Symptom flag",
        "Fever / temperature alert",
        paste0("Temperature >= ", HIGH_TEMP_THRESHOLD, " C"),
        "Medical care sought"
      ),
      Records = c(
        sum(df$symptom_alert, na.rm = TRUE),
        sum(df$fever_flag, na.rm = TRUE),
        sum(df$high_temperature_flag, na.rm = TRUE),
        sum(df$medical_care_flag, na.rm = TRUE)
      )
    ) %>%
      arrange(Records)
    
    plot_ly(
      d,
      x = ~Records,
      y = ~Indicator,
      type = "bar",
      orientation = "h",
      text = ~Records,
      textposition = "auto",
      marker = list(color = "#f39c12")
    ) %>%
      layout(xaxis = list(title = "Records"), yaxis = list(title = ""))
  })
  
  output$exposure_plot <- renderPlotly({
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      return(empty_plotly())
    }
    
    d <- tibble(
      Indicator = c(
        "Contact with EVD case",
        "Funeral attendance",
        "Touched body",
        "Cared for sick person",
        "Visited health facility",
        "Body fluids exposure",
        "Wild animals / bushmeat",
        "Animal bite",
        "Affected area travel"
      ),
      Records = c(
        sum(df$contact_flag, na.rm = TRUE),
        sum(df$funeral_flag, na.rm = TRUE),
        sum(df$touched_body_flag, na.rm = TRUE),
        sum(df$cared_sick_flag, na.rm = TRUE),
        sum(df$health_facility_flag, na.rm = TRUE),
        sum(df$body_fluids_flag, na.rm = TRUE),
        sum(df$wildlife_flag, na.rm = TRUE),
        sum(df$animal_bite_flag, na.rm = TRUE),
        sum(df$affected_area_flag, na.rm = TRUE)
      )
    ) %>%
      arrange(Records)
    
    plot_ly(
      d,
      x = ~Records,
      y = ~Indicator,
      type = "bar",
      orientation = "h",
      text = ~Records,
      textposition = "auto",
      marker = list(color = "#dd4b39")
    ) %>%
      layout(xaxis = list(title = "Records"), yaxis = list(title = ""))
  })
  
  output$risk_plot <- renderPlotly({
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      return(empty_plotly())
    }
    
    d <- df %>%
      count(risk_category, name = "records")
    
    plot_ly(
      d,
      labels = ~risk_category,
      values = ~records,
      type = "pie",
      hole = 0.55,
      textinfo = "label+percent",
      marker = list(colors = unname(risk_colors[d$risk_category]))
    ) %>%
      layout(showlegend = FALSE)
  })
  
  output$followup_plot <- renderPlotly({
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      return(empty_plotly())
    }
    
    d <- tibble(
      Status = c("Follow-up info complete", "Follow-up information gap"),
      Records = c(
        sum(!df$missing_followup_flag, na.rm = TRUE),
        sum(df$missing_followup_flag, na.rm = TRUE)
      )
    )
    
    plot_ly(
      d,
      labels = ~Status,
      values = ~Records,
      type = "pie",
      hole = 0.55,
      textinfo = "label+percent",
      marker = list(colors = c("#00a65a", "#dd4b39"))
    ) %>%
      layout(showlegend = FALSE)
  })
  
  output$demographic_plot <- renderPlotly({
    df <- filtered_data() %>%
      filter(!is.na(age_group))
    
    if (nrow(df) == 0) {
      return(empty_plotly("No age/sex data available"))
    }
    
    d <- df %>%
      count(age_group, sex, name = "records")
    
    plot_ly(
      d,
      x = ~age_group,
      y = ~records,
      color = ~sex,
      type = "bar",
      text = ~records,
      textposition = "auto",
      colors = sex_colors
    ) %>%
      layout(
        barmode = "stack",
        xaxis = list(title = "Age group"),
        yaxis = list(title = "Screened travelers"),
        legend = list(orientation = "h", x = 0.05, y = -0.2)
      )
  })
}

shinyApp(ui, server)
