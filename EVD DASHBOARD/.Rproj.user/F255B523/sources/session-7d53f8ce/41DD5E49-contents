# ============================================================
# MODULE: RESTRICTED RED-FLAG VISITOR DETAILS
# File: modules/mod_restricted_redflags.R
# Purpose:
#   - Email-gated restricted tab for reviewing and clearing alert records
#   - Default view "Awaiting review" matches the caution bar on the JIA Screening Detail tab
#   - Keep the public dashboard aggregated only
#   - Show full original ODK/Kobo details only for approved health officer emails
# ============================================================

mod_restricted_redflags_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = tagList(icon("lock"), "3. Restricted View Alerts and Signal Details"),
    
    div(
      class = "tab-pointer-banner",
      span(class = "arrow", HTML("&#8593;")),
      "Restricted health-officer view. Enter an approved email to unlock full ODK/Kobo alert records."
    ),
    
    fluidRow(
      box(
        title = tagList(icon("lock"), "Health Officer Restricted Alert Details"),
        width = 12,
        status = "danger",
        solidHeader = TRUE,
        uiOutput(ns("authorized_access_notice"))
      )
    ),
    uiOutput(ns("restricted_redflag_details_ui"))
  )
}

mod_restricted_redflags_server <- function(
    id,
    evd_data,
    raw_kobo_data,
    authorized_emails,
    refresh_interval_ms = 120000,
    cleared_ids = reactive(character()),
    mark_alerts_cleared = NULL,
    restore_alerts = NULL,
    clearance_log = reactive(tibble())
) {
  moduleServer(id, function(input, output, session) {
    
    `%||%` <- function(x, y) {
      if (is.null(x)) y else x
    }
    
    get_logged_in_email <- function(session) {
      user_email <- ""
      
      if (!is.null(session$user) && nzchar(session$user)) {
        user_email <- session$user
      }
      
      tolower(trimws(user_email))
    }
    
    normalize_email <- function(x) {
      tolower(trimws(as.character(x)))
    }
    
    submitted_email <- reactiveVal("")
    login_attempted <- reactiveVal(FALSE)
    
    observeEvent(input$unlock_redflag_details, {
      submitted_email(normalize_email(input$restricted_email))
      login_attempted(TRUE)
    }, ignoreInit = TRUE)
    
    observeEvent(input$clear_restricted_login, {
      submitted_email("")
      login_attempted(FALSE)
      updateTextInput(session, "restricted_email", value = "")
    }, ignoreInit = TRUE)
    
    session_email <- reactive({
      get_logged_in_email(session)
    })
    
    authorized_user <- reactive({
      approved <- normalize_email(authorized_emails)
      typed_ok <- isTRUE(login_attempted()) && nzchar(submitted_email()) && submitted_email() %in% approved
      session_ok <- nzchar(session_email()) && session_email() %in% approved
      typed_ok || session_ok
    })
    
    active_user_email <- reactive({
      if (nzchar(session_email()) && session_email() %in% normalize_email(authorized_emails)) {
        session_email()
      } else if (isTRUE(login_attempted()) && nzchar(submitted_email())) {
        submitted_email()
      } else {
        ""
      }
    })
    
    make_main_surveillance_columns <- function(d) {
      if (is.null(d) || nrow(d) == 0) {
        return(tibble())
      }
      
      get_col <- function(candidates) {
        existing <- candidates[candidates %in% names(d)]
        
        if (length(existing) == 0) {
          return(rep("", nrow(d)))
        }
        
        out <- as.character(d[[existing[1]]])
        out[is.na(out)] <- ""
        
        if (length(existing) > 1) {
          for (nm in existing[-1]) {
            nxt <- as.character(d[[nm]])
            nxt[is.na(nxt)] <- ""
            out <- dplyr::coalesce(na_if(out, ""), na_if(nxt, ""))
            out[is.na(out)] <- ""
          }
        }
        
        out
      }
      
      tibble(
        `Submission Time` = get_col(c("calculated_submission_time", "_submission_time", "submission_date")),
        `Submission Date` = get_col(c("calculated_submission_date", "submission_date")),
        `Screening ID` = get_col(c("calculated_screening_id", "screening_id")),
        `Reason for Review` = get_col(c("calculated_review_reason")),
        `Awaiting Review` = get_col(c("calculated_awaiting_review")),
        `Action Priority` = get_col(c("calculated_action_priority")),
        `Alert Reason` = get_col(c("calculated_red_flag_reason")),
        `Risk Category` = get_col(c("calculated_risk_category", "risk_assessment/risk_category", "risk_category")),
        `Clearance Status` = get_col(c("calculated_clearance_status", "clearance_status")),
        `Symptom Alert` = get_col(c("calculated_symptom_alert")),
        `Exposure Alert` = get_col(c("calculated_exposure_alert")),
        `High Risk` = get_col(c("calculated_high_risk")),
        `Needs Review` = get_col(c("calculated_needs_review")),
        `Follow-up Gap` = get_col(c("calculated_followup_gap")),
        `Incomplete Form` = get_col(c("calculated_incomplete_form")),
        
        `Full Name` = get_col(c("personal_info/full_name", "Full Name")),
        `Sex` = get_col(c("personal_info/sex", "Sex")),
        `Date of Birth` = get_col(c("personal_info/date_of_birth", "Date of Birth")),
        `Age` = get_col(c("personal_info/age", "Age")),
        `Nationality` = get_col(c("personal_info/nationality", "Nationality")),
        `Passport / National ID` = get_col(c("personal_info/passport_id", "Passport/National ID Number")),
        `Phone Number` = get_col(c("personal_info/phone_number", "Phone Number")),
        `Alternative Phone` = get_col(c("personal_info/alternative_phone", "personal_info/alternative_phone_number", "Alternative Phone Number")),
        `Occupation` = get_col(c("personal_info/occupation", "Occupation")),
        `Home Address` = get_col(c("personal_info/home_address", "Home Address")),
        `Current Residence` = get_col(c("personal_info/current_residence", "Current Residence")),
        
        `Country of Departure` = get_col(c("calculated_country_departure_clean", "travel_info/country", "Country of Departure")),
        `City of Departure` = get_col(c("travel_info/city", "City of Departure")),
        `Countries Visited in Last 21 Days` = get_col(c("calculated_countries_visited_clean", "travel_info/countries", "Countries Visited in Last 21 Days")),
        `Date of Arrival` = get_col(c("travel_info/arrival_date", "Date of Arrival")),
        `Means of Transport` = get_col(c("travel_info/transport_mode", "Means of Transport")),
        `Flight / Airline` = get_col(c("travel_info/vehicle_flight_name", "Flight Name / Airline")),
        `Flight Number` = get_col(c("travel_info/vehicle_flight_number", "travel_info/flight_number", "Flight Number")),
        `Seat Number` = get_col(c("travel_info/seat_number", "Seat Number")),
        `Vehicle Registration` = get_col(c("travel_info/vehicle_registration", "Vehicle Registration Number")),
        `Final Destination in South Sudan` = get_col(c("travel_info/destination_ss", "Final Destination in South Sudan")),
        `Duration of Stay` = get_col(c("travel_info/duration_stay", "Duration of Stay in South Sudan")),
        `Purpose of Travel` = get_col(c("travel_info/purpose_travel", "Purpose of Travel")),
        
        `Fever / Temperature Response` = get_col(c("health_screening/temperature", "Do you have a fever (temperature >= 37.5 C)?", "Temperature")),
        `Symptoms Reported` = get_col(c("health_screening/symptoms", "Select any symptoms currently present", "symptoms")),
        `Sought Medical Care` = get_col(c("health_screening/seek_medical", "health_screening/seeking_medical_care", "Have you sought medical care for these symptoms?")),
        
        `Contact with Confirmed/Suspected EVD Case` = get_col(c("risk_assessment/contact_ebola", "Contact with confirmed/suspected Ebola patient in last 21 days?")),
        `Attended Funeral or Burial` = get_col(c("risk_assessment/funeral_attendance", "Attended funeral or burial ceremony in last 21 days?")),
        `Touched Dead Body` = get_col(c("risk_assessment/touched_body", "Touched a dead body during burial rituals?")),
        `Cared for Sick Person` = get_col(c("risk_assessment/cared_sick", "Cared for sick person with fever, vomiting, diarrhea, or bleeding?")),
        `Visited Health Facility Treating EVD` = get_col(c("risk_assessment/visited_health_facility", "Visited health facility treating Ebola patients?")),
        `Handled Blood / Body Fluids` = get_col(c("risk_assessment/body_fluids", "Handled blood or body fluids without protection?")),
        `Handled Bushmeat / Wild Animals` = get_col(c("risk_assessment/wild_animals", "Handled bush meat or wild animals?")),
        `Animal Bite or Scratch` = get_col(c("risk_assessment/animal_bite", "Been bitten or scratched by a wild animal?")),
        `Travel to EVD-Affected Area` = get_col(c("risk_assessment/affected_area", "Any history of travel to  Ebola-affected area in last 21 days?")),
        
        `Host / Contact Person` = get_col(c("followup_contact/host_name", "Name of Host/Contact Person in South Sudan")),
        `Host Phone Number` = get_col(c("followup_contact/host_phone", "Phone Number of Host")),
        `Host Address / Location` = get_col(c("followup_contact/host_address", "Address/Location in South Sudan")),
        `Declaration` = get_col(c("followup_contact/declaration", "I declare that the information provided is true and complete to the best of my knowledge."))
      )
    }
    
    full_redflag_details_unfiltered <- reactive({
      raw_df <- raw_kobo_data()
      clean_df <- evd_data()
      
      if (is.null(raw_df) || is.null(clean_df) || nrow(raw_df) == 0 || nrow(clean_df) == 0) {
        return(tibble())
      }
      
      # Page 2 flags: PoE alert, high-risk (screener), suspected case, ...
      clean_df <- jia_add_flags(clean_df)
      
      # Match the raw Ona rows to the cleaned rows by screening ID (not by row
      # position), so personal details always belong to the right traveller.
      raw_ids <- safe_chr(first_existing_col(raw_df, c("screening_id", "participant_id", "_uuid", "_id")))
      if (anyDuplicated(raw_ids[raw_ids != ""]) > 0) {
        message("Restricted tab: duplicate screening IDs in the Ona data; details for duplicated IDs show the first matching record.")
      }
      raw_df <- raw_df[match(clean_df$screening_id, raw_ids), , drop = FALSE]
      
      # Awaiting review = exactly what the caution bar on page 2 counts
      cleared  <- cleared_ids()
      awaiting <- (clean_df$poe_alert | clean_df$high_risk) & !clean_df$screening_id %in% cleared
      
      alert_index <- which(
        awaiting |
          clean_df$action_priority %in% c("Immediate action", "Follow-up required") |
          clean_df$red_flag |
          ("active_evd_alert" %in% names(clean_df) & clean_df$active_evd_alert) |
          ("followup_required" %in% names(clean_df) & clean_df$followup_required) |
          clean_df$symptom_alert |
          clean_df$exposure_alert |
          clean_df$risk_category == "High risk" |
          clean_df$clearance_status != "Cleared"
      )
      
      if (length(alert_index) == 0) {
        return(tibble())
      }
      
      alert_flags <- tibble(
        calculated_submission_time = clean_df$submission_time,
        calculated_submission_date = as.Date(clean_df$submission_time),
        calculated_screening_id = clean_df$screening_id,
        calculated_awaiting_review = awaiting,
        calculated_review_reason = jia_review_reason(clean_df),
        calculated_poe_alert = clean_df$poe_alert,
        calculated_suspected_case = clean_df$suspected_case,
        calculated_action_priority = clean_df$action_priority,
        calculated_red_flag = clean_df$red_flag,
        calculated_red_flag_reason = clean_df$red_flag_reason,
        calculated_symptom_alert = clean_df$symptom_alert,
        calculated_exposure_alert = clean_df$exposure_alert,
        calculated_high_risk = clean_df$high_risk,
        calculated_needs_review = clean_df$clearance_status != "Cleared",
        calculated_risk_category = clean_df$risk_category_form,
        calculated_clearance_status = clean_df$clearance_status,
        calculated_country_departure_clean = clean_df$country_departure,
        calculated_countries_visited_clean = clean_df$countries_visited,
        calculated_followup_gap = clean_df$missing_followup_flag,
        calculated_incomplete_form = clean_df$incomplete_form_flag
      )
      
      bind_cols(alert_flags, raw_df) %>%
        slice(alert_index) %>%
        filter(!calculated_screening_id %in% cleared) %>%
        arrange(
          desc(calculated_awaiting_review),
          desc(calculated_poe_alert),
          desc(calculated_high_risk),
          desc(calculated_submission_time)
        )
    })
    
    filtered_redflag_details <- reactive({
      d <- full_redflag_details_unfiltered()
      
      if (nrow(d) == 0) {
        return(d)
      }
      
      # "Awaiting review" view: only the records counted in the caution bar
      if (identical(input$restricted_view %||% "review", "review")) {
        d <- d %>% filter(calculated_awaiting_review)
      }
      
      if (!is.null(input$restricted_date_filter) && length(input$restricted_date_filter) == 2) {
        d <- d %>%
          filter(
            calculated_submission_date >= input$restricted_date_filter[1],
            calculated_submission_date <= input$restricted_date_filter[2]
          )
      }
      
      if (!is.null(input$restricted_priority_filter) && length(input$restricted_priority_filter) > 0) {
        d <- d %>% filter(calculated_action_priority %in% input$restricted_priority_filter)
      }
      
      if (!is.null(input$restricted_alert_type_filter) && length(input$restricted_alert_type_filter) > 0) {
        selected <- input$restricted_alert_type_filter
        
        keep <- rep(FALSE, nrow(d))
        
        if ("Red flag" %in% selected) keep <- keep | d$calculated_red_flag
        if ("Exposure alert" %in% selected) keep <- keep | d$calculated_exposure_alert
        if ("Symptom alert" %in% selected) keep <- keep | d$calculated_symptom_alert
        if ("High risk" %in% selected) keep <- keep | d$calculated_high_risk
        if ("Requires review" %in% selected) keep <- keep | d$calculated_needs_review
        if ("Follow-up gap" %in% selected) keep <- keep | d$calculated_followup_gap
        if ("Incomplete critical record" %in% selected) keep <- keep | d$calculated_incomplete_form
        
        d <- d[keep, , drop = FALSE]
      }
      
      d
    })
    
    output$authorized_access_notice <- renderUI({
      approved <- normalize_email(authorized_emails)
      typed <- submitted_email()
      signed_in <- session_email()
      
      if (authorized_user()) {
        shown_email <- active_user_email()
        
        return(
          div(
            style = "
              background:#e8f7ee;
              border-left:8px solid #00a65a;
              border-radius:10px;
              padding:15px;
              font-weight:700;
              color:#14532d;
            ",
            icon("check-circle"),
            paste0(
              " Access granted for: ",
              shown_email,
              ". Full red-flag visitor details are visible below for authorized public health follow-up only."
            )
          )
        )
      }
      
      warning_text <- NULL
      
      if (isTRUE(login_attempted()) && nzchar(typed) && !(typed %in% approved)) {
        warning_text <- div(
          style = "margin-top:10px;color:#b91c1c;font-weight:800;",
          icon("times-circle"),
          " Email not authorized. Please use an approved health officer email."
        )
      }
      
      detected_text <- if (nzchar(signed_in)) {
        paste0("Authenticated deployment email detected: ", signed_in)
      } else {
        "No authenticated deployment email detected. Use the email access box below."
      }
      
      div(
        style = "
          background:#fff3f0;
          border-left:8px solid #dd4b39;
          border-radius:10px;
          padding:15px;
          color:#7f1d1d;
        ",
        div(
          style = "font-weight:800;margin-bottom:10px;",
          icon("lock"),
          " Restricted visitor/person details. Only approved health officers can unlock full alert records."
        ),
        div(style = "font-size:13px;margin-bottom:10px;", detected_text),
        fluidRow(
          column(
            width = 8,
            textInput(
              inputId = session$ns("restricted_email"),
              label = "Health officer email",
              value = "",
              placeholder = "Enter approved health officer email"
            )
          ),
          column(
            width = 4,
            br(),
            actionButton(
              inputId = session$ns("unlock_redflag_details"),
              label = "Unlock restricted details",
              icon = icon("unlock"),
              class = "btn-danger",
              width = "100%"
            ),
            br(),
            br(),
            actionButton(
              inputId = session$ns("clear_restricted_login"),
              label = "Clear",
              icon = icon("eraser"),
              class = "btn-default",
              width = "100%"
            )
          )
        ),
        warning_text,
        tags$span(
          style = "font-weight:500;",
          "The public dashboard remains limited to aggregated, non-identifiable public health indicators."
        )
      )
    })
    
    output$restricted_redflag_details_ui <- renderUI({
      if (!authorized_user()) {
        return(NULL)
      }
      
      d <- full_redflag_details_unfiltered()
      
      date_min <- if (nrow(d) == 0 || !"calculated_submission_date" %in% names(d) || all(is.na(d$calculated_submission_date))) {
        Sys.Date()
      } else {
        min(d$calculated_submission_date, na.rm = TRUE)
      }
      
      date_max <- if (nrow(d) == 0 || !"calculated_submission_date" %in% names(d) || all(is.na(d$calculated_submission_date))) {
        Sys.Date()
      } else {
        max(d$calculated_submission_date, na.rm = TRUE)
      }
      
      priority_choices <- if ("calculated_action_priority" %in% names(d)) {
        sort(unique(d$calculated_action_priority))
      } else {
        character()
      }
      priority_choices <- priority_choices[!is.na(priority_choices) & priority_choices != ""]
      if (length(priority_choices) == 0) priority_choices <- c("Immediate action", "Enhanced monitoring")
      
      tagList(
        fluidRow(
          box(
            title = tagList(icon("filter"), "Restricted Details Filters"),
            width = 12,
            status = "warning",
            solidHeader = TRUE,
            fluidRow(
              column(
                width = 12,
                radioGroupButtons(
                  inputId = session$ns("restricted_view"),
                  label = "Show",
                  choices = c(
                    "Awaiting review (matches the caution bar)" = "review",
                    "All flagged records (incl. follow-up gaps)" = "all"
                  ),
                  selected = "review",
                  status = "danger"
                )
              )
            ),
            fluidRow(
              column(
                width = 4,
                dateRangeInput(
                  inputId = session$ns("restricted_date_filter"),
                  label = "Submission date range",
                  start = date_min,
                  end = date_max,
                  min = date_min,
                  max = date_max
                )
              ),
              column(
                width = 4,
                pickerInput(
                  inputId = session$ns("restricted_priority_filter"),
                  label = "Action priority",
                  choices = priority_choices,
                  selected = priority_choices,
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE)
                )
              ),
              column(
                width = 4,
                pickerInput(
                  inputId = session$ns("restricted_alert_type_filter"),
                  label = "Alert type",
                  choices = c(
                    "Red flag",
                    "Exposure alert",
                    "Symptom alert",
                    "High risk",
                    "Requires review",
                    "Follow-up gap",
                    "Incomplete critical record"
                  ),
                  selected = c(
                    "Red flag",
                    "Exposure alert",
                    "Symptom alert",
                    "High risk",
                    "Requires review",
                    "Follow-up gap",
                    "Incomplete critical record"
                  ),
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE)
                )
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = tagList(icon("users"), "Main Surveillance Details for Alert Records"),
            width = 12,
            status = "danger",
            solidHeader = TRUE,
            p(
              style = "font-weight:700;color:#7f1d1d;",
              "Restricted view: contains only the main surveillance and follow-up fields needed for public health action. Metadata and technical Kobo fields are hidden."
            ),
            div(
              style = "
                background:#f8fafc;
                border-left:6px solid #003b6f;
                border-radius:10px;
                padding:12px;
                margin-bottom:12px;
              ",
              tags$strong("Clear followed-up alerts"),
              tags$p(
                style = "margin-top:6px;margin-bottom:8px;",
                "Tick one or more records, classify the review outcome, add a follow-up note, then save the review. Reviewed records will leave the active alert queue, but they will remain counted under total screened travelers and retained in the clearance history."
              ),
              textAreaInput(
                inputId = session$ns("clearance_note"),
                label = "Follow-up / clearance note",
                placeholder = "Example: Does not meet EVD case definition; Port Health verified traveler; host contact confirmed; surveillance focal person notified.",
                width = "100%",
                height = "80px"
              ),
              fluidRow(
                column(
                  width = 4,
                  selectInput(
                    inputId = session$ns("clearance_action"),
                    label = "Review outcome",
                    choices = c(
                      "Reviewed - discarded / does not meet EVD alert definition",
                      "Reviewed - follow-up required",
                      "Reviewed - referred for clinical/public health action",
                      "Duplicate record - retained for audit only"
                    ),
                    selected = "Reviewed - discarded / does not meet EVD alert definition"
                  )
                ),
                column(
                  width = 4,
                  radioButtons(
                    inputId = session$ns("case_definition_met"),
                    label = "Meets EVD alert/case definition?",
                    choices = c("No", "Yes", "Unclear"),
                    selected = "No",
                    inline = TRUE
                  )
                ),
                column(
                  width = 4,
                  radioButtons(
                    inputId = session$ns("followup_required_clearance"),
                    label = "Follow-up still required?",
                    choices = c("No", "Yes"),
                    selected = "No",
                    inline = TRUE
                  )
                )
              ),
              actionButton(
                inputId = session$ns("clear_selected_alerts"),
                label = "Save review and remove from active alert queue",
                icon = icon("check-circle"),
                class = "btn-success"
              ),
              tags$span(" "),
              downloadButton(
                outputId = session$ns("download_full_redflag_details"),
                label = "Download active alert details CSV",
                class = "btn-danger"
              )
            ),
            uiOutput(session$ns("review_count_note")),
            DTOutput(session$ns("restricted_redflag_table")),
            style = "overflow-x:auto;"
          )
        ),
        fluidRow(
          box(
            title = tagList(icon("history"), "Follow-up / Clearance History"),
            width = 12,
            status = "success",
            solidHeader = TRUE,
            DTOutput(session$ns("clearance_history_table")),
            style = "overflow-x:auto;"
          )
        )
      )
    })
    
    output$vb_restricted_alert_records <- renderValueBox({
      req(authorized_user())
      d <- filtered_redflag_details()
      
      valueBox(
        comma(nrow(d)),
        "Restricted: Red-Flag Records",
        icon = icon("flag"),
        color = "red"
      )
    })
    
    output$vb_restricted_immediate_records <- renderValueBox({
      req(authorized_user())
      d <- filtered_redflag_details()
      
      n <- if ("calculated_action_priority" %in% names(d)) {
        sum(d$calculated_action_priority == "Immediate action", na.rm = TRUE)
      } else {
        0
      }
      
      valueBox(
        comma(n),
        "Restricted: Immediate Action",
        icon = icon("exclamation-triangle"),
        color = "red"
      )
    })
    
    output$vb_restricted_exposure_records <- renderValueBox({
      req(authorized_user())
      d <- filtered_redflag_details()
      
      n <- if ("calculated_exposure_alert" %in% names(d)) {
        sum(d$calculated_exposure_alert, na.rm = TRUE)
      } else {
        0
      }
      
      valueBox(
        comma(n),
        "Restricted: Exposure Alerts",
        icon = icon("user-md"),
        color = "yellow"
      )
    })
    
    output$vb_restricted_symptom_records <- renderValueBox({
      req(authorized_user())
      d <- filtered_redflag_details()
      
      n <- if ("calculated_symptom_alert" %in% names(d)) {
        sum(d$calculated_symptom_alert, na.rm = TRUE)
      } else {
        0
      }
      
      valueBox(
        comma(n),
        "Restricted: Symptom Alerts",
        icon = icon("stethoscope"),
        color = "yellow"
      )
    })
    
    output$review_count_note <- renderUI({
      req(authorized_user())
      d <- full_redflag_details_unfiltered()
      n <- if (nrow(d) == 0) 0 else sum(d$calculated_awaiting_review)
      div(
        style = "background:#fff3f0;border-left:6px solid #dd4b39;border-radius:6px;padding:10px;margin-bottom:10px;",
        icon("clipboard-list"), " ",
        tags$b(n), if (n == 1) " traveller" else " travellers",
        " awaiting review (PoE alert or classified high-risk, not yet cleared). ",
        "This matches the caution bar on the JIA Screening Detail tab."
      )
    })
    
    output$restricted_redflag_table <- renderDT({
      req(authorized_user())
      raw_d <- filtered_redflag_details()
      d <- make_main_surveillance_columns(raw_d)
      
      if (nrow(d) == 0) {
        return(
          datatable(
            tibble(Message = "No active red-flag visitor records currently available for the selected filters."),
            rownames = FALSE,
            options = list(dom = "t")
          )
        )
      }
      
      if (!"calculated_screening_id" %in% names(raw_d)) {
        return(
          datatable(
            tibble(Message = "No screening ID found for selected alert records."),
            rownames = FALSE,
            options = list(dom = "t")
          )
        )
      }
      
      d <- d %>%
        mutate(
          `Clear Followed Up` = paste0(
            '<input type="checkbox" class="clear-alert-checkbox" value="',
            raw_d$calculated_screening_id,
            '">'
          )
        ) %>%
        relocate(`Clear Followed Up`, .before = 1)
      
      datatable(
        d,
        rownames = FALSE,
        escape = FALSE,
        filter = "top",
        extensions = c("Buttons", "Scroller"),
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          scrollY = "650px",
          scroller = TRUE,
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel"),
          searchHighlight = TRUE,
          columnDefs = list(
            list(targets = 0, orderable = FALSE, searchable = FALSE)
          )
        ),
        callback = JS(
          sprintf(
            "
            table.on('change', '.clear-alert-checkbox', function() {
              var selected = [];
              $('.clear-alert-checkbox:checked').each(function() {
                selected.push($(this).val());
              });
              Shiny.setInputValue('%s', selected, {priority: 'event'});
            });
            ",
            session$ns("selected_alert_ids")
          )
        ),
        class = "stripe hover compact"
      )
    })
    
    observeEvent(input$clear_selected_alerts, {
      req(authorized_user())
      
      selected_ids <- input$selected_alert_ids
      
      if (is.null(selected_ids) || length(selected_ids) == 0) {
        showNotification(
          "Please tick at least one alert record before clearing.",
          type = "warning",
          duration = 5
        )
        return()
      }
      
      if (is.null(mark_alerts_cleared)) {
        showNotification(
          "Clearance function is not connected.",
          type = "error",
          duration = 6
        )
        return()
      }
      
      user_email <- active_user_email()
      
      if (!nzchar(user_email)) {
        user_email <- session_email()
      }
      
      mark_alerts_cleared(
        screening_ids = selected_ids,
        cleared_by = user_email,
        note = input$clearance_note %||% "",
        clearance_action = input$clearance_action %||% "Reviewed - discarded / does not meet EVD alert definition",
        case_definition_met = input$case_definition_met %||% "No",
        followup_required = input$followup_required_clearance %||% "No"
      )
      
      showNotification(
        paste0(length(selected_ids), " alert record(s) reviewed and removed from the active alert queue."),
        type = "message",
        duration = 6
      )
    })
    
    output$clearance_history_table <- renderDT({
      req(authorized_user())
      
      d <- clearance_log()
      
      if (is.null(d) || nrow(d) == 0) {
        return(
          datatable(
            tibble(Message = "No alerts have been cleared yet."),
            rownames = FALSE,
            options = list(dom = "t")
          )
        )
      }
      
      datatable(
        d %>% arrange(desc(cleared_at)),
        rownames = FALSE,
        filter = "top",
        options = list(
          pageLength = 10,
          scrollX = TRUE
        ),
        class = "stripe hover compact"
      )
    })
    
    output$download_full_redflag_details <- downloadHandler(
      filename = function() {
        paste0(
          "restricted_filtered_redflag_visitor_details_",
          format(Sys.time(), "%Y%m%d_%H%M%S"),
          ".csv"
        )
      },
      content = function(file) {
        req(authorized_user())
        d <- make_main_surveillance_columns(filtered_redflag_details())
        readr::write_csv(d, file)
      }
    )
  })
}