# ============================================================
# STEP 2 - ALERTS-PER-COUNTY MAP IN A SHINY DASHBOARD
# ------------------------------------------------------------
# This is a complete, working mini-dashboard. Run it as it is first
# (it makes up some sample alerts), check the map appears, then copy
# the parts into your own app.
#
# Before running: do STEP 1 (1_prepare_county_map.R) so that the
# "data" folder has ssd_states.rds and ssd_counties.rds.
#
# The file is in three parts, the same three places the code goes
# in any Shiny app:
#
#   PART A  - TOP OF THE APP  (libraries, map layers, colours, your data)
#   PART B  - UI              (where the map sits on the page)
#   PART C  - SERVER          (the code that draws and updates the map)
#
# Look for the lines marked  >>> CHANGE  for the bits you'll edit.
# ============================================================



# ############################################################
# PART A - TOP OF THE APP
# Goes at the top of app.R (or in global.R), OUTSIDE ui and server.
# ############################################################

# install.packages(c("shiny", "shinydashboard", "leaflet", "sf", "dplyr", "htmltools"))  # first time only
library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(dplyr)
library(htmltools)

# ---- A1. Load the map layers made in STEP 1 ----
MAP_STATES   <- readRDS("data/ssd_states.rds")     # state border lines
MAP_COUNTIES <- readRDS("data/ssd_counties.rds")   # county shapes: columns county, state

# ---- A2. Matching county names ----
# Your alert data and the shapefile may spell counties slightly differently
# ("Juba" vs "JUBA" vs "Juba County "). This turns every name into a simple
# key (lower case, letters only) so they still match.
county_key <- function(x) gsub("[^a-z]", "", tolower(trimws(x)))
MAP_COUNTIES$key <- county_key(MAP_COUNTIES$County)

# For names that are genuinely different (not just capitals/spaces),
# add them here:  "name in your data" = "name in the shapefile".
COUNTY_RENAMES <- c(
  # "Kajo Keji" = "Kajo-keji",        # >>> CHANGE - add any that don't match
  # "Panyijar"  = "Panyijiar"
)

fix_county_names <- function(x) {
  hit <- x %in% names(COUNTY_RENAMES)
  x[hit] <- COUNTY_RENAMES[x[hit]]
  x
}

# ---- A3. Colours for the number of alerts ----
# Each county gets the colour of the group its alert count falls into.
#   0 | 1-2 | 3-5 | 6-10 | 11-20 | 21+
ALERT_BREAKS  <- c(0, 1, 3, 6, 11, 21)                           # >>> CHANGE - start of each group
ALERT_LABELS  <- c("0", "1–2", "3–5", "6–10", "11–20", "21+")    # >>> CHANGE - must match the breaks
ALERT_COLOURS <- c("#f7f7f7", "#fee5d9", "#fcae91", "#fb6a4a", "#de2d26", "#a50f15")
alert_colour  <- function(n) ALERT_COLOURS[findInterval(n, ALERT_BREAKS)]

# ---- A4. Your alert data ----
# The map needs ONE ROW PER ALERT with at least:
#   alert_date  - a Date
#   county      - the county name
#
# >>> CHANGE - delete the sample data below and load your own instead, e.g.
#   alerts <- readxl::read_excel("alerts.xlsx") %>%
#     transmute(alert_date = as.Date(date_of_alert), county = county_name)
set.seed(1)
alerts <- tibble(
  alert_id   = 1:300,
  alert_date = Sys.Date() - sample(0:89, 300, replace = TRUE),
  county     = sample(sample(MAP_COUNTIES$County, 30), 300, replace = TRUE)
)

alerts <- alerts %>% mutate(county = fix_county_names(county),
                            key    = county_key(county))



# ############################################################
# PART B - UI
# The fluidRow()s below go inside dashboardBody(), wherever you want
# the map on the page (e.g. inside a tabPanel()).
# ############################################################

ui <- dashboardPage(
  dashboardHeader(title = "Alerts by county"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    
    # ---- B1. Filters (copy these if you don't already have them) ----
    fluidRow(
      box(
        width = 12,
        column(4, dateRangeInput("map_dates", "Alert date",
                                 start = Sys.Date() - 29, end = Sys.Date(),
                                 format = "dd-M-yyyy")),
        column(4, selectInput("map_state", "State",
                              choices = c("All states", sort(unique(MAP_COUNTIES$state)))))
      )
    ),
    
    # ---- B2. The map ----
    fluidRow(
      box(
        title = tagList(icon("map"), "Alerts by county"),
        width = 12, status = "primary", solidHeader = TRUE,
        leafletOutput("alert_map", height = "600px"),   # >>> CHANGE height if you like
        uiOutput("map_note")
      )
    )
  )
)



# ############################################################
# PART C - SERVER
# Everything between the lines goes INSIDE your
#   server <- function(input, output, session) { ... }
# ############################################################

server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # C1. Count alerts per county for the chosen dates
  # ----------------------------------------------------------
  alerts_in_period <- reactive({
    req(input$map_dates)
    alerts %>%
      filter(alert_date >= input$map_dates[1],
             alert_date <= input$map_dates[2])
  })
  
  county_counts <- reactive({
    alerts_in_period() %>% count(key, name = "alerts")
  })
  
  # Join the counts onto the county shapes. Counties with no alerts get 0.
  map_data <- reactive({
    d <- MAP_COUNTIES %>%
      left_join(county_counts(), by = "key") %>%
      mutate(alerts = coalesce(alerts, 0L))
    if (input$map_state != "All states") d <- filter(d, state == input$map_state)
    d
  })
  
  # ----------------------------------------------------------
  # C2. The base map: drawn ONCE (background, state lines, legend)
  # ----------------------------------------------------------
  output$alert_map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 5)) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%   # if grey/blank, use addTiles() instead
      setView(lng = 30.5, lat = 7.3, zoom = 6) %>%
      
      # "Panes" fix the drawing order: counties underneath, state lines on top
      addMapPane("counties_pane", zIndex = 380) %>%
      addMapPane("states_pane",   zIndex = 390) %>%
      
      addPolylines(
        data = sf::st_boundary(MAP_STATES),
        color = "#003b6f", weight = 1.8, opacity = 0.8,
        options = pathOptions(pane = "states_pane", interactive = FALSE)
      ) %>%
      
      addLegend(
        position = "bottomright", title = "Alerts",
        colors = ALERT_COLOURS, labels = ALERT_LABELS, opacity = 0.9
      )
  })
  
  # The map only exists once the page has drawn it. This waits for that,
  # so the counties are never "drawn" onto a map that isn't there yet
  # (matters if the map is on a tab that isn't open when the app starts).
  map_ready <- reactiveVal(FALSE)
  observeEvent(input$alert_map_zoom, map_ready(TRUE), once = TRUE)
  
  # ----------------------------------------------------------
  # C3. Colour the counties: redrawn whenever the filters change,
  #     WITHOUT resetting the zoom (that's what leafletProxy does)
  # ----------------------------------------------------------
  observe({
    req(map_ready())
    d <- map_data()
    
    hover <- sprintf("<b>%s</b><br>%s<br>Alerts: %d", d$county, d$state, d$alerts)
    
    leafletProxy("alert_map") %>%
      clearGroup("Counties") %>%
      addPolygons(
        data = d, group = "Counties",
        fillColor = alert_colour(d$alerts), fillOpacity = 0.8,
        color = "#ffffff", weight = 0.8,                      # county outline
        label = lapply(hover, htmltools::HTML),
        labelOptions = labelOptions(textsize = "12px", direction = "auto"),
        highlightOptions = highlightOptions(weight = 2.5, color = "#333333"),
        options = pathOptions(pane = "counties_pane")
      )
  })
  
  # ----------------------------------------------------------
  # C4. Zoom to the chosen state (optional - delete if not wanted)
  # ----------------------------------------------------------
  observeEvent(list(input$map_state, map_ready()), {
    req(map_ready())
    proxy <- leafletProxy("alert_map")
    if (input$map_state == "All states") {
      proxy %>% setView(lng = 30.5, lat = 7.3, zoom = 6)
    } else {
      bb <- sf::st_bbox(filter(MAP_COUNTIES, state == input$map_state))
      proxy %>% fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
    }
  }, ignoreInit = TRUE)
  
  # ----------------------------------------------------------
  # C5. Note under the map: alerts whose county isn't on the map
  #     (blank, misspelt, or needs adding to COUNTY_RENAMES in PART A)
  # ----------------------------------------------------------
  output$map_note <- renderUI({
    a <- alerts_in_period()
    not_found <- a %>%
      filter(is.na(county) | !key %in% MAP_COUNTIES$key) %>%
      count(county = coalesce(county, "(blank)"))
    
    div(
      style = "font-size: 12px; color: #555; margin-top: 6px;",
      paste0(format(nrow(a), big.mark = ","), " alerts in the selected dates. ",
             "Hover over a county for the number of alerts."),
      if (nrow(not_found) > 0) div(
        style = "color: #b9770e; margin-top: 4px;",
        icon("exclamation-triangle"),
        paste0(" ", sum(not_found$n), " alert(s) not on the map – county not recognised: ",
               paste0(not_found$county, " (", not_found$n, ")", collapse = ", "))
      )
    )
  })
  
}  # end of server



# ############################################################
# Run the app (keep this as the last line of app.R)
# ############################################################
shinyApp(ui, server)
