# ============================================================
# PREPARE MAP LAYERS (run once, not part of the app)
# Reads the shapefiles, simplifies them for web display and saves
# light .rds files that the dashboard loads at startup.
# Re-run only if the boundaries change.
#
# Run from the EVD DASHBOARD folder:  source("prepare_map_layers.R")
# ============================================================

library(sf)
library(dplyr)

# ---- Paths (edit if your folders are named differently) ----
STATES_SHP   <- "data/SSD_Boundaries/states_admin_ssd.shp"
COUNTIES_SHP <- "data/SSD_Boundaries/county_ssd.shp"
DRC_HZ_SHP   <- "data/rdc_zones-de-sante/RDC_Zones de santé.shp"
OUT_DIR      <- "data"

DRC_BORDER_KM   <- 25    # keep DRC health zones within this distance of South Sudan
SIMPLIFY_METRES <- 300   # larger = lighter map, less boundary detail

dir.create(OUT_DIR, showWarnings = FALSE)

# Read, fix invalid shapes, simplify (in metres, UTM 36N), return to lat/long
prep_layer <- function(path) {
  x <- st_read(path, quiet = TRUE) %>%
    st_make_valid() %>%
    st_transform(32636) %>%
    st_simplify(dTolerance = SIMPLIFY_METRES, preserveTopology = TRUE)
  x
}

states   <- prep_layer(STATES_SHP)
counties <- prep_layer(COUNTIES_SHP)
drc      <- prep_layer(DRC_HZ_SHP)

# DRC health zones that border (or are within DRC_BORDER_KM of) South Sudan
ssd_outline <- st_union(states)
near <- lengths(st_is_within_distance(drc, ssd_outline, dist = DRC_BORDER_KM * 1000)) > 0
drc_border <- drc[near, ]

# Save in lat/long for leaflet
saveRDS(st_transform(states,     4326), file.path(OUT_DIR, "ssd_states.rds"))
saveRDS(st_transform(counties,   4326), file.path(OUT_DIR, "ssd_counties.rds"))
saveRDS(st_transform(drc_border, 4326), file.path(OUT_DIR, "drc_border_health_zones.rds"))

# ---- Summary: check the names columns to use as map labels ----
cat("States:  ", nrow(states),   "shapes | columns:", paste(names(states),   collapse = ", "), "\n")
cat("Counties:", nrow(counties), "shapes | columns:", paste(names(counties), collapse = ", "), "\n")
cat("DRC health zones near the border:", nrow(drc_border), "of", nrow(drc),
    "| columns:", paste(names(drc_border), collapse = ", "), "\n")
cat("Saved to", normalizePath(OUT_DIR), "\n")
