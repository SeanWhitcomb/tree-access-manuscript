library(sf)
library(tmap)
library(osrm)
library(tidyverse)

# Build osrm foot profile ======================================

# Start Docker
# Start terminal

# docker run -t -i -v C:\osrm:/data osrm/osrm-backend:v5.25.0 osrm-extract -p /opt/foot.lua /data/arizona-260326.osm.pbf

# docker run -t -i -v C:\osrm:/data osrm/osrm-backend:v5.25.0 osrm-partition /data/arizona-260326.osrm

# docker run -t -i -v C:\osrm:/data osrm/osrm-backend:v5.25.0 osrm-customize /data/arizona-260326.osrm

# Build osrm car profile ======================================

# Start Docker
# Start terminal

# docker run -t -i -v C:\osrm:/data osrm/osrm-backend:v5.25.0 osrm-extract -p /opt/car.lua /data/arizona-260326.osm.pbf

# docker run -t -i -v C:\osrm:/data osrm/osrm-backend:v5.25.0 osrm-partition /data/arizona-260326.osrm

# docker run -t -i -v C:\osrm:/data osrm/osrm-backend:v5.25.0 osrm-customize /data/arizona-260326.osrm

# Run osrm server ===================================================

# docker run -t -i -p 5000:5000 -v C:\osrm:/data osrm/osrm-backend:v5.25.0 osrm-routed --algorithm mld /data/arizona-260326.osrm

# Set to local OSRM server

options(osrm.server = "http://localhost:5000/")
options(osrm.profile = "car")

# Load distance matrix that was previously created

# D <- readRDS("park_bg_dist_matrix.rds")

# Read in 3-mi and 5-mi park buffers

buffer_park_5mi <- st_read("phx_park_tree_access.gpkg", layer = "buffer_all_5mi")

tm_shape(buffer_park_5mi) +
  tm_polygons()

# Read in Maricopa block group polygons

maricopa_bg_poly <- st_read("phx_park_tree_access.gpkg", layer = "maricopa_bg_poly")

tm_shape(maricopa_bg_poly) +
  tm_polygons(fill = "lightblue") + 
  tm_shape(buffer_park_5mi) +
  tm_polygons()

# Read in Maricopa block group pop-weighted points

maricopa_bg_pw <- st_read("phx_park_tree_access.gpkg", layer = "maricopa_bg_pw")

tm_shape(buffer_park_5mi) +
  tm_polygons(fill = "lightblue") +
  tm_shape(maricopa_bg_pw) +
  tm_dots()

# Filter Maricopa block groups to only those within the park buffer

park_bg_pw <- st_filter(maricopa_bg_pw, buffer_park_5mi, .predicate = st_within)

tm_shape(buffer_park_5mi) +
  tm_polygons(fill = "lightblue") +
  tm_shape(park_bg_pw) +
  tm_dots()

# Write park block groups to new layer

# st_write(park_bg_pw, "phx_park_tree_access.gpkg", layer = "park_bg_pw")
park_bg_pw <- st_read("phx_park_tree_access.gpkg", layer = "park_bg_pw")

# Map parks, block group centroids, and buffer
tm_shape(buffer_park_5mi) +
  tm_polygons(fill = "lightblue") +
  tm_shape(park_bg_pw) +
  tm_dots() +
  tm_shape(parks) +
  tm_bubbles(fill = "green", size = 1)

# Read in park entrances and transform to 4326

park_entrances <- st_read("phx_park_tree_access.gpkg", layer = "park_entrances_new")

# Build distance matrix ===========================================

# 1. Prepare coordinates ==============================================

ent_coords <- st_coordinates(park_entrances)

ent_df <- data.frame(
    lon = ent_coords[, 1],
    lat = ent_coords[, 2],
    row.names = paste0("ent_", seq_len(nrow(park_entrances)))  # assign unique id values to each entrance
)

bg_coords <- st_coordinates(park_bg_pw)

bg_df <- data.frame(
    lon = bg_coords[, 1],
    lat = bg_coords[, 2],
    row.names = as.character(park_bg_pw$GEOID)
)

# Create park_id lookup table to align with unique entrance ids
entrance_park_ids <- as.character(park_entrances$park_id)

# 2. Query OSRM: sources = block groups, destinations = park entrances =========================
# Result rows = block groups, cols = individual entrances

# Split into batches to not exceed limit

batch_size <- 3

bg_ids <- rownames(bg_df)
batches <- split(bg_ids, ceiling(seq_along(bg_ids) / batch_size))

# Create a raw list of distances in small batches

raw_list <- vector("list", length(batches))

for (i in seq_along(batches)) {
  cat("Batch", i, "of", length(batches), "\n")
  Sys.sleep(1)
  tryCatch({
    raw_list[[i]] <- osrmTable(
      src     = bg_df[batches[[i]], , drop = FALSE],
      dst     = ent_df,
      measure = "distance"
    )$distances
  }, error = function(e) {
    cat("Error at batch", i, ":", conditionMessage(e), "\n")
  })
}

raw <- do.call(rbind, raw_list) # raw list of distances from all parks to all entrances

# Remove block groups that return infinite distance due to isolation from parks

raw <- raw[!rownames(raw) %in% c("040130611001", "040130611002", "040130611003"), ]
park_bg_pw <- park_bg_pw[!park_bg_pw$GEOID %in% c("040130611001", "040130611002", "040130611003"), ]

# Build block group to park distance matrix

D_bg_park <- do.call(cbind, lapply(unique(entrance_park_ids), function(pid) {
  cols <- raw[, entrance_park_ids == pid, drop = F]
  apply(cols, 1, min, na.rm = T)
}))

colnames(D_bg_park) <- unique(entrance_park_ids) 
# rownames are already inherited from raw GEOIDs

# transpose dist matrix so park_id is the supply and GEOID is the demand

D <- t(D_bg_park)

# reorder D rows to match the order of parks$park_id

D <- D[as.character(parks$park_id), ]

# Save distance matrix

## saveRDS(D, "park_bg_dist_matrix_drive.rds")

