library(tmaptools)
library(sf)
library(tidyverse)

# Read in parks object

parks <- st_read("phx_park_tree_access.gpkg", layer = "parks_4326")

# Remove old land cover data from parks and reorder columns

# parks_old <- parks

# parks <- subset(parks, select = -c(tree_pct, tree_area_ha, green_space_pct, green_space_area_ha, amenity_score))
# parks <- parks |> select(park_id, name, park_type, area_ha, tree_area = tree_area_rc, 
#   green_space_area = green_space_area_rc, impervious_area = impervious_area_rc, tree_pct = tree_pct_rc, grass_pct = grass_pct_rc,
# impervious_pct = impervious_pct_rc, water_pct = water_pct_rc, land_cover_1, land_cover_2, land_cover_3, land_cover_4,
# land_cover_5, land_cover_6, land_cover_7)

# Write parks layer into geopackage

## st_write(parks, "phx_park_tree_access.gpkg", layer = "parks_4326", delete_layer = T)

# Ensure park IDs in distance matrix match parks list

park_ids <- parks$park_id

stopifnot(length(park_ids) == nrow(D))

park_types <- parks$park_type

# Set distance thresholds for each park type

thresholds <- c(
  Linear = 804.67,
  Pocket = 804.67,
  Neighborhood = 1609.34,
  Community = 4828.03,
  Regional = 8046.72
)

park_thresholds <- thresholds[park_types]

# Create Gaussian decay weight based on park type thresholds
# Wij = e(^(-1/2(D/threshold distance))^2)

W <- exp(-0.5 * (D / park_thresholds)^2)

# Compute the supply to demand ratio for each park using tree canopy as the supply factor

sd_ratio <- parks$tree_area / W %*% park_bg_pw$population

summary(sd_ratio)

# compute tree canopy access score for each block group (transpose W so block groups are rows)

bg_access <- t(W) %*% sd_ratio

summary(bg_access)

# Convert bg_access to a dataframe for joining to Maricopa block groups

bg_access_df <- data.frame(
  GEOID = rownames(bg_access),
  tree_access = bg_access[ ,1]
)

# Read in phx_bg object
phx_bg <- readRDS("phx_bg.rds")

# phx_bg <- st_read("phx_park_tree_access.gpkg", layer = "phx_bg")

# # join demographic data to Phoenix block group polygons

# phx_bg <- left_join(phx_bg, st_drop_geometry(maricopa_bg_pw), by = c("geoid" = "GEOID"))

# join bg tree access scores to Phoenix bg polygons

phx_bg <- left_join(phx_bg, bg_access_df, by = c("geoid" = "GEOID"))

phx_bg <- phx_bg |> 
  select(-tree_access) |> 
  left_join(bg_access_df, by = c("geoid" = "GEOID"))

# Remove block groups with <=10 residents

## phx_bg_backup <- phx_bg

# phx_bg <- phx_bg[phx_bg$population > 10, ]

# Write phx_bg to geopackage

# st_write(phx_bg, "phx_park_tree_access.gpkg", layer = "phx_bg", delete_layer = T)

# Map block groups by tree access score

tmap_mode("view")

tm_shape(phx_bg) +
  tm_polygons(fill = "tree_access", fill.scale = tm_scale_intervals(style = "quantile", values = "brewer.blues")) +
  tm_shape(parks) +
  tm_polygons(fill = "tree_area", col = "lightgreen", 
              fill.scale = tm_scale_intervals(style = "quantile", values = "brewer.greens"))

# Save parks and phx_bg objects

saveRDS(parks, "parks.rds")
saveRDS(phx_bg, "phx_bg.rds")

