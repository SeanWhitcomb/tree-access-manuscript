install.packages("bispdep")
install.packages("spam")
install.packages("rgeoda")


library(sf)
library(mapview)
library(spdep)
library(tmap)
library(tidyverse)
library(bispdep)
library(spam)
library(rgeoda)

# Save RDS files
saveRDS(parks, "parks.rds")
saveRDS(phx_bg, "phx_bg.rds")
saveRDS(phx_bg_all, here::here("phx_bg_all.rds"))

# Write objects to geopackage layers
st_write(parks, "phx_park_tree_access.gpkg", layer = "parks", delete_layer = T)
st_write(phx_bg, "phx_park_tree_access.gpkg", layer = "phx_bg", delete_layer = T)

# Read in parks and bg polygons
parks <- readRDS("parks.rds")
phx_bg <- readRDS("phx_bg.rds")

## Prepare phx_bg object ======================================

# Read in Phoenix block group land cover data and filter to only land cover columns
# land_cover <- st_read("phx_park_tree_access.gpkg", layer = "bg_land_cover_wo_parks")
# land_cover <- land_cover |> select(geoid, land_cover_0:pixel_cover)

# Join land cover data to block group polygons
# phx_bg <- left_join(phx_bg, st_drop_geometry(land_cover), by = "geoid")

# Calculate tree canopy and green cover percent per block group (without parks)
# phx_bg$pct_tree <- 
#   (phx_bg$land_cover_1 / (phx_bg$land_cover_1 + phx_bg$land_cover_2 +
#     phx_bg$land_cover_3 + phx_bg$land_cover_4 + phx_bg$land_cover_5 +
#     phx_bg$land_cover_6 + phx_bg$land_cover_7)) * 100

# phx_bg$pct_green <- 
#   ((phx_bg$land_cover_1 + phx_bg$land_cover_2)/ (phx_bg$land_cover_1 + phx_bg$land_cover_2 +
#     phx_bg$land_cover_3 + phx_bg$land_cover_4 + phx_bg$land_cover_5 +
#     phx_bg$land_cover_6 + phx_bg$land_cover_7)) * 100

# Convert demographic columns to percents
# phx_bg$pct_white <- phx_bg$pct_white * 100
# phx_bg$pct_black <- phx_bg$pct_black * 100
# phx_bg$pct_hispanic <- phx_bg$pct_hispanic * 100
# phx_bg$pct_nonwhite <- 100 - phx_bg$pct_white

# Remove block groups with NA land cover columns
# phx_bg <- phx_bg |> 
#   filter(!is.na(pct_tree))

# phx_bg <- phx_bg |> 
#   filter(geoid != "040136130002")
	
## Map tree cover per block group and park access ========================
tmap_mode("view")

tree_map <- tm_shape(phx_bg) +
  tm_polygons(fill = "pct_tree",
              fill.scale = tm_scale_intervals(style = "quantile", values = "brewer.greens")) +
  tm_layout(legend.outside = T)

park_map <- tm_shape(phx_bg) +
  tm_polygons(fill = "tree_access",
              fill.scale = tm_scale_intervals(style = "quantile", values = "brewer.blues")) +
  tm_layout(legend.outside = T)

adi_map <- tm_shape(phx_bg) +
  tm_polygons(fill = "adi",
              fill.scale = tm_scale_intervals(style = "quantile", values = "brewer.purples")) +
  tm_layout(legend.outside = T)

hisp_map <- tm_shape(phx_bg) +
  tm_polygons(fill = "pct_hispanic",
              fill.scale = tm_scale_intervals(style = "quantile", values = "brewer.reds")) +
  tm_layout(legend.outside = T)

black_map <- tm_shape(phx_bg) +
  tm_polygons(fill = "pct_black",
              fill.scale = tm_scale_intervals(style = "quantile", values = "brewer.oranges")) +
  tm_layout(legend.outside = T)

tmap_arrange(tree_map, park_map, adi_map, hisp_map, black_map, ncol = 2)

# # Find null block groups
# bg_null <- phx_bg |> 
#   filter(geoid == c("040131147051", "040139810001"))

# tm_shape(bg_null) +
#   tm_borders() +
#   tm_basemap("OpenStreetMap")

# # Remove null block groups (prisons)
# phx_bg <- phx_bg |> 
#   filter(!geoid %in% c("040131147051", "040139810001"))

# Read in population-weighted block groups and filter to match phx_bg
phx_bg_pw <- st_read("phx_park_tree_access.gpkg", layer = "maricopa_bg_pw") |> 
  filter(GEOID %in% phx_bg$geoid)

# Create neighborhood matrix

nb8 <- knn2nb(knearneigh(phx_bg_pw, k = 8))
nbw8 <- nb2listw(nb8, style = "W")

# Map neighbor connections
plot(st_geometry(phx_bg), border = "lightgray")
plot.nb(nb8, st_geometry(phx_bg_pw), add = T) 

plot(st_geometry(phx_bg), border = "lightgray")
plot.nb(nb5, st_geometry(phx_bg_pw), add = T) 

## Local Moran's I =====================================
# localmoran() function of spdep
# output: 
  # Ii: Local Moran's I for each area
  # E.Ii: Expected Local Moran's I
  # Var.Ii: Variance of Local Moran's I
  # Z.Ii: z-score
  # Pr(z...): p-value for hypothesis tested

# Calculate 2-sided local Moran's I for tree access
moran_access <- localmoran(phx_bg$tree_access, nbw8, alternative = "two.sided")
head(moran_access)

# Add tree access Moran's I values to phx_bg
phx_bg$access_I <- moran_access[ , 1]
phx_bg$access_z <- moran_access[ , 4]
phx_bg$access_p <- moran_access[ , 5]

# Identify clusters using a scatterplot
mp_access <- moran.plot(as.vector(scale(phx_bg$tree_access)), nbw8)

# create quadrants of each cluster type
phx_bg$access_cluster <- NA
phx_bg[(mp_access$x >= 0 & mp_access$wx >= 0) & (phx_bg$access_p <= 0.05), "access_cluster"] <- "HH"
phx_bg[(mp_access$x <= 0 & mp_access$wx <= 0) & (phx_bg$access_p <= 0.05), "access_cluster"] <- "LL"
phx_bg[(mp_access$x >= 0 & mp_access$wx <= 0) & (phx_bg$access_p <= 0.05), "access_cluster"] <- "HL"
phx_bg[(mp_access$x <= 0 & mp_access$wx >= 0) & (phx_bg$access_p <= 0.05), "access_cluster"] <- "LH"
phx_bg[(phx_bg$access_p > 0.05), "access_cluster"] <- "NS"


# Calculate 2-sided local Moran's I for canopy cover
moran_canopy <- localmoran(phx_bg$pct_tree, nbw8, alternative = "two.sided")
head(moran_canopy)

# Add block group tree canopy Moran's I values to phx_bg
phx_bg$canopy_I <- moran_access[ , 1]
phx_bg$canopy_z <- moran_access[ , 4]
phx_bg$canopy_p <- moran_access[ , 5]

# Identify clusters using a scatterplot
mp_canopy <- moran.plot(as.vector(scale(phx_bg$pct_tree)), nbw8)

# create quadrants of each cluster type
phx_bg$canopy_cluster <- NA
phx_bg[(mp_canopy$x >= 0 & mp_canopy$wx >= 0) & (phx_bg$canopy_p <= 0.05), "canopy_cluster"] <- "HH"
phx_bg[(mp_canopy$x <= 0 & mp_canopy$wx <= 0) & (phx_bg$canopy_p <= 0.05), "canopy_cluster"] <- "LL"
phx_bg[(mp_canopy$x >= 0 & mp_canopy$wx <= 0) & (phx_bg$canopy_p <= 0.05), "canopy_cluster"] <- "HL"
phx_bg[(mp_canopy$x <= 0 & mp_canopy$wx >= 0) & (phx_bg$canopy_p <= 0.05), "canopy_cluster"] <- "LH"
phx_bg[(phx_bg$canopy_p > 0.05), "canopy_cluster"] <- "NS"

# Map tree access clusters and canopy clusters
tmap_mode("view")

access_clusters <- tm_shape(phx_bg) +
  tm_polygons(fill = "access_cluster",
              fill.scale = tm_scale_ordinal(values = c("#FF0000", "#0000FF", "#f4ada8", "#a7adf9", "gray90"),
                                              labels = c("High-High", "Low-Low", "High-Low",
                                                        "Low-High", "Non-significant")),
              fill.legend = tm_legend(title = "Park tree access clusters", text.size = 1)) +
  tm_borders(fill_alpha = 0.5)

canopy_clusters <- tm_shape(phx_bg) +
  tm_polygons(fill = "canopy_cluster",
              fill.scale = tm_scale_ordinal(values = c("#FF0000", "#0000FF", "#f4ada8", "#a7adf9", "gray90"),
                                              labels = c("High-High", "Low-Low", "High-Low",
                                                        "Low-High", "Non-significant")),
              fill.legend = tm_legend(title = "Block group tree canopy clusters", text.size = 1)) +
  tm_borders(fill_alpha = 0.5)

tmap_arrange(access_clusters, canopy_clusters, ncol = 1)

## Classify block groups based on above/below median categories for pct_tree and tree_access
summary(phx_bg$pct_tree)
summary(phx_bg$tree_access)

phx_bg$canopy_access_type <- NA
phx_bg[(phx_bg$pct_tree >= median(phx_bg$pct_tree) & phx_bg$tree_access >= median(phx_bg$tree_access)), 
  "canopy_access_type"] <- "HH"
phx_bg[(phx_bg$pct_tree < median(phx_bg$pct_tree) & phx_bg$tree_access < median(phx_bg$tree_access)), 
  "canopy_access_type"] <- "LL"
phx_bg[(phx_bg$pct_tree >= median(phx_bg$pct_tree) & phx_bg$tree_access < median(phx_bg$tree_access)), 
  "canopy_access_type"] <- "HL"
phx_bg[(phx_bg$pct_tree < median(phx_bg$pct_tree) & phx_bg$tree_access >= median(phx_bg$tree_access)), 
  "canopy_access_type"] <- "LH"

tm_shape(phx_bg) +
  tm_polygons(fill = "canopy_access_type",
              fill.scale = tm_scale_ordinal(levels = c("HH", "LL", "HL", "LH"),
                                            values = c("#FF0000", "#0000FF", "#f4ada8", "#a7adf9"),
                                              labels = c("High-High", "Low-Low", "High-Low",
                                                        "Low-High")),
              fill.legend = tm_legend(title = "Block group tree canopy/park tree access", text.size = 1)) +
  tm_borders(fill_alpha = 0.5) + 
  tm_shape(parks) +
  tm_polygons(fill = "tree_area", col = "lightgreen",
              fill.scale = tm_scale_intervals(style = "quantile", values = "brewer.greens"))

# Summarize mean canopy area by canopy access type quadrant
parks_bg <- st_join(parks, phx_bg[ , c("geoid", "canopy_access_type")], join = st_within)

parks_bg <-  st_drop_geometry(parks_bg) |> 
  group_by(canopy_access_type) |> 
  summarize(mean_tree_area = mean(tree_area, na.rm = T), n_parks = n())
parks_bg

# Compare tree cover in bg with and without parks

phx_bg_all <- phx_bg_all |> 
  mutate(has_park = pct_tree_parks != pct_tree_wo_parks)

wilcox.test(pct_tree_parks ~ has_park, data = phx_bg_all)
