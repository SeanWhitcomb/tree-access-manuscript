library(tidyverse)
library(tmap)
library(sf)
library(tigris)
library(spdep)
library(ggplot2)
library(cowplot)

## Map of block group data and park types ======================================

# Create park colors pallette
park_colors <- c("Pocket" = "#4daf4a",
                 "Linear" = "#377eb8",
                 "Neighborhood" = "#e41a1c",
                 "Community" = "#984ea3",
                 "Regional" = "#a65628")

# Create boundary objects for mapping
options(tigris_use_cache = TRUE)

us_states <- us_states |> 
  mutate(is_az = ifelse(NAME == "Arizona", "Arizona", "Other"))
az_state <- states(cb= TRUE) |> 
  filter(STUSPS == "AZ")
az_county <- counties(state = "AZ", cb = TRUE)
maricopa <- az_county |> 
  filter(NAME == "Maricopa")

# Change county and state labels
maricopa <- maricopa |> mutate(label = "Maricopa County")

# Create Arizona inset map
inset_az <- tm_shape(us_states) +
  tm_polygons(
    fill = "is_az",
    fill.scale = tm_scale_categorical(
      values = c("Arizona" = "lightblue", "Other" = "white")
    ),
    fill.legend = tm_legend(show = FALSE)
  ) +
  tm_layout(frame = TRUE, inner.margins = 0.1, frame.lwd = 0.3)

# Create Phoenix inset map
inset_phx <- tm_shape(az_state) +
  tm_polygons(fill = "lightblue", 
              col = "gray40", 
              lwd = 0.5) +
  tm_text("NAME", size = 1.2, ymod = 5) +
  tm_shape(phx_city_limits) +
  tm_polygons(fill = "darkblue",
              col = "darkblue") +
  tm_text(text = "Phoenix",
          size = 0.8, 
          xmod = 4, 
          ymod = 4) +
  tm_layout(frame = TRUE, inner.margins = 0.1, frame.lwd = 0.3)

# Create legend element
legend_bg_parks <- tm_shape(phx_bg_all) +
  tm_polygons(fill = "#fee08b") +
  tm_add_legend(
    title = "Block group data",
    type = "polygons",
    fill = c("#fee08b", "#fdae61", "gray90"),
    col = c("black", "black", "black"),
    labels = c("Land cover data only",
               "Land cover & demographic data",
               "Missing data"),
    frame = TRUE
  ) +
  tm_add_legend(
    title = "Park area (ha)",
    type = "bubbles",
    fill = "gray50",
    size = c(0.3, 0.6, 1, 1.2, 1.4),
    labels = c("< 10", "10 – 20", "20 – 40", "40 – 80", "80+")
  ) +
  tm_add_legend(
    title = "Park type",
    type = "bubbles",
    size = 1,
    fill = park_colors,
    col = c("black"),
    labels = c("Pocket", 
               "Linear", 
               "Neighborhood", 
               "Community", 
               "Regional"),
    frame = TRUE
  ) +
  tm_layout(legend.only = TRUE,
            legend.frame = TRUE,
            legend.frame.lwd = 1,
            legend.bg.col = "white")

# # Create map of block groups and parks
# map_phx <- tm_shape(phx_city_limits) +
#   tm_polygons(fill = "gray90", 
#               col = "black", 
#               lwd = 1) +
#   tm_shape(phx_bg_all) +
#   tm_polygons(fill = "#fee08b",
#               lwd = 0.3) +
#   tm_shape(phx_bg) +
#   tm_polygons(fill = "#fdae61") +
#   tm_shape(parks) +
#   tm_polygons("park_type",
#               fill.scale = tm_scale(values =c("Pocket" = "#4daf4a", 
#                                               "Linear" = "#377eb8", 
#                                               "Neighborhood" = "#e41a1c", 
#                                               "Community" = "#984ea3", 
#                                               "Regional" = "#a65628")
#               ),
#               lwd = 0.8
#   ) +
#   tm_scalebar(breaks = c(0, 2, 4, 6, 8, 10),
#               position = tm_pos_out("right", "bottom"),
#               text.size = 0.8) +
#   tm_layout(frame = FALSE, legend.show = FALSE)

# Create map of block groups and parks with parks as bubbles
map_phx <- tm_shape(phx_city_limits) +
  tm_polygons(fill = "gray90", 
              col = "black", 
              lwd = 1) +
  tm_shape(phx_bg_all) +
  tm_polygons(fill = "#fee08b",
              lwd = 0.3) +
  tm_shape(phx_bg) +
  tm_polygons(fill = "#fdae61") +
  tm_shape(parks) +
  tm_bubbles(size = "area_ha",
             size.scale = tm_scale_intervals(values = 
               c(0.3, 0.6, 1, 1.2, 1.4),
               breaks = c(0, 10, 20, 40, 80, 120)
             ),
             fill = "park_type",
             fill.scale = tm_scale_categorical(
               values = c("Pocket" = "#4daf4a",
               "Linear" = "#377eb8",
               "Neighborhood" = "#e41a1c",
               "Community" = "#984ea3",
               "Regional" = "#a65628")),
              lwd = 0.8,
             fill_alpha = 0.8
  ) +
  tm_scalebar(breaks = c(0, 2, 4, 6, 8, 10),
              position = tm_pos_out("right", "bottom"),
              text.size = 0.8) +
  tm_layout(frame = FALSE, legend.show = FALSE)

# Plot all elements
map_bg_parks <- ggdraw() +
  draw_plot(tmap_grob(map_phx),
            x = 0.15) +
  draw_plot(tmap_grob(legend_bg_parks), 
            x = 0.75, 
            y = 0.55, 
            width = 0.3, 
            height = 0.35) +
  draw_plot(tmap_grob(inset_az),
            x = 0.053,
            y = 0.4,
            width = 0.4,
            height = 0.4) +
  draw_plot(tmap_grob(inset_phx), 
            x = 0.053, 
            y = 0.05, 
            width = 0.4, 
            height = 0.4)

map_bg_parks

ggsave("images/map_bg_parks.png", plot = map_bg_parks, bg = "white", width = 8, height = 6)

tmap_save(map_phx, "images/map_phx.png")

saveRDS(map_bg_parks, "map_bg_parks.rds")


## Maps of Residential tree canopy cover and park tree access score ============================

# Create map of RTCC
map_rtcc <- tm_shape(phx_city_limits) +
  tm_polygons(fill = "gray90", 
              col = "black", 
              lwd = 1.3) +
  tm_shape(phx_bg) +
  tm_polygons(fill = "pct_tree", 
              fill.scale = tm_scale(values = "brewer.greens",
                                    style = "jenks",
                                    values.range = c(0.4, 1),
                                    value.na = "gray90",
                                    label.na = "Missing data",
                                    label.format = list(digits = 1)),
              fill.legend = tm_legend(title = "Residential tree \ncanopy cover (%)",
                                      position = tm_pos_out("center",
                                                            "bottom",
                                                            pos.h = 0.25,
                                                            pos.v = 1))) +
  tm_scalebar(breaks = c(0, 2, 4, 6, 8, 10),
              position = tm_pos_out("center", 
                                    "bottom",
                                    pos.h = 0.5,
                                    pos.v = 1),
              text.size = 0.8) +
  tm_title_in("a", 
              position = tm_pos_in("left", 
                                   "top",
                                   pos.h = 0.2,
                                   pos.v = 1),
              size = 1.4) +
  tm_layout(frame = FALSE, 
            legend.show = TRUE)

# Create map of PTAS
map_ptas <- tm_shape(phx_city_limits) +
  tm_polygons(fill = "gray90", 
              col = "black", 
              lwd = 1.3) +
  tm_shape(phx_bg) +
  tm_polygons(fill = "tree_access", 
              fill.scale = tm_scale(values = "brewer.blues",
                                    style = "jenks",
                                    values.range = c(0.4, 1),
                                    value.na = "gray90",
                                    label.na = "Missing data"),
              fill.legend = tm_legend(title = "Park tree \naccess score",
                                      group_id = "combined_legend")) +
  tm_shape(parks) +
  tm_bubbles(size = "tree_area",
             size.scale = tm_scale_continuous(values.range = c(0.3, 1.4),
                                              ticks = c(1, 2, 4, 8, 16),
                                              labels = c("0 - 0.9", 
                                                         "1.0 - 1.9", 
                                                         "2.0 - 3.9", 
                                                         "4.0 - 7.9",  
                                                         "8.0 - 16")
             ),
             fill = "#4daf4a",
             lwd = 0.8,
             fill_alpha = 0.6,
             size.legend = tm_legend(title = "Tree canopy \narea (ha) in parks",
                                     group_id = "combined_legend")) +
  tm_components("combined_legend",
                position = tm_pos_out("center", "bottom", pos.h = 0.15, pos.v = 1),
                stack = "horizontal",
                frame_combine = TRUE) +
  tm_scalebar(breaks = c(0, 2, 4, 6, 8, 10),
              position = tm_pos_out("center", "bottom", pos.h = 0.6, pos.v = 1),
              text.size = 0.8) +
  tm_title_in("b", 
              position = tm_pos_in("left", "top", pos.h = 0.2, pos.v = 1),
              size = 1.4) +
  tm_layout(frame = FALSE, 
            legend.show = TRUE)




# Combine maps
map_rtcc_ptas <- tmap_arrange(map_rtcc, map_ptas, nrow = 1)

map_rtcc_ptas

saveRDS(map_rtcc_ptas, "map_rtcc_ptas.rds")

## Maps of demographic factors =========================================

# Create map of Hispanic population
map_hisp <- tm_shape(phx_city_limits) +
  tm_polygons(fill = "gray90", 
              col = "black", 
              lwd = 1.3) +
  tm_shape(phx_bg) +
  tm_polygons(fill = "pct_hispanic",
              fill.scale = tm_scale_intervals(
                values = "brewer.blues",              
                style = "jenks",
                values.range = c(0.3, 1),
                value.na = "gray90",
                label.na = "Missing data",
                label.format = list(digits = 1)),
              fill.legend = tm_legend(title = "Hispanic population (%)",
                                      position = tm_pos_out("center",
                                                            "bottom",
                                                            pos.h = 0.25,
                                                            pos.v = 1))) +
  tm_scalebar(breaks = c(0, 2, 4, 6, 8, 10),
              position = tm_pos_out("center", 
                                    "bottom", 
                                    pos.h = 0.75,
                                    pos.v = 1),
              text.size = 0.8) +
  tm_title_in("a", 
              position = tm_pos_in("left", 
                                   "top"),
              size = 1.4) +
  tm_layout(frame = FALSE, legend.show = TRUE)

# Create map of Black population
map_black <- tm_shape(phx_city_limits) +
  tm_polygons(fill = "gray90", 
              col = "black", 
              lwd = 1.3) +
  tm_shape(phx_bg) +
  tm_polygons(fill = "pct_black", 
              fill.scale = tm_scale_intervals(
                values = "brewer.purples",
                style = "jenks",
                values.range = c(0.4, 1),
                value.na = "gray90",
                label.na = "Missing data",
                label.format = list(digits = 1)),
              fill.legend = tm_legend(title = "Black population (%)",
                                      position = tm_pos_out("center",
                                                            "bottom",
                                                            pos.h = 0.25,
                                                            pos.v = 1))) +
  tm_scalebar(breaks = c(0, 2, 4, 6, 8, 10),
              position = tm_pos_out("center", 
                                    "bottom", 
                                    pos.h = 0.75,
                                    pos.v = 1),
              text.size = 0.8) +
  tm_title_in("b", 
               position = tm_pos_in("left", 
                                     "top"),
               size = 1.4) +
  tm_layout(frame = FALSE, 
            legend.show = TRUE) 


# Create map of ADI rank
map_adi <- tm_shape(phx_city_limits) +
  tm_polygons(fill = "gray90", 
              col = "black", 
              lwd = 1.3) +
  tm_shape(phx_bg) +
  tm_polygons(fill = "adi_rank", 
              fill.scale = tm_scale_intervals(
                values = "brewer.oranges",
                style = "jenks",
                values.range = c(0.3, 1),
                value.na = "gray90",
                label.na = "Missing data",
                label.format = list(digits = 0)),
              fill.legend = tm_legend(title = "ADI rank",
                                      position = tm_pos_out("center",
                                                            "bottom",
                                                            pos.h = 0.25,
                                                            pos.v = 1))) +
  tm_scalebar(breaks = c(0, 2, 4, 6, 8, 10),
              position = tm_pos_out("center", 
                                    "bottom", 
                                    pos.h = 0.64,
                                    pos.v = 1),
              text.size = 0.8) +
  tm_title_in("c", 
              position = tm_pos_in("left", 
                                   "top"),
              size = 1.4) +
  tm_layout(frame = FALSE, legend.show = TRUE)

# Arrange all three maps in one row
map_demographics <- tmap_arrange(map_hisp, map_black, map_adi, nrow = 1)

map_demographics

saveRDS(map_demographics, "map_demographics.rds")

# Map of tree access canopy categories =====================================================


# Create map of tree canopy access categories
map_access <- tm_shape(phx_city_limits) +
  tm_polygons(fill = "gray90", 
              col = "black", 
              lwd = 1.3) +
  tm_shape(phx_bg) +
  tm_polygons(fill = "canopy_access_type_15",
              fill.scale = tm_scale_ordinal(levels = c("HH", "HL", "LH", "LL"),
                                            values = c("#FF0000", "#f4ada8", "#a7adf9", "#0000FF"),
                                            labels = c("HH (High RTCC/High PTAS)", 
                                                       "HL (High RTCC/Low PTAS)", 
                                                       "LH (Low RTCC/High PTAS)", 
                                                       "LL (Low RTCC/Low PTAS)"),
                                            value.na = "gray90",
                                            label.na = "Missing data"),
                                            fill.legend = tm_legend(title = "Tree canopy access categories",
                                                                    position = tm_pos_out("center",
                                                                                          "bottom",
                                                                                          pos.h = 0.1,
                                                                                          pos.v = 1))) +
  tm_scalebar(breaks = c(0, 2, 4, 6, 8, 10),
              position = tm_pos_out("center", 
                                    "bottom", 
                                    pos.h = 0.8,
                                    pos.v = 1),
              text.size = 0.8) +
  tm_layout(frame = FALSE, legend.show = TRUE)

map_access

saveRDS(map_access, "map_access.rds")
