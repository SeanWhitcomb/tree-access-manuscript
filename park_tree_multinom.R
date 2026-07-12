library(sf)
library(tidyverse)
library(tmap)
library(stargazer)
library(nnet)
library(labelled)

# Read in RDS files
phx_bg <- readRDS("phx_bg.rds")
parks <- readRDS("parks.rds")

# Save RDS files
saveRDS(phx_bg, "phx_bg.rds")
saveRDS(mn_model_15, "mn_model_15.rds")

# Write objects to geopackage layers
st_write(phx_bg, "phx_park_tree_access.gpkg", layer = "phx_bg", delete_layer = T)

attach(phx_bg)

phx_bg <- subset(phx_bg, select = -adi)

# Label variables with labelled library
phx_bg <- phx_bg |> set_variable_labels(
  pct_hispanic = "Hispanic population (%)",
  pct_black = "Black population (%)",
  adi_rank = "ADI rank",
  canopy_access_type_15 = "Tree canopy access category",
  tree_access = "Park tree access score",
  pct_tree = "Private tree canopy cover"
)

# Descriptive stats for block group canopy cover
summary(pct_tree)

phx_bg |> 
  ggplot() +
  geom_histogram(aes(x = pct_tree)) +
  xlab("Tree canopy cover percent per block group")

# Set "high canopy" level to >=15% and recode canopy_access_type categories
sum(pct_tree >= 15)
sum(pct_tree >= 15) / nrow(phx_bg)

phx_bg$canopy_access_type_15 <- NA
phx_bg[(phx_bg$pct_tree >= 15 & phx_bg$tree_access >= median(phx_bg$tree_access)), 
       "canopy_access_type_15"] <- "HH"
phx_bg[(phx_bg$pct_tree < 15 & phx_bg$tree_access < median(phx_bg$tree_access)), 
       "canopy_access_type_15"] <- "LL"
phx_bg[(phx_bg$pct_tree >= 15 & phx_bg$tree_access < median(phx_bg$tree_access)), 
       "canopy_access_type_15"] <- "HL"
phx_bg[(phx_bg$pct_tree < 15 & phx_bg$tree_access >= median(phx_bg$tree_access)), 
       "canopy_access_type_15"] <- "LH"

# Map canopy access type categories based on 15% canopy and median canopy cover
map_canopy_access_15 <- tm_shape(phx_bg) +
  tm_polygons(fill = "canopy_access_type_15",
              fill.scale = tm_scale_ordinal(levels = c("HH", "LL", "HL", "LH"),
                                            values = c("#FF0000", "#0000FF", "#f4ada8", "#a7adf9"),
                                            labels = c("High-High", "Low-Low", "High-Low",
                                                       "Low-High")),
              fill.legend = tm_legend(title = "Block group tree canopy/park tree access", text.size = 1)) +
  tm_borders(fill_alpha = 0.5) + 
  tm_shape(parks) +
  tm_polygons(fill = "tree_area", col = "lightgreen",
              fill.scale = tm_scale_intervals(style = "quantile", values = "brewer.greens"),
              fill.legend = tm_legend(title = "Park tree area (ha)", text.size = 1))

map_canopy_access_med <- tm_shape(phx_bg) +
  tm_polygons(fill = "canopy_access_type",
              fill.scale = tm_scale_ordinal(levels = c("HH", "LL", "HL", "LH"),
                                            values = c("#FF0000", "#0000FF", "#f4ada8", "#a7adf9"),
                                            labels = c("High-High", "Low-Low", "High-Low",
                                                       "Low-High")),
              fill.legend = tm_legend(title = "Block group tree canopy/park tree access", text.size = 1)) +
  tm_borders(fill_alpha = 0.5) + 
  tm_shape(parks) +
  tm_polygons(fill = "tree_area", col = "lightgreen",
              fill.scale = tm_scale_intervals(style = "quantile", values = "brewer.greens"),
              fill.legend = tm_legend(title = "Park tree area (ha)", text.size = 1))

tmap_arrange(map_canopy_access_15, map_canopy_access_med, ncol = 1)

## Multinomial logistic regression on canopy 15% categories ===========================

# Specify baseline condition (high canopy (15)/high park tree access)
phx_bg$canopy_access_type_15 <- relevel(factor(phx_bg$canopy_access_type_15), ref = "HH")
mn_model_15 <- multinom(canopy_access_type_15 ~ pct_hispanic + pct_black + adi, 
                        data = phx_bg)
summary(mn_model_15)

z_15 <- summary(mn_model_15)$coefficients/summary(mn_model_15)$standard.errors
z_15

# 2-tailed t-test
p_15 <- (1 - pnorm(abs(z_15), 0, 1)) * 2
p_15

# Compute odds ratios
exp(coef(mn_model_15))

## Multinomial logistic regression on canopy >median categories ===========================

# Specify baseline condition (high canopy (>median)/high park tree access)
phx_bg$canopy_access_type <- relevel(factor(phx_bg$canopy_access_type), ref = "HH")
mn_model_med <- multinom(canopy_access_type ~ pct_hispanic + pct_black + adi, 
                        data = phx_bg)
summary(mn_model_med)

z_med <- summary(mn_model_med)$coefficients/summary(mn_model_med)$standard.errors
z_med

# 2-tailed t-test
p_med <- (1 - pnorm(abs(z_med), 0, 1)) * 2
p_med

# Compute odds ratios
exp(coef(mn_model_med))
