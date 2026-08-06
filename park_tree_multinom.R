library(sf)
library(tidyverse)
library(tmap)
library(stargazer)
library(nnet)
library(labelled)
library(spatialreg)
library(CARBayes)
library(spdep)

# Read in RDS files
phx_bg <- readRDS("phx_bg.rds")
parks <- readRDS("parks.rds")

# Save RDS files
saveRDS(phx_bg, "phx_bg.rds")
saveRDS(multinom1, "multinom1.rds")
saveRDS(mn_model_15, "mn_model_15.rds")
saveRDS(HH_glm, "HH_glm.rds")
saveRDS(HH_glm_sf, "HH_glm_sf.rds")
saveRDS(HL_glm, "HL_glm.rds")
saveRDS(HL_glm_sf, "HL_glm_sf.rds")
saveRDS(LH_glm, "LH_glm.rds")
saveRDS(LH_glm_sf, "LH_glm_sf.rds")
saveRDS(LL_glm, "LL_glm.rds")
saveRDS(LL_glm_sf, "LL_glm_sf.rds")

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
# Run multinomial regression model on canopy access category (HH, HL, LH, LL)
multinom1 <- multinom(canopy_access_type_15 ~ 
                        pct_hispanic + pct_black + adi_rank, 
                      data = phx_bg)

# Run multinom model with interactions (no significant interactions are found)
# multinom2 <- multinom(canopy_access_type_15 ~
#                         adi_rank * pct_hispanic + adi_rank * pct_black, 
#                       data = phx_bg)

# Save model objects
odds <- exp(coef(multinom1))
ci <- exp(confint(multinom1))
z_15 <- summary(multinom1)$coefficients / summary(multinom1)$standard.errors
p_15 <- (1 - pnorm(abs(z_15), 0, 1)) * 2

## Test multinomial residuals for spatial autocorrelation ==========================

# Add multinomial residuals to phx_bg
phx_bg <- phx_bg |> 
  mutate(mn_resid = resid(multinom1))

# Separate residual categories into 4 columns
phx_bg <- phx_bg |> 
  mutate(
    resid_HH = mn_resid[, "HH"],
    resid_HL = mn_resid[, "HL"],
    resid_LH = mn_resid[, "LH"],
    resid_LL = mn_resid[, "LL"]
  )

# Map multinomial residuals
map_mn_HH <- tm_shape(phx_bg) +
  tm_polygons(
    fill = "resid_HH", 
    fill.scale = tm_scale_intervals(
      style = "quantile",
      values = "brewer.greens"
    ),
    fill.legend = tm_legend(title = ""),         
    col = NA,
    lwd = 0                           
  )

map_mn_HL <- tm_shape(phx_bg) +
  tm_polygons(
    fill = "resid_HL", 
    fill.scale = tm_scale_intervals(
      style = "quantile",
      values = "brewer.greens"
    ),
    fill.legend = tm_legend(title = ""),         
    col = NA,
    lwd = 0                           
  )

map_mn_LH <- tm_shape(phx_bg) +
  tm_polygons(
    fill = "resid_LH", 
    fill.scale = tm_scale_intervals(
      style = "quantile",
      values = "brewer.greens"
    ),
    fill.legend = tm_legend(title = ""),         
    col = NA,
    lwd = 0                           
  )

map_mn_LL <- tm_shape(phx_bg) +
  tm_polygons(
    fill = "resid_LL", 
    fill.scale = tm_scale_intervals(
      style = "quantile",
      values = "brewer.greens"
    ),
    fill.legend = tm_legend(title = ""),         
    col = NA,
    lwd = 0                           
  )

tmap_arrange(map_mn_HH, map_mn_HL, map_mn_LH, map_mn_LL, ncol = 2)

# Test residuals for autocorrelation with global Moran's I
moran.mc(phx_bg$resid_HH, nbw8, nsim = 999)
moran.mc(phx_bg$resid_HL, nbw8, nsim = 999)
moran.mc(phx_bg$resid_LH, nbw8, nsim = 999)
moran.mc(phx_bg$resid_LL, nbw8, nsim = 999)

## Include spatial autocorrelation in the regression analysis using spatial filtering

# Create binary columns for tree access categories
phx_bg$cat15HH <- ifelse(phx_bg$canopy_access_type_15 == "HH", 1, 0)
phx_bg$cat15HL <- ifelse(phx_bg$canopy_access_type_15 == "HL", 1, 0)
phx_bg$cat15LH <- ifelse(phx_bg$canopy_access_type_15 == "LH", 1, 0)
phx_bg$cat15LL <- ifelse(phx_bg$canopy_access_type_15 == "LL", 1, 0)

# Run binomial GLM on tree access categories
HH_glm <- glm(cat15HH ~ pct_hispanic + pct_black + adi_rank, 
              data = phx_bg, 
              family = "binomial")
HL_glm <- glm(cat15HL ~ pct_hispanic + pct_black + adi_rank, 
              data = phx_bg, 
              family = "binomial")
LH_glm <- glm(cat15LH ~ pct_hispanic + pct_black + adi_rank, 
              data = phx_bg, 
              family = "binomial")
LL_glm <- glm(cat15LL ~ pct_hispanic + pct_black + adi_rank, 
              data = phx_bg, 
              family = "binomial")

summary(HH_glm)
summary(HL_glm)
summary(LH_glm)
summary(LL_glm)

moran.mc(resid(HH_glm), nbw8, nsim = 999)
moran.mc(resid(HL_glm), nbw8, nsim = 999)
moran.mc(resid(LH_glm), nbw8, nsim = 999)
moran.mc(resid(LL_glm), nbw8, nsim = 999)

# Select Moran eigenvectors
HH_me <- ME(cat15HH ~ pct_hispanic + pct_black + adi_rank, 
            data = phx_bg,
            family = binomial,
            listw = nbw8,
            alpha = 0.08)

HH_me

# Rerun GLM with selected eigenvectors included
HH_glm_sf <- glm(cat15HH ~ pct_hispanic + pct_black + adi_rank + fitted(HH_me), 
                 data = phx_bg,
                 family = binomial)

anova(HH_glm, HH_glm_sf, test = "Chisq")

moran.mc(resid(HH_glm_sf), nbw8, nsim = 999)

summary(HH_glm)
summary(HH_glm_sf)

# Select Moran eigenvectors
HL_me <- ME(cat15HL ~ pct_hispanic + pct_black + adi_rank, 
            data = phx_bg,
            family = binomial,
            listw = nbw8,
            alpha = 0.08)

HL_me

# Rerun GLM with selected eigenvectors included
HL_glm_sf <- glm(cat15HL ~ pct_hispanic + pct_black + adi_rank + fitted(HL_me), 
                 data = phx_bg,
                 family = binomial)

anova(HL_glm, HL_glm_sf, test = "Chisq")

moran.mc(resid(HL_glm_sf), nbw8, nsim = 999)

summary(HL_glm)
summary(HL_glm_sf)

# Select Moran eigenvectors
LH_me <- ME(cat15LH ~ pct_hispanic + pct_black + adi_rank, 
            data = phx_bg,
            family = binomial,
            listw = nbw8,
            alpha = 0.08)

LH_me

# Rerun GLM with selected eigenvectors included
LH_glm_sf <- glm(cat15LH ~ pct_hispanic + pct_black + adi_rank + fitted(LH_me), 
                 data = phx_bg,
                 family = binomial)

anova(LH_glm, LH_glm_sf, test = "Chisq")

moran.mc(resid(LH_glm_sf), nbw8, nsim = 999)

summary(LH_glm)
summary(LH_glm_sf)

# Select Moran eigenvectors
LL_me <- ME(cat15LL ~ pct_hispanic + pct_black + adi_rank, 
            data = phx_bg,
            family = binomial,
            listw = nbw8,
            alpha = 0.08)

LL_me

# Rerun GLM with selected eigenvectors included
LL_glm_sf <- glm(cat15LL ~ pct_hispanic + pct_black + adi_rank + fitted(LL_me), 
                 data = phx_bg,
                 family = binomial)

anova(LL_glm, LL_glm_sf, test = "Chisq")

moran.mc(resid(LL_glm_sf), nbw8, nsim = 999)

summary(LL_glm)
summary(LL_glm_sf)
