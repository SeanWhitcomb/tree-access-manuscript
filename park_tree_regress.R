library(sf)
library(tidyverse)
library(tidycensus)
library(corrr)
library(tmap)
library(spdep)
library(tigris)
library(rmapshaper)
library(flextable)
library(car)
library(spatialreg)
library(stargazer)
library(patchwork)
library(gtsummary)
library(performance)

# Save RDS files
saveRDS(phx_bg, "G:/My Drive/Research/Phoenix park tree access/phx_bg.rds")
saveRDS(ols_canopy, "G:/My Drive/Research/Phoenix park tree access/ols_canopy.rds")
saveRDS(ols_access, "G:/My Drive/Research/Phoenix park tree access/ols_access.rds")
saveRDS(ols_access_sqrt, "G:/My Drive/Research/Phoenix park tree access/ols_access_sqrt.rds")

# Write objects to geopackage layers
st_write(phx_bg, "phx_park_tree_access.gpkg", layer = "phx_bg", delete_layer = T)

# Read in RDS files
phx_bg <- readRDS("G:/My Drive/Research/Phoenix park tree access/phx_bg.rds")
ols_canopy <- readRDS("G:/My Drive/Research/Phoenix park tree access/ols_canopy.rds")
ols_access_sqrt <- readRDS("G:/My Drive/Research/Phoenix park tree access/ols_access_sqrt.rds")


# Attach dataframe
attach(phx_bg)

# ## Add demographic factors to block groups
#   # households
#   # % 65 and older
#   # % under 18
#   # vehicle ownership
# vars = c(  
#   total_pop = "B01001_001",
  
#   # Males under 18 (cells 003–006)
#   m_under5  = "B01001_003",
#   m_5_9     = "B01001_004",
#   m_10_14   = "B01001_005",
#   m_15_17   = "B01001_006",
  
#   # Females under 18 (cells 027–030)
#   f_under5  = "B01001_027",
#   f_5_9     = "B01001_028",
#   f_10_14   = "B01001_029",
#   f_15_17   = "B01001_030",
  
#   # Males 65+ (cells 020–025)
#   m_65_66   = "B01001_020",
#   m_67_69   = "B01001_021",
#   m_70_74   = "B01001_022",
#   m_75_79   = "B01001_023",
#   m_80_84   = "B01001_024",
#   m_85plus  = "B01001_025",
  
#   # Females 65+ (cells 044–049)
#   f_65_66   = "B01001_044",
#   f_67_69   = "B01001_045",
#   f_70_74   = "B01001_046",
#   f_75_79   = "B01001_047",
#   f_80_84   = "B01001_048",
#   f_85plus  = "B01001_049",
# )

# mar_demo <- get_acs(geography = "block group",
# year = 2024,
# variables = vars,
# state = "AZ",
# county = "Maricopa",
# survey = "acs5",
# output = "wide",
# geometry = F)

# mar_demo <- mar_demo |> 
#   mutate(
#     pct_under18 = (m_under5E + m_5_9E + m_10_14E + m_15_17E +
#                   f_under5E + f_5_9E + f_10_14E + f_15_17E) / total_popE * 100,
#     pct_65plus = (m_65_66E + m_67_69E + m_70_74E + m_75_79E + m_80_84E + m_85plusE +
#                   f_65_66E + f_67_69E + f_70_74E + f_75_79E + f_80_84E + f_85plusE) 
#                   / total_popE * 100
#   )

# mar_demo_age <- mar_demo |> select(GEOID, pct_under18, pct_65plus)

# phx_bg <- left_join(phx_bg, mar_demo_age, by = c("geoid" = "GEOID"))

# Add official ADI data to block groups
adi_rank <- read.csv("data/adi_az_2023.csv") |>
  mutate(FIPS = str_pad(FIPS, width = 12, pad = "0", side = "left")) |>
  select(FIPS, ADI_NATRANK)

phx_bg <- phx_bg |>
  left_join(adi_rank, by = c("geoid" = "FIPS"))

phx_bg <- phx_bg |>
  rename(adi_rank = ADI_NATRANK)

# Convert adi_rank column to numeric
phx_bg$adi_rank <- as.numeric(phx_bg$adi_rank)

# Remove 10 block groups that are mostly group quarters
saveRDS(phx_bg, "phx_bg_backup.rds")
phx_bg <- phx_bg |> 
  filter(!is.na(adi_rank))

phx_bg_pw <- phx_bg_pw |>
  filter(geoid %in% phx_bg$geoid)

# Multiply tree_access by 1e6 to make more readable
phx_bg$tree_access_raw <- phx_bg$tree_access
phx_bg$tree_access <- phx_bg$tree_access * 1000000
summary(phx_bg$tree_access)
summary(phx_bg$tree_access_raw)
summary(sqrt_tree_access)

## Exploratory data analysis
phx_bg |> 
  select(pct_nonwhite, pct_hispanic, pct_under18, pct_65plus, 
adi, pct_black, pct_tree, tree_access) |> 
  st_drop_geometry() |> 
  summary()

## Test for correlations between variables
corr.table <- phx_bg |> 
  st_drop_geometry() |> 
  select(pct_nonwhite, pct_hispanic, pct_under18, pct_65plus, 
adi, pct_black, pct_tree, tree_access) |> 
  correlate()

corr.table

# Results: 
  # nonwhite - hispanic = 0.895
  # nonwhite - adi = 0.750
  # hispanic - adi = 0.705

# Test for collinearity
vif(lm(pct_tree ~ pct_nonwhite + pct_hispanic + pct_under18 + pct_65plus + adi_rank, 
data = phx_bg))

vif(lm(pct_tree ~ pct_hispanic + pct_black + adi_rank, 
       data = phx_bg))

vif(lm(sqrt_tree_access ~ pct_hispanic + pct_black + adi_rank, 
data = phx_bg))

## Run OLS on block group canopy (-parks) vs. demographic factors
ols_canopy_hisp <- lm(pct_tree ~ pct_hispanic, 
data = phx_bg)

summary(ols_canopy_hisp)

ols_canopy_adi <- lm(pct_tree ~ adi_rank, 
data = phx_bg)

summary(ols_canopy_adi)

ols_canopy_black <- lm(pct_tree ~ pct_black, 
                      data = phx_bg)

summary(ols_canopy_black)


## Run OLS on park tree canopy access vs. demographic factors
ols_access_hisp <- lm(tree_access ~ pct_hispanic, 
data = phx_bg)

summary(ols_access_hisp)

ols_access_adi <- lm(tree_access ~ adi, 
data = phx_bg)

summary(ols_access_adi)

ols_access_black <- lm(tree_access ~ pct_black, 
                      data = phx_bg)

summary(ols_access_black)

# OLS on bg canopy vs. multiple factors
ols_canopy_all <- lm(pct_tree ~ pct_hispanic + pct_under18 + pct_65plus + adi, 
data = phx_bg)

ols_canopy <- lm(pct_tree ~ pct_hispanic + pct_black + adi_rank, 
                 data = phx_bg)

summary(ols_canopy)

# Check model assumptions
png("check_model.png", width = 1200, height = 900)
check_model(ols_canopy)
dev.off()

ols_canopy |> 
  as_flextable()

# # OLS on park tree canopy access vs. multiple factors
# ols_access_all <- lm(tree_access ~ pct_hispanic + pct_under18 + pct_65plus + adi, 
# data = phx_bg)
# 
# ols_access <- lm(tree_access ~ pct_hispanic + pct_black + adi_rank, 
#                  data = phx_bg)
# 
# summary(ols_access)
# 
# ols_access |> 
#   as_flextable()

## Test for normality of residuals

# # Tree access is right skewed - log transform and multiply by 1e6 to make more workable numbers
# phx_bg$log_tree_access <- log1p(tree_access * 1e6)
# 
# # Run access OLS on log transformed data
# ols_access_log <- lm(log_tree_access ~ pct_hispanic + + pct_black + adi, 
# data = phx_bg)
# summary(ols_access_log)
# 
# qqPlot(ols_access_log)
# 
# h_ols_access_log <- phx_bg |> 
#   ggplot() +
#   geom_histogram(aes(x = resid(ols_access_log))) +
#   xlab("OLS log of park tree canopy access residuals")
# h_ols_access_log

# log Tree access is left skewed - sqrt transform 
phx_bg$sqrt_tree_access <- sqrt(phx_bg$tree_access)

# Run access OLS on sqrt transformed data
ols_access_sqrt <- lm(sqrt_tree_access ~ pct_hispanic + pct_black + adi_rank, 
data = phx_bg)
summary(ols_access_sqrt)

# Check model assumptions
png("check_model_access.png", width = 1200, height = 900)
check_model(ols_access_sqrt)
dev.off()

# Q-Q plots 
qqPlot(ols_canopy)
qqPlot(ols_access_sqrt)

# Histograms of residuals
h_ols_canopy <- phx_bg |> 
  ggplot() +
  geom_histogram(aes(x = resid(ols_canopy))) +
  xlab("OLS tree canopy residuals")

h_ols_access_sqrt <- phx_bg |> 
  ggplot() +
  geom_histogram(aes(x = resid(ols_access_sqrt))) +
  xlab("OLS sqrt of park tree canopy access residuals")

h_ols_canopy + h_ols_access_sqrt
plot_layout(ncol = 1)

## Exploratory spatial data analysis

# Add OLS residuals to phx_bg
phx_bg <- phx_bg |> 
  mutate(ols_canopy_resid = resid(ols_canopy), ols_access_resid = resid(ols_access_sqrt))


# Map residuals from OLS models
map_ols_canopy <- tm_shape(phx_bg) +
  tm_polygons(
    fill = "ols_canopy_resid", 
    fill.scale = tm_scale_intervals(
      style = "quantile",
      values = "brewer.greens"
    ),
    fill.legend = tm_legend(title = ""),         
    col = NA,
    lwd = 0                           
  ) +
  tm_layout(
    main.title = "Residuals from linear regression on tree canopy",
    main.title.size = 0.95,
    frame = FALSE,
    legend.outside = TRUE
  )

map_ols_access <- tm_shape(phx_bg) +
  tm_polygons(
    fill = "ols_access_resid", 
    fill.scale = tm_scale_intervals(
      style = "quantile",
      values = "brewer.reds"
    ),
    fill.legend = tm_legend(title = ""),         
    col = NA,
    lwd = 0                           
  ) +
  tm_layout(
    main.title = "Residuals from linear regression on square root-transformed park tree access",
    main.title.size = 0.95,
    frame = FALSE,
    legend.outside = TRUE
  )

tmap_arrange(map_ols_canopy, map_ols_access, ncol = 1)

## Test for spatial autocorrelation =================================

# Create neighborhood matrix based on 8 nearest neighbors
phx_bg_pw <- readRDS("phx_bg_pw.rds")
nb8 <- knn2nb(knearneigh(phx_bg_pw, k = 8))
nbw8 <- nb2listw(nb8, style = "W")

# Global Moran's I for tree canopy
moran.mc(phx_bg$ols_canopy_resid, nbw8, nsim = 999)

# Global Moran's I for park tree access
moran.mc(phx_bg$ols_access_resid, nbw8, nsim = 999)

## Test which spatial model to use by the Rao's test
lm.RStests(ols_canopy, nbw8, test = "all")
lm.RStests(ols_access_sqrt, nbw8, test = "all")

## Run spatial error model on bg canopy
err_canopy <- errorsarlm(pct_tree~ pct_hispanic + pct_black + adi_rank,
data = phx_bg,
listw = nbw8)
err_canopy

## Run spatial lag model on park tree access
# Park catchment areas overlap across neighboring block groups, so scores are driven by neighboring scores
lag_access_sqrt <- lagsarlm(sqrt_tree_access ~ pct_hispanic + pct_black + adi_rank,
data = phx_bg,
listw = nbw8)
lag_access_sqrt

## Run spatial error model on park tree access
# Rao's tests revealed similar scores for both models
err_access_sqrt <- errorsarlm(sqrt_tree_access ~ pct_hispanic + pct_black + adi_rank,
data = phx_bg,
listw = nbw8)
err_access_sqrt

# Create results tables
stargazer(ols_canopy, err_canopy, 
title = "Block group canopy regression results")

stargazer(ols_access_sqrt, err_access_sqrt, 
title = "Park tree canopy access regression results")