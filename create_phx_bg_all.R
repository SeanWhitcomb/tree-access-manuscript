tree_cover <- readRDS("g:/My Drive/Research/Phoenix park tree access/tree_cover.rds")

phx_bg_tree <- phx_bg

phx_bg_all <- st_read("G:/My Drive/Research/Phoenix park tree access/phx_park_tree_access.gpkg", 
                      layer = "phoenix_bg_poly_all")

phx_bg_all <- phx_bg_all |> 
  full_join(st_drop_geometry(tree_cover) |> select(geoid, tree_cover_pct_parks, tree_cover_pct_wo_parks), 
            by = "geoid")

phx_bg_all <- phx_bg_all |> 
  mutate(
    pct_tree_parks = coalesce(tree_cover_pct_parks, pct_tree),
    pct_tree_wo_parks = coalesce(tree_cover_pct_wo_parks, pct_tree)
  )

phx_bg_all <- phx_bg_all |>
  select(-tree_cover_pct_parks, -tree_cover_pct_wo_parks)

summary(phx_bg_all$pct_tree_parks)

summary(phx_bg_all$pct_tree_wo_parks)

# calculate tree cover with and without parks for NA block groups and join to phx_bg_all
phx_bg_NA <- phx_bg_all |> filter(is.na(phx_bg_all$pct_tree_parks))

st_write(phx_bg_NA, "G:/My Drive/Research/Phoenix park tree access/phx_park_tree_access.gpkg", 
                      layer = "phx_bg_NA", delete_layer = T)

tmap_mode("view")
tm_shape(phx_bg_all |> filter(!is.na(phx_bg_all$pct_tree_wo_parks))) +
  tm_polygons(fill = "blue",
              fill_alpha = 0.8) +
  tm_shape(parks) +
  tm_polygons(fill = "darkgreen")

phx_bg_na_lc <- st_read("G:/My Drive/Research/Phoenix park tree access/phx_park_tree_access.gpkg", 
        layer = "phx_bg_na_w_parks")

phx_bg_all <- phx_bg_all |> 
  left_join(st_drop_geometry(phx_bg_na_lc) |> select(geoid, pct_tree_parks), 
            by = "geoid") |> 
  mutate(pct_tree_parks = coalesce(pct_tree_parks.x, pct_tree_parks.y)) |> 
  select(-pct_tree_parks.x, -pct_tree_parks.y)

phx_bg_all <- phx_bg_all |> 
  mutate(
    pct_tree_wo_parks = coalesce(pct_tree_wo_parks, pct_tree_parks)
  )

saveRDS(phx_bg_all, "G:/My Drive/Research/Phoenix park tree access/phx_bg_all.rds")

# Create phx_bg_parks for only those block groups with parks
phx_bg_parks <- phx_bg_all |> 
  filter(pct_tree_parks != pct_tree_wo_parks)

nrow(phx_bg_parks)
saveRDS(phx_bg_parks, "phx_bg_parks.rds")
