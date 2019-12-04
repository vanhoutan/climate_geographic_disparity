rm(list = ls())

# shape <- readOGR(dsn = "/Users/ktanaka/clim_geo_disp/data/TEOW", layer = "wwf_terr_ecos")  # read the shapefile in by name not the lack of .shp extension
# shape <- ms_simplify(shape, keep = 0.001, keep_shapes = F) # simplify shapefile (saves computing time)
# save(shape, file = "/Users/ktanaka/clim_geo_disp/data/teow_0.001.RData")
load("/Users/ktanaka/clim_geo_disp/data/teow_0.001.RData")
shape <- shape %>% st_as_sf()  

# assign names to biomes
shape$BIOME <- as.factor(shape$BIOME)
shape$BIOME <- fct_recode(shape$BIOME, 
                          Tropical_and_Subtropical_Moist_Broadleaf_Forests             = "1",
                          Tropical_and_Subtropical_Dry_Broadleaf_Forests               = "2",
                          Tropical_and_Subtropical_Coniferous_Forests                  = "3",
                          Temperate_Broadleaf_and_Mixed_Forests                        = "4",
                          Temperate_Conifer_Forests                                    = "5",
                          Boreal_Forests_Taiga                                         = "6",
                          Tropical_and_Subtropical_Grasslands_Savannas_and_Shrublands  = "7",
                          Temperate_Grasslands_Savannas_and_Shrublands                 = "8",
                          Flooded_Grasslands_and_Savannas                              = "9", 
                          Montane_Grasslands_and_Shrublands                            = "10",
                          Tundra                                                       = "11",
                          Mediterranean_Forests_Woodlands_and_Scrub                    = "12",
                          Deserts_and_Xeric_Shrublands                                 = "13",
                          Mangroves                                                    = "14",
                          Large_Inland_Waterbodies                                     = "98",
                          Polar_Artic                                                  = "99"
)

# pull out inland water bodies
shape <- shape %>% filter(!BIOME %in% c("Large_Inland_Waterbodies",""))

pdf("~/Desktop/unit-biome.pdf", width = 10, height = 8)
shape %>% ggplot() + 
  geom_sf(aes(group = BIOME, fill = BIOME), color = "NA", show.legend = T) + 
  scale_fill_viridis_d("") + 
  theme_pubr() +
  # dark_theme_classic() + 
  guides(fill = guide_legend(nrow = 8), "") + 
  theme(legend.position = "bottom")
dev.off()


# shape <- readOGR(dsn = "/Users/ktanaka/clim_geo_disp/data/MEOW_2", layer = "WCMC-036-MEOW-PPOW-2007-2012-NoCoast")
# shape <- ms_simplify(shape, keep = 0.01, keep_shapes = F) # simplify shapefile (saves computing time)
# save(shape, file = "/Users/ktanaka/clim_geo_disp/data/meow_0.01.RData")
load("/Users/ktanaka/clim_geo_disp/data/meow_0.01.RData")
shape <- shape %>% st_as_sf()  
# clip out marine ecoregions overlapping on land
load(paste0("~/clim_geo_disp/data/land_ocean_df.RData"))
land <- land %>% st_set_precision(1000000) %>% lwgeom::st_make_valid()
shape <- st_difference(shape, st_union(land))

pdf("~/Desktop/unit-realm.pdf", width = 10, height = 8)
shape %>% ggplot() + 
  geom_sf(aes(group = REALM, fill = REALM), color = "NA", show.legend = T) + 
  scale_fill_viridis_d("") + 
  theme_pubr() +
  # dark_theme_classic() + 
  guides(fill = guide_legend(nrow = 5), "") + 
  theme(legend.position = "bottom")
dev.off()

shape <- ne_countries(scale = "small", returnclass = "sf") #worldwide country polygon
shape <- shape[57] %>% st_as_sf()  
shape <- shape %>% filter(!subregion %in% c("Seven seas (open ocean)",""))

pdf("~/Desktop/unit-subregions.pdf", width = 10, height = 8)
shape %>% ggplot() + 
  geom_sf(aes(group = subregion, fill = subregion), color = "NA", show.legend = T) + 
  scale_fill_viridis_d("") + 
  theme_pubr() +
  # dark_theme_classic() + 
  guides(fill = guide_legend(nrow = 5), "") + 
  theme(legend.position = "bottom")
dev.off()

