library(rnaturalearthdata)
library(rnaturalearth)
library(spatial.tools)
library(RColorBrewer)
library(rmapshaper)
library(tidyverse)
library(ggridges)
library(raster)
library(rgdal)
library(sf)
library(rnaturalearthhires)
library(DescTools)
library(colorRamps)
library(maps)
library(ggpubr)
library(sp)

setwd("~/Desktop/climate/climate_disp_2019-master/data_output")

#  Popular ggPlot theme  #
theme_themeo <- function () { 
  theme_classic()+
    theme(strip.background = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_text(margin = margin( 0.2, unit = "cm")),
          axis.text.y = element_text(margin = margin(c(1, 0.2), unit = "cm")),
          axis.ticks.length = unit(-0.1, "cm"),
          panel.border = element_rect(colour = "black", fill = NA, size = .5),
          legend.title = element_blank(),
          strip.text=element_text(hjust = 0) )}

source("/Users/ktanaka/Google Drive/R/misc/color palette function.R")

steps = c("blue", "white", "red")
change = color.palette(steps, space="rgb")

# read in emissions/ anomaly data, calculate ratio and disparity as in "basic_index_2" --------
# worldwide country polygon
world <- ne_countries(scale = "medium", returnclass = "sf")

# black carbon emissions color theme
steps = c("white", "red", "black")
bce = color.palette(steps, space="rgb")
bce = pals::parula
bce = matlab.like

ts_BCE <- raster("/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/causes/GHG_ts_products/BCEMAN_sum_Jan 31.grd") #%>% plot()
# png("BCE.png", res = 500, height = 10, width = 8, units = "in")
par(mfrow = c(2,1))
plot(ts_BCE, col = bce(100), main = "BCE", axes = F);map(add = T); degAxis(1); degAxis(2, las = 2)
plot(log10(ts_BCE), col = bce(100), main = "log10(BCE)", axes = F); map(add = T); degAxis(1); degAxis(2, las = 2)
# dev.off()

# climate anomaly data, pick rcp scenario
steps = c("blue", "white", "red")
change = color.palette(steps, space="rgb")
# change = pals::parula
# change = matlab.like

# png("Anomaly.png", res = 500, height = 10, width = 10.6, units = "in")
par(mfrow = c(3,2))
anomaly <- stack("/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/impacts/temperature/anomally_products/MPI_ESM_future_anom_Jan 31.grd")[["MPI_rcp26"]]
anomaly <- spatial_sync_raster(anomaly,ts_BCE, method = "ngb", size_only = F, verbose = T)
plot(anomaly, col = change(100), zlim = c(-20.5,20.5), main = "RCP26, Anomaly"); map(add = T)
plot(abs(anomaly), col = change(100),  zlim = c(0,21), main = "RCP26, Absolute Anomaly"); map(add = T)

anomaly <- stack("/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/impacts/temperature/anomally_products/MPI_ESM_future_anom_Jan 31.grd")[["MPI_rcp45"]]
anomaly <- spatial_sync_raster(anomaly,ts_BCE, method = "ngb", size_only = F, verbose = T)
plot(anomaly, col = change(100), zlim = c(-20.5,20.5),main = "RCP45, Anomaly"); map(add = T)
plot(abs(anomaly), col = change(100),  zlim = c(0,21), main = "RCP45, Absolute Anomaly"); map(add = T)

anomaly <- stack("/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/impacts/temperature/anomally_products/MPI_ESM_future_anom_Jan 31.grd")[["MPI_rcp85"]]
anomaly <- spatial_sync_raster(anomaly,ts_BCE, method = "ngb", size_only = F, verbose = T)
plot(anomaly, col = change(100), zlim = c(-20.5,20.5),main = "RCP85, Anomaly"); map(add = T)
plot(abs(anomaly), col = change(100),  zlim = c(0,21), main = "RCP85, Absolute Anomaly"); map(add = T)
# dev.off()

# select climate anomaly data, pick rcp scenario
anomaly <- stack("/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/impacts/temperature/anomally_products/MPI_ESM_future_anom_Jan 31.grd")[["MPI_rcp26"]]
anomaly <- stack("/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/impacts/temperature/anomally_products/MPI_ESM_future_anom_Jan 31.grd")[["MPI_rcp45"]]
anomaly <- stack("/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/impacts/temperature/anomally_products/MPI_ESM_future_anom_Jan 31.grd")[["MPI_rcp85"]]
anomaly <- spatial_sync_raster(anomaly,ts_BCE, method = "ngb", size_only = F, verbose = T)
anomaly <- abs(anomaly) # make absolute or not absolute anomally

raw_ratio <- 
  stack(ts_BCE, anomaly) %>% 
  rasterToPoints() %>% 
  data.frame() %>% 
  mutate(BCE = .[[3]], anomaly =.[[4]]) %>% 
  # mutate(BCE = DescTools::Winsorize(BCE, probs = c(0,.999), na.rm = T),
  #        anomaly = DescTools::Winsorize(anomaly, probs = c(0,.999), na.rm = T)) %>%
  dplyr::select(x, y, BCE, anomaly) %>% 
  mutate(ratio = BCE / anomaly) 

# rise
rise <- (max(raw_ratio$anomaly, na.rm = T) - min(raw_ratio$anomaly, na.rm = T) )

#run
run <- (max(raw_ratio$BCE, na.rm = T)  - min(raw_ratio$BCE, na.rm = T))

# slope
slope = rise / run
slope


# Convert disparity to point file ------------------------------------------

# calculate orthogonal distance rather than residual
#raw_ratio$disparity <- -(((slope*raw_ratio$BCE) + (-1*raw_ratio$anomaly + 0))/(sqrt((slope^2)-(1^2)))) #this one works!
raw_ratio$disparity <- (raw_ratio$anomaly - (slope*raw_ratio$BCE + min(raw_ratio$anomaly, na.rm = T)))/(sqrt((slope^2)-(1^2))) # so does this one

# turn in to point file to detect overlap with regions of interest
disparity <- st_as_sf(x = raw_ratio, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" )

# input x y plot
xy_plot <-
  ggplot(raw_ratio %>% sample_frac(1))+
  geom_point(aes(x = BCE, y = anomaly, fill = disparity),
             size = 4, 
             alpha = .6, 
             shape = 21,
             show.legend = T) +
  geom_abline(intercept = min(raw_ratio$anomaly, na.rm = T), slope = slope) +
  scale_fill_gradient2(
    limits = c(max(abs(raw_ratio$disparity), na.rm = T)*-1,max(abs(raw_ratio$disparity), na.rm = T)),
    high = "Red2",
    low = "Black",
    name = "Disparity")+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  coord_fixed(ratio = 1/slope)+
  theme_themeo()
xy_plot

# spatial plot
map_plot <-
  ggplot(raw_ratio) +
  geom_tile(aes(x = x, y = y, 
                fill = disparity
                # fill = scale(disparity)
  ), 
  show.legend = T) +
  geom_sf(data = world, fill = NA, size = .25, color = "black") +
  scale_fill_gradient2(high = "Red2", low = "Black", name = "Disparity") +
  coord_sf() +
  scale_x_continuous(expand = c(-0.005, 0)) +
  scale_y_continuous(expand = c(-0.005, 0)) +
  theme_classic() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) 
map_plot

gridExtra::grid.arrange(xy_plot, map_plot, ncol = 1)

# plotting inputs
hist_of_BCE <- ggplot(raw_ratio) + geom_histogram(aes(BCE)) + theme_pubr()

map_plotBCE <-
  ggplot(raw_ratio) +
  geom_raster(aes(x = x, 
                  y = y, 
                  fill = BCE), # when inputs are raw
              show.legend = T) +
  geom_sf(data = world, fill = NA,size = .1,color = "black")+
  scale_fill_gradientn(colours = c("white", "Red2",  "Black"), "BCE (kg sq m-2 s-1)")+
  coord_sf() +
  scale_x_continuous(expand = c(-0.005, 0)) +
  scale_y_continuous(expand = c(-0.005, 0)) +
  # theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) 

hist_of_anom <- ggplot(raw_ratio) + geom_histogram(aes(anomaly)) + theme_pubr()

map_plotANOM <-
  ggplot(raw_ratio) +
  geom_raster(aes(x = x, 
                  y = y,
                  fill = anomaly), # when inputs are raw
              show.legend = T)+
  geom_sf(data = world, fill = NA,size = .1, color = "black")+
  scale_fill_gradientn(colours = c("white", "Red2"), "Temperature Anomaly (Deg C)")+
  coord_sf() +
  scale_x_continuous(expand = c(-0.005, 0)) +
  scale_y_continuous(expand = c(-0.005, 0)) +
  # theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) 

gridExtra::grid.arrange(map_plotBCE, hist_of_BCE, map_plotANOM,hist_of_anom)
gridExtra::grid.arrange(map_plotBCE, map_plotANOM)

# Read in terrestorial biomes shapefile -----------------------------------
shape <- readOGR(dsn = "/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/summarization/TEOW", 
                 layer = "wwf_terr_ecos")  # read the shapefile in by name not the lack of .shp extension

# simplify shapefile (saves computing time)
shape <- ms_simplify(shape, keep = 0.05, keep_shapes = F)
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
shape <- shape %>% 
  filter(!BIOME %in% c("Large_Inland_Waterbodies",""))

# find intersections with disparity
intersection_biome <- st_intersection(disparity,shape)

# Read in Marine biomes shapefile -----------------------------------------
#shape <- readOGR(dsn = "./data/summarization/MEOW", layer = "meow_ecos") # read the shapefile in by name not the lack of .shp extension
shape_MEOW <- readOGR(dsn = "/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/summarization/MEOW_2", layer = "WCMC-036-MEOW-PPOW-2007-2012-NoCoast")  # read the shapefile in by name not the lack of .shp extension

# simplify shapefile (saves computing time)
shape_MEOW <- ms_simplify(shape_MEOW, keep = 0.05, keep_shapes = F)
shape_MEOW <- shape_MEOW %>% st_as_sf()  
shape$REALM <- as.factor(shape$REALM)

# clip out marine ecoregions overlapping on land
land <- ne_download(type = "land", category = 'physical', returnclass = "sf") 
shape_MEOW <- st_difference(shape_MEOW, st_union(land))

# find intersections with disparity
intersection_realm <- st_intersection(disparity,shape_MEOW)


# Read in global political boundaries shapefile  ---------------------------
#shape <- readOGR(dsn = "./data/summarization/MEOW", layer = "meow_ecos") # read the shapefile in by name not the lack of .shp extension
shape_WORLD <- world

# find intersections with disparity
intersection_world <- st_intersection(disparity,shape_WORLD)



# Read in US states boundaries shapefile -----------------------------------
states <- ne_states(country = "United States of America", returnclass = "sf")

# find intersections with disparity
intersection_states <- st_intersection(disparity,states)



# Read in EEZ land_union shapefile -----------------------------------
eez_land <- readOGR(dsn = "/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/summarization/EEZ_land_union", layer = "EEZ_land_v2_201410")  # read the shapefile in by name not the lack of .shp extension

# # simplify shapefile (saves computing time)
# eez_land <- ms_simplify(eez_land, keep = 0.05, keep_shapes = F)
eez_land <- eez_land %>% st_as_sf()  
country_list = names(table(world$geounit))
eez_land <- eez_land %>% filter(Country %in% country_list)
eez_land$Country <- as.factor(eez_land$Country)

# find intersections with disparity
intersection_land_eez <- st_intersection(disparity,eez_land)

# Ocean vs. Land ----------------------------------------------------------
ocean <- ne_download(type = "ocean", category = 'physical', returnclass = "sf") 
land <- ne_download(type = "land", category = 'physical', returnclass = "sf") 

# find intersections with disparity
land_intersection <- st_intersection(disparity,land)
ocean_intersection <- st_intersection(disparity,ocean)

# matchup column order
ocean_intersection <- ocean_intersection %>% dplyr::select(BCE,anomaly,ratio,disparity,featurecla,scalerank,min_zoom,geometry)

colnames(land_intersection)
colnames(ocean_intersection)

earth <- rbind(land_intersection,ocean_intersection)
save.image("~/Desktop/climate/climate_disp_2019-master/data_output/intersection_result.RData")

# extract quantiles -------------------------------------------------------
load("~/Desktop/climate/climate_disp_2019-master/data_output/intersection_result.RData")
## TERRESTRIAL BIOME
#intersection_biome %>% group_by(BIOME) %>% mutate(median_disp = median(disparity)) %>% View() # test
# calculate quantiles 
intersection_biome_quantiles <- intersection_biome %>% group_by(BIOME) %>% 
  summarise(quant10_disp = quantile(disparity, 0.10, na.rm = TRUE), 
            quant50_disp = quantile(disparity, 0.50, na.rm = TRUE), 
            quant90_disp = quantile(disparity, 0.90, na.rm = TRUE)) 
# convert to tibble
intersection_biome_quantiles <- intersection_biome_quantiles %>% st_set_geometry(NULL) # drop geometry


## MARINE REALM
# calculate quantiles 
intersection_realm_quantiles <- intersection_realm %>% group_by(REALM) %>% 
  summarise(quant10_disp = quantile(disparity, 0.10, na.rm = TRUE), 
            quant50_disp = quantile(disparity, 0.50, na.rm = TRUE), 
            quant90_disp = quantile(disparity, 0.90, na.rm = TRUE)) 
# convert to tibble
intersection_realm_quantiles <- intersection_realm_quantiles %>% st_set_geometry(NULL) # drop geometry

## Land vs Sea
# calculate quantiles 
intersection_earth_quantiles <- earth %>% group_by(featurecla) %>% 
  summarise(quant10_disp = quantile(disparity, 0.10, na.rm = TRUE), 
            quant50_disp = quantile(disparity, 0.50, na.rm = TRUE), 
            quant90_disp = quantile(disparity, 0.90, na.rm = TRUE)) 
# convert to tibble
intersection_earth_quantiles <- intersection_earth_quantiles %>% st_set_geometry(NULL) # drop geometry

# Political boundaries
## US STATES
# calculate quantiles 
intersection_states_quantiles <- intersection_states %>% group_by(name) %>% 
  summarise(quant10_disp = quantile(disparity, 0.10, na.rm = TRUE), 
            quant50_disp = quantile(disparity, 0.50, na.rm = TRUE), 
            quant90_disp = quantile(disparity, 0.90, na.rm = TRUE)) 
# convert to tibble
intersection_states_quantiles <- intersection_states_quantiles %>% st_set_geometry(NULL) # drop geometry


# GLOBAL SUBREGIONS
# calculate quantiles 
intersection_subr_quantiles <- intersection_world %>% group_by(subregion) %>% 
  summarise(quant10_disp = quantile(disparity, 0.10, na.rm = TRUE), 
            quant50_disp = quantile(disparity, 0.50, na.rm = TRUE), 
            quant90_disp = quantile(disparity, 0.90, na.rm = TRUE)) 
# convert to tibble
intersection_subr_quantiles <- intersection_subr_quantiles %>% st_set_geometry(NULL) # drop geometry

# COUNTRIES
# calculate quantiles 
intersection_country_quantiles <- intersection_world %>% group_by(geounit) %>% 
  summarise(quant10_disp = quantile(disparity, 0.10, na.rm = TRUE), 
            quant50_disp = quantile(disparity, 0.50, na.rm = TRUE), 
            quant90_disp = quantile(disparity, 0.90, na.rm = TRUE)) 
# convert to tibble
intersection_country_quantiles <- intersection_country_quantiles %>% st_set_geometry(NULL) # drop geometry

# visualizations of quantile ranking --------------------------------------
### BIOMES

#### combine marine and terrestrial into one
#rename columns so ecoregion names match
colnames(intersection_realm_quantiles)[colnames(intersection_realm_quantiles)=="REALM"] <- "BIOME"

# create new column for land or sea
intersection_realm_quantiles$ecoregion <- "ocean"
intersection_biome_quantiles$ecoregion <- "land"

#merge into new df
intersection_allbiomes <- rbind(intersection_realm_quantiles, intersection_biome_quantiles)

#plot
intersection_allbiomes %>% 
  mutate(BIOME = fct_reorder(BIOME, quant50_disp)) %>% #I think they shuold be ordered by median (krt)
  ggplot()+
  geom_segment(aes(x = BIOME, xend = BIOME, y = quant10_disp, yend = quant90_disp), color = "lightgrey")+
  # geom_point(aes(x = BIOME, y = quant10_disp), color = "black")+
  # geom_point(aes(x = BIOME, y = quant50_disp, color = ecoregion))+
  # geom_point(aes(x = BIOME, y = quant90_disp), color = "darkgrey")+
  geom_point(aes(x = BIOME, y = quant10_disp, color = ecoregion))+
  geom_point(aes(x = BIOME, y = quant50_disp, color = ecoregion))+
  geom_point(aes(x = BIOME, y = quant90_disp, color = ecoregion))+
  scale_color_manual(values = c("orange", "darkturquoise"))+
  coord_flip()+
  xlab("biome")+
  ylab("disparity quantiles (10/50/90)")+
  # theme_themeo() + 
  theme_pubr()


### do marine and terrestrial separately as well
#### Terrestrial biome
terr_biome_rank <- intersection_biome_quantiles %>% 
  mutate(BIOME = fct_reorder(BIOME, quant10_disp)) %>% 
  ggplot()+
  geom_segment(aes(x = BIOME, xend = BIOME, y = quant10_disp, yend = quant90_disp), color = "lightgrey")+
  geom_point(aes(x = BIOME, y = quant10_disp),color = "black")+
  geom_point(aes(x = BIOME, y = quant50_disp),color = "#41ae76")+
  geom_point(aes(x = BIOME, y = quant90_disp),color = "darkgrey")+
  coord_flip()+
  xlab("biome")+
  ylab("disparity quantiles (10/50/90)")+
  # theme_themeo() + 
  theme_pubr()

#### marine realm
marine_biome_rank <- intersection_realm_quantiles %>% 
  mutate(BIOME = fct_reorder(BIOME, quant50_disp)) %>% 
  ggplot()+
  geom_segment(aes(x = BIOME, xend = BIOME, y = quant10_disp, yend = quant90_disp), color = "lightgrey")+
  geom_point(aes(x = BIOME, y = quant10_disp),color = "black")+
  geom_point(aes(x = BIOME, y = quant50_disp),color = "#6baed6")+
  geom_point(aes(x = BIOME, y = quant90_disp),color = "darkgrey")+
  coord_flip()+
  xlab("biome")+
  ylab("disparity quantiles (10/50/90)")+
  theme_themeo()

# POLITICAL BOUNDARIES
#### subregion
subr_rank <- intersection_subr_quantiles %>% 
  mutate(subregion = fct_reorder(subregion, quant10_disp)) %>% 
  ggplot()+
  geom_segment(aes(x = subregion, xend = subregion, y = quant10_disp, yend = quant90_disp), color = "lightgrey")+
  geom_point(aes(x = subregion, y = quant10_disp), color = "black")+
  geom_point(aes(x = subregion, y = quant50_disp), color = "#8c96c6")+
  geom_point(aes(x = subregion, y = quant90_disp), color = "darkgrey")+
  xlab("global subregion")+
  ylab("disparity quantiles (10/50/90)")+
  coord_flip()+
  theme_themeo()

#### country
country_rank <- intersection_country_quantiles %>% 
  mutate(geounit = fct_reorder(geounit, quant10_disp)) %>% 
  ggplot()+
  geom_segment(aes(x = geounit, xend = geounit, y = quant10_disp, yend = quant90_disp), color = "lightgrey")+
  geom_point(aes(x = geounit, y = quant10_disp), color = "black")+
  geom_point(aes(x = geounit, y = quant50_disp), color = "#fc8d59")+
  geom_point(aes(x = geounit, y = quant90_disp), color = "darkgrey")+
  xlab("country")+
  ylab("disparity quantiles (10/50/90)")+
  coord_flip()+
  theme_themeo()


#### US state
states_rank <- intersection_states_quantiles %>% 
  mutate(name = fct_reorder(name, quant10_disp)) %>% 
  ggplot()+
  geom_segment(aes(x = name, xend = name, y = quant10_disp, yend = quant90_disp), color = "lightgrey")+
  geom_point(aes(x = name, y = quant10_disp), color = "black")+
  geom_point(aes(x = name, y = quant50_disp), color = "#df65b0")+
  geom_point(aes(x = name, y = quant90_disp), color = "darkgrey")+
  xlab("state")+
  ylab("disparity quantiles (10/50/90)")+
  coord_flip()+
  theme_themeo()

# export csvs of quantile info for later analysis
write.csv(intersection_states_quantiles, "data_output/intersection_states_quantiles.csv")
write.csv(intersection_country_quantiles, "data_output/intersection_country_quantiles.csv")
write.csv(intersection_subr_quantiles, "data_output/intersection_subr_quantiles.csv")
write.csv(intersection_realm_quantiles, "data_output/intersection_realm_quantiles.csv")
write.csv(intersection_biome_quantiles, "data_output/intersection_biome_quantiles.csv")


######################################################### make new maps with fill = quant10
library(RColorBrewer)
##### terrestrial biome
# merge to bring in quantiles
intersection_biome <- left_join(intersection_biome, intersection_biome_quantiles, by = "BIOME")
shape <- left_join(shape, intersection_biome_quantiles, by = "BIOME")

orders_biome <- fct_reorder(intersection_biome$BIOME,intersection_biome$quant10_disp) %>% levels()
#colors_for_joy <- colorRampPalette(brewer.pal(8,"Spectral"))(20) %>% sample()

terr_biome_map <- shape %>% 
  mutate(BIOME = fct_relevel(BIOME, orders_biome %>% rev())) %>% 
  ggplot() +
  geom_sf(aes(group = BIOME, 
              fill = quant10_disp),
          color = "NA", show.legend = T)+
  scale_fill_distiller(palette = "BuGn")+
  theme_void() + 
  theme(panel.grid.major = element_line(colour = "white")) 

gridExtra::grid.arrange(terr_biome_map,
                        terr_biome_rank, 
                        ncol = 1)

##### marine realm
intersection_realm_quantiles$REALM <- intersection_realm_quantiles$BIOME #add realm back in for joining

# merge to bring in quantiles
intersection_realm <- left_join(intersection_realm, intersection_realm_quantiles, by = "REALM")
shape_MEOW <- left_join(shape_MEOW, intersection_realm_quantiles, by = "REALM")

orders_realm <- fct_reorder(intersection_realm$REALM,intersection_realm$quant10_disp) %>% levels()
#colors_for_joy <- colorRampPalette(brewer.pal(8,"Spectral"))(length(orders_from_joyplot_realm)) %>% sample()

marine_biome_map <- shape_MEOW %>% 
  mutate(REALM = fct_relevel(REALM,orders_realm %>% rev())) %>% 
  ggplot() +
  geom_sf( aes(group = REALM, fill = quant10_disp )
           ,color = "NA"
           ,show.legend = T )+
  scale_fill_distiller(palette = "Blues")+
  theme_void()+
  theme(panel.grid.major = element_line(colour = "white"))

gridExtra::grid.arrange(marine_biome_map,
                        marine_biome_rank, 
                        ncol = 1)

##### SUBREGION 
# merge to bring in quantiles
intersection_subr<- left_join(intersection_world, 
                              intersection_subr_quantiles, 
                              by = "subregion")

shape_subr <- left_join(shape_WORLD, 
                        intersection_subr_quantiles, 
                        by = "subregion")

orders_subr <- fct_reorder(intersection_subr$subregion,
                           intersection_subr$quant10_disp) %>% levels()
#colors_for_joy <- colorRampPalette(brewer.pal(8,"Spectral"))(24) %>% sample()

subr_map <- shape_subr %>% 
  mutate(subregion = fct_relevel(subregion,orders_subr %>% rev())) %>% 
  ggplot() +
  geom_sf( aes(group = subregion, 
               fill =  quant10_disp),
           color = "NA", show.legend = T)+
  scale_fill_distiller(palette = "BuPu")+
  theme_void()+
  theme(panel.grid.major = element_line(colour = "white"))

gridExtra::grid.arrange(subr_map, subr_rank, ncol = 1)

##### COUNTRY 
# merge to bring in quantiles
intersection_country<- left_join(intersection_world, intersection_country_quantiles, by = "geounit")
shape_country <- left_join(shape_WORLD, intersection_country_quantiles, by = "geounit")

orders_country <- fct_reorder(intersection_country$geounit,intersection_country$quant10_disp) %>% levels()
#colors_for_joy <- colorRampPalette(brewer.pal(8,"Spectral"))(241) %>% sample()

country_map <- shape_country %>% 
  mutate(geounit = fct_relevel(geounit,orders_country  %>% rev())) %>% 
  ggplot() +
  geom_sf( aes(group = geounit, fill =  quant10_disp)
           ,color = "NA", show.legend = T)+
  scale_fill_distiller(palette = "OrRd")+
  theme_void()+
  theme(panel.grid.major = element_line(colour = "white"))

gridExtra::grid.arrange(country_map, country_rank, ncol = 1)

###### STATES
# merge to bring in quantiles
intersection_states<- left_join(intersection_states, intersection_states_quantiles, by = "name")
states <- left_join(states, intersection_states_quantiles, by = "name")

orders_states  <- fct_reorder(intersection_states$name,intersection_states$quant10_disp) %>% levels()
#colors_for_joy <- colorRampPalette(brewer.pal(8,"Spectral"))(51) %>% sample()

states_map <- states %>% 
  mutate(subregion = fct_relevel(name,orders_states  %>% rev())) %>% 
  ggplot() +
  geom_sf( aes(group = name, fill =  quant10_disp)
           ,color = "NA", show.legend = T)+
  scale_fill_distiller(palette = "PuRd")+
  theme_void()+
  theme(panel.grid.major = element_line(colour = "white"))

gridExtra::grid.arrange(states_map, states_rank, ncol = 1)

#### this one will be weird
### OCEAN VS LAND

# merge to bring in quantiles
earth<- left_join(earth, intersection_earth_quantiles, by = "featurecla")

orders_earth <- fct_reorder(intersection_earth_quantiles$featurecla, intersection_earth_quantiles$quant10_disp) %>% levels()
#colors_for_joy <- colorRampPalette(brewer.pal(8,"Spectral"))(2) %>% sample()

earth %>% 
  #land %>% 
  #filter(BIOME == "Artic_North") %>% 
  mutate(subregion = fct_relevel(featurecla,orders_earth %>% rev())) %>% 
  ggplot() +
  geom_sf( aes(group = featurecla, fill =  quant10_disp),
           color = "white",
           size = .25,
           show.legend = F)+
  # scale_fill_manual(values = c("#1a9850", "#4575b4"))+
  theme_void()+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  
  theme(panel.grid.major = element_line(colour = NA),
        panel.background = element_rect(fill = '#4575b4', colour = '#4575b4'))



