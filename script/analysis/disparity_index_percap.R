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

##############################
###  Popular ggPlot theme  ###
##############################
theme_themeo <- function () { 
  theme_classic()+
    theme(strip.background = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_text(margin = margin( 0.2, unit = "cm")),
          axis.text.y = element_text(margin = margin(c(1, 0.2), unit = "cm")),
          axis.ticks.length=unit(-0.1, "cm"),
          panel.border = element_rect(colour = "black", fill=NA, size=.5),
          legend.title=element_blank(),
          strip.text=element_text(hjust=0) )}

# I'm navigating the options of developing a ratio of emissions to anomaly.
# I think we'll get closer if we clearly define what it is that we want as a final product. Rather
# than develop a bunch of products that are not quite what we want. 
# 
# What do we want?
# 1. We want to define what regions are contributing emissions to climate change but do not expect to 
# see much. 
# 2. And in contrast, what areas will see lots of climate change, but are not contributing lots of
# emissions? 
# 3. And in between, what areas generate some emissions and will see some change. Or, wont see
# change and are not emitting emissions.

# What would this look like in the ideal ratio?
# 1. Areas that generate lots of emissions and wont see much change will be > 1
# 2. Areas that generate little emissions but will see lots of change will be < 1
# 3. Areas seeing some change and emit some will be close to 1


# What are the tools, transformations, and questions to consider when developing this metric?
# 1. Are negative anomalies a good thing? Or is any anomally a bad thing? 
# 2. scaling: 0-1, 2-3, z-score
# 3. winsorizing for visualization

# worldwide country polygon
world <- ne_countries(scale = "medium", returnclass = "sf")

# black carbon emissions
ts_BCE <- raster("/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/causes/GHG_ts_products/BCEMAN_sum_Jan 31.grd") #%>% plot()

# climate anomaly data
anomaly <- stack("/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data//impacts/temperature/anomally_products/MPI_ESM_future_anom_Jan 31.grd")[["MPI_rcp85"]]
anomaly <- spatial_sync_raster(anomaly,ts_BCE,method = "ngb", size_only = FALSE, verbose = T)
anomaly <- abs(anomaly) # make absolute or not absolute anomally

# population data
gpw_pop <- stack("/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/gpw-v4-population-density-rev10_totpop_30_min_nc/gpw_v4_e_atotpopbt_dens_30_min.nc",
                 varname = "Population Density, v4.10 (2000, 2005, 2010, 2015, 2020): 30 arc-minutes")
gpw_pop <- gpw_pop[["X5"]]
gpw_pop <- reclassify(gpw_pop,rcl = c(NA,NA,0))
gpw_pop <- spatial_sync_raster(gpw_pop,ts_BCE,method = "ngb", size_only = FALSE, verbose = T)
gpw_pop <- gpw_pop + 1#0.00000001
plot(log(gpw_pop), col = matlab.like(100))

# black carbon anthropogenic emissions data
ts_BCE_sum <- ts_BCE
BCE_sum <- ts_BCE_sum+0.0000000001

BCE_adj_for_pop <- BCE_sum/gpw_pop

plot(log(gpw_pop),  col = matlab.like(100))
plot(ts_BCE,  col = matlab.like(100))
plot(BCE_sum,  col = matlab.like(100))
plot(BCE_adj_for_pop,  col = matlab.like(100))


raw_ratio <- 
  stack(BCE_adj_for_pop,anomaly) %>% 
  rasterToPoints() %>% 
  data.frame() %>% 
  mutate(BCE = .[[3]],
         anomaly =.[[4]]) %>% 
  
  mutate(BCE = DescTools::Winsorize(BCE, probs = c(0,.99), na.rm = T),
         anomaly = DescTools::Winsorize(anomaly, probs = c(0,.99), na.rm = T)) %>% 
  
  dplyr::select(x,y,BCE,anomaly) %>% 
  mutate(ratio = BCE / anomaly) 

# rise
rise <- (max(raw_ratio$anomaly, na.rm = T) - min(raw_ratio$anomaly, na.rm = T) )
#run
run <- (max(raw_ratio$BCE, na.rm = T)  - min(raw_ratio$BCE, na.rm = T))
# slope
slope = rise / run


###################################
# convert disparity to point file #
###################################
# calculate distance from 1 to 1 line.
#raw_ratio$disparity <- raw_ratio$anomaly - (slope*raw_ratio$BCE + min(raw_ratio$anomaly, na.rm = T) )

# calculate corrected 1 to 1 distance
#raw_ratio$disparity <- sqrt(abs(raw_ratio$disparity)/2) #don't think this works but keep for now

#### calculate orthogonal distance rather than residual
#raw_ratio$disparity <- -(((slope*raw_ratio$BCE) + (-1*raw_ratio$anomaly + 0))/(sqrt((slope^2)-(1^2)))) #this one works!
raw_ratio$disparity <- (raw_ratio$anomaly - (slope*raw_ratio$BCE + min(raw_ratio$anomaly, na.rm = T)))/(sqrt((slope^2)-(1^2))) # so does this one


#### convert slope intercept to general equation
#y=mx+b
#y=slope*x + 0

#Ax + By  + C = 0
#slope*x - y  + 0 = 0

### use general equation to calculate disparity
#(A*m + B*n + C)/(sqrt(A^2+B^2))
#((slope*raw_ratio$BCE) + (-1*raw_ratio$anomaly + 0))/(sqrt((slope^2)-(1^2)))

#### test that this works with some real numbers
#(-2*5 + 3*1 + 4)/(sqrt(4+9))
#(-2*5 + 3*6 + 4)/(sqrt(4+9))


# turn in to point file to detect overlap with regions of interest
disparity <- st_as_sf(x = raw_ratio, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" )

# input x y plot
xy_plot <-
  ggplot(raw_ratio %>% sample_frac(.2))+
  geom_point(aes(  x = BCE
                   , y = anomaly
                   #, fill = ratio
                   #,fill = ifelse(ratio > 1/slope,1,0) # when inputs are raw
                   ,fill = disparity
                   #,fill = anomaly - (slope*BCE + min(raw_ratio$anomaly, na.rm = T)) 
                   #,size = ratio
                   #,size = anomaly - ((-slope*BCE) + max(raw_ratio$anomaly, na.rm = T) )
                   
  )
  , size = 4
  , alpha = .6
  , shape = 21
  , show.legend = T)+
  
  geom_abline(intercept = min(raw_ratio$anomaly, na.rm = T), slope = slope) +
  #geom_abline(intercept = max(raw_ratio$anomaly, na.rm = T), slope = - slope) +
  scale_fill_gradient2(low = "Red2", high = "Black", name = "disparity")+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  coord_fixed(ratio = 1/slope)+
  theme_themeo()

# spatial plot
map_plot <-
  ggplot(raw_ratio)+
  geom_tile(aes(  x = x
                  , y = y
                  #,fill = ifelse(ratio > 1/slope,1,0) # when inputs are raw
                  # ,fill = anomaly - (slope*BCE + min(raw_ratio$anomaly, na.rm = T)) 
                  #,fill = anomaly - ((-slope*BCE) + max(raw_ratio$anomaly, na.rm = T) )
                  ,fill = disparity
                  
  )
  ,show.legend = F
  )+
  
  geom_sf(data = world, fill = NA,size = .25,color = "black")+
  scale_fill_gradient2(low = "Red2", high = "Black", name = "disparity")+
  
  coord_sf() +
  scale_x_continuous(expand = c(-0.005,0))+
  scale_y_continuous(expand = c(0,0)) +
  
  theme_themeo() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

gridExtra::grid.arrange(xy_plot,map_plot, ncol = 1)


# plotting inputs
hist_of_BCE <-
  ggplot(raw_ratio)+geom_histogram(aes(BCE))

map_plotBCE <-
  ggplot(raw_ratio)+
  geom_raster(aes(  x = x
                    , y = y
                    ,fill = BCE # when inputs are raw
  ),show.legend = T)+
  
  geom_sf(data = world, fill = NA,size = .1,color = "black")+
  scale_fill_gradientn(colours = c("white", "Red2",  "Black"))+
  coord_sf()


hist_of_anom <-
  ggplot(raw_ratio)+geom_histogram(aes(anomaly))

map_plotANOM <-
  ggplot(raw_ratio)+
  geom_raster(aes(  x = x
                    , y = y
                    ,fill = anomaly # when inputs are raw
                    #,fill = anomaly - (slope*BCE)
                    
  ), show.legend = T)+
  
  geom_sf(data = world, fill = NA,size = .1,color = "black")+
  scale_fill_gradientn(colours = c( "white","Red2"))+
  coord_sf()

gridExtra::grid.arrange(map_plotBCE,hist_of_BCE, map_plotANOM,hist_of_anom)
gridExtra::grid.arrange(map_plotBCE, map_plotANOM)

########################################### 
###   Read in Terrestrial biomes shapefile  ###
###########################################
shape <- readOGR(dsn = "/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/summarization/TEOW", layer = "wwf_terr_ecos")  # read the shapefile in by name not the lack of .shp extension

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
intersection <- st_intersection(disparity,shape)

orders_from_joyplot <- fct_reorder(intersection$BIOME,intersection$disparity, .fun = mean) %>% levels()
colors_for_joy <- colorRampPalette(brewer.pal(8,"Spectral"))(20) %>% sample()

# box plot
MEOW_joy <- intersection %>%
  dplyr::select(disparity, BIOME) %>% 
  mutate(BIOME = fct_relevel(BIOME,orders_from_joyplot %>% rev())) %>% 
  ggplot()+
  geom_histogram(aes(x = disparity, fill = BIOME), show.legend = F, bins = 70) +
  geom_vline(xintercept = 0, lty = "dashed", size = .5)+
  scale_fill_manual(values = colors_for_joy)+
  facet_wrap(~BIOME,ncol = 3, scale = "free_y")+
  theme_minimal()+
  theme(axis.title.y = element_blank())


meow_disparity <- ggplot() +
  geom_raster(data = raw_ratio, aes(x = x, y = y, fill = disparity), show.legend = F)+
  #geom_sf(data = shape, aes(group = BIOME, fill = BIOME),
  #         fill = "black", alpha = .5, color = "NA", show.legend = F, size = .01
  #        )+
  scale_fill_gradient2(low = "Red2", high = "Black")+
  coord_fixed()+
  theme_void()

gridExtra::grid.arrange(meow_disparity,MEOW_joy)


terre_biome <- shape %>% 
  #filter(BIOME == "Artic_North") %>% 
  mutate(BIOME = fct_relevel(BIOME,orders_from_joyplot %>% rev())) %>% 
  ggplot() +
  geom_sf( aes(group = BIOME, fill = BIOME)
           ,color = "NA", show.legend = F)+
  scale_fill_manual(values = colors_for_joy)+
  theme_void()+
  theme(panel.grid.major = element_line(colour = "white"))

gridExtra::grid.arrange(terre_biome,MEOW_joy)




########################################### 
###   Read in Marine biomes shapefile  ###
###########################################
#shape <- readOGR(dsn = "./data/summarization/MEOW", layer = "meow_ecos")      # read the shapefile in by name not the lack of .shp extension
shape_MEOW <- readOGR(dsn = "/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/summarization/MEOW_2", layer = "WCMC-036-MEOW-PPOW-2007-2012-NoCoast")  # read the shapefile in by name not the lack of .shp extension

# simplify shapefile (saves computing time)
shape_MEOW <- ms_simplify(shape_MEOW, keep = 0.05, keep_shapes = F)
shape_MEOW <- shape_MEOW %>% st_as_sf()  
shape$REALM <- as.factor(shape$REALM)

# clip out marine ecoregions overlapping on land
land <- ne_download(type = "land", category = 'physical', returnclass = "sf") 
shape_MEOW <- st_difference(shape_MEOW, st_union(land))


# find intersections with disparity
intersection <- st_intersection(disparity,shape_MEOW)

# MEOW_joy <- intersection %>%
#   dplyr::select(disparity, REALM) %>% 
#   ggplot()+
#   geom_density_ridges_gradient(aes(x = disparity,y = fct_reorder(REALM,disparity, .fun = mean), fill = ..x..)
#                                , rel_min_height = 0.01, scale = 2, bandwidth = .25, show.legend = F) +
#   #stat_binline(aes(x = disparity,y = fct_reorder(BIOME,disparity, .fun = mean)), binwidth = binwidth, fill = rainbow(n_bins)) +
#   
#   geom_vline(xintercept = 0, lty = "dashed")+
#   scale_fill_gradient2(low = "Red2", high = "Black")+
#   theme_minimal()+
#   theme(axis.title.y = element_blank())

#orders_from_joyplot <- fct_reorder(intersection$REALM,intersection$disparity, .fun = mean) %>% levels()

# playing with alt rankings
orders_from_joyplot <- fct_reorder(intersection$REALM,intersection$disparity, .fun = mean) %>% levels()

intersection %>% 
  group_by(REALM) %>% 
  summarise(disp_sum = sum(disparity, na.rm = T),
            cell_num = n()) %>% 
  mutate(ratio = disp_sum / cell_num) %>% 
  arrange(disp_sum)


colors_for_joy <- colorRampPalette(brewer.pal(8,"Spectral"))(length(orders_from_joyplot)) %>% sample()

# box plot
MEOW_joy <- intersection %>%
  dplyr::select(disparity, REALM) %>% 
  mutate(REALM = fct_relevel(REALM,orders_from_joyplot %>% rev())) %>% 
  ggplot()+
  #annotate("rect", xmin = -15, xmax = 0, ymin = -50, ymax = 1500, fill = "red4", alpha = .5)+
  geom_histogram(aes(x = disparity, fill = REALM), show.legend = F, bins = 70) +
  geom_vline(xintercept = 0, lty = "dashed", size = .5)+
  #scale_fill_gradient2(low = "Red2", high = "Black")+
  scale_fill_manual(values = colors_for_joy)+
  facet_wrap(~REALM,ncol = 4, scales = "free_y")+
  theme_minimal()+
  theme(axis.title.y = element_blank()
  )

marine_realm <- shape_MEOW %>% 
  #filter(BIOME == "Artic_North") %>% 
  mutate(REALM = fct_relevel(REALM,orders_from_joyplot %>% rev())) %>% 
  ggplot() +
  geom_sf( aes(group = REALM, fill =  REALM)
           ,color = "NA"
           ,show.legend = F
  )+
  scale_fill_manual(values = colors_for_joy )+
  theme_void()+
  theme(panel.grid.major = element_line(colour = "white"))

gridExtra::grid.arrange(marine_realm, MEOW_joy)

########################################### 
###   Merge both ecoregion domains  ###
###########################################
# shape$REALM <- NULL
# colnames(shape)[5] <- "REALM"
# 
# union_REALMS <- st_union(shape_MEOW %>% dplyr::select(REALM), shape %>% dplyr::select(REALM))
# 
# union_REALMS %>% 
#   #filter(BIOME == "Artic_North") %>% 
#   ggplot() +
#   geom_sf( aes(group = REALM, fill = REALM)
#            ,color = "NA")+
#   scale_fill_manual(values = colorRampPalette(brewer.pal(8,"Spectral"))(20) %>% sample() )+
#   theme_void()+
#   theme(panel.grid.major = element_line(colour = "white"))
# 



########################################### 
###   Read in political boundaries shapefile  ###
###########################################
#shape <- readOGR(dsn = "./data/summarization/MEOW", layer = "meow_ecos")      # read the shapefile in by name not the lack of .shp extension
shape_WORLD <- world

# find intersections with disparity
intersection <- st_intersection(disparity,shape_WORLD)

orders_from_joyplot <- fct_reorder(intersection$subregion,intersection$disparity, .fun = mean) %>% levels()
colors_for_joy <- colorRampPalette(brewer.pal(8,"Spectral"))(24) %>% sample()

# box plot
MEOW_joy <- intersection %>%
  dplyr::select(disparity, subregion) %>% 
  mutate(subregion = fct_relevel(subregion,orders_from_joyplot %>% rev())) %>% 
  ggplot()+
  #annotate("rect", xmin = -15, xmax = 0, ymin = -50, ymax = 1500, fill = "red4", alpha = .5)+
  geom_histogram(aes(x = disparity, fill = subregion), show.legend = F, bins = 70) +
  geom_vline(xintercept = 0, lty = "dashed", size = .5)+
  #scale_fill_gradient2(low = "Red2", high = "Black")+
  scale_fill_manual(values = colors_for_joy)+
  facet_wrap(~subregion,ncol = 4, scales = "free_y")+
  theme_minimal()+
  theme(axis.title.y = element_blank()
  )

# lets look at 
# sub_region, continent, region_wb, income_grp
marine_realm <- shape_WORLD %>% 
  #filter(BIOME == "Artic_North") %>% 
  mutate(subregion = fct_relevel(subregion,orders_from_joyplot %>% rev())) %>% 
  ggplot() +
  geom_sf( aes(group = subregion, fill =  subregion)
           ,color = "NA", show.legend = F)+
  scale_fill_manual(values = colors_for_joy )+
  theme_void()+
  theme(panel.grid.major = element_line(colour = "white"))

gridExtra::grid.arrange(marine_realm, MEOW_joy)


####### STATES

states <- ne_states(country = "United States of America", returnclass = "sf")
#plot(states)

# find intersections with disparity
intersection <- st_intersection(disparity,states)

orders_from_joyplot <- fct_reorder(intersection$name,intersection$disparity, .fun = median) %>% levels()
colors_for_joy <- colorRampPalette(brewer.pal(8,"Spectral"))(51) %>% sample()

# box plot
MEOW_joy <- intersection %>%
  dplyr::select(disparity, name) %>% 
  mutate(name = fct_relevel(name,orders_from_joyplot %>% rev())) %>% 
  ggplot()+
  #annotate("rect", xmin = -15, xmax = 0, ymin = -50, ymax = 1500, fill = "red4", alpha = .5)+
  geom_histogram(aes(x = disparity, fill = name), show.legend = F, bins = 25) +
  geom_vline(xintercept = 0, lty = "dashed", size = .5)+
  #scale_fill_gradient2(low = "Red2", high = "Black")+
  scale_fill_manual(values = colors_for_joy)+
  facet_wrap(~name,ncol = 4, scales = "free_y")+
  theme_minimal()+
  theme(axis.title.y = element_blank()
  )

# lets look at 
# sub_region, continent, region_wb, income_grp
marine_realm <- states %>% 
  #filter(BIOME == "Artic_North") %>% 
  mutate(subregion = fct_relevel(name,orders_from_joyplot %>% rev())) %>% 
  ggplot() +
  geom_sf( aes(group = name, fill =  name)
           ,color = "NA", show.legend = F)+
  scale_fill_manual(values = colors_for_joy )+
  theme_void()+
  theme(panel.grid.major = element_line(colour = "white"))

gridExtra::grid.arrange(marine_realm, MEOW_joy)






















#### SCRATCH ####
##################

########################################### 
###   Read in political boundaries shapefile  ###
###########################################
#
ocean <- ne_download(type = "ocean", category = 'physical', returnclass = "sf") 
land <- ne_download(type = "land", category = 'physical', returnclass = "sf") 

plot(land)
plot(ocean)

# find intersections with disparity
land_intersection <- st_intersection(disparity,land)
ocean_intersection <- st_intersection(disparity,ocean)

# matchup column order
ocean_intersection <- ocean_intersection %>% dplyr::select(BCE,anomaly,ratio,disparity,featurecla,scalerank,min_zoom,geometry)

colnames(land_intersection)
colnames(ocean_intersection)

earth <- rbind(land_intersection,ocean_intersection)


orders_from_joyplot <- fct_reorder(intersection$featurecla,intersection$disparity, .fun = mean) %>% levels()
colors_for_joy <- colorRampPalette(brewer.pal(8,"Spectral"))(2) %>% sample()

# box plot
MEOW_joy <- earth %>%
  dplyr::select(disparity, featurecla) %>% 
  mutate(subregion = fct_relevel(featurecla,orders_from_joyplot %>% rev())) %>% 
  ggplot()+
  #annotate("rect", xmin = -15, xmax = 0, ymin = -50, ymax = 1500, fill = "red4", alpha = .5)+
  geom_histogram(aes(x = disparity, fill = featurecla), show.legend = F, bins = 70) +
  geom_vline(xintercept = 0, lty = "dashed", size = .5)+
  #scale_fill_gradient2(low = "Red2", high = "Black")+
  scale_fill_manual(values = c("#1a9850", "#4575b4"))+
  facet_wrap(~featurecla,ncol = 1, scales = "free_y")+
  theme_minimal()+
  theme(axis.title.y = element_blank()
  )

# lets look at 
# sub_region, continent, region_wb, income_grp
marine_realm <- land %>% 
  #filter(BIOME == "Artic_North") %>% 
  mutate(subregion = fct_relevel(featurecla,orders_from_joyplot %>% rev())) %>% 
  ggplot() +
  geom_sf( aes(group = featurecla, fill =  featurecla),
           color = "white",
           size = .25,
           show.legend = F)+
  scale_fill_manual(values = c("#1a9850", "#4575b4"))+
  theme_void()+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  
  theme(panel.grid.major = element_line(colour = NA),
        panel.background = element_rect(fill = '#4575b4', colour = '#4575b4'))

gridExtra::grid.arrange(marine_realm, MEOW_joy)







