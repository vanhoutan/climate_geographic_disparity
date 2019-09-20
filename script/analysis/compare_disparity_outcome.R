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

# worldwide country polygon
world <- ne_countries(scale = "medium", returnclass = "sf")

ts_BCE <- raster("/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/causes/GHG_ts_products/BCEMAN_sum_Jan 31.grd")

rcp = c("rcp26", "rcp45", "rcp85")

value = c("absolute", "original")

scale = c("normalized", "raw")[2]

type = c("xy", "map")

disp = function(rcp, value, scale, type){
  
  scenario = rcp
  value = value
  scale = scale
  type = type
  
  # select climate anomaly data, pick rcp scenario
  anomaly <- stack("/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/impacts/temperature/anomally_products/MPI_ESM_future_anom_Jan 31.grd")[[paste0("MPI_", scenario)]]
  
  anomaly <- spatial_sync_raster(anomaly,ts_BCE, method = "ngb", size_only = F, verbose = T)
  
  if (value == "absolute") anomaly <- abs(anomaly) # make absolute or not absolute anomally
  
  raw_ratio <- 
    stack(ts_BCE, anomaly) %>% 
    rasterToPoints() %>% 
    data.frame() %>% 
    mutate(BCE = .[[3]], anomaly =.[[4]]) %>% 
    mutate(BCE = DescTools::Winsorize(BCE, probs = c(0,.99), na.rm = T),
           anomaly = DescTools::Winsorize(anomaly, probs = c(0,.99), na.rm = T)) %>% 
    dplyr::select(x, y, BCE, anomaly) %>% 
    mutate(ratio = BCE / anomaly) 
  
  rise <- (max(raw_ratio$anomaly, na.rm = T) - min(raw_ratio$anomaly, na.rm = T) )
  run <- (max(raw_ratio$BCE, na.rm = T)  - min(raw_ratio$BCE, na.rm = T))
  slope = rise / run
  
  raw_ratio$disparity <- (raw_ratio$anomaly - (slope*raw_ratio$BCE + min(raw_ratio$anomaly, na.rm = T)))/(sqrt((slope^2)-(1^2)))
  
  disparity <- st_as_sf(x = raw_ratio,
                        coords = c("x", "y"),
                        crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" )
  
  # input x y plot
  
  if (scale == "normalized") {
    
    raw_ratio$disparity = scale(raw_ratio$disparity)
    
    if (type == "xy") {
      
      p <- ggplot(raw_ratio %>% sample_frac(.2))+
        geom_point(aes(x = BCE, 
                       y = anomaly, 
                       fill = disparity),
                   size = 4, 
                   alpha = .6, 
                   shape = 21,
                   show.legend = F) +
        geom_abline(intercept = min(raw_ratio$anomaly, na.rm = T), slope = slope) +
        scale_fill_gradient2(
          limits = c(max(abs(raw_ratio$disparity), na.rm = T)*-1,
                     max(abs(raw_ratio$disparity), na.rm = T)),
          low = "Red2",
          high = "Black",
          name = paste0("Disparity_", rcp, "_", scale))+
        scale_x_continuous(expand = c(0,0))+
        scale_y_continuous(expand = c(0,0))+
        coord_fixed(ratio = 1/slope)+
        theme_classic(I(20)) + 
        theme(legend.position = "top")  
        
      # spatial plot
    }
    
    if (type == "map") {
      p <- ggplot(raw_ratio) +
        geom_tile(aes(x = x, y = y, 
                      fill = disparity), 
                  show.legend = T) +
        geom_sf(data = world, fill = NA, size = .25, color = "black") +
        scale_fill_gradient2(limits = c(max(abs(raw_ratio$disparity), na.rm = T)*-1,max(abs(raw_ratio$disparity), na.rm = T)),
                             low = "Red2", high = "Black", name = "Normalized disparity") +
        coord_sf() +
        scale_x_continuous(expand = c(-0.005, 0)) +
        scale_y_continuous(expand = c(0,0)) +
        theme_classic(I(20)) + 
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(), 
              legend.position = "bottom") 
    }
    
    
  } else {
    
    if (type == "xy") {
      
      p <-
        ggplot(raw_ratio %>% sample_frac(.2))+
        geom_point(aes(x = BCE, 
                       y = anomaly, 
                       fill = disparity),
                   size = 4, 
                   alpha = .6, 
                   shape = 21,
                   show.legend = F) +
        geom_abline(intercept = min(raw_ratio$anomaly, na.rm = T), slope = slope) +
        scale_fill_gradient2(
          limits = c(max(abs(raw_ratio$disparity), na.rm = T)*-1,max(abs(raw_ratio$disparity), na.rm = T)),
          low = "Red2",
          high = "Black",
          name = "Disparity")+
        scale_x_continuous(expand = c(0,0))+
        scale_y_continuous(expand = c(0,0))+
        coord_fixed(ratio = 1/slope)+
        theme_classic(I(20))
    }
    
    if (type == "map") {
      
      p <-
        ggplot(raw_ratio) +
        geom_tile(aes(x = x, y = y, 
                      fill = disparity), 
                  show.legend = T) +
        geom_sf(data = world, fill = NA, size = .25, color = "black") +
        scale_fill_gradient2(limits = c(max(abs(raw_ratio$disparity), na.rm = T)*-1,max(abs(raw_ratio$disparity), na.rm = T)),
                             low = "Red2", high = "Black", name = "Disparity") +
        coord_sf() +
        scale_x_continuous(expand = c(-0.005, 0)) +
        scale_y_continuous(expand = c(0,0)) +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(), 
              legend.position = "top")
      
      
    }
  }
  
  return(p)
}

p1 = disp("rcp26", "absolute", "raw", "xy")
p2 = disp("rcp45", "absolute", "raw", "xy")
p3 = disp("rcp85", "absolute", "raw", "xy")
p4 = disp("rcp26", "original", "raw", "xy")
p5 = disp("rcp45", "original", "raw", "xy")
p6 = disp("rcp85", "original", "raw", "xy")

png("Disparity_xyplots_comparison.png", res = 100, height = 15, width = 20, units = "in")
gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 3)
dev.off()

