rm(list = ls())

scale_x_longitude <- function(xmin=-180, xmax=180, step=1, ...) {
  xbreaks <- seq(xmin,xmax,step)
  xlabels <- unlist(lapply(xbreaks, function(x) ifelse(x < 0, parse(text=paste0(x,"^o", "*W")), ifelse(x > 0, parse(text=paste0(x,"^o", "*E")),x))))
  return(scale_x_continuous("", breaks = xbreaks, labels = xlabels, expand = c(0, 0), ...))
}
scale_y_latitude <- function(ymin=-90, ymax=90, step=0.5, ...) {
  ybreaks <- seq(ymin,ymax,step)
  ylabels <- unlist(lapply(ybreaks, function(x) ifelse(x < 0, parse(text=paste0(x,"^o", "*S")), ifelse(x > 0, parse(text=paste0(x,"^o", "*N")),x))))
  return(scale_y_continuous("", breaks = ybreaks, labels = ylabels, expand = c(0, 0), ...))
}    


# load libraries
library(raster) # raster manipulation
library(tidyverse)  # data tidying
library(ncdf4)
library(colorRamps)
library(mapdata)
library(ncdf4)
library(tiff)
library(quickPlot)
# data processing
library(ggplot2)
# spatial
library(rasterVis)
library(rgdal)

#1deg_ncdf
setwd("~/Desktop/climate/data/CO2/ODIAC/1deg_ncdf/")
odiac <- raster::stack() # build an empty stack to put the desired emission metric in to

for (i in 2000:2017) {
  
  d1 <- stack(paste0('odiac2018_1x1d_', i, '.nc'), varname = "land")
  d2 <- stack(paste0('odiac2018_1x1d_', i, '.nc'), varname = "intl_bunker")
  
  df = stack(d1, d2); rm(d1, d2)
  present_mean <- mean(df)
  
  # plot(present_mean, col = matlab.like(100)); map(add = T)
  # plot(log10(present_mean), col = matlab.like(100), main = i)
  # map(add = T)

  odiac <- stack(odiac, present_mean) # add to the timeseries stack
  
  print(i)
  
}

save(odiac, file = "/Users/ktanaka/Desktop/ODIAC_CO2_2000-2017.RData")
load("/Users/ktanaka/clim_geo_disp/output/ODIAC_CO2_2000-2017_Stacked.RData")

c = mean(odiac)

p1 = gplot(c) + 
  geom_point(aes(color = value)) +
  coord_equal() +
  coord_quickmap(xlim = c(-180, 180),
                 ylim = c(-90, 90)) +
  theme_bw() + 
  borders(xlim = c(-180, 180),
          ylim = c(-180, 180), 
          fill = NA, size = 0.1) +
  scale_x_longitude(xmin = -180, xmax = 180, step = 60) +
  scale_y_latitude(ymin = -180, ymax = 180, step = 60) +
  scale_color_gradientn(colours = c( "black","cyan", "red"), name = "1000 tons C cell-1)/m-2") 

p2 = gplot(c) + 
  geom_point(aes(color = log10(value))) +
  coord_equal() +
  coord_quickmap(xlim = c(-180, 180),
                 ylim = c(-90, 90)) +
  theme_bw() + 
  borders(xlim = c(-180, 180),
          ylim = c(-180, 180), 
          fill = NA, size = 0.1) +
  scale_x_longitude(xmin = -180, xmax = 180, step = 60) +
  scale_y_latitude(ymin = -180, ymax = 180, step = 60) +
  scale_color_gradientn(colours = c( "black","cyan", "red"), name = "log10(1000 tons C cell-1)kg/m-2)") 

pdf("/Users/ktanaka/Dropbox/PAPER climate geographic disparities/figures/supplemental/Odiac_2018_CO2_2000-2017.pdf", height = 4, width = 15)

cowplot::plot_grid(p1, p2, cols = 2)

dev.off()


# par(mfrow = c(2,1), mar = c(2,2,4,4))
plot(log10(c), col = matlab.like(100), main = "Mean global fossil fuel CO2 emission distribution from ODIAC 2000-2017 ln(1000 tons C cell-1)")
map(add = T)
# plot(mean(odiac), col = matlab.like(100));map(add = T)

dev.off()

#1km_tiff, too large, no ocean coverage
d = raster("/Users/ktanaka/Desktop/Odiac_2018/1km_tiff/2000/odiac2018_1km_excl_intl_0001.tif")
plot(log(d+1),col = matlab.like(100), pch = ".");map(add = T)
Plot(d, col = matlab.like(10), speedup = 10, new = T)
