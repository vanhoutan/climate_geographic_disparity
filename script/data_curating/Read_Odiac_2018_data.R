# load libraries
library(raster) # raster manipulation
library(tidyverse)  # data tidying
library(ncdf4)
library(colorRamps)
library(mapdata)
library(ncdf4)
library(tiff)
library(quickPlot)

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
load("/Users/ktanaka/Desktop/ODIAC_CO2_2000-2017.RData")

c = mean(odiac)

png("/Users/ktanaka/Desktop/Odiac_2018_CO2_2000-2017.png", res = 500, height = 6, width = 10, units = "in")
# par(mfrow = c(2,1), mar = c(2,2,4,4))
plot(log10(c), col = matlab.like(100), main = "Mean global fossil fuel CO2 emission distribution from ODIAC 2000-2017 ln(1000 tons C cell-1)")
map(add = T)
# plot(mean(odiac), col = matlab.like(100));map(add = T)

dev.off()

#1km_tiff, too large, no ocean coverage
d = raster("/Users/ktanaka/Desktop/Odiac_2018/1km_tiff/2000/odiac2018_1km_excl_intl_0001.tif")
plot(log(d+1),col = matlab.like(100), pch = ".");map(add = T)
Plot(d, col = matlab.like(10), speedup = 10, new = T)
