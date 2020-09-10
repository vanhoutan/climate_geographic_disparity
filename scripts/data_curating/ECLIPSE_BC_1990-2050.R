# load libraries
library(raster) # raster manipulation
library(tidyverse)  # data tidying
library(ncdf4)
library(colorRamps)
library(mapdata)

source("/Users/ktanaka/Google Drive/R/misc/color palette function.R")
steps = c("blue", "white", "red")
change = color.palette(steps, space="rgb")

setwd("~/Desktop/climate/data/BC/Eclipse_V5a/ECLIPSE_V5a_baseline_CLE")
df <- stack('ECLIPSE_base_CLE_V5a_BC.nc') #%>% rotate()

png("/Users/ktanaka/Desktop/Eclipse_V5a_Baseline_BCE_1990-2050.png", res = 500, height = 10, width = 10, units = "in")

par(mfrow = c(3,2))

#historic (1990-2020)
start_date <- df$X1990 @data @band
end_date <- df$X2020 @data @band
present_mean <- mean(df[[start_date:end_date]])
plot(present_mean, col = matlab.like(100), zlim = c(0,1.451719), main = "mean BCE 1990-2020 (Kt/year)"); map(add = T)
plot(log10(present_mean), col = matlab.like(100), zlim = c(-8,0), main = "mean BCE 1990-2020 log10(Kt/year)"); map(add = T)

#future
start_date <- df$X2020 @data @band
end_date <- df$X2050 @data @band
future_mean <- mean(df[[start_date:end_date]])
plot(future_mean, col = matlab.like(100), zlim = c(0,1.451719), main = "mean BCE 2020-2050 (Kt/year)"); map(add = T)
plot(log10(future_mean), col = matlab.like(100), zlim = c(-8,0), main = "mean BCE 2020-2050 log10(Kt/year)"); map(add = T)

#future-historic
diff = future_mean-present_mean
plot(diff, col = change(100), main = "Future-historic difference (Kt/year)", zlim = c(-0.3605889, 0.3605889)); map(add = T)
plot(log10(diff), col = change(100), main = "Future-historic difference log10(Kt/year)"); map(add = T)

dev.off()
