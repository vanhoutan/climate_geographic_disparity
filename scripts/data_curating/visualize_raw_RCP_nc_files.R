
setwd("/Users/ktanaka/clim_geo_disp/data")

library(raster)
library(colorRamps)

rm(list = ls())

rcp45_anom_1 = stack("CMIP5 ENSMN RCP4.5 anomaly (2006-2055)-(1956-2005).nc", varname = "anomaly")
rcp45_anom_2 = stack("CMIP5 ENSMN RCP4.5 anomaly (2050-2099)-(1956-2005).nc", varname = "anomaly")
rcp45_ensem_mean_change_1 = stack("CMIP5 ENSMN RCP4.5 ensemble mean change (2006-2055)-(1956-2005).nc", varname = "anomaly")
rcp45_ensem_mean_change_2 = stack("CMIP5 ENSMN RCP4.5 ensemble mean change (2050-2099)-(1956-2005).nc", varname = "anomaly")
rcp45_ensem_stdanom_1 = stack("CMIP5 ENSMN RCP4.5 ensemble stdanom (2006-2055)-(1956-2005).nc", varname = "anomaly")
rcp45_ensem_stdanom_2 = stack("CMIP5 ENSMN RCP4.5 ensemble stdanom (2050-2099)-(1956-2005).nc", varname = "anomaly")
rcp45_historical_stdanom_1 = stack("CMIP5 ENSMN RCP4.5 historical stdanom (2006-2055)-(1956-2005).nc", varname = "anomaly")
rcp45_historical_stdanom_2 = stack("CMIP5 ENSMN RCP4.5 historical stdanom (2050-2099)-(1956-2005).nc", varname = "anomaly")

rcp85_anom_1 = stack("CMIP5 ENSMN RCP8.5 anomaly (2006-2055)-(1956-2005).nc", varname = "anomaly")
rcp85_anom_2 = stack("CMIP5 ENSMN RCP8.5 anomaly (2050-2099)-(1956-2005).nc", varname = "anomaly")
rcp85_ensem_mean_change_1 = stack("CMIP5 ENSMN RCP8.5 ensemble mean change (2006-2055)-(1956-2005).nc", varname = "anomaly")
rcp85_ensem_mean_change_2 = stack("CMIP5 ENSMN RCP8.5 ensemble mean change (2050-2099)-(1956-2005).nc", varname = "anomaly")
rcp85_ensem_stdanom_1 = stack("CMIP5 ENSMN RCP8.5 ensemble stdanom (2006-2055)-(1956-2005).nc", varname = "anomaly")
rcp85_ensem_stdanom_2 = stack("CMIP5 ENSMN RCP8.5 ensemble stdanom (2050-2099)-(1956-2005).nc", varname = "anomaly")
rcp85_historical_stdanom_1 = stack("CMIP5 ENSMN RCP8.5 historical stdanom (2006-2055)-(1956-2005).nc", varname = "anomaly")
rcp85_historical_stdanom_2 = stack("CMIP5 ENSMN RCP8.5 historical stdanom (2050-2099)-(1956-2005).nc", varname = "anomaly")

rcp_anom = stack(rcp45_anom_1, rcp45_anom_2,
                 rcp85_anom_1, rcp85_anom_2)

names(rcp_anom) = c("RCP4.5_2006_2055", 
                    "RCP4.5_2050_2099",
                    "RCP8.5_2006_2055",
                    "RCP8.5_2050_2099")

rcp_ensem_mean_chang = stack(rcp45_ensem_mean_change_1, rcp45_ensem_mean_change_2,
                             rcp85_ensem_mean_change_1, rcp85_ensem_mean_change_2)

names(rcp_ensem_mean_chang) = c("RCP4.5_2006_2055", 
                                "RCP4.5_2050_2099",
                                "RCP8.5_2006_2055",
                                "RCP8.5_2050_2099")

rcp_ensem_stdanom = stack(rcp45_ensem_stdanom_1, rcp45_ensem_stdanom_2,
                          rcp85_ensem_stdanom_1, rcp85_ensem_stdanom_2)

names(rcp_ensem_stdanom) = c("RCP4.5_2006_2055", 
                             "RCP4.5_2050_2099",
                             "RCP8.5_2006_2055",
                             "RCP8.5_2050_2099")

rcp_historical_stdanom = stack(rcp45_historical_stdanom_1, rcp45_historical_stdanom_2,
                               rcp85_historical_stdanom_1, rcp85_historical_stdanom_2)

names(rcp_historical_stdanom) = c("RCP4.5_2006_2055", 
                                  "RCP4.5_2050_2099",
                                  "RCP8.5_2006_2055",
                                  "RCP8.5_2050_2099")

pdf("~/Desktop/rcp_anom.pdf", width = 11, height = 6)
par(mfrow = c(2,2), mar = c(2,3,3,3)
    # fg = 'white', col.axis = 'white', col.main="white", col.lab = 'white'
    )
plot(raster::rotate(rcp_anom$RCP4.5_2006_2055), main = "RCP4.5 2006-2055", zlim = c(0,11), col = matlab.like(100), axes = F); map(add = T, lwd = 0.5); degAxis(1); degAxis(2, las = 1)
plot(raster::rotate(rcp_anom$RCP4.5_2050_2099), main = "RCP4.5 2050-2099", zlim = c(0,11), col = matlab.like(100), axes = F); map(add = T, lwd = 0.5); degAxis(1); degAxis(2, las = 1)
plot(raster::rotate(rcp_anom$RCP8.5_2006_2055), main = "RCP8.5 2006-2055", zlim = c(0,11), col = matlab.like(100), axes = F); map(add = T, lwd = 0.5); degAxis(1); degAxis(2, las = 1)
plot(raster::rotate(rcp_anom$RCP8.5_2050_2099), main = "RCP8.5 2050-2099", zlim = c(0,11), col = matlab.like(100), axes = F); map(add = T, lwd = 0.5); degAxis(1); degAxis(2, las = 1)
dev.off()

pdf("~/Desktop/rcp_ensem_mean_chang.pdf", width = 11, height = 6)
par(mfrow = c(2,2), mar = c(2,3,3,3)
    # fg = 'white', col.axis = 'white', col.main="white", col.lab = 'white'
)
plot(raster::rotate(rcp_ensem_mean_chang$RCP4.5_2006_2055), main = "RCP4.5 2006-2055", zlim = c(0,11), col = matlab.like(100), axes = F); map(add = T, lwd = 0.5); degAxis(1); degAxis(2, las = 1)
plot(raster::rotate(rcp_ensem_mean_chang$RCP4.5_2050_2099), main = "RCP4.5 2050-2099", zlim = c(0,11), col = matlab.like(100), axes = F); map(add = T, lwd = 0.5); degAxis(1); degAxis(2, las = 1)
plot(raster::rotate(rcp_ensem_mean_chang$RCP8.5_2006_2055), main = "RCP8.5 2006-2055", zlim = c(0,11), col = matlab.like(100), axes = F); map(add = T, lwd = 0.5); degAxis(1); degAxis(2, las = 1)
plot(raster::rotate(rcp_ensem_mean_chang$RCP8.5_2050_2099), main = "RCP8.5 2050-2099", zlim = c(0,11), col = matlab.like(100), axes = F); map(add = T, lwd = 0.5); degAxis(1); degAxis(2, las = 1)

dev.off()

pdf("~/Desktop/rcp_ensem_stdanom.pdf", width = 11, height = 6)
par(mfrow = c(2,2), mar = c(2,3,3,3)
    # fg = 'white', col.axis = 'white', col.main="white", col.lab = 'white'
)
plot(raster::rotate(rcp_ensem_stdanom$RCP4.5_2006_2055), main = "RCP4.5 2006-2055", zlim = c(0,4.7), col = matlab.like(100), axes = F); map(add = T, lwd = 0.5); degAxis(1); degAxis(2, las = 1)
plot(raster::rotate(rcp_ensem_stdanom$RCP4.5_2050_2099), main = "RCP4.5 2050-2099", zlim = c(0,4.7), col = matlab.like(100), axes = F); map(add = T, lwd = 0.5); degAxis(1); degAxis(2, las = 1)
plot(raster::rotate(rcp_ensem_stdanom$RCP8.5_2006_2055), main = "RCP8.5 2006-2055", zlim = c(0,4.7), col = matlab.like(100), axes = F); map(add = T, lwd = 0.5); degAxis(1); degAxis(2, las = 1)
plot(raster::rotate(rcp_ensem_stdanom$RCP8.5_2050_2099), main = "RCP8.5 2050-2099", zlim = c(0,4.7), col = matlab.like(100), axes = F); map(add = T, lwd = 0.5); degAxis(1); degAxis(2, las = 1)
dev.off()

pdf("~/Desktop/rcp_historical_stdanom.pdf", width = 11, height = 6)
par(mfrow = c(2,2), mar = c(2,3,3,3)
    # fg = 'white', col.axis = 'white', col.main="white", col.lab = 'white'
)
plot(raster::rotate(rcp_historical_stdanom$RCP4.5_2006_2055), main = "RCP4.5 2006-2055", zlim = c(0,14.9), col = matlab.like(100), axes = F); map(add = T, lwd = 0.5); degAxis(1); degAxis(2, las = 1)
plot(raster::rotate(rcp_historical_stdanom$RCP4.5_2050_2099), main = "RCP4.5 2050-2099", zlim = c(0,14.9), col = matlab.like(100), axes = F); map(add = T, lwd = 0.5); degAxis(1); degAxis(2, las = 1)
plot(raster::rotate(rcp_historical_stdanom$RCP8.5_2006_2055), main = "RCP8.5 2006-2055", zlim = c(0,14.9), col = matlab.like(100), axes = F); map(add = T, lwd = 0.5); degAxis(1); degAxis(2, las = 1)
plot(raster::rotate(rcp_historical_stdanom$RCP8.5_2050_2099), main = "RCP8.5 2050-2099", zlim = c(0,14.9), col = matlab.like(100), axes = F); map(add = T, lwd = 0.5); degAxis(1); degAxis(2, las = 1)
dev.off()
