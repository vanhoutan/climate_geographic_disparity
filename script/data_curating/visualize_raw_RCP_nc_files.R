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

names(rcp_anom) = c("RCP4.5_anomaly_(2006-2055)", 
                    "RCP4.5_anomaly_(2050-2099)",
                    "RCP8.5_anomaly_(2006-2055)",
                    "RCP8.5_anomaly_(2050-2099)")

rcp_ensem_mean_chang = stack(rcp45_ensem_mean_change_1, rcp45_ensem_mean_change_2,
                             rcp85_ensem_mean_change_1, rcp85_ensem_mean_change_2)

names(rcp_ensem_mean_chang) = c("RCP4.5_ensem_mean_change_(2006-2055)", 
                                "RCP4.5_ensem_mean_change_(2050-2099)",
                                "RCP8.5_ensem_mean_change_(2006-2055)",
                                "RCP8.5_ensem_mean_change_(2050-2099)")

rcp_ensem_stdanom = stack(rcp45_ensem_stdanom_1, rcp45_ensem_stdanom_2,
                          rcp85_ensem_stdanom_1, rcp85_ensem_stdanom_2)

names(rcp_ensem_stdanom) = c("RCP4.5_ensem_stdanom_(2006-2055)", 
                                "RCP4.5_ensem_stdanom_(2050-2099)",
                                "RCP8.5_ensem_stdanom_(2006-2055)",
                                "RCP8.5_ensem_stdanom_(2050-2099)")

rcp_historical_stdanom = stack(rcp45_historical_stdanom_1, rcp45_historical_stdanom_2,
                               rcp85_historical_stdanom_1, rcp85_historical_stdanom_2)

names(rcp_historical_stdanom) = c("RCP4.5_historical_stdanom_(2006-2055)", 
                                "RCP4.5_historical_stdanom_(2050-2099)",
                                "RCP8.5_historical_stdanom_(2006-2055)",
                                "RCP8.5_historical_stdanom_(2050-2099)")

pdf("~/Dropbox/PAPER climate geographic disparities/figures/supplemental/rcp_anom.pdf", width = 9.5, height = 5)
plot(rcp_anom, zlim = c(0,11), col = matlab.like(100))
dev.off()

pdf("~/Dropbox/PAPER climate geographic disparities/figures/supplemental/rcp_ensem_mean_chang.pdf", width = 9.5, height = 5)
plot(rcp_ensem_mean_chang, zlim = c(0,11), col = matlab.like(100))
dev.off()

pdf("~/Dropbox/PAPER climate geographic disparities/figures/supplemental/rcp_ensem_stdanom.pdf", width = 9.5, height = 5)
plot(rcp_ensem_stdanom, zlim = c(0,4.7), col = matlab.like(100))
dev.off()

pdf("~/Dropbox/PAPER climate geographic disparities/figures/supplemental/rcp_historical_stdanom.pdf", width = 9.5, height = 5)
plot(rcp_historical_stdanom, zlim = c(0,14.9), col = matlab.like(100))
dev.off()