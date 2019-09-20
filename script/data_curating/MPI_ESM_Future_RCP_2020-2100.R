# load libraries
library(raster) # raster manipulation
library(tidyverse)  # data tidying
library(ncdf4)
library(colorRamps)
library(maps)

# atmospheric model = MPI_ESM_MR

# How to start ESGF - https://pcmdi.llnl.gov/mips/cmip5/data-access-getting-started.html
# Searching and downloading ESGF products - https://esgf-node.llnl.gov/projects/cmip5/
# I'm hosting these files locally currently, they are not huge, but we will need to figure out an easy way to host and collectively work on them.

# Alternative download: Create a wget script using a special URL http://esgf-data.dkrz.de/esg-search/wget?project=CMIP5&experiment=decadal2000&variable=tas will generate a script for download of all surface temperature files for experiment decadal2000 across all CMIP5 models. can be irritating to navigate.

#### read in MPI_ESM RCP scenarios ####
#Historic 1850-2005
MPI_ESM_historical_tas <- stack('/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/impacts/temperature/CMIP5/MPI-ESM-MR/tas_Amon_MPI-ESM-MR_historical_r1i1p1_185001-200512.nc') #%>% rotate()

# rcp scenario
# 2006 - 2100
MPI_ESM_rcp26_tas <- stack('/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/impacts/temperature/CMIP5/MPI-ESM-MR/tas_Amon_MPI-ESM-MR_rcp26_r1i1p1_200601-210012.nc') #%>% rotate()
MPI_ESM_rcp45_tas <- stack('/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/impacts/temperature/CMIP5/MPI-ESM-MR/tas_Amon_MPI-ESM-MR_rcp45_r1i1p1_200601-210012.nc') #%>% rotate()
MPI_ESM_rcp85_tas <- stack('/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/impacts/temperature/CMIP5/MPI-ESM-MR/tas_Amon_MPI-ESM-MR_rcp85_r1i1p1_200601-210012.nc') #%>% rotate()

# script below allows checking of varnames in nc file to confirm 'tas' retreival
# netcdf.file <- "/Users/tgagne/Downloads/ccb_Amon_MRI-CGCM3_rcp60_r1i1p1_200601-210012.nc"
# nc = ncdf4::nc_open(netcdf.file)
# variables = names(nc[['var']])
# variables

# calculate future temperature anomaly ------------------------------------

# historical time range (1850-2005)
start_date <- MPI_ESM_historical_tas$X1850.01.16 @data @band
end_date <- MPI_ESM_historical_tas$X2005.01.16 @data @band
historic_mean <- mean(MPI_ESM_historical_tas[[start_date:end_date]])
plot(historic_mean-273.15, col = matlab.like(100))

# present anomaly (2019)
start_date <- MPI_ESM_rcp85_tas$X2018.01.16 @data @band
end_date <- MPI_ESM_rcp85_tas$X2019.12.16 @data @band
present_mean <- mean(MPI_ESM_rcp85_tas[[start_date:end_date]])
plot(present_mean-273.15, col = matlab.like(100))
present_anomaly = present_mean - historic_mean
plot(present_anomaly, col = matlab.like(100) )

# future scenario rcp 26 (2020-2100)
future_start_date <- MPI_ESM_rcp26_tas$X2020.01.16 @data @band
future_end_date <- MPI_ESM_rcp26_tas$X2100.12.16 @data @band
rcp26_mean <- mean(MPI_ESM_rcp26_tas[[future_start_date:future_end_date]])
plot(rcp26_mean-273.15, col = matlab.like(100))
future_rcp26_anomally <- rcp26_mean -  historic_mean 
plot(future_rcp26_anomally, col = matlab.like(100) )

# future scenario rcp 45
future_start_date <- MPI_ESM_rcp45_tas$X2020.01.16 @data @band
future_end_date <- MPI_ESM_rcp45_tas$X2100.12.16 @data @band
rcp45_mean <- mean(MPI_ESM_rcp45_tas[[future_start_date:future_end_date]])
plot(rcp45_mean-273.15, col = matlab.like(100))
future_rcp45_anomally <- rcp45_mean -  historic_mean 
plot(future_rcp45_anomally, col = matlab.like(100) )

# future scenario rcp 55
future_start_date <- MPI_ESM_rcp85_tas$X2020.01.16 @data @band
future_end_date <- MPI_ESM_rcp85_tas$X2100.12.16 @data @band
rcp85_mean <- mean(MPI_ESM_rcp85_tas[[future_start_date:future_end_date]])
plot(rcp86_mean-273.15, col = matlab.like(100))
future_rcp85_anomally <- rcp85_mean -  historic_mean 
plot(future_rcp85_anomally, col = matlab.like(100) )

future_anom_stack <- stack(present_anomaly, future_rcp26_anomally,future_rcp45_anomally,future_rcp85_anomally)
names(future_anom_stack) <- c("2019", "MPI_rcp26","MPI_rcp45","MPI_rcp85")
future_anom_stack
plot(future_anom_stack, col = matlab.like(100), zlim = c(-1.3,11.6))

#writeRaster(future_anom_stack, paste0("./data/impacts/temperature/anomally_products/","MPI_ESM_future_anom_", date() %>% substr(5,10),".grd"), format="raster", overwrite=TRUE)
