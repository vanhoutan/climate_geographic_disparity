# load libraries
library(raster) # raster manipulation
library(tidyverse)  # data tidying
library(ncdf4)
library(colorRamps)


# How to start ESGF - https://pcmdi.llnl.gov/mips/cmip5/data-access-getting-started.html
# Searching and downloading ESGF products - https://esgf-node.llnl.gov/projects/cmip5/
# I'm hosting these files locally currently, they are not huge, but we will need to figure out an easy way to host and collectively work on them.

# Alternative download: Create a wget script using a special URL http://esgf-data.dkrz.de/esg-search/wget?project=CMIP5&experiment=decadal2000&variable=tas will generate a script for download of all surface temperature files for experiment decadal2000 across all CMIP5 models. can be irritating to navigate.

#### read in scenarios ####
# rcp = Representative Concentration Pathway
# Four pathways have been selected for climate modeling and research, which describe different climate futures, all of which are considered possible depending on how much greenhouse gases are emitted in the years to come. The four RCPs, namely RCP2.6, RCP4.5, RCP6, and RCP8.5, are labelled after a possible range of radiative forcing values in the year 2100 relative to pre-industrial values (+2.6, +4.5, +6.0, and +8.5 W/m2, respectively) historical is a reanalysis of historical record, will be used to estimate future anomally
MPI_ESM_historical_tas <- stack('/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/impacts/temperature/CMIP5/MPI-ESM-MR/tas_Amon_MPI-ESM-MR_historical_r1i1p1_185001-200512.nc') #%>% rotate()

# rcp scenario
# atmospheric model = MPI_ESM_MR
# 2006 - 2100
MPI_ESM_rcp26_tas <- stack('/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/impacts/temperature/CMIP5/MPI-ESM-MR/tas_Amon_MPI-ESM-MR_rcp26_r1i1p1_200601-210012.nc') #%>% rotate()
MPI_ESM_rcp45_tas <- stack('/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/impacts/temperature/CMIP5/MPI-ESM-MR/tas_Amon_MPI-ESM-MR_rcp45_r1i1p1_200601-210012.nc') #%>% rotate()
MPI_ESM_rcp85_tas <- stack('/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/impacts/temperature/CMIP5/MPI-ESM-MR/tas_Amon_MPI-ESM-MR_rcp85_r1i1p1_200601-210012.nc') #%>% rotate()

# script below allows checking of varnames in nc file to confirm 'tas' retreival
# library(ncdf4)
# netcdf.file <- "/Users/tgagne/Downloads/ccb_Amon_MRI-CGCM3_rcp60_r1i1p1_200601-210012.nc"
# nc = ncdf4::nc_open(netcdf.file)
# variables = names(nc[['var']])
# variables
#############################
# estimating future anomaly #
#############################
# historical/present time range
start_date <- MPI_ESM_historical_tas$X1950.01.16 @data @band
end_date <- MPI_ESM_historical_tas$X2005.01.16 @data @band
present_mean <- mean(MPI_ESM_historical_tas[[start_date:end_date]])
plot(present_mean-273.15, col = matlab.like(100))

# future scenario rcp 26
future_start_date <- MPI_ESM_rcp26_tas$X2020.01.16 @data @band
future_end_date <- MPI_ESM_rcp26_tas$X2100.12.16 @data @band
rcp26_mean <- mean(MPI_ESM_rcp26_tas[[future_start_date:future_end_date]])
plot(rcp26_mean-273.15, col = matlab.like(100))
future_rcp26_anomally <- rcp26_mean -  present_mean 
plot(future_rcp26_anomally, col = matlab.like(100) )

# future scenario rcp 45
future_start_date <- MPI_ESM_rcp45_tas$X2020.01.16 @data @band
future_end_date <- MPI_ESM_rcp45_tas$X2100.12.16 @data @band
rcp45_mean <- mean(MPI_ESM_rcp45_tas[[future_start_date:future_end_date]])
plot(rcp45_mean-273.15, col = matlab.like(100))
future_rcp45_anomally <- rcp45_mean -  present_mean 
plot(future_rcp45_anomally, col = matlab.like(100) )

# future scenario rcp 55
future_start_date <- MPI_ESM_rcp85_tas$X2020.01.16 @data @band
future_end_date <- MPI_ESM_rcp85_tas$X2100.12.16 @data @band
rcp85_mean <- mean(MPI_ESM_rcp85_tas[[future_start_date:future_end_date]])
plot(rcp86_mean-273.15, col = matlab.like(100))
future_rcp85_anomally <- rcp85_mean -  present_mean 
plot(future_rcp85_anomally, col = matlab.like(100) )

future_anom_stack <- stack(future_rcp26_anomally,future_rcp45_anomally,future_rcp85_anomally)
names(future_anom_stack) <- c("MPI_rcp26","MPI_rcp45","MPI_rcp85")
future_anom_stack
plot(future_anom_stack, col = matlab.like(100), zlim = c(-0.5,10))

#writeRaster(future_anom_stack, paste0("./data/impacts/temperature/anomally_products/","MPI_ESM_future_anom_", date() %>% substr(5,10),".grd"), format="raster", overwrite=TRUE)
