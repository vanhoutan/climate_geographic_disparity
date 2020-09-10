rm(list = ls())

library(spatial.tools)
library(ncdf4)
library(tidyverse)  # data tidying
library(raster)
library(colorRamps)
library(ggpubr)
library(sm)
library(maps)

##########################################################
### read MERRA2 BC layers, then stack and average them ###
##########################################################

# # list all files in the MERRA model folder
# BC <- list.files('/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/causes/GHGs', pattern = '\\.nc4$')
# # BC = BC[226:441] #BC data Jan 2000 - December 2017
# BC <- paste0("/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/causes/GHGs/", BC) # add parent directories
# 
# ghg <- c("OCEMAN","BCEMAN","SO2EMAN","SO4EMAN")[2]
# 
# bc = raster::stack()
# 
# for (t in 1:length(BC)) {
# 
#   filename <- BC[t] # pull the first file,first year
# 
#   year_month_read <- stack(filename, varname = ghg) # read it in as a raster
#   names(year_month_read) <- paste0(ghg,"_", gsub(".*Nx.\\s*|.nc4.*", "", filename)) # generate name: Emission_yearmonth
#   print(names(year_month_read)) # print in the loop to keep an eye on progress
#   bc <- stack(bc,year_month_read) # add to the timeseries stack
# 
# }
# 
# bc_mean = mean(bc)
# 
# bc = readAll(bc)
# bc_mean = readAll(bc_mean)
# 
# save.image("/Users/ktanaka/clim_geo_disp/output/MERRA2_BC_Mean_1980-2018.RData")

load("/Users/ktanaka/clim_geo_disp/output/MERRA2_BC_Mean_1980-2018.RData")


################
### CO2 excl ###
################
GHG_ts <- list.files('/Users/ktanaka/Desktop/edgar/v50_CO2_excl_short-cycle_org/v50_CO2_excl_short-cycle_org_C_TOTALS_nc/', pattern = '\\.nc$')
GHG_ts <- paste0("/Users/ktanaka/Desktop/edgar/v50_CO2_excl_short-cycle_org/v50_CO2_excl_short-cycle_org_C_TOTALS_nc/", GHG_ts) 

emissions <- "emi_co2"

ghg <- raster::stack() # build an empty stack to put the desired emission metric in to

for( t in 1:length(GHG_ts)){ # for each file that is year in a month
  
  filename <- GHG_ts[t] # pull the first file,first year
  year_month_read <- stack(filename, varname = emissions) # read it in as a raster
  names(year_month_read) <- paste0(emissions,"_", gsub(".*Nx.\\s*|.nc4.*", "", filename)) # generate name: Emission_yearmonth
  print(names(year_month_read)) # print in the loop to keep an eye on progress
  ghg <- stack(ghg,year_month_read) # add to the timeseries stack
  
}

co2_excl_mean = mean(ghg)


###############
### CO2 org ###
###############
GHG_ts <- list.files('/Users/ktanaka/Desktop/edgar/v50_CO2_org_short-cycle_C/v50_CO2_org_short-cycle_C_TOTALS_nc/', pattern = '\\.nc$')
GHG_ts <- paste0("/Users/ktanaka/Desktop/edgar/v50_CO2_org_short-cycle_C/v50_CO2_org_short-cycle_C_TOTALS_nc/", GHG_ts)

emissions <- "emi_co2"

ghg <- raster::stack() # build an empty stack to put the desired emission metric in to

for( t in 1:length(GHG_ts)){ # for each file that is year in a month
  
  filename <- GHG_ts[t] # pull the first file,first year
  year_month_read <- stack(filename, varname = emissions) # read it in as a raster
  names(year_month_read) <- paste0(emissions,"_", gsub(".*Nx.\\s*|.nc4.*", "", filename)) # generate name: Emission_yearmonth
  print(names(year_month_read)) # print in the loop to keep an eye on progress
  ghg <- stack(ghg,year_month_read) # add to the timeseries stack
  
}

co2_org_mean = mean(ghg)

co2_mean = sum(co2_excl_mean, co2_org_mean)

#use bilinear interpolation method to resample BC on 0.1 by 0.1 deg grid
bc_mean_0.1 = resample(bc_mean, raster::rotate(co2_mean), method = "bilinear") 


#multiply by global temperature value for average 20-100 years, see Bond et al (2013)

#Unadjusted
bc_mean_0.1_unadj = bc_mean_0.1
bc_co2_unadjusted = sum(raster::rotate(co2_mean), bc_mean_0.1_unadj)

bc_mean_0.1_adj = bc_mean_0.1*400
bc_co2_adjusted = sum(raster::rotate(co2_mean), bc_mean_0.1_adj)

pdf("/Users/ktanaka/Dropbox/PAPER climate geographic disparities/figures/supplemental/Edgar CO2 1970-2018.pdf", height = 9, width = 5)
par(mfrow = c(3,1))
plot(raster::rotate(log10(co2_excl_mean)), col = matlab.like(100), main = "EDGAR CO2 EXCL 1970-2018 log10(kg m-2 yr-1)"); maps::map(add = T)
plot(raster::rotate(log10(co2_org_mean)), col = matlab.like(100), main = "EDGAR CO2 ORG 1970-2015 log10(kg m-2 yr-1)"); maps::map(add = T)
plot(raster::rotate(log10(co2_mean)), col = matlab.like(100), main = "EDGAR CO2 EXCL+ORG log10(kg m-2 yr-1)"); maps::map(add = T)
dev.off()

pdf("/Users/ktanaka/Dropbox/PAPER climate geographic disparities/figures/supplemental/MERRA2 BC 1980-2018.pdf", height = 9, width = 7.3)
par(mfrow = c(2,1))
plot(log10(bc_mean_0.1_adj), col = matlab.like(100), main = "MERRA2 BC 1980-2018 log10(kg m-2 yr-1) adjusted"); maps::map(add = T)
plot(log10(bc_co2_unadjusted), col = matlab.like(100), main = "MERRA2 BC 1980-2018 log10(kg m-2 yr-1) unadjusted"); maps::map(add = T)
dev.off()

pdf("/Users/ktanaka/Dropbox/PAPER climate geographic disparities/figures/supplemental/BC+CO2.pdf", height = 6, width = 10)
par(mfrow = c(1,1))
plot(log10(bc_co2_adjusted), col = matlab.like(100), main = "MERRA2 (BC 1980-2018) + EDGAR (CO2 1970-2018) log10(kg m-2 yr-1)"); maps::map(add = T)
dev.off()

bc_co2_adjusted = readAll(bc_co2_adjusted)
bc_co2_unadjusted = readAll(bc_co2_unadjusted)

save(bc_co2_adjusted, bc_co2_unadjusted, file = "/Users/ktanaka/clim_geo_disp/output/BC-CO2_Combined_1970-2018.RData")

