rm(list = ls())

library(spatial.tools)
library(ncdf4)
library(tidyverse)
library(raster)
library(colorRamps)
library(ggpubr)
library(sm)
library(maps)


########################################################@@@@@@@@@@@@@@@@@@#################
### Modern-Era Retrospective Analysis for Research and Applications Version 2 (MERRA-2) ###
### BCEMAN (Anthropogenic Black Carbon)                                                 ###
### 1980-2018                                                                           ###
### 0.5° × 0.625°                                                                       ###
### kg m-2 s-1                                                                          ###
###########################################################################################

BC <- list.files('/Desktop/climate/KV_climate/climate_impacts_2019/data/causes/GHGs', pattern = '\\.nc4$') # list all netcdf files in the folder
BC <- paste0("/Desktop/climate/KV_climate/climate_impacts_2019/data/causes/GHGs/", BC) # add parent directories
ghg <- c("OCEMAN","BCEMAN","SO2EMAN","SO4EMAN")[2] #BCEMAN = Black Carbon Anthropogenic Emissions

bc = raster::stack()

for (t in 1:length(BC)) {
  
  filename <- BC[t] # pull the first file,first year
  year_month_read <- stack(filename, varname = ghg) # read as a raster
  names(year_month_read) <- paste0(ghg,"_", gsub(".*Nx.\\s*|.nc4.*", "", filename)) # generate name: Emission_yearmonth
  print(names(year_month_read))
  bc <- stack(bc,year_month_read) # add to the timeseries stack
  
}

bc_mean = mean(bc)


#######################################################################################################
### Emission Database for Global Atmospheric Research Global Greenhouse Gas Emissions (EDGAR v5.0)  ###
### CO2_excl_short-cycle  1970-2018                                                                 ###
### CO2_org_short-cycle   1970-2015                                                                 ### 
### CH4                   1970-2015                                                                 ### 
### N2O                   1970-2015                                                                 ###
### 0.1°* x 0.1°                                                                                    ###
### kg m-2 s-1                                                                                      ### 
#######################################################################################################

################
### CO2 excl ###
################
var <- list.files('/Desktop/edgar/v50_CO2_excl_short-cycle_org/v50_CO2_excl_short-cycle_org_C_TOTALS_nc/', pattern = '\\.nc$')
var <- paste0("/Desktop/edgar/v50_CO2_excl_short-cycle_org/v50_CO2_excl_short-cycle_org_C_TOTALS_nc/", var) 

emissions <- "emi_co2"

ghg <- raster::stack()

for( t in 1:length(var)){ 
  
  filename <- var[t]
  year_month_read <- stack(filename, varname = emissions)
  names(year_month_read) <- paste0(emissions,"_", gsub(".*Nx.\\s*|.nc4.*", "", filename))
  print(names(year_month_read))
  ghg <- stack(ghg,year_month_read)
  
}

co2_excl_mean = mean(ghg)

###############
### CO2 org ###
###############
var <- list.files('/Desktop/edgar/v50_CO2_org_short-cycle_C/v50_CO2_org_short-cycle_C_TOTALS_nc/', pattern = '\\.nc$')
var <- paste0("/Desktop/edgar/v50_CO2_org_short-cycle_C/v50_CO2_org_short-cycle_C_TOTALS_nc/", var)

emissions <- "emi_co2"

ghg <- raster::stack() 

for( t in 1:length(var)){ 
  
  filename <- var[t]
  year_month_read <- stack(filename, varname = emissions) 
  names(year_month_read) <- paste0(emissions,"_", gsub(".*Nx.\\s*|.nc4.*", "", filename)) 
  print(names(year_month_read)) 
  ghg <- stack(ghg,year_month_read) 
  
}

co2_org_mean = mean(ghg)


###########
### CH4 ###
###########
var <- list.files('/Desktop/edgar/v50_CH4_1970_2015/v50_CH4_TOTALS_nc', pattern = '\\.nc$') 
var <- paste0("/Desktop/edgar/v50_CH4_1970_2015/v50_CH4_TOTALS_nc/", var) 

emissions <- "emi_ch4"

ghg <- raster::stack() 

for( t in 1:length(var)){
  
  filename <- var[t] 
  year_month_read <- stack(filename, varname = emissions) 
  names(year_month_read) <- paste0(emissions,"_", gsub(".*Nx.\\s*|.nc4.*", "", filename))
  print(names(year_month_read))
  ghg <- stack(ghg,year_month_read)
  
}

ch4_mean = mean(ghg)

###########
### N2O ###
###########
var <- list.files('/Desktop/edgar/v50_N2O/v50_N2O_TOTALS_nc/', pattern = '\\.nc$')
var <- paste0("/Desktop/edgar/v50_N2O/v50_N2O_TOTALS_nc/", var)

emissions <- "emi_n2o"

ghg <- raster::stack()

for( t in 1:length(var)){
  
  filename <- var[t]
  year_month_read <- stack(filename, varname = emissions)
  names(year_month_read) <- paste0(emissions,"_", gsub(".*Nx.\\s*|.nc4.*", "", filename))
  print(names(year_month_read))
  ghg <- stack(ghg,year_month_read)
  
}

n2o_mean = mean(ghg)


###############################################################################################
### combine EDGAR v5.0 data and multiply non-co2 data according to average GTP20-100 values ###
###############################################################################################
edgar = sum(co2_excl_mean, co2_org_mean, ch4_mean*40.5, n2o_mean*290.5)


###############################################################################
### use bilinear interpolation method to resample BC on 0.1 by 0.1 deg grid ###
###############################################################################
bc_mean_0.1 = resample(bc_mean, raster::rotate(edgar), method = "bilinear") 


##################################################################################################
### multiply BC by average global temperature potenital value 20-100 years (Bond et al (2013)) ###
##################################################################################################
bc_mean_0.1_adj = bc_mean_0.1*400


#######################################
### combine MERRA-2 and EDGAR v5.0  ###
#######################################
bc_co2_ch4_n2o_adjusted = sum(raster::rotate(edgar), bc_mean_0.1_adj)

plot(bc_co2_ch4_n2o_adjusted, col = matlab.like(100))
plot(log10(bc_co2_ch4_n2o_adjusted), col = matlab.like(100))


###################################
### save unified emission layer ###
###################################
bc_co2_ch4_n2o_adjusted = readAll(bc_co2_ch4_n2o_adjusted)
save(bc_co2_ch4_n2o_adjusted, file = "/clim_geo_disp/output/BC_CO2_CH4_N2O_Combined_1970-2018.RData")