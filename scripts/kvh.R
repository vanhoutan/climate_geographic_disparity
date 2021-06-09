library(raster)
library(rasterVis)
library(sf)
library(rmapshaper)

rm(list = ls())

# 1. go to https://osf.io/b53fy/
# 2. download data/GH Emissions folder
# 3. unzip, create two subfolders "excl" and "org" under the "co2" folder. 
# 4. separate EDGAR CO2 netcdf files by CO2-excl and CO2-org (see lines 16 & 19)

##########################
### CO2, ch4, n2o, so2 ###
##########################
GHG_co2_excl <- list.files('G:/EDGAR_v5.0/co2/excl/', pattern = '\\.nc$') # list all files in thel folder
GHG_co2_excl <- paste0("G:/EDGAR_v5.0/co2/excl/", GHG_co2_excl) # add parent directories

GHG_co2_org <- list.files('G:/EDGAR_v5.0/co2/org/', pattern = '\\.nc$') # list all files in thel folder
GHG_co2_org <- paste0("G:/EDGAR_v5.0/co2/org/", GHG_co2_org) # add parent directories

co2_emissions <- "emi_co2"

GHG_ch4 <- list.files('G:/EDGAR_v5.0/ch4/', pattern = '\\.nc$') # list all files in thel folder
GHG_ch4 <- paste0("G:/EDGAR_v5.0/ch4/", GHG_ch4) # add parent directories

ch4_emissions <- "emi_ch4"

GHG_n2o <- list.files('G:/EDGAR_v5.0/n2o/', pattern = '\\.nc$') # list all files in thel folder
GHG_n2o <- paste0("G:/EDGAR_v5.0/n2o/", GHG_n2o) # add parent directories

n2o_emissions <- "emi_n2o"

GHG_so2 <- list.files('G:/EDGAR_v5.0/so2/', pattern = '\\.nc$') # list all files in thel folder
GHG_so2 <- paste0("G:/EDGAR_v5.0/so2/", GHG_so2) # add parent directories

so2_emissions <- "emi_so2"

ghg <- raster::stack() # build an empty stack to put the desired emission metric in to

for( t in 1:length(GHG_so2)){ # years available four all four GHG (1970-2012)
  
  filename_co2_excl <- GHG_co2_excl[t] # pull the first file,first year
  filename_co2_org <- GHG_co2_org[t] # pull the first file,first year
  filename_ch4 <- GHG_ch4[t] # pull the first file,first year
  filename_n2o <- GHG_n2o[t] # pull the first file,first year
  filename_so2 <- GHG_so2[t] # pull the first file,first year
    
  year_month_read_co2_excl <- stack(filename_co2_excl, varname = co2_emissions) # read it in as a raster
  year_month_read_co2_org <- stack(filename_co2_org, varname = co2_emissions) # read it in as a raster
  year_month_read_ch4 <- stack(filename_ch4, varname = ch4_emissions) # read it in as a raster
  year_month_read_n2o <- stack(filename_n2o, varname = n2o_emissions) # read it in as a raster
  year_month_read_so2 <- stack(filename_so2, varname = so2_emissions) # read it in as a raster

  year_month_read = sum(year_month_read_co2_excl, year_month_read_co2_org, year_month_read_ch4, year_month_read_n2o, year_month_read_so2)
  
  names(year_month_read) <- paste0("co2+ch4+so2+n2o_", t) # generate name: Emission_yearmonth
  print(names(year_month_read)) # print in the loop to keep an eye on progress
  ghg <- sum(ghg, year_month_read) # add to the timeseries stack
  
}

ghg = as.data.frame(rasterToPoints(ghg))

########################################################################
### turn in to point file to detect overlap with regions of interest ###
########################################################################
ghg <- st_as_sf(x = ghg, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" )

########################
#### Ocean vs. Land ####
########################

load(paste0("data/land_ocean_df.RData"))

## saves computation time + debug
# ghg = sample_frac(ghg, 0.01)
# land <- ms_simplify(land, keep = 0.001, keep_shapes = F) # simplify shapefile (saves computing time)
# ocean <- ms_simplify(ocean, keep = 0.001, keep_shapes = F) # simplify shapefile (saves computing time)

# find intersections with disparity
land_intersection <- st_intersection(ghg,land)
ocean_intersection <- st_intersection(ghg,ocean)

dim(ocean_intersection)[1] + dim(land_intersection)[1]
dim(ghg)

sum_total = sum(land_intersection$layer + ocean_intersection$layer)
sum_ocean = sum(ocean_intersection$layer)

sum_ocean/sum_total

