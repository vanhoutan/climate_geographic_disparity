library(raster)
library(rasterVis)
library(sf)
library(rmapshaper)

rm(list = ls())

################
### CO2 excl ###
################
GHG_ts_excl <- list.files('/Users/kisei//Desktop/co2/excl/', pattern = '\\.nc$') # list all files in thel folder
GHG_ts_excl <- paste0("/Users/kisei/Desktop/co2/excl/", GHG_ts_excl) # add parent directories

GHG_ts_org <- list.files('/Users/kisei/Desktop/co2/org/', pattern = '\\.nc$') # list all files in thel folder
GHG_ts_org <- paste0("/Users/kisei/Desktop/co2/org/", GHG_ts_org) # add parent directories

emissions <- "emi_co2"

ghg <- raster::stack() # build an empty stack to put the desired emission metric in to

for( t in 1:length(GHG_ts_org)){ # for each file that is year in a month
  
  filename_excl <- GHG_ts_excl[t] # pull the first file,first year
  filename_org <- GHG_ts_org[t] # pull the first file,first year
  
  year_month_read_excl <- stack(filename_excl, varname = emissions) # read it in as a raster
  year_month_read_org <- stack(filename_org, varname = emissions) # read it in as a raster
  
  year_month_read = sum(year_month_read_excl, year_month_read_org)
  
  names(year_month_read) <- paste0(emissions,"_", gsub("/Users/kisei/Desktop/co2/excl/v50_CO2_excl_short-cycle_org_C_", "", filename_excl)) # generate name: Emission_yearmonth
  print(names(year_month_read)) # print in the loop to keep an eye on progress
  ghg <- stack(ghg,year_month_read) # add to the timeseries stack
  
}

ghg = as.data.frame(rasterToPoints(ghg))

########################################################################
### turn in to point file to detect overlap with regions of interest ###
########################################################################
ghg <- st_as_sf(x = ghg, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" )

########################
#### Ocean vs. Land ####
########################

load(paste0("/Users/kisei/climate_geographic_disparity/data/land_ocean_df.RData"))

land <- ms_simplify(land, keep = 0.1, keep_shapes = F) # simplify shapefile (saves computing time)
ocean <- ms_simplify(ocean, keep = 0.1, keep_shapes = F) # simplify shapefile (saves computing time)

# find intersections with disparity
land_intersection <- st_intersection(ghg,land)
ocean_intersection <- st_intersection(ghg,ocean)

dim(ocean_intersection)[1] + dim(land_intersection)[1]
dim(ghg)

