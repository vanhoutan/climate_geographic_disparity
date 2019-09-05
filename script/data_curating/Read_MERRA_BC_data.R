library(spatial.tools)
library(ncdf4)
library(tidyverse)  # data tidying
library(raster)
library(colorRamps)

BC <- list.files('/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/causes/GHGs', pattern = '\\.nc4$') # list all files in the MERRA model folder
BC = BC[226:441] #BC data Jan 2000 - December 2017
BC <- paste0("/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/causes/GHGs/", BC) # add parent directories

ghg <- c("OCEMAN","BCEMAN","SO2EMAN","SO4EMAN")[2]

total_bc <- raster::stack() # build an empty stack to put the desired emission metric in to

for (t in 2000:2017) {
  
  year = paste0(".",t)
  
  BC_y = BC[grep(year, BC)]
  
  ts_BCE <- raster::stack() 
  
  for( tt in 1:length(BC_y)){ 
    
    filename <- BC_y[tt] 
    
    year_month_read <- stack(filename, varname = ghg) 
    names(year_month_read) <- paste0(ghg,"_", gsub(".*Nx.\\s*|.nc4.*", "", filename)) 
    print(names(year_month_read)) 
    ts_BCE <- stack(ts_BCE,year_month_read) 
    
  }
  
  bc = mean(ts_BCE)
  total_bc <- stack(total_bc, bc) 
  
  print(t)
  
}

save.image("/Users/ktanaka/Desktop/MERRA2_BCEMAN_2000_2017_Stacked.RData")
load("/Users/ktanaka/Desktop/MERRA2_BCEMAN_2000_2017_Stacked.RData")

bc = mean(total_bc)

png("/Users/ktanaka/Desktop/MERRA_BCE_2000-2017.png", res = 500, height = 6, width = 10, units = "in")
# par(mfrow = c(2,1), mar = c(2,2,4,4))
plot(log(mean(bc)), col = matlab.like(100), main = "Mean global black carbon emission distribution from MERRA2 2000-2017 ln(kg m-2")
map(add = T)
# plot(mean(odiac), col = matlab.like(100));map(add = T)

dev.off()
