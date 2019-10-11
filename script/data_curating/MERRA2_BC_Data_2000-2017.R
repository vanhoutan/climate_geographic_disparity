library(spatial.tools)
library(ncdf4)
library(tidyverse)  # data tidying
library(raster)
library(colorRamps)
library(maps)
library(rgdal)
library(rasterVis)
library(RColorBrewer)
library(rnaturalearth)

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
load("/Users/ktanaka/clim_geo_disp/output/MERRA2_BCEMAN_2000_2017_Stacked.RData")

bc = mean(total_bc)

p1 = gplot(bc) + 
  geom_point(aes(color = value)) +
  coord_equal() +
  coord_quickmap(xlim = c(-180, 180),
                 ylim = c(-90, 90)) +
  theme_bw() + 
  borders(xlim = c(-180, 180),
          ylim = c(-180, 180), 
          fill = NA, size = 0.1) +
  scale_x_longitude(xmin = -180, xmax = 180, step = 60) +
  scale_y_latitude(ymin = -180, ymax = 180, step = 60) +
  scale_color_gradientn(colours = c( "black","cyan", "red"), name = "kg/m-2") 

p2 = gplot(bc) + 
  geom_point(aes(color = log10(value))) +
  coord_equal() +
  coord_quickmap(xlim = c(-180, 180),
                 ylim = c(-90, 90)) +
  theme_bw() + 
  borders(xlim = c(-180, 180),
          ylim = c(-180, 180), 
          fill = NA, size = 0.1) +
  scale_x_longitude(xmin = -180, xmax = 180, step = 60) +
  scale_y_latitude(ymin = -180, ymax = 180, step = 60) +
  scale_color_gradientn(colours = c( "black","cyan", "red"), name = "log10(kg/m-2)") 

pdf("/Users/ktanaka/Dropbox/PAPER climate geographic disparities/figures/supplemental/MERRA_BCE_2000-2017.pdf", height = 4, width = 15)
# par(mfrow = c(2,1), mar = c(2,4,2,4))
# plot(bc, col = matlab.like(100), axes = F
#      , main = "(kg m-2)"
#      )
# degAxis(1); degAxis(2, las = 2)
# map(add = T)
# 
# plot(log10(mean(bc)), col = matlab.like(100), axes = F
#      # , main = "Mean global black carbon emission distribution from MERRA2 2000-2017 ln(kg m-2)"
# )
# degAxis(1); degAxis(2, las = 2)
# map(add = T)
# # plot(mean(odiac), col = matlab.like(100));map(add = T)

cowplot::plot_grid(p1, p2, cols = 2)

dev.off()
