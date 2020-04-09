# Reproducible example
# Example: download from Sea surface temperatures collected by PCMDI for use by the IPCC.
wget -c https://www.unidata.ucar.edu/software/netcdf/examples/tos_O1_2001-2002.nc
# Then adjust the path to your pc...
setwd('/Users/ktanaka/Downloads/anthromes_1_NetCDF')
# Then...
fn <- 'anthromes_v1.nc'
#
library(raster)
library(ncdf4)
library(rgdal)
library(rgeos)
#
ncfile <- nc_open(fn)
print(ncfile) # see all the object's structure and attributes
YourBrick <- brick(fn)
YourBrick
#
plot(YourBrick[[1]]) # plot the first variable or name
graphics.off()
r <- raster(YourBrick, layer=1) # Select which data is needed to be saved
r
class(r)
plot(r,axes=T)

load(paste0("/Users/", dir, "/clim_geo_disp/output/previous results/BC-CO2_Combined_1970-2018.RData")) #BC + CO2
r = resample(r, bc_co2_adjusted, method = "bilinear") #use bilinear interpolation method to resample layer on 1 by 1 deg grid

p <- rasterToPolygons(r, dissolve = F, digits = 0)
p
p <- ms_simplify(p, keep = 0.001, keep_shapes = F) # simplify shapefile (saves computing time)

plot(p,axes=T)
# Save as shapefile
writeOGR(p, dsn='/Users/ktanaka/Desktop/anthromes_v1', layer='anthromes_v1', driver="ESRI Shapefile",overwrite_layer=T)
