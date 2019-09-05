library(spatial.tools)
library(ncdf4)
library(tidyverse)  # data tidying
library(raster)
library(colorRamps)
library(ggpubr)
library(sm)

BC <- list.files('/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/causes/GHGs', pattern = '\\.nc4$') # list all files in the MERRA model folder
BC = BC[226:441] #BC data Jan 2000 - December 2017
BC <- paste0("/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/causes/GHGs/", BC) # add parent directories

ghg <- c("OCEMAN","BCEMAN","SO2EMAN","SO4EMAN")[2]

bc_co2_total = NULL

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

  d1 <- stack(paste0('~/Desktop/Odiac/odiac2018_1x1d_',t , '.nc'), varname = "land")
  d2 <- stack(paste0('~/Desktop/Odiac/odiac2018_1x1d_',t , '.nc'), varname = "intl_bunker")
  
  co2 = stack(d1, d2); rm(d1, d2)
  co2 <- mean(co2)
  
  bc = rasterToPoints(bc, spatial = T)
  proj4string(bc)
  geo.prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 
  bc = spTransform(bc, CRS(geo.prj)) 
  proj4string(bc)
  bc@data <- data.frame(bc@data, long=coordinates(bc)[,1], lat=coordinates(bc)[,2])                         
  
  co2 = rasterToPoints(co2, spatial = T)
  proj4string(co2)
  geo.prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 
  co2 = spTransform(co2, CRS(geo.prj)) 
  proj4string(co2)
  co2@data <- data.frame(co2@data, long=coordinates(co2)[,1], lat=coordinates(co2)[,2])     
  
  bc_lon = as.data.frame(cbind(bc$long, scale(bc$layer))); bc_lon$Var = "BC"
  bc_lat = as.data.frame(cbind(bc$lat, scale(bc$layer))); bc_lat$Var = "BC"
  
  co2_lon = as.data.frame(cbind(co2$long, scale(co2$layer))); co2_lon$Var = "CO2"
  co2_lat = as.data.frame(cbind(co2$lat, scale(co2$layer))); co2_lat$Var = "CO2"
  
  lon = rbind(bc_lon, co2_lon); lon$axis = "lon"
  lat = rbind(bc_lat, co2_lat); lat$axis = "lat"
  
  bc_co2 = rbind(lon, lat)
  bc_co2$Year = t
  
  bc_co2_total = rbind(bc_co2_total, bc_co2)
  
  print(t)
  
  # plot(bc$lat, scale(bc$layer), col = alpha("blue", 0.2), pch = '.', main = t)
  # points(co2$lat, scale(co2$layer), col = alpha("orange", 0.2), pch = '.')
  # plot(bc$long, scale(bc$layer), col = alpha("blue", 0.2), pch = '.', main = t)
  # points(co2$long, scale(co2$layer), col = alpha("orange", 0.2), pch = '.')
  

}

load("~/Desktop/climate/KV_climate/climate_impacts_2019/data/OdiacCO2_MerraBC_Match_Up_2000-2017.RData")

df = subset(bc_co2_total, V2 > 0)

par(mfrow = c(2,1))
df$group = ifelse(df$Var == "BC", 1, 0)
df_1 = subset(df, axis == "lat")
df_2 = subset(df, axis == "lon")

sm.density.compare(df_1$V1, df_1$group, model = "equal")
sm.density.compare(df_2$V1, df_2$group, model = "equal")

ggplot(df %>% sample_frac(1)) + 
  # geom_point(aes(x = V1, y = V2,
  #                colour = Var,
  #                fill = Var),
  #            # size = 2,
  #            alpha = .2,
  #            # shape = 21,
  #            show.legend = T) +
  geom_density(aes(x = V1, colour = Var, fill = Var), alpha = 0.4) +
  # facet_wrap(.~axis + Year, scales = "free") +
  facet_wrap(.~axis, scales = "free", ncol = 1) +
  # facet_wrap(.~Year, scales = "free") +
  xlim(-180,180)+
  theme_pubr()


# match BC and CO2 layers -------------------------------------------------

load("/Users/ktanaka/Desktop/ODIAC_CO2_2000-2017_Stacked.RData")
load("/Users/ktanaka/Desktop/MERRA2_BCEMAN_2000_2017_Stacked.RData")

bce = mean(total_bc)
co2 = mean(odiac)

v1 = projectRaster(bce, co2, method = "ngb", over = T) # fro categorical variables
v2 = projectRaster(bce, co2, method = "bilinear", over = T) #appropriate for continious variables 
v3 = resample(bce, co2, method = "bilinear", )
v4 = resample(bce, co2, method = "ngb")

plot(log10(stack(co2, v1, v2, v3, v4)), 
     # zlim = c(-35, 0.875115),
     col = matlab.like(100))

bce_1deg = resample(bce, co2, method = "bilinear")
bce_1deg_g = bce_1deg/1000

#multiply by global warminig potenital value for 20 years
#Bond et al (2013) 3200 (270-6200)
#Bond & Sun (2005) 2200 (690-4700)

bce_1deg_g_adj = bce_1deg_g*2200
bce_1deg_g_adj = bce_1deg_g*3200

par(mfrow = c(1,3))
plot(log10(bce_1deg_g_adj), col = matlab.like(100), zlim = c(-50,1), main = "BCE resampled at 1*1 degree, adjusted by GWP20 log10(g/m-2)"); map(add = T)
plot(log10(co2), col = matlab.like(100), zlim = c(-50,1), main = "Fossil fuel CO2 log10(g/m-2)"); map(add = T)
plot(log(sum(bce_1deg_g_adj, co2)), col = matlab.like(100), zlim = c(-50,1), main = "Adjusted BCE + CO2 log10(g/m-2)"); map(add = T)

plot(log10(stack(bce_1deg_g_adj, co2)), zlim = c(-22, 1), col = matlab.like(100))

