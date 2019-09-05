library(spatial.tools)
library(ncdf4)
library(tidyverse)  # data tidying
library(raster)

# function for rescaling raster values
rescale <-                                                            
  function(x, x.min = NULL, x.max = NULL, new.min = 0, new.max = 1) {
    if(is.null(x.min)) x.min = min(x)
    if(is.null(x.max)) x.max = max(x)
    new.min + (x - x.min) * ((new.max - new.min) / (x.max - x.min))}

# Downloading data from the MERRA-2 GEODISC database
# you will need to get an earthdata login and establish credentials 
# download everything in this folder
# wget --load-cookies ~/.urs_cookies --save-cookies ~/.urs_cookies --keep-session-cookies -r -c -nH -nd -np -A nc4 --content-disposition "https://goldsmr4.gesdisc.eosdis.nasa.gov/data/MERRA2_MONTHLY/M2TMNXADG.5.12.4/"
# 
# # script below allows checking of varnames in nc file to confirm variables retreived
#  netcdf.file <- "./data/causes/GHGs/MERRA2_100.tavgM_2d_adg_Nx.198001.nc4"
#  nc = ncdf4::nc_open(netcdf.file)
# names(nc[['var']])
# 
#  # quick look at some of these anthropogenic GHGs
# human1980_BC <- stack("./data/causes/GHGs/MERRA2_100.tavgM_2d_adg_Nx.198001.nc4", varname = "BCEMAN")
# human1980_OC <- stack("./data/causes/GHGs/MERRA2_100.tavgM_2d_adg_Nx.198001.nc4", varname = "OCEMAN")
# human1980_SO2 <- stack("./data/causes/GHGs/MERRA2_100.tavgM_2d_adg_Nx.198001.nc4", varname = "SO2EMAN")
# human1980_SO4 <- stack("./data/causes/GHGs/MERRA2_100.tavgM_2d_adg_Nx.198001.nc4", varname = "SO4EMAN")

# CO Product to pursue: MERRA2_100.tavgM_2d_chm_Nx.198001.nc4

# script below allows checking of varnames in nc file to confirm variables retreived
netcdf.file <- "/Users/tgagne/Downloads/MERRA2_100.tavgM_2d_chm_Nx.198001.nc4"
nc = ncdf4::nc_open(netcdf.file)
names(nc[['var']])

stack('/Users/tgagne/Downloads/MERRA2_100.tavgM_2d_chm_Nx.198001.nc4')

# par(mfrow = c(4,1))
# plot(human1980_BC, col = viridis::viridis(10))
# plot(human1980_OC, col = viridis::viridis(10))
# plot(human1980_SO2, col = viridis::viridis(10))
# plot(human1980_SO4, col = viridis::viridis(10))
# 
# rasterVis::levelplot(human1980_BC)
# rasterVis::levelplot(human1980_OC)
# rasterVis::levelplot(human1980_SO2)
# rasterVis::levelplot(human1980_SO4)
# 
# par(mfrow = c(3,2))
# plot(human1980_BC,human1980_OC)
# plot(human1980_BC %>% log(),human1980_OC %>% log())
# 
# plot(human1980_BC,human1980_SO2)
# plot(human1980_BC %>% log(),human1980_SO2 %>% log())
# 
# plot(human1980_BC,human1980_SO4)
# plot(human1980_BC %>% log(),human1980_SO4 %>% log())
# 
# ##############################################################
# # for loop to build a time series of anthropogenic emissions #
# ##############################################################
GHG_ts <- list.files('/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/causes/GHGs', pattern = '\\.nc4$') # list all files in the MERRA model folder
GHG_ts <- paste0("/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/causes/GHGs/", GHG_ts) # add parent directories

emissions <- c("OCEMAN","BCEMAN","SO2EMAN","SO4EMAN")[2]

for(gas in 4){# in 1:length(emissions)){
  
  ts_BCE <- raster::stack() # build an empty stack to put the desired emission metric in to
  
  for( each_month_year_raster in 1:length(GHG_ts)){ # for each file that is year in a month
  
    filename <- GHG_ts[each_month_year_raster] # pull the first file,first year
    
    # year_month_read <- stack(filename, varname = emissions[gas]) # read it in as a raster
    # names(year_month_read) <- paste0(emissions[gas],"_", gsub(".*Nx.\\s*|.nc4.*", "", filename)) # generate name: Emission_yearmonth
    #plot(year_month_read, col = viridis::viridis(10), main = names(year_month_read))
    
    year_month_read <- stack(filename, varname = emissions) # read it in as a raster
    names(year_month_read) <- paste0(emissions,"_", gsub(".*Nx.\\s*|.nc4.*", "", filename)) # generate name: Emission_yearmonth
    print(names(year_month_read)) # print in the loop to keep an eye on progress
    ts_BCE <- stack(ts_BCE,year_month_read) # add to the timeseries stack
  
  }
  
  plot(sum(ts_BCE))

  # writeRaster(sum(ts_BCE), paste0("./data/causes/GHG_ts_products/",emissions[gas],"_",date() %>% substr(5,10),".grd"), format="raster", overwrite=TRUE)
  
  writeRaster(sum(ts_BCE),
              paste0("/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/causes/GHG_ts_products/",
                     emissions[gas], "_", date() %>% substr(5,10), ".grd"),
              format = "raster",
              overwrite = TRUE)
  
  # sum_ts_BCE = sum(ts_BCE)
  # save.image("/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/causes/GHG_ts_products/ts_BCE_198001_201811.RData")
}

ts_BCE <- stack("/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/causes/GHG_ts_products/OCEMAN_sum_Jan 31.grd") #%>% plot(col = matlab.like(100))
ts_BCE <- stack("/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/causes/GHG_ts_products/BCEMAN_sum_Jan 31.grd") #%>% plot(col = matlab.like(100))
ts_BCE <- stack("/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/causes/GHG_ts_products/SO4EMAN_Jan 31.grd") #%>% plot(col = matlab.like(100))
ts_BCE <- stack("/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/causes/GHG_ts_products/SO4EMAN_Jan 31.grd") #%>% plot(col = matlab.like(100))

df_ <- ts_BCE %>% rasterToPoints() %>% as.data.frame() 
str(df_)

ggplot(df_,aes(x,y,fill =layer ) ) +
  geom_raster()+
  scale_fill_viridis_c()+
  scale_fill_distiller(palette = "Spectral")+
  coord_fixed()
ggplot(df_,aes(x,y,fill = ifelse(layer > quantile(layer, 0.99), quantile(layer, 0.99), layer) ) ) +
  geom_raster()+
  scale_fill_viridis_c()+
  scale_fill_distiller(palette = "Spectral")+
  coord_fixed()

rm(each_month_year_raster,filename,year_month_read)

###############################################################
# testing out inequity with RCP8.5 and Black Carbon emissions #
###############################################################
# population data
gpw_pop <- stack("/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/gpw-v4-population-density-rev10_totpop_30_min_nc/gpw_v4_e_atotpopbt_dens_30_min.nc",varname = "Population Density, v4.10 (2000, 2005, 2010, 2015, 2020): 30 arc-minutes")
gpw_pop <- gpw_pop[["X5"]]
gpw_pop <- reclassify(gpw_pop,rcl = c(NA,NA,0))
gpw_pop <- spatial_sync_raster(gpw_pop,ts_BCE,method = "ngb", size_only = FALSE, verbose = T)
gpw_pop <- gpw_pop + 1#0.00000001
plot(log10(gpw_pop), col = matlab.like(100))

# climate anomaly data
anomaly <- stack("/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/impacts/temperature/anomally_products/MPI_ESM_future_anom_Jan 31.grd")[["MPI_rcp85"]]
anomaly <- spatial_sync_raster(anomaly,ts_BCE,method = "ngb", size_only = FALSE, verbose = T)
anomaly <- abs(anomaly)
anomaly <- rescale(anomaly, x.min = anomaly@data@min , x.max = anomaly@data@max,new.min = 2, new.max = 3)
plot(anomaly, col = matlab.like(100))

# black carbon anthropogenic emissions data
ts_BCE_sum <- ts_BCE
BCE_sum <- ts_BCE_sum+0.0000000001

# BCE emissions adjusted for population, i.e. emission per person in a cell
#BCE_adj_for_pop <- BCE_sum/(reclassify(gpw_pop,c(quantile(gpw_pop, probs = c(0.90), type = 7),Inf,quantile(gpw_pop, probs = c(0.90), type = 7))))
BCE_adj_for_pop <- BCE_sum/gpw_pop

BCE_adj_for_pop <- rescale(BCE_adj_for_pop, x.min = BCE_adj_for_pop@data@min , x.max = BCE_adj_for_pop@data@max,new.min = 2, new.max = 3)
plot(log(BCE_adj_for_pop), col = matlab.like(100))

# scaled raw BCE emissions
BCE_sum_scale <- rescale(BCE_sum, x.min = BCE_sum@data@min , x.max = BCE_sum@data@max,new.min = 2, new.max = 3)
plot(BCE_sum_scale, col = matlab.like(100))
plot(reclassify(BCE_sum_scale,
                c(quantile(BCE_sum_scale, probs = c(0.99), ype = 7),Inf,quantile(BCE_sum_scale, probs = c(0.99), type = 7))), col = matlab.like(100))

# without population corraction :: calculate ratio of generated emission to expected future change 
inequ_ras_wo_pop <- BCE_sum_scale / anomaly
plot(inequ_ras_wo_pop, col = viridis(50))
plot(inequ_ras_wo_pop,BCE_sum_scale)

# with population corraction :: calculate ratio of generated emission to expected future change 
inequ_ras_w_pop <- BCE_adj_for_pop / anomaly
plot(inequ_ras_w_pop)
plot(inequ_ras_w_pop,BCE_sum_scale)


inequ_ras_wo_pop %>% 
  #reclassify(inequ_ras_w_pop,c(quantile(inequ_ras_w_pop, probs = c(0.99), type = 7),Inf,quantile(inequ_ras_w_pop, probs = c(0.99), type = 7))) %>% 
  rasterToPoints() %>% 
  data.frame() %>% 
  ggplot()+
  geom_raster(aes(x = x, y = y, fill = layer))+
  scale_fill_gradientn( colours = rev(c("#2166ac","#2166ac","#4575b4","#74add1","#d1e5f0","white","white","white","#fddbc7","#f46d43","#d73027","#b2182b","#b2182b")))+
  coord_fixed()

par(mfrow = c(2,2))
hist(inequ_ras_wo_pop)
hist(BCE_sum_scale)
hist(anomaly)

inequ_ras_w_pop %>% 
  
  #reclassify(inequ_ras_w_pop,c(quantile(inequ_ras_w_pop, probs = c(0.99), type = 7),Inf,quantile(inequ_ras_w_pop, probs = c(0.99), type = 7))) %>% 
  rasterToPoints() %>% 
  data.frame() %>% 
  ggplot()+
  geom_raster(aes(x = x, y = y, fill = layer))+
  scale_fill_gradientn(colours = rev(c("#2166ac","#2166ac","#4575b4","#74add1","#d1e5f0","white","white","white","#fddbc7","#f46d43","#d73027","#b2182b","#b2182b")))+
  coord_fixed()

inequ_ras_wo_pop %>% 
  rasterToPoints() %>% 
  data.frame() %>% 
  ggplot()+
  geom_raster(aes(x = x, y = y, fill = layer))+
  scale_fill_gradientn(colours = rev(c("#2166ac","#2166ac","#4575b4","#74add1","#d1e5f0","white","white","white","#fddbc7","#f46d43","#d73027","#b2182b","#b2182b")))+
  coord_fixed()


plot(log(BCE_adj_for_pop),log(anomaly))

inequ <- inequ_ras_wo_pop %>% rasterToPoints() %>% as.data.frame() 
inequ <- inequ_ras_w_pop %>% rasterToPoints() %>% as.data.frame() 

str(inequ)
hist(inequ$layer)

inequ %>% 
  #filter(layer > 1) %>% 
  ggplot(aes(x, y, fill = layer )  ) +
  geom_raster()+
  scale_fill_viridis_c()+
  scale_fill_distiller(palette = "Spectral", direction = -1)+
  coord_fixed()

inequ %>% 
  #filter(layer > 1) %>% 
  ggplot(aes(x, y, fill =  ifelse(layer > quantile(layer, 0.99), quantile(layer, 0.99), layer) )  ) +
  geom_raster()+
  scale_fill_viridis_c()+
  scale_fill_distiller(palette = "Spectral", direction = -1)+
  coord_fixed()

inequ$layer_manip <- ifelse(inequ$layer > quantile(inequ$layer, 0.95), quantile(inequ$layer, 0.95), inequ$layer)

raw_ratio <- ggplot(inequ,aes(x,y,fill =  layer ) ) +
  geom_raster()+
  scale_fill_viridis_c()+
  scale_fill_distiller(palette = "Spectral")+
  coord_fixed(ylim = c(-50,50))
trimmed_ratio <- ggplot(inequ,aes(x,y,fill =  layer_manip ) ) +
  geom_raster()+
  scale_fill_viridis_c()+
  scale_fill_distiller(palette = "Spectral")+
  coord_fixed()
log_ratio <- ggplot(inequ,aes(x,y,fill =  log(layer+0.01) ) ) +
  geom_raster()+
  scale_fill_viridis_c()+
  scale_fill_distiller(palette = "Spectral")+
  coord_fixed()

gridExtra::grid.arrange(raw_ratio,trimmed_ratio,log_ratio, ncol = 1)


hist(inequ$layer)
hist(df_$layer)

rcp85_anomally <- rcp85_anom_scale %>% rasterToPoints() %>% as.data.frame()
BCE_sum_scale_plot <- BCE_sum_scale %>% rasterToPoints() %>% as.data.frame()

rcp85_anomally_plot <- ggplot(rcp85_anomally,aes(x,y,fill =  layer ) ) +
  geom_raster()+
  scale_fill_viridis_c()+
  scale_fill_distiller(palette = "Spectral")+
  coord_fixed(
    #ylim = c(-50,50),xlim = c(50,130)
  )

sum_BCE <- ggplot(BCE_sum_scale_plot,aes(x,y,fill =  layer ) ) +
  geom_raster()+
  scale_fill_viridis_c()+
  scale_fill_distiller(palette = "Spectral")+
  coord_fixed(
    #ylim = c(-50,50),xlim = c(50,130)
  )



ggplot(inequ, aes(x,y,fill = layer )) +
  geom_raster()+
  scale_fill_viridis_c()+
  scale_fill_distiller(palette = "Spectral")+
  coord_fixed(
    #ylim = c(-50,50),xlim = c(50,130)
  )

inequ$layer_tran <- inequ$layer + 0.1
inequ$layer_tran <- ifelse(inequ$layer_tran < 1.01,  ( inequ$layer_tran * (1/inequ$layer_tran)) *-1, inequ$layer_tran) 

ratio <- ggplot(inequ, aes(x,y,fill = log(layer+1))) +
  geom_raster()+
  scale_fill_viridis_c()+
  scale_fill_distiller(palette = "Spectral")+
  coord_fixed(
    #ylim = c(-50,50),xlim = c(50,130)
  )


gridExtra::grid.arrange(rcp85_anomally_plot,sum_BCE,ratio, ncol = 1)





inequ %>% ggplot()+geom_histogram(aes(x = layer))#+scale_x_continuous(limits = c(-.5,1))
inequ %>% ggplot()+geom_histogram(aes(x = layer_tran))+scale_x_continuous(limits = c(-1.000001,-.999999))

summary(inequ)





