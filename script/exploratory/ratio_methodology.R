ts_BCE <- raster("./data/causes/GHG_ts_products/BCEMAN_sum_Jan 31.grd") #%>% plot()

ts_BCE <- raster("/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/causes/GHG_ts_products/BCEMAN_sum_Jan 31.grd") #%>%  plot()

# climate anomaly data
anomaly <- stack("/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/impacts/temperature/anomally_products/MPI_ESM_future_anom_Jan 31.grd")[["MPI_rcp45"]]

anomaly <- spatial_sync_raster(anomaly,ts_BCE,method = "ngb", size_only = FALSE, verbose = T)
#anomaly <- abs(anomaly)
#anomaly <- rescale(anomaly, x.min = anomaly@data@min , x.max = anomaly@data@max,new.min = 2, new.max = 3)

# index
#anomaly <- rescale(anomaly, x.min = anomaly@data@min , x.max = anomaly@data@max, new.min = 0, new.max = 1)
#ts_BCE <- rescale(ts_BCE, x.min = ts_BCE@data@min , x.max = ts_BCE@data@max, new.min = 0, new.max = 1)


full_ras <- stack(ts_BCE, anomaly) %>% rasterToPoints() %>% data.frame()
full_ras$MPI_rcp45 <- abs(full_ras$MPI_rcp45)

str(full_ras)



ggplot()+
  geom_point(aes(x = full_ras$layer,
                 y = full_ras$MPI_rcp45),
             size = .2)

full_ras$scaled_BCE  <- scale(full_ras$layer)
full_ras$scaled_anom <- scale(full_ras$MPI_rcp45)

ggplot()+
  geom_point(aes(x = full_ras$scaled_BCE,
                 y = full_ras$scaled_anom),
             size = .2)


full_ras$scaled_BCE01  <- scales::rescale(full_ras$layer, to = c(0,1))
full_ras$scaled_anom01 <- scales::rescale(full_ras$MPI_rcp45, to = c(0,1))

ggplot()+
  geom_point(aes(x = full_ras$scaled_BCE01 %>% log(),
                 y = full_ras$scaled_anom01),
             size = .2)

full_ras$scaled_BCE23  <- scales::rescale(full_ras$layer, to = c(2,3))
full_ras$scaled_anom23 <- scales::rescale(full_ras$MPI_rcp45, to = c(2,3))

ggplot()+
  geom_point(aes(x = full_ras$scaled_BCE23,
                 y = full_ras$scaled_anom23),
             size = .2)


str(full_ras)


full_ras$raw_index <- full_ras$layer/full_ras$MPI_rcp45
full_ras$zeroone_index <- full_ras$scaled_BCE01/full_ras$scaled_anom01
full_ras$twothree_index <- full_ras$scaled_BCE23/full_ras$scaled_anom23

par(mfrow = c(3,5))

hist(full_ras$layer)
hist(full_ras$MPI_rcp45)
hist(full_ras$raw_index)
plot(full_ras$layer,full_ras$raw_index)
plot(full_ras$MPI_rcp45,full_ras$raw_index)

hist(full_ras$scaled_BCE01)
hist(full_ras$scaled_anom01)
hist(full_ras$zeroone_index)
plot(full_ras$scaled_BCE01,full_ras$zeroone_index)
plot(full_ras$scaled_anom01,full_ras$zeroone_index)

hist(full_ras$scaled_BCE23)
hist(full_ras$scaled_anom23)
hist(full_ras$twothree_index)
plot(full_ras$scaled_BCE23,full_ras$twothree_index)
plot(full_ras$scaled_anom23,full_ras$twothree_index)

# base index
ggplot(full_ras)+
  geom_raster(aes(x,y,fill = layer))+
  scale_fill_distiller(palette = "Spectral")+
  coord_sf()
ggplot(full_ras)+
  geom_raster(aes(x,y,fill = MPI_rcp45))+
  scale_fill_distiller(palette = "Spectral")+
  coord_sf()
ggplot(full_ras)+
  geom_raster(aes(x,y,fill = raw_index))+
  scale_fill_distiller(palette = "Spectral")+
  coord_sf()
ggplot(full_ras)+
  geom_raster(aes(x,y,fill = DescTools::Winsorize(raw_index, prob = c(0,.99), na.rm = T ) ) )+
  scale_fill_distiller(palette = "Spectral")+
  coord_sf()




