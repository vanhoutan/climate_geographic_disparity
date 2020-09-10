library(rnaturalearth)
library(rnaturalearthdata)
library(spatial.tools)

# function for rescaling raster values
rescale <-                                                            
  function(x, x.min = NULL, x.max = NULL, new.min = 0, new.max = 1) {
    if(is.null(x.min)) x.min = min(x)
    if(is.null(x.max)) x.max = max(x)
    new.min + (x - x.min) * ((new.max - new.min) / (x.max - x.min))}


##############################
###  Popular ggPlot theme  ###
##############################
theme_themeo <- function () { 
  theme_classic()+
    theme(strip.background = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_text(margin = margin( 0.2, unit = "cm")),
          axis.text.y = element_text(margin = margin(c(1, 0.2), unit = "cm")),
          axis.ticks.length=unit(-0.1, "cm"),
          panel.border = element_rect(colour = "black", fill=NA, size=.5),
          legend.title=element_blank(),
          strip.text=element_text(hjust=0) )}


# I'm having a lot of trouble navigating the options of developing a ratio of emissions to anomaly.
# I think we'll get closer if we clearly define what it is that we want as a final product. Rather
# than develop a bunch of products that are not quite what we want. 
# 
# What do we want?
# 1. We want to define what regions are contributing emissions to climate change but do not expect to 
# see much. 
# 2. And in contrast, what areas will see lots of climate change, but are not contributing lots of
# emissions? 
# 3. And in between, what areas generate some emissions and will see some change. Or, wont see
# change and are not emitting emissions.

# What would this look like in the ideal ratio?
# 1. Areas that generate lots of emissions and wont see much change will be > 1
# 2. Areas that generate little emissions but will see lots of change will be < 1
# 3. Areas seeing some change and emit some will be close to 1

# What are the tools, transformations, and questions to consider when developing this metric?
# 1. Are negative anomalies a good thing? Or is any anomally a bad thing? 
# 2. scaling: 0-1, 2-3, z-score
# 3. winsorizing for visualization

# worldwide country polygon
world <- ne_countries(scale = "medium", returnclass = "sf")

# black carbon emissions
ts_BCE <- raster("/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/causes/GHG_ts_products/BCEMAN_sum_Jan 31.grd") #%>%  plot()

# climate anomaly data
anomaly <- stack("/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/impacts/temperature/anomally_products/MPI_ESM_future_anom_Jan 31.grd")[["MPI_rcp26"]]
anomaly <- spatial_sync_raster(anomaly, ts_BCE, method = "ngb", size_only = FALSE, verbose = T)
anomaly <- abs(anomaly)
#anomaly <- rescale(anomaly, x.min = anomaly@data@min , x.max = anomaly@data@max,new.min = 0, new.max = 1)
plot(anomaly, col = matlab.like(100))

# index
#anomaly <- rescale(anomaly, x.min = anomaly@data@min , x.max = anomaly@data@max, new.min = 2, new.max = 3)
#ts_BCE <- rescale(ts_BCE, x.min = ts_BCE@data@min , x.max = ts_BCE@data@max, new.min = 2, new.max = 3)

#anomaly <- scale(anomaly)
#ts_BCE <- scale(ts_BCE)

par(mfrow = c(1,2))
plot(anomaly, col = matlab.like(100), axes = F); axis(1); axis(2, las = 2)
map("world", fill = F, add = T, resolution = 0)
plot(ts_BCE, col = matlab.like(100), axes = F); axis(1); axis(2, las = 2)
map("world", fill = F, add = T, resolution = 0)

index <- ts_BCE/anomaly
#index <- reclassify(index,c(quantile(index, probs = c(0.99), type = 7),Inf,quantile(index, probs = c(0.99), type = 7))) 

# sum of BCE
map_of_emission <-
  ts_BCE %>% 
  rasterToPoints() %>% 
  data.frame() %>% 
  ggplot()+
  geom_raster(aes(x = x, y = y, fill = layer ))+
  geom_sf(data = world, fill = NA, col = "gray")+
  # scale_fill_distiller(palette = "Spectral", direction = -1)  +
  scale_fill_viridis("total BCE")  +
  coord_sf()+
  scale_x_continuous(expand = c(0,0), "")+
  scale_y_continuous(expand = c(0,0), "")+
  # theme_void()+ 
  theme_pubr() + 
  theme(legend.position = "right")

# histo of sum of BCE
hist_of_emission <-
  ts_BCE %>% 
  rasterToPoints() %>% 
  data.frame() %>% 
  ggplot()+
  geom_histogram(aes(layer))+
  ylab("# of grids") + xlab("BCE (ADD UNIT)") + 
  # theme_themeo() + 
  theme_pubr() + 
  theme(legend.position = "right")

# map of anomaly
map_of_anomally <- 
  anomaly %>% 
  rasterToPoints() %>% 
  data.frame() %>% 
  ggplot()+
  geom_tile(aes(x = x, y = y, fill = layer))+
  geom_sf(data = world, fill = NA, col = "gray")+
  scale_fill_distiller(palette = "Spectral", "Anomaly", direction = -1)  +
  # scale_fill_viridis("Anomaly")  +
  coord_sf()+
  scale_x_continuous(expand = c(0,0), "")+
  scale_y_continuous(expand = c(0,0), "")+
  # theme_void() + 
  theme_pubr() + 
  theme(legend.position = "right")

# histo of anomally
hist_of_anomaly <- 
  anomaly %>% 
  rasterToPoints() %>% 
  data.frame() %>% 
  ggplot()+
  geom_histogram(aes(layer))+
  ylab("# of grids") + xlab("Anomaly (ADD UNIT)") + 
  # theme_themeo() + 
  theme_pubr()

# map of index
map_of_index <- 
  index %>% 
  rasterToPoints() %>% 
  data.frame() %>% 
  mutate(layer = DescTools::Winsorize(layer, probs = c(.025,.95), na.rm = T)) %>% 
  #mutate(layer = ifelse(layer > 1, 1, 0)) %>% 
  ggplot()+
  geom_tile(aes(x = x, y = y, fill = layer))+
  geom_sf(data = world, fill = NA, col = "gray")+
  # scale_fill_distiller(palette = "Spectral", direction = -1)  +
  scale_fill_viridis("Index", limits = c(-6,6))  +
  coord_sf()+
  scale_x_continuous(expand = c(0,0), "")+
  scale_y_continuous(expand = c(0,0), "")+
  # theme_void() + 
  theme_pubr() + 
  theme(legend.position = "right") 

# histo of sum of index
hist_of_index <- 
  index %>% 
  rasterToPoints() %>% 
  data.frame() %>% 
  mutate(layer = DescTools::Winsorize(layer, probs = c(.025,.95), na.rm = T)) %>% 
  ggplot()+
  geom_histogram(aes(layer))+
  ylab("# of grids") + xlab("Index (ADD UNIT)") + 
  # theme_themeo() + 
  theme_pubr()

gridExtra::grid.arrange(map_of_emission,
                        map_of_anomally,
                        map_of_index, 
                        hist_of_emission,
                        hist_of_anomaly,
                        hist_of_index,
                        nrow = 2)

# rise
rise <- (anomaly@data@max - anomaly@data@min)

#run
run <- (ts_BCE@data@max - ts_BCE@data@min)

rise/run


stack(ts_BCE,anomaly) %>% 
  rasterToPoints() %>% 
  data.frame() %>% 
  ggplot() +
  geom_point(aes(x = layer.1, y = layer.2,
                 fill = ifelse(layer.1/layer.2 > 1/(rise/run), 1, 0) # when inputs are raw
                #,fill = ifelse(layer.1/layer.2 > 1,1,0)
                #,fill = layer.1/layer.2
                 ), 
             size = 1, 
             alpha = 0.5,
             shape = 21
             ) + 
  geom_abline(intercept = 0, slope = rise / run) + 
  geom_hline(aes(yintercept = median(layer.2))) +
  
 # scale_fill_distiller(palette = "RdBu")+
  scale_fill_gradientn(colours = rev(c("#2166ac","#2166ac","#4575b4",
                                       "#74add1","#d1e5f0","white","#fddbc7","#f46d43","#d73027",
                                       "#b2182b","#b2182b")))  
  
  
median(ts_BCE, na.rm = T)
median(anomaly, na.rm = T)

mean(ts_BCE@data@values, na.rm = T)
mean(anomaly@data@values, na.rm = T)

  
 



