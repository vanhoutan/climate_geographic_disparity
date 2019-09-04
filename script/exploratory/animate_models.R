# Plotting multifile CMIP5 outputs

#modified by ktanaka on 08.21.2019

setwd("~/Desktop/climate/KV_climate/climate_impacts_2019/data/impacts/temperature/CMIP5/CMCC-CM/historical")

historical_tas <- list.files('/Users/tgagne/climate_impacts_2019/data/impacts/temperature/CMIP5/CMCC-CM/historical/', pattern = '\\.nc$')
# historical_tas <- paste0('~/climate_impacts_2019/data/impacts/temperature/CMIP5/CMCC-CM/historical/',historical_tas)

series <- stack()

for(i in 1:length(historical_tas)){
  decade <- stack(historical_tas[i])
  series <- stack(series,decade)
}

series

#animate(series)
plot(series[[1:50]])

raster_df <- 
  series %>% 
  rasterToPoints() %>% 
  as.data.frame() %>% 
  reshape2::melt(id.vars = c("x","y")) %>% 
  mutate(variable = str_remove(variable, pattern = "X") %>% 
           lubridate::ymd(), value = value - 273.15) 

