str(raster_df)

pheno <- raster_df %>% 
            #filter(lubridate::year(variable) < 1890) %>% 
            mutate(month = lubridate::month(variable))

pheno %>% sample_n(100000) %>% 
  filter(x > 60) %>% 
  group_by(lubridate::year(variable),
           lubridate::month(variable)
           ) %>% 
  mutate(ann_mn = mean(value),
           year = lubridate::year(variable)) %>% 
  
  
  ungroup() %>% #filter(year == 1870) %>% 
  ggplot()+
  geom_smooth(aes(x=month,y = ann_mn, group = year, color = year), se = F, alpha = .5,size = .3)+
  scale_color_distiller(palette = 'Spectral')+
  theme_bw()+
  scale_x_continuous(expand = c(0,0))







raster <- MPI_ESM_rcp85_tas
# convert raster stack to dataframe and plot spatial time series, only for viz:
climateNC_to_dataframe <- function(raster){
  
  raster_df <- 
    raster %>% 
    rasterToPoints() %>% 
    as.data.frame() %>% 
    reshape2::melt(id.vars = c("x","y")) %>% 
    mutate(variable = str_remove(variable, pattern = "X") %>% lubridate::ymd(),
           value = value - 273.15)  # convert from Kelvin to C
  
  str(raster_df)
  
  
  spatial_ts_plot <- 
    raster_df %>% filter(lubridate::year(variable) == 1980 ) %>% 
    ggplot()+
    geom_raster(aes(x,y, fill = value))+facet_wrap(~variable)+
    scale_fill_viridis_c()+coord_fixed()
  
  
  ts_plot <- raster_df %>% sample_n(10000) %>% 
    mutate(north_south = ifelse(y > 0, "north", "south")) %>% 
    group_by(north_south,
             lubridate::year(variable)
             #lubridate::month(variable)
    ) %>% 
    summarise(mean_by_year = median(value)) %>% 
    ungroup() %>%  
    
    ggplot()+
    geom_line(aes(x = `lubridate::year(variable)` , y = mean_by_year,color = north_south))+
    scale_color_manual(values = c("blue4", "red4"))+
    
    #geom_point(data = spatial_ts %>% filter(y > 0) %>% sample_n(1000),aes(x = variable, y = value), size = .1, alpha = .5)+
    #geom_point(data = spatial_ts %>% filter(y < 0) %>% sample_n(1000),aes(x = variable, y = value), size = .1, alpha = .5)+
    theme_bw()
  
  
  gridExtra::grid.arrange(spatial_ts_plot,ts_plot)
  
}

#climateNC_to_dataframe(MPI_ESM_historical_tas)

# calculate future anommally from historical record...
# anomally = (mean of monthly means in present era) - (mean of monthly means in future projects)



