rm(list = ls())

library(rnaturalearthdata)
library(rnaturalearth)
library(spatial.tools)
library(RColorBrewer)
library(rmapshaper)
library(tidyverse)
library(ggridges)
library(raster)
library(rgdal)
library(sf)
library(rnaturalearthhires)
library(DescTools)
library(colorRamps)
library(maps)
library(ggpubr)
library(sp)
library(colortools)
library(cowplot)
library(ggdark)


###########################################################
### Global Emission Layer (CO2+BC+CH$+N2O GTP adjusted) ###
###########################################################
load("~/clim_geo_disp/outputs/BC_CO2_CH4_N2O_Combined_1970-2018.RData")
anomaly = stack("~/clim_geo_disp/data/CMIP5 ENSMN RCP8.5 anomaly (2050-2099)-(1956-2005).nc", varname = "anomaly") # CMIP5 data that has 1*1 resolution
bc_co2_ch4_n2o_adjusted = resample(ge, raster::rotate(anomaly), method = "bilinear") # interpolate the emission layer on 1*1 resolution
bco2 = bc_co2_ch4_n2o_adjusted
bco2 = bco2 * 31556952 # 31556952 Seconds equals 1 Gregorian Year
xlab = bquote('Emissions  ('*CO[2]* '+BC+' *CH[4]* '+' *N[2]* 'O: kg ' *m^-2~y^-1*')') #label A
xlab = bquote('Emissions  (kg ' *m^-2~y^-1*')') #label B
rm(bc_co2_ch4_n2o_adjusted)


#####################################################################################################################
### Gridded Population of the World (GPWv4), use in case of per capita analysis. https://doi.org/10.7927/H49C6VHW ###
#####################################################################################################################
gpw_pop <- stack("~/clim_geo_disp/data/gpw/gpw_v4_population_density_rev11_1_deg.nc", varname = "Population Density, v4.11 (2000, 2005, 2010, 2015, 2020): 1 degree")
# gpw_pop <- stack("/Desktop/gpw/gpw_v4_population_density_rev11_30_min.nc", varname = "Population Density, v4.11 (2000, 2005, 2010, 2015, 2020): 30 arc-minutes")
# gpw_pop <- stack("/Desktop/gpw/gpw_v4_population_density_rev11_15_min.nc", varname = "Population Density, v4.11 (2000, 2005, 2010, 2015, 2020): 15 arc-minutes")
# gpw_pop <- stack("/Desktop/gpw/gpw_v4_population_density_rev11_2pt5_min.nc", varname = "Population Density, v4.11 (2000, 2005, 2010, 2015, 2020): 2.5 arc-minutes")

gpw_pop1 <- gpw_pop[["X1"]] #2000 pop density estimate
gpw_pop2 <- gpw_pop[["X2"]] #2005 pop density estimate
gpw_pop3 <- gpw_pop[["X3"]] #2010 pop density estimate
gpw_pop4 <- gpw_pop[["X4"]] #2015 pop density estimate
gpw_pop5 <- gpw_pop[["X5"]] #2020 pop density estimate

gpw_pop = stack(gpw_pop1, gpw_pop2, gpw_pop3, gpw_pop4, gpw_pop5)
gpw_pop = mean(gpw_pop)

gpw_pop <- reclassify(gpw_pop,rcl = c(NA,NA,0))
gpw_pop <- spatial_sync_raster(gpw_pop, bco2, method = "ngb", size_only = FALSE, verbose = T)

# emission per capita
bco2_adj_for_pop <- bco2/gpw_pop
bco2_adj_for_pop[!is.finite(bco2_adj_for_pop)] <- NA
bco2_adj_for_pop

par(mfrow = c(3,1))
plot(log10(gpw_pop+1), col = matlab.like(100), main = bquote('Avegrage population count 2000-2020,  log(pop ' *km^-2*'+1)'))
plot(log10(bco2), col = matlab.like(100), main = bquote('Anthropogenic Emissions, ('*CO[2]* '+' *CH[4]* '+' *N[2]* 'O+BC: kg ' *m^-2~y^-1*')'))
plot(log10(bco2_adj_for_pop), col = matlab.like(100), main = bquote('Anthropogenic Emissions per capita, ('*CO[2]* '+' *CH[4]* '+' *N[2]* 'O+BC: kg ' *m^-2~y^-1*')'))

world <- ne_countries(scale = "small", returnclass = "sf") #worldwide country polygon

gpw_pop <- gpw_pop %>% rasterToPoints() %>% data.frame()

pdf("/Desktop/CIESIN_GPWv4.pdf", height = 6, width = 10)
ggplot(gpw_pop %>% sample_frac(1)) +
  geom_raster(aes(x = x, y = y, fill = log10(layer+1)), show.legend = T) +
  geom_sf(data = world, fill = NA, size = .1, color = "gray") +
  scale_fill_gradientn(colours = c( "black", "cyan", "red"), 
                       name = bquote('Avegrage population count 2000-2020,  log10(pop ' *km^-2*'+1)')) +
  coord_sf(xlim = range(gpw_pop$x), ylim = range(gpw_pop$y)) +
  scale_x_continuous(expand = c(-0, 0)) +
  scale_y_continuous(expand = c(-0, 0)) +
  theme_pubr() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        legend.position = "bottom",
        legend.justification = c(1, 0))
dev.off()

xlab_per_capita = bquote('Emissions per capita  (kg ' *m^-2~y^-1*')')

rm(gpw_pop, gpw_pop1, gpw_pop2, gpw_pop3, gpw_pop4, gpw_pop5)


#########################################################
### calculate LCDI for every climate change scenario  ### 
### RCP = 4.5 & 8.5                                   ###
### 2005-2055 (ensemble_1)                            ###
### 2050-2099 (ensemnle_2)                            ###
#########################################################

scenario = function(clim_anom, rcp){
  
  # clim_anom = "ensemble_1"
  # rcp = "RCP4.5"
  
  setwd(paste0("~/clim_geo_disp/data"))
  
  #CMIP5 ENSMN anomalies
  if (clim_anom == "ensemble_1") anomaly = stack(paste0("CMIP5 ENSMN ", rcp, " anomaly (2006-2055)-(1956-2005).nc"), varname = "anomaly") #RCP8.5 or 4.5 anomaly (2050-2099)-(1956-2005)
  if (clim_anom == "ensemble_2") anomaly = stack(paste0("CMIP5 ENSMN ", rcp, " anomaly (2050-2099)-(1956-2005).nc"), varname = "anomaly") #RCP8.5 or 4.5 anomaly (2006-2055)-(1956-2005)
  
  anomaly <- spatial_sync_raster(anomaly, bco2, method = "ngb", size_only = F, verbose = T)
  
  emission_temp <- 
    stack(bco2, anomaly) %>%
    # stack(bco2_adj_for_pop, anomaly) %>%
    rasterToPoints() %>% 
    data.frame() %>% 
    mutate(emission = .[[3]], anomaly =.[[4]]) %>% 
    mutate(emission = DescTools::Winsorize(emission, probs = c(0,.999), na.rm = T),
           anomaly = DescTools::Winsorize(anomaly, probs = c(0,.999), na.rm = T)) %>%
    dplyr::select(x, y, emission, anomaly) %>% 
    mutate(ratio = emission / anomaly) 
  
  emission_temp <- emission_temp[!is.na(emission_temp$anomaly),]
  
  # rise, A
  rise <- (max(emission_temp$anomaly, na.rm = T) - min(emission_temp$anomaly, na.rm = T))
  
  # run, B
  run <- (max(emission_temp$emission, na.rm = T)  - min(emission_temp$emission, na.rm = T))
  
  # slope
  slope = rise / run
  round(slope, 2)
  
  ###############################################
  ### Calculate euclidean distance from slope ###
  ###############################################
  emission_temp$disparity <- (emission_temp$anomaly - (slope*emission_temp$emission + min(emission_temp$anomaly, na.rm = T)))/(sqrt(abs((slope^2)-1)))
  
  ########################################################################
  ### turn in to point file to detect overlap with regions of interest ###
  ########################################################################
  disparity <- st_as_sf(x = emission_temp, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" )
  summary(disparity)
  
  ratio = emission_temp[which(complete.cases(emission_temp$disparity)),]
  positive = round(dim((subset(ratio, disparity > 0)))[1]/dim(ratio)[1], 3)
  negative = round(dim((subset(ratio, disparity < 0)))[1]/dim(ratio)[1], 3)
  positive; negative
  
  if (clim_anom == "ensemble_1") label = paste0("Experiment: ", rcp, " \n21st Century Period: 2006-2055")
  if (clim_anom == "ensemble_2") label = paste0("Experiment: ", rcp, " \n21st Century Period: 2050-2099")
  
  label = paste0(label, "\n+/- Disparity Ratio = ", positive, "/", negative)
  
  
  ################################
  ### Set Universal LCDI limit ###
  ################################
  disparity_limits = c(-max(abs(emission_temp$disparity), na.rm = T), max(abs(emission_temp$disparity), na.rm = T)) 
  disparity_limits
  
  
  #########################
  ### LCDI scatter plot ###
  #########################
  
  xy_plot_suppl <-
    ggplot(emission_temp %>% 
             sample_frac(1)) +
    geom_point(aes(x = emission, y = anomaly, color = disparity),
               size = 4, 
               alpha = 0.5, 
               shape = 21,
               show.legend = T) +
    geom_abline(
      intercept = min(emission_temp$anomaly, na.rm = T),
      color = "black",
      slope = slope) +
    scale_color_gradientn(
      colours = c("cyan", "black", "red"),
      limits = disparity_limits,
      name = "LCDI") + 
    scale_x_continuous(expand = c(0,0), limits = c(0, 4.92)) +
    # scale_x_continuous(expand = c(0,0), limits = c(0, 5000)) +
    scale_y_continuous(expand = c(0,0), limits = c(0, 10.5)) +
    xlab(xlab) +
    ylab(expression(paste('Surface Temperature Anomaly (',~degree,'C)', sep = ''))) + 
    theme_pubr() +
    theme(legend.position = c(0.1, 0.85), text = element_text(size = 15)) + 
    annotate("text",
             x = Inf,
             y = Inf,
             hjust = 1,
             vjust = 1,
             color = "black",
             size = 5,
             label = label)
  
  xy_plot_main <-
    ggplot(emission_temp %>% 
             sample_frac(0.6)) +
    geom_point(aes(x = emission, y = anomaly, color = disparity),
               size = 6,
               alpha = 0.6,
               shape = 21) +
    geom_abline(
      intercept = min(emission_temp$anomaly, na.rm = T),
      slope = slope, 
      color = "black") +
    scale_color_gradientn(colours = c("cyan", "black", "red"), 
                          values = scales::rescale(c(-0.5, -0.04, 0.1, 0.2, 0.5)),
                          limits = disparity_limits,
                          name = "LCDI") + 
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    xlab(bquote('Emissions  (kg ' *m^-2~y^-1*')')) +
    ylab(expression(paste('Temperature Anomaly (',~degree,'C)', sep = ''))) + 
    theme_bw(I(20)) +
    theme(legend.position = "none", 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) 
  
  xy_plot_main_blank <-
    ggplot(emission_temp %>% sample_frac(0.6), aes(x = emission, y = anomaly)) + 
    geom_blank() + 
    geom_abline(intercept = min(emission_temp$anomaly, na.rm = T), slope = slope) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    xlab(bquote('Emissions  (kg ' *m^-2~y^-1*')')) +
    ylab(expression(paste('Temperature Anomaly (',~degree,'C)', sep = ''))) + 
    theme_bw(I(20)) +
    theme(legend.position = "none", 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) 
  
  
  #####################
  ### LCDI map plot ###
  #####################
  
  world <- ne_countries(scale = "small", returnclass = "sf") #worldwide country polygon
  
  map_plot_suppl <-
    ggplot(emission_temp) +
    geom_raster(aes(x = x, y = y, fill = disparity), show.legend = T) +
    geom_sf(data = world, fill = NA, size = 0.15, color = "lightgray") +
    scale_fill_gradientn(colours = c("cyan", "black", "red"), 
                         limits = disparity_limits,
                         name = "LCDI") +
    scale_x_continuous(expand = c(-0.005, 0), "") +
    scale_y_continuous(expand = c(-0.005, 0), "") +
    ggtitle(label) +
    theme_pubr() + 
    theme(
      legend.position = "right",
      legend.justification = c(1, 0))
  
  
  map_plot_main <-
    ggplot(emission_temp) +
    geom_raster(aes(x = x, y = y, fill = disparity), show.legend = T) +
    geom_sf(data = world, fill = NA, size = 0.15, color = "lightgray") +
    scale_fill_gradientn(colours = c("cyan", "black", "red"), 
                         values = scales::rescale(c(-0.5, -0.04, 0.1, 0.2, 0.5)),
                         limits = disparity_limits,
                         name = "LCDI") +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(-0.01, 0)) +
    theme_void(I(15)) +
    theme(legend.position = "bottom", legend.justification = c(0.99,0.01))+ 
    guides(fill = guide_colorbar(title.position = "bottom", 
                                 title.vjust = 0.9,
                                 title.hjust = 0.5,
                                 frame.linewidth = 1,
                                 frame.colour = "black", 
                                 barwidth = 10, barheight = 1.5)) 
  
  ### save figures ###
  setwd("~/Desktop")
  
  pdf("Figure_2a_blank.pdf", height = 5, width = 5)
  print(xy_plot_main_blank)
  dev.off()
  
  pdf("Figure_2a.pdf", height = 5, width = 5)
  print(xy_plot_main)
  dev.off()
  
  pdf("Figure_2a.pdf", height = 5, width = 5)
  p = ggdraw() +
    draw_plot(xy_plot_main, x = 0, y = 0, width = 0.9, height = 0.9) + 
    draw_plot_label(label = "a", size = 25, x = 0, y = 1)
  print(p)
  dev.off()
  
  pdf("Figure_2b.pdf", height = 5, width = 9.5)
  p = ggdraw() +
    draw_plot(map_plot_main, x = 0.02, y = 0, width = 0.98, height = 0.98) + 
    draw_plot_label(label = "b", size = 30, x = 0, y = 1)
  print(p)
  dev.off()
  
  pdf("modified_LCDI_legend.pdf", height = 1, width = 3)
  legend <- cowplot::get_legend(map_plot_main)
  grid::grid.newpage()
  grid::grid.draw(legend)
  dev.off()
  
  if (clim_anom == "ensemble_1") pdf(paste0("Disparity_2006-2055_Ensemble_XY_", rcp , ".pdf"), height = 6, width = 6)
  if (clim_anom == "ensemble_2") pdf(paste0("Disparity_2050-2099_Ensemble_XY_", rcp , ".pdf"), height = 6, width = 6)
  print(xy_plot_suppl)  
  dev.off()
  
  if (clim_anom == "ensemble_1") pdf(paste0("Disparity_2006-2055_Ensemble_Map_", rcp , ".pdf"), height = 6, width = 10)
  if (clim_anom == "ensemble_2") pdf(paste0("Disparity_2050-2099_Ensemble_Map_", rcp , ".pdf"), height = 6, width = 10)
  print(map_plot_suppl)  
  dev.off()
  
  
  ################################################
  ### visualize emissions and temperature data ###
  ################################################
  if (clim_anom == "ensemble_1") title = paste0("Experiment: ", rcp, "\n21st century period: 2006-2055")
  if (clim_anom == "ensemble_2") title = paste0("Experiment: ", rcp, "\n21st century period: 2050-2099")
  
  ## emissions/temp ratio map ###
  map_ratio = ggplot(emission_temp) +
    geom_raster(aes(x = x, y = y, fill = emission/anomaly), show.legend = T) +
    geom_sf(data = world, fill = NA, size = .2, color = "lightgray")+
    scale_fill_gradientn(colours = c( "black", "cyan", "red"), 
                         values = scales::rescale(c(-0.5, -0.4, -0.3, -0.2, 0.5)),
                         bquote('Emissions ('*CO[2]* '+BC+' *CH[4]* '+' *N[2]* 'O: kg ' *m^-2~y^-1*') / Surface Temperature Anomaly (°C)')) +
    coord_sf(xlim = range(emission_temp$x), ylim = range(emission_temp$y)) +
    scale_x_continuous(expand = c(-0, 0), "") +
    scale_y_continuous(expand = c(-0, 0), "") +
    theme_pubr() + 
    theme(legend.position = "bottom", legend.justification = c(1,0))+
    ggtitle(title)
  
  pdf(paste0("~/Desktop/Ratio_", title,".pdf"), height = 6, width = 9)
  print(map_ratio)
  dev.off()
  
  
  hist_emission <- 
    ggplot(emission_temp) + geom_histogram(aes(emission)) +   
    ylab("") +
    xlab("Emissions (kg m-2)") +
    theme_pubr()
  
  map_emission_suppl_a <-
    ggplot(emission_temp) +
    geom_raster(aes(x = x, y = y, fill = emission), show.legend = T) +
    geom_sf(data = world, fill = NA, size = .1,color = "gray")+
    scale_fill_gradientn(colours = c( "black", "cyan", "red"), 
                         name = bquote('kg ' *m^-2~y^-1*''))+
    coord_sf(xlim = range(emission_temp$x), ylim = range(emission_temp$y)) +
    scale_x_continuous(expand = c(-0, 0)) +
    scale_y_continuous(expand = c(-0, 0)) +
    theme_pubr() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(), 
          legend.position = "bottom",
          legend.justification = c(1, 0))
  
  map_emission_suppl_b <-
    ggplot(emission_temp) +
    geom_raster(aes(x = x, y = y, fill = log10(emission)), show.legend = T) +
    geom_sf(data = world, fill = NA, size = .1, color = "white")+
    scale_fill_gradientn(colours = c( "black", "cyan", "red"), 
                         name = bquote('log10(kg ' *m^-2~y^-1*')'))+
    coord_sf(xlim = range(emission_temp$x), ylim = range(emission_temp$y)) +
    scale_x_continuous(expand = c(-0, 0)) +
    scale_y_continuous(expand = c(-0, 0)) +
    theme_pubr() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(), 
          legend.position = "bottom",
          legend.justification = c(1, 0))
  
  map_emission_main_a <-
    ggplot(emission_temp) +
    geom_raster(aes(x = x, 
                    y = y, 
                    fill = log10(emission + 1)),
                show.legend = T) +
    geom_sf(data = world, fill = NA, size = .1,color = "gray")+
    scale_fill_gradientn(colours = c( "black", "cyan", "red"), 
                         values = scales::rescale(c(-0.5, -0.49, 0, 0.0001, 0.5)),
                         name = bquote('Emissions, log10(kg ' *m^-2~y^-1*'+1)'))+
    coord_sf(xlim = range(emission_temp$x), ylim = range(emission_temp$y)) +
    scale_y_continuous(expand = c(-0, 0)) +
    scale_x_continuous(expand = c(-0, 0)) +
    theme_void(I(15)) +
    theme(legend.position = "bottom", legend.justification = c(0.9, 0.1)) + 
    guides(fill = guide_colorbar(title.position = "bottom", 
                                 title.vjust = 1, 
                                 title.hjust = 1,
                                 frame.colour = "black", 
                                 barwidth = 15, 
                                 barheight = 1.5)) 
  
  lat_mean = aggregate(emission_temp$emission, list(emission_temp$y), mean)
  colnames(lat_mean) <- c("Lat", "emission")
  
  map_emission_main_b <- 
    ggplot(lat_mean, aes(x="", y=Lat, fill=emission)) + 
    geom_raster() + 
    # coord_cartesian(ylim = range(emission_temp$y)) +
    # scale_y_continuous(expand = c(-0, 0)) +
    # scale_x_discrete(expand = c(-0, 0)) +
    theme_void(I(10)) +
    theme(legend.position="none") + 
    scale_fill_gradientn(colours = c( "black", "cyan", "red"), name = "")
  
  hist_anomlay <- 
    ggplot(emission_temp) + geom_histogram(aes(anomaly)) + 
    ylab("") + xlab("Temperature anomaly (deg C)") +
    theme_pubr()
  
  map_anomaly_suppl <-
    ggplot(emission_temp) +
    geom_raster(aes(x = x, 
                    y = y,
                    fill = anomaly),
                show.legend = T) +
    geom_sf(data = world, fill = NA, size = .2, color = "lightgray")+
    scale_fill_gradientn(colours = c("black", "cyan", "red"), 
                         limits = c(0, 10.5),
                         expression(paste('(',~degree,'C)', sep = ''))) +
    coord_sf(xlim = range(emission_temp$x), ylim = range(emission_temp$y)) +
    scale_x_continuous(expand = c(-0, 0)) +
    scale_y_continuous(expand = c(-0, 0)) +
    theme_pubr() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(), 
          legend.position = "bottom",
          legend.justification = c(1, 0)) + 
    ggtitle(title)
  
  map_anomaly_main_a <-
    ggplot(emission_temp) +
    geom_raster(aes(x = x, 
                    y = y,
                    fill = anomaly), 
                show.legend = T)+
    geom_sf(data = world, fill = NA, size = .2, color = "lightgray")+
    scale_fill_gradientn(colours = c("black", "cyan", "red"), 
                         breaks = c(2,4,6,8,10),
                         expression(paste('Surface temperature anomaly (',~degree,'C)', sep = ''))) +
    coord_sf(xlim = range(emission_temp$x), ylim = range(emission_temp$y)) +
    scale_x_continuous(expand = c(-0, 0)) +
    scale_y_continuous(expand = c(-0, 0)) +
    theme_void(I(15)) +
    theme(legend.position = "bottom",
          legend.justification = c(0.99, 0)) + 
    guides(fill = guide_colorbar(title.position = "left", 
                                 title.vjust = 1, 
                                 title.hjust = 0.5,
                                 frame.colour = "black", 
                                 barwidth = 10, 
                                 barheight = 1.5)) 
  
  lat_mean = aggregate(emission_temp$anomaly, list(emission_temp$y), mean)
  colnames(lat_mean) <- c("Lat", "anomaly")
  
  map_anomaly_main_b <- 
    ggplot(lat_mean, aes(x="", y=Lat, fill=anomaly)) + 
    geom_raster() +
    # coord_cartesian(ylim = range(emission_temp$y)) +
    # scale_y_continuous(expand = c(-0, 0)) +
    # scale_x_discrete(expand = c(-0, 0)) +
    theme_void(I(10)) + 
    theme(legend.position="none") + 
    scale_fill_gradientn(colours = c( "black", "cyan", "red"), name = "")
  
  
  pdf("Emissions_log10.pdf", height = 4, width = 6)
  print(map_emission_suppl_b)
  dev.off()
  
  pdf("Figure_1a.pdf", height = 5, width = 10)
  print(map_emission_main_a)
  dev.off()
  
  pdf("Figure_1b.pdf", height = 5, width = 1)
  print(map_emission_main_b)
  dev.off()
  
  pdf("Figure_1c.pdf", height = 5, width = 10)
  print(map_anomaly_main_a)
  dev.off()
  
  pdf("Figure_1d.pdf", height = 5, width = 1)
  print(map_anomaly_main_b)
  dev.off()
  
  if (clim_anom == "ensemble_1") pdf(paste0("Disparity_2006-2055_Ensemble_Anomaly_", rcp , ".pdf"), height = 5, width = 9)
  if (clim_anom == "ensemble_2") pdf(paste0("Disparity_2050-2099_Ensemble_Anomaly_", rcp , ".pdf"), height = 5, width = 9)
  map_anomaly_suppl
  dev.off()
  
  
  ###########################################################
  ### Regional boundary datasets and disparity summaries  ###
  ### 1. Terrestorical biomes                             ###
  ### 2. Marine Realms                                    ###
  ### 3. Geographical subregions                          ###
  ### 4. US States                                        ###
  ###########################################################
  
  ###############################################################
  ### Terrestrial Ecosystems of the World (TEOW), version 2.0 ###
  ###############################################################
  
  # Olson, D. M., Dinerstein, E., Wikramanayake, E. D., Burgess, N. D., Powell, G. V. N., Underwood, E. C., D'Amico, J. A., Itoua, I., Strand, H. E., Morrison, J. C., Loucks, C. J., Allnutt, T. F., Ricketts, T. H., Kura, Y., Lamoreux, J. F., Wettengel, W. W., Hedao, P., Kassem, K. R. 2001. Terrestrial ecoregions of the world: a new map of life on Earth. Bioscience 51(11):933-938.
  
  shape <- readOGR(dsn = "~/clim_geo_disp/data/TEOW", layer = "wwf_terr_ecos") 
  
  # shape <- ms_simplify(shape, keep = 0.001, keep_shapes = F) # simplify shapefile (saves computing time)
  shape <- shape %>% st_as_sf()  
  
  shape$BIOME <- as.factor(shape$BIOME)
  shape$BIOME <- fct_recode(shape$BIOME, 
                            Tropical_and_Subtropical_Moist_Broadleaf_Forests             = "1",
                            Tropical_and_Subtropical_Dry_Broadleaf_Forests               = "2",
                            Tropical_and_Subtropical_Coniferous_Forests                  = "3",
                            Temperate_Broadleaf_and_Mixed_Forests                        = "4",
                            Temperate_Conifer_Forests                                    = "5",
                            Boreal_Forests_Taiga                                         = "6",
                            Tropical_and_Subtropical_Grasslands_Savannas_and_Shrublands  = "7",
                            Temperate_Grasslands_Savannas_and_Shrublands                 = "8",
                            Flooded_Grasslands_and_Savannas                              = "9", 
                            Montane_Grasslands_and_Shrublands                            = "10",
                            Tundra                                                       = "11",
                            Mediterranean_Forests_Woodlands_and_Scrub                    = "12",
                            Deserts_and_Xeric_Shrublands                                 = "13",
                            Mangroves                                                    = "14",
                            Large_Inland_Waterbodies                                     = "98",
                            Polar_Artic                                                  = "99"
  )
  
  # pull out inland water bodies
  shape <- shape %>% filter(!BIOME %in% c("Large_Inland_Waterbodies",""))
  
  # find intersections with calculated LCDI
  intersection_biome <- st_intersection(disparity,shape)
  rm(shape)
  
  
  ########################################################################################
  ### Marine Ecoregions of the World: A Bioregionalization of Coastal and Shelf Areas. ###
  ########################################################################################
  
  # Spalding, M.D., Fox, H.E., Allen, G.R., Davidson, N., Ferdaña, Z.A., Finlayson, M.A.X., Halpern, B.S., Jorge, M.A., Lombana, A.L., Lourie, S.A. and Martin, K.D., 2007. Marine ecoregions of the world: a bioregionalization of coastal and shelf areas. BioScience, 57(7), pp.573-583.
  
  shape_MEOW <- readOGR(dsn = "~/clim_geo_disp/data/MEOW_2", layer = "WCMC-036-MEOW-PPOW-2007-2012-NoCoast")  
  
  # shape_MEOW <- ms_simplify(shape_MEOW, keep = 0.001, keep_shapes = F) # simplify shapefile (saves computing time)
  shape_MEOW <- shape_MEOW %>% st_as_sf()  
  
  # clip out marine realms overlapping on land
  # land <- ne_download(type = "land", category = 'physical', returnclass = "sf")
  load("~/clim_geo_disp/data/land_ocean_df.RData")
  land <- land %>% st_set_precision(1000000) %>% lwgeom::st_make_valid()
  shape_MEOW <- st_difference(shape_MEOW, st_union(land))
  
  # find intersections with calculated LCDI
  intersection_realm <- st_intersection(disparity,shape_MEOW)
  rm(land, shape_MEOW)
  
  
  ##########################################
  ### National boundaries including EEZs ###
  ##########################################
  
  # Flanders Marine Institute (2018). Maritime Boundaries Geodatabase: Maritime Boundaries and Exclusive Economic Zones (200NM), version 10. Available online at http://www.marineregions.org/. https://doi.org/10.14284/312
  
  # EEZ land_union shapefile
  eez_land <- readOGR(dsn = "~/clim_geo_disp/data/EEZ_land_union", layer = "EEZ_land_v2_201410")  
  # eez_land <- ms_simplify(eez_land, keep = 0.001, keep_shapes = F) # simplify shapefile (saves computing time)
  
  eez_land <- eez_land %>% st_as_sf()  
  
  eez_land$Country <- as.factor(eez_land$Country)
  
  # find intersections with calculated LCDI
  intersection_land_eez <- st_intersection(disparity,eez_land)
  rm(eez_land)
  
  
  #################
  ### US states ###
  #################
  
  #  Andy South (2017). rnaturalearth: World Map Data from Natural Earth. R package version 0.1.0. https://CRAN.R-project.org/package=rnaturalearth
  
  states <- ne_states(country = "United States of America", returnclass = "sf")
  
  # find intersections with calculated LCDI
  intersection_states <- st_intersection(disparity, states)
  rm(states)
  
  
  ########################
  #### Ocean vs. Land ####
  ########################
  
  # ocean <- ne_download(type = "ocean", category = 'physical', returnclass = "sf") 
  # land <- ne_download(type = "land", category = 'physical', returnclass = "sf") 
  load("~/clim_geo_disp/data/land_ocean_df.RData")
  
  # find intersections with calculated LCDI
  land_intersection <- st_intersection(disparity,land)
  ocean_intersection <- st_intersection(disparity,ocean)
  
  ###################################
  ### save reginal LCDI summaries ###
  ###################################
  
  # matchup column order
  ocean_intersection <- ocean_intersection %>% dplyr::select(emission, anomaly, ratio, disparity, featurecla, scalerank, min_zoom,geometry)
  
  colnames(land_intersection)
  colnames(ocean_intersection)
  
  earth <- rbind(land_intersection,ocean_intersection)
  
  rm(ocean, land, land_intersection,ocean_intersection)
  
  setwd("~/Desktop/")
  
  save(intersection_biome, intersection_realm, intersection_world, intersection_land_eez, intersection_states, earth, 
       file = paste0("intersection_result_", clim_anom, "_", rcp, ".RData"))
  
}

scenario("ensemble_1", "RCP4.5")
scenario("ensemble_2", "RCP4.5")
scenario("ensemble_1", "RCP8.5")
scenario("ensemble_2", "RCP8.5")