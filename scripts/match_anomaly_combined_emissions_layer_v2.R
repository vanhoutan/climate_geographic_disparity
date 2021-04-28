rm(list = ls())

dir = Sys.info()[7]

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

##################################################
### Combined BC-CO2 emission layer (2000-2017) ###
##################################################
load(paste0("/Users/", dir, "/climate_geographic_disparity/outputs/previous results/BC-CO2_Combined_2000-2017.RData")) #BC + CO2
xlab = "BC + CO2 emission (g m-2)"

##################################################
### Combined BC-CO2 emission layer (1970-2018) ###
##################################################
load(paste0("/Users/", dir, "/climate_geographic_disparity/outputs/previous results/BC-CO2_Combined_1970-2018.RData")) #BC + CO2
bc_co2_adjusted = resample(bc_co2_adjusted, bco2, method = "bilinear") #use bilinear interpolation method to resample layer on 1 by 1 deg grid
bco2 = bc_co2_adjusted
xlab = "BC + CO2 emission (kg m-2)"
bco2*1000

##################################################################################################
### Combined CO2+BC+CH$+N2O emission layer (1970-2018) adjusted by average GWP20-100 yrs value ###
##################################################################################################
load(paste0("/Users/", dir, "/climate_geographic_disparity/outputs/BC_CO2_CH4_N2O_Combined_1970-2018.RData"))     #BC+CO2+CH4+N2O
load(paste0("/Users/", dir, "/climate_geographic_disparity/outputs/BC_CO2_CH4_N2O_NO2_Combined_1970-2018.RData")) #BC+CO2+CH4+N2O+NO2
load(paste0("/Users/", dir, "/climate_geographic_disparity/outputs/CO2_CH4_N2O_Combined_1970-2018.RData"))        #CO2+CH4+N2O

ge = resample(ge, bco2, method = "bilinear") #use bilinear interpolation method to resample layer on 1 by 1 deg grid
bco2 = ge
bco2 = bco2 * 31556952 #31556952 seconds in one Gregorian calendar year (365.2425 days)
# xlab = bquote('Emissions  ('*CO[2]* '+BC+' *CH[4]* '+' *N[2]* 'O: kg ' *m^-2~y^-1*')') #label A
# xlab = bquote('Emissions  (kg ' *m^-2~y^-1*')') #label B
xlab = bquote('Net emissions (kg ' *m^-2~y^-1*')') #label B
# xlab = bquote('Net emissions per capita(kg ' *m^-2~y^-1*')') #label B

rm(bc_co2_adjusted, bc_co2_ch4_n2o_adjusted, bc_co2_unadjusted)

#############################################################################
### convert emissions to emissions per capita using NASA pop density data ###
#############################################################################
gpw_pop <- stack(paste0("/Users/", dir, "/Desktop/gpw/gpw_v4_population_density_rev11_1_deg.nc"), varname = "Population Density, v4.11 (2000, 2005, 2010, 2015, 2020): 1 degree")

gpw_pop1 <- gpw_pop[["X1"]] #2000 pop density estimate
gpw_pop2 <- gpw_pop[["X2"]] #2005 pop density estimate
gpw_pop3 <- gpw_pop[["X3"]] #2010 pop density estimate
gpw_pop4 <- gpw_pop[["X4"]] #2015 pop density estimate
gpw_pop5 <- gpw_pop[["X5"]] #2020 pop density estimate

gpw_pop = stack(gpw_pop1, gpw_pop2, gpw_pop3, gpw_pop4, gpw_pop5)
gpw_pop = mean(gpw_pop)

gpw_pop <- reclassify(gpw_pop,rcl = c(NA,NA,0))
gpw_pop <- spatial_sync_raster(gpw_pop,bco2, method = "ngb", size_only = FALSE, verbose = T)

# emission per capita
bco2_adj_for_pop <- bco2/gpw_pop
bco2_adj_for_pop[!is.finite(bco2_adj_for_pop)] <- NA
bco2_adj_for_pop

rm(gpw_pop, gpw_pop1, gpw_pop2, gpw_pop3, gpw_pop4, gpw_pop5)

######################################################
### select climate anomaly data, pick rcp scenario ###
######################################################

scenario = function(clim_anom, rcp, variable){
  
  clim_anom = c("ensemble_1", "ensemble_2")[2]
  rcp = c("RCP4.5", "RCP8.5")[1]
  variable = c("anomaly", "historical stdanom", "ensemble stdanom")[2]

  setwd(paste0("/Users/", dir, "/climate_geographic_disparity/data"))
  
  #CMIP5 ENSMN based anomalies
  if (clim_anom == "ensemble_1") anomaly = stack(paste0("CMIP5 ENSMN ", rcp, " ", variable, " (2006-2055)-(1956-2005).nc"), varname = "anomaly") #RCP8.5 or 4.5 anomaly (2050-2099)-(1956-2005)
  if (clim_anom == "ensemble_2") anomaly = stack(paste0("CMIP5 ENSMN ", rcp, " ", variable, " (2050-2099)-(1956-2005).nc"), varname = "anomaly") #RCP8.5 or 4.5 anomaly (2006-2055)-(1956-2005)
  
  anomaly <- spatial_sync_raster(anomaly, bco2, method = "ngb", size_only = F, verbose = T)
  anomaly <- abs(anomaly) # make absolute or not absolute anomally
  
  raw_ratio <- 
    stack(bco2, anomaly) %>%
    # stack(bco2_adj_for_pop, anomaly) %>%
    rasterToPoints() %>% 
    data.frame() %>% 
    mutate(BCE = .[[3]], anomaly =.[[4]]) %>% 
    mutate(BCE = DescTools::Winsorize(BCE, probs = c(0,.999), na.rm = T),
           anomaly = DescTools::Winsorize(anomaly, probs = c(0,.999), na.rm = T)) %>%
    dplyr::select(x, y, BCE, anomaly) %>% 
    mutate(ratio = BCE / anomaly) 
  
  raw_ratio <- raw_ratio[!is.na(raw_ratio$anomaly),]
  # rm(anomaly, bco2, bco2_adj_for_pop)
  
  #if you want to rescale temp and emissions 0-1
  range01 <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}
  raw_ratio$anomaly_01 = range01(raw_ratio$anomaly)
  raw_ratio$BCE_01 = range01(raw_ratio$BCE)

  # rise, A
  rise <- (max(raw_ratio$anomaly, na.rm = T) - min(raw_ratio$anomaly, na.rm = T))
  
  # run, B
  run <- (max(raw_ratio$BCE, na.rm = T)  - min(raw_ratio$BCE, na.rm = T))
  
  # slope
  slope = rise / run
  round(slope, 2)
  
  ####################################
  ### calculate euclidean distance ###
  ####################################
  
  # raw_ratio$disparity <- -(((slope*raw_ratio$BCE) + (-1*raw_ratio$anomaly + 0))/(sqrt((slope^2)-(1^2)))) #this one works!
  # raw_ratio$disparity <- (raw_ratio$anomaly - (slope*raw_ratio$BCE + min(raw_ratio$anomaly, na.rm = T)))/(sqrt((slope^2)-(1^2))) # so does this one
  raw_ratio$disparity <- (raw_ratio$anomaly - (slope*raw_ratio$BCE + min(raw_ratio$anomaly, na.rm = T)))/(sqrt(abs((slope^2)-(1^2)))) # changed to abs(sqrt(()))
  # raw_ratio$disparity <- (raw_ratio$anomaly - (slope*raw_ratio$BCE + min(raw_ratio$anomaly, na.rm = T)))/(sqrt(abs((slope^2)-1))) # changed to abs(sqrt(()))
  # raw_ratio$disparity <- abs(rise*raw_ratio$BCE - run*raw_ratio$anomaly + min(raw_ratio$BCE, na.rm = T)) / sqrt(rise^2 + run^2)

  ########################################################################
  ### turn in to point file to detect overlap with regions of interest ###
  ########################################################################
  disparity <- st_as_sf(x = raw_ratio, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" )
  summary(disparity)
  
  ratio = raw_ratio[which(complete.cases(raw_ratio$disparity)),]
  positive = round(dim((subset(ratio, disparity > 0)))[1]/dim(ratio)[1], 3)
  negative = round(dim((subset(ratio, disparity < 0)))[1]/dim(ratio)[1], 3)
  positive; negative
  
  if (clim_anom == "ensemble_1") label = paste0("Experiment: ", rcp, " \n21st Century Period: 2006-2055")
  if (clim_anom == "ensemble_2") label = paste0("Experiment: ", rcp, " \n21st Century Period: 2050-2099")

  label = paste0(label, "\n+/- Disparity Ratio = ", positive, "/", negative)
  
  ### Set Universal Color Scale ###
  disparity_limits = c(-max(abs(raw_ratio$disparity), na.rm = T), max(abs(raw_ratio$disparity), na.rm = T)) 
  disparity_limits
  
  # input x y plot
  xy_plot <-
    ggplot(raw_ratio %>% 
             sample_frac(1)) +
    geom_point(aes(x = BCE, y = anomaly, color = disparity),
               # size = 4, 
               alpha = 0.5, 
               # shape = 20,
               show.legend = T) +
    geom_abline(
      intercept = min(raw_ratio$anomaly, na.rm = T),
      # intercept = 0,
      color = "black",
      slope = slope) +
    scale_color_gradientn(
      colours = c("cyan", "black", "red"),
      # colours = c("black", "cyan", "red"), 
      values = scales::rescale(c(-0.5, -0.2, 0, 0.2, 0.5)),
      limits = disparity_limits,
      name = "LCDI") + 
    scale_x_continuous(expand = c(0,0), limits = c(0, max(raw_ratio$BCE))) +
    # scale_x_continuous(expand = c(0,0), limits = c(0, 5000)) +
    # scale_y_continuous(expand = c(0,0), limits = c(0, 10.5)) +
    xlab(xlab) +
    ylab(expression(paste('Surface Temperature Anomaly (',~degree,'C)', sep = ''))) + 
    # coord_fixed(ratio = 1/slope) +
    theme_pubr() +
    theme(legend.position = c(0.1, 0.85), 
          text = element_text(size = 15)) + 
    annotate("text",
             x = Inf,
             y = Inf,
             hjust = 1,
             vjust = 1,
             color = "black",
             size = 5,
             label = label)
  
  xy_plot_main_text <-
    ggplot(raw_ratio %>% 
             sample_frac(0.6)) +
    geom_point(aes(x = BCE, y = anomaly, color = disparity),
               size = 6,
               alpha = 0.6,
               shape = 21) +
    geom_abline(
      intercept = min(raw_ratio$anomaly, na.rm = T),
      slope = slope) +
    scale_color_gradientn(colours = c("cyan", "black", "red"), 
                          values = scales::rescale(c(-0.5, -0.04, 0.1, 0.2, 0.5)),
                          limits = disparity_limits,
                          name = "Disparity") + 
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    xlab(bquote('Emissions  (kg ' *m^-2~y^-1*')')) +
    ylab(expression(paste('Temperature Anomaly (',~degree,'C)', sep = ''))) + 
    theme_bw(I(20)) +
    theme(legend.position = "none", 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) 
  
  # spatial plot
  world <- ne_countries(scale = "small", returnclass = "sf") #worldwide country polygon
  
  map_plot_main_text <-
    ggplot(raw_ratio) +
    geom_raster(aes(x = x, y = y, fill = disparity), show.legend = T) +
    geom_sf(data = world, fill = NA, size = 0.15, color = "lightgray") +
    scale_fill_gradientn(colours = c("cyan", "black", "red"), 
                         values = scales::rescale(c(-0.5, -0.04, 0.1, 0.2, 0.5)),
                         limits = disparity_limits,
                         name = "Disparity") +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(-0.01, 0)) +
    theme_void(I(15)) +
    theme(legend.position = "bottom", legend.justification = c(0.99,0.01))+ 
    guides(fill = guide_colorbar(title.position = "bottom", 
                                 title.vjust = 0.9,
                                 title.hjust = 0.5,
                                 frame.linewidth = 1,
                                 frame.colour = "black", # draw border around the legend
                                 barwidth = 10, barheight = 1.5)) 
  
  pdf(paste0("/Users/", dir, "/Desktop/Figure_2a.pdf"), height = 5, width = 5)
  p = ggdraw() +
    draw_plot(xy_plot_main_text, x = 0, y = 0, width = 0.9, height = 0.9) + 
    draw_plot_label(label = "a", size = 25, x = 0, y = 1)
  print(p)
  dev.off()
  
  pdf(paste0("/Users/", dir, "/Desktop/Figure_2b.pdf"), height = 5, width = 9.5)
  p = ggdraw() +
    draw_plot(map_plot_main_text, x = 0.02, y = 0, width = 0.98, height = 0.98) + 
    draw_plot_label(label = "b", size = 30, x = 0, y = 1)
  print(p)
  dev.off()
  
  map_carbon <-
    ggplot(raw_ratio) +
    geom_raster(aes(x = x, y = y, fill = BCE), show.legend = T) +
    geom_sf(data = world, fill = NA, size = .1,color = "gray")+
    scale_fill_gradientn(colours = c( "black", "cyan", "red"), 
                         name = bquote('kg ' *m^-2~y^-1*''))+
    coord_sf(xlim = range(raw_ratio$x), ylim = range(raw_ratio$y)) +
    # coord_sf() +
    scale_x_continuous(expand = c(-0, 0)) +
    scale_y_continuous(expand = c(-0, 0)) +
    theme_pubr() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(), 
          legend.position = "bottom",
          legend.justification = c(1, 0))
  
  map_carbon_log10 <-
    ggplot(raw_ratio) +
    geom_raster(aes(x = x, y = y, fill = log10(BCE)), show.legend = T) +
    geom_sf(data = world, fill = NA, size = .1, color = "white")+
    scale_fill_gradientn(colours = c( "black", "cyan", "red"), 
                         name = bquote('log10(kg ' *m^-2~y^-1*')'))+
    coord_sf(xlim = range(raw_ratio$x), ylim = range(raw_ratio$y)) +
    # coord_sf() +
    scale_x_continuous(expand = c(-0, 0)) +
    scale_y_continuous(expand = c(-0, 0)) +
    theme_pubr() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(), 
          legend.position = "bottom",
          legend.justification = c(1, 0))
  
  map_carbon_main_text <-
    ggplot(raw_ratio) +
    geom_raster(aes(x = x, 
                    y = y, 
                    fill = log10(BCE+1)), # when inputs are raw
                show.legend = T) +
    geom_sf(data = world, fill = NA, size = .1,color = "gray")+
    scale_fill_gradientn(colours = c( "black", "cyan", "red"), 
                         values = scales::rescale(c(-0.5, -0.49, 0, 0.0001, 0.5)),
                         name = bquote('Emissions, log10(kg ' *m^-2~y^-1*'+1)'))+
    coord_sf(xlim = range(raw_ratio$x), ylim = range(raw_ratio$y)) +
    # coord_sf() +
    scale_y_continuous(expand = c(-0.01, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    theme_void(I(15)) +
    theme(legend.position = "bottom", legend.justification = c(0.9, 0.1)) + 
    guides(fill = guide_colorbar(title.position = "bottom", 
                                 title.vjust = 1, 
                                 title.hjust = 1,
                                 frame.colour = "black", # draw border around the legend
                                 barwidth = 15, 
                                 barheight = 1.5)) 
  
  lat_mean = aggregate(raw_ratio$BCE, list(raw_ratio$y), mean)
  colnames(lat_mean) <- c("Lat", "BCE")
  
  map_carbon_main_text_b <- 
    ggplot(lat_mean, aes(x="", y=Lat, fill=BCE)) + 
    geom_raster() + 
    coord_cartesian(ylim = range(raw_ratio$y)) +
    theme_void(I(10)) + 
    theme(legend.position="none") + 
    scale_fill_gradientn(colours = c( "black", "cyan", "red"), name = "")
  
  if (clim_anom == "ensemble_1") title = paste0("Experiment: ", rcp, "\n21st century period: 2006-2055")
  if (clim_anom == "ensemble_2") title = paste0("Experiment: ", rcp, "\n21st century period: 2050-2099")
  
  # #plot emissions/temp ratio
  map_ratio = ggplot(raw_ratio) +
    geom_raster(aes(x = x, y = y, fill = BCE/anomaly), show.legend = T) +
    geom_sf(data = world, fill = NA, size = .2, color = "lightgray")+
    scale_fill_gradientn(colours = c( "black", "cyan", "red"), 
                         values = scales::rescale(c(-0.5, -0.4, -0.3, -0.2, 0.5)),
                         bquote('Emissions ('*CO[2]* '+BC+' *CH[4]* '+' *N[2]* 'O: kg ' *m^-2~y^-1*') / Surface Temperature Anomaly (°C)')) +
    coord_sf(xlim = range(raw_ratio$x), ylim = range(raw_ratio$y)) +
    scale_x_continuous(expand = c(-0, 0), "") +
    scale_y_continuous(expand = c(-0, 0), "") +
    theme_pubr() + 
    theme(legend.position = "bottom", legend.justification = c(1,0))+
    # theme(legend.position = "none")+ 
    ggtitle(title)
  
  pdf(paste0("~/Desktop/Ratio_", title,".pdf"), height = 6, width = 9)
  print(map_ratio)
  dev.off()
  
  map_anomaly <-
    ggplot(raw_ratio) +
    geom_raster(aes(x = x, 
                    y = y,
                    fill = anomaly), # when inputs are raw
                show.legend = T) +
    geom_sf(data = world, fill = NA, size = .2, color = "lightgray")+
    scale_fill_gradientn(colours = c("black", "cyan", "red"), 
                         # limits = c(0,10.5),
                         expression(paste('(',~degree,'C)', sep = ''))) +
    coord_sf() +
    scale_x_continuous(expand = c(-0.005, 0)) +
    scale_y_continuous(expand = c(-0.005, 0)) +
    theme_pubr() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(), 
          legend.position = "bottom",
          legend.justification = c(1, 0))+ 
    ggtitle(title)
  
  map_anomaly_main_text <-
    ggplot(raw_ratio) +
    geom_raster(aes(x = x, 
                    y = y,
                    fill = anomaly), # when inputs are raw
                show.legend = T)+
    geom_sf(data = world, fill = NA, size = .2, color = "lightgray")+
    scale_fill_gradientn(colours = c("black", "cyan", "red"), 
                         breaks = c(2,4,6,8,10),
                         expression(paste('Surface temperature anomaly (',~degree,'C)', sep = ''))) +
    coord_sf() +
    scale_y_continuous(expand = c(-0.01, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    theme_void(I(15)) +
    theme(legend.position = "bottom",
          legend.justification = c(0.99, 0)) + 
    guides(fill = guide_colorbar(title.position = "left", 
                                 title.vjust = 1, 
                                 title.hjust = 0.5,
                                 frame.colour = "black", # draw border around the legend
                                 barwidth = 10, 
                                 barheight = 1.5)) 
  
  lat_mean = aggregate(raw_ratio$anomaly, list(raw_ratio$y), mean)
  colnames(lat_mean) <- c("Lat", "anomaly")
  
  map_anomaly_main_text_b <- 
    ggplot(lat_mean, aes(x="", y=Lat, fill=anomaly)) + 
    geom_raster() + 
    coord_cartesian(ylim = range(raw_ratio$y)) +
    theme_void(I(10)) + 
    theme(legend.position="none") + 
    scale_fill_gradientn(colours = c( "black", "cyan", "red"), name = "")

  pdf("~/Desktop/Emissions.pdf", height = 6, width = 17)
  p = ggdraw() +
    draw_plot(map_carbon, x = 0, y = 0, width = 0.5, height = 1) +
    draw_plot(map_carbon_log10, x = 0.5, y = 0, width = 0.5, height = 1) +
    draw_plot_label(label = c("a", "b"), size = 25, x = c(0, 0.5), y = c(1, 1))  
  print(p)
  dev.off()
  
  pdf("~/Desktop/Emissions.pdf", height = 4, width = 6)
  print(map_carbon_log10)
  dev.off()
  
  pdf(paste0("/Users/", dir, "/Desktop/Figure_1a.pdf"), height = 3.5, width = 7)
  print(map_carbon_main_text)
  dev.off()
  
  pdf(paste0("/Users/", dir, "/Desktop/Figure_1.pdf"), height = 7, width = 8)
  p = ggdraw() +
    draw_plot(map_carbon_main_text, x = 0, y = 0.5, width = 1, height = 0.49) +
    draw_plot(map_anomaly_main_text, x = 0, y = 0, width = 1, height = 0.49) +
    draw_plot_label(label = c("a", "b"), size = 25, color = "white",
                    x = c(0.08, 0.08), y = c(1, 0.5))  
  print(p)
  dev.off()
  
  pdf(paste0("/Users/", dir, "/Desktop/Figure_1_v2.pdf"), height = 7, width = 13)
  p = ggdraw() +
    draw_plot(map_carbon_main_text,    x = 0,    y = 0.5, width = 0.55, height = 0.45) +
    draw_plot(map_carbon_main_text_b,  x = 0.51, y = 0.5, width = 0.25, height = 0.45) +
    draw_plot(map_anomaly_main_text,   x = 0,    y = 0,   width = 0.55, height = 0.45) +
    draw_plot(map_anomaly_main_text_b, x = 0.51, y = 0,   width = 0.25, height = 0.45) +
    draw_plot_label(label = c("a", "c", "b", "d"), size = 25, color = c("white", "white", "black", "black"),
                    x = c(0.07, 0.07, 0.5, 0.5), y = c(0.95, 0.45, 0.95, 0.45))  
  print(p)
  dev.off()
  
  pdf(paste0("/Users/", dir, "/Desktop/Figure_1_v3.pdf"), height = 7, width = 6)
  p = ggdraw() +
    draw_plot(map_carbon_main_text,    x = 0,    y = 0.5, width = 0.95, height = 0.5) +
    draw_plot(map_carbon_main_text_b,  x = 0.91, y = 0.585, width = 0.045, height = 0.4001) +
    draw_plot(map_anomaly_main_text,   x = 0,    y = 0,   width = 0.95, height = 0.5) +
    draw_plot(map_anomaly_main_text_b, x = 0.91, y = 0.085,   width = 0.045, height = 0.4001) +
    draw_plot_label(label = c("a", "b"), size = 25, color = c("white", "white"), x = c(0.04, 0.04), y = c(0.98, 0.48))  
  print(p)
  dev.off()
  
  if (clim_anom == "ensemble_1") pdf(paste0("Disparity_2006-2055_Ensemble_Anomaly_", rcp , ".pdf"), height = 5, width = 9)
  if (clim_anom == "ensemble_2") pdf(paste0("Disparity_2050-2099_Ensemble_Anomaly_", rcp , ".pdf"), height = 5, width = 9)
  map_anomaly
  dev.off()
  
  rm(map_anomaly, hist_anomlay, map_carbon, hist_carbon, high, low, disp_col, map_plot, xy_plot, anomaly, bco2, raw_ratio, rise, run, slope, world, p, scenario)
  
  
  ################################################
  ### Intersect with different analysis layers ###
  ################################################
  
  ###########################
  ### Terrestorial Biomes ###
  ###########################
  
  shape <- readOGR(dsn = paste0("/Users/", dir, "/climate_geographic_disparity/data/TEOW"), layer = "wwf_terr_ecos")  # read the shapefile in by name not the lack of .shp extension
  
  # shape <- ms_simplify(shape, keep = 0.0001, keep_shapes = F) # simplify shapefile (saves computing time)
  shape <- shape %>% st_as_sf()  
  
  # assign names to biomes
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
  shape <- shape %>% 
    filter(!BIOME %in% c("Large_Inland_Waterbodies",""))
  
  # find intersections with disparity
  intersection_biome <- st_intersection(disparity,shape)
  rm(shape)
  
  
  #####################
  ### Marine Biomes ###
  #####################
  
  #shape <- readOGR(dsn = "./data/summarization/MEOW", layer = "meow_ecos") # read the shapefile in by name not the lack of .shp extension
  shape_MEOW <- readOGR(dsn = paste0("/Users/", dir, "/climate_geographic_disparity/data/MEOW_2"), layer = "WCMC-036-MEOW-PPOW-2007-2012-NoCoast")  
  
  # shape_MEOW <- ms_simplify(shape_MEOW, keep = 0.001, keep_shapes = F) # simplify shapefile (saves computing time)
  shape_MEOW <- shape_MEOW %>% st_as_sf()  
  
  # clip out marine ecoregions overlapping on land
  # land <- ne_download(type = "land", category = 'physical', returnclass = "sf")
  load(paste0("/Users/", dir, "/climate_geographic_disparity/data/land_ocean_df.RData"))
  land <- land %>% st_set_precision(1000000) %>% sf::st_make_valid()
  shape_MEOW <- st_difference(shape_MEOW, st_union(land))
  
  # find intersections with disparity
  intersection_realm <- st_intersection(disparity,shape_MEOW)
  rm(land, shape_MEOW)
  
  
  #############################
  ### Countries without EEZ ###
  #############################
  
  #shape <- readOGR(dsn = "./data/summarization/MEOW", layer = "meow_ecos") # read the shapefile in by name not the lack of .shp extension
  world <- ne_countries(scale = "large", returnclass = "sf") #worldwide country polygon
  # world <- ms_simplify(world, keep = 0.001, keep_shapes = F) # simplify shapefile (saves computing time)
  
  # find intersections with disparity
  intersection_world <- st_intersection(disparity, world)
  rm(world)
  
  
  ##########################
  ### Countries with EEZ ###
  ##########################
  
  #EEZ land_union shapefile
  eez_land <- readOGR(dsn = paste0("/Users/", dir, "/climate_geographic_disparity/data/EEZ_land_union"), layer = "EEZ_land_v2_201410")  # read the shapefile in by name not the lack of .shp extension
  
  # eez_land <- ms_simplify(eez_land, keep = 0.001, keep_shapes = F) # simplify shapefile (saves computing time)
  
  eez_land <- eez_land %>% st_as_sf()  
  
  eez_land$Country <- as.factor(eez_land$Country)
  
  # find intersections with disparity
  intersection_land_eez <- st_intersection(disparity,eez_land)
  rm(eez_land)
  
  
  #################
  ### US states ###
  #################
  
  states <- ne_states(country = "United States of America", returnclass = "sf")
  
  # find intersections with disparity
  intersection_states <- st_intersection(disparity, states)
  rm(states)
  
  ########################
  ### Anthromes Biomes ###
  ########################
  
  load(paste0("/Users/", dir, "/climate_geographic_disparity/data/anthrome_1.RData"))
  
  # anthrome <- ms_simplify(anthrome, keep = 0.001, keep_shapes = F) # simplify shapefile (saves computing time)
  anthrome <- anthrome %>% st_as_sf() 
  
  # assign names to biomes
  anthrome$layer <- as.factor(anthrome$layer)
  anthrome$layer <- fct_recode(anthrome$layer, 
                            Urban = "11",
                            Dense_settlement = "12",
                            Rice_villages = "21",
                            Irrigated_villages = "22",
                            Cropped_Pastoral_villages = "23",
                            Pastoral_villages = "24",
                            Rainfed_villages = "25",
                            Rainfed_mosaic_villages = "26",
                            Residential_irrigated_cropland = "31", 
                            Residential_rainfed_mosaic = "32",
                            Populated_irrigated_cropland = "33",
                            Populated_rainfed_croplands = "34",
                            Remote_croplands = "35",
                            Residential_rangelads = "41",
                            Populated_rangelands = "42",
                            Remote_rangelands = "43",
                            Populated_forests = "51",
                            Remote_forests = "52",
                            Wild_forests = "61",
                            Sparse_trees = "62",
                            Barren = "63"
  )
  
  # find intersections with disparity
  intersection_anthromes <- st_intersection(disparity, anthrome)
  rm(anthrome)

  
  ########################
  #### Ocean vs. Land ####
  ########################
  
  # ocean <- ne_download(type = "ocean", category = 'physical', returnclass = "sf") 
  # land <- ne_download(type = "land", category = 'physical', returnclass = "sf") 
  load(paste0("/Users/", dir, "/climate_geographic_disparity/data/land_ocean_df.RData"))
  
  land <- ms_simplify(land, keep = 0.001, keep_shapes = F) # simplify shapefile (saves computing time)
  ocean <- ms_simplify(ocean, keep = 0.001, keep_shapes = F) # simplify shapefile (saves computing time)
  
  # find intersections with disparity
  land_intersection <- st_intersection(disparity,land)
  ocean_intersection <- st_intersection(disparity,ocean)
  
  # matchup column order
  ocean_intersection <- ocean_intersection %>% dplyr::select(BCE,anomaly,ratio,disparity,featurecla,scalerank,min_zoom,geometry)
  
  colnames(land_intersection)
  colnames(ocean_intersection)
  
  land_intersection = land_intersection[,c("BCE", "anomaly", "ratio", "disparity", "geometry")]
  ocean_intersection = ocean_intersection[,c("BCE", "anomaly", "ratio", "disparity", "geometry")]
  
  earth <- rbind(land_intersection,ocean_intersection)
  
  rm(ocean, land, land_intersection,ocean_intersection)
  
  setwd(paste0("/Users/", dir, "/Desktop"))
  
  save(intersection_biome, intersection_realm, intersection_world, intersection_land_eez, intersection_states, intersection_anthromes, earth, 
       file = paste0("intersection_result_", clim_anom, "_", rcp, "_", variable, ".RData"))
  
}

varaible = c("anomaly", "historical stdanom", "ensemble stdanom")

# disparity = scenario("original")
scenario("ensemble_1", "RCP4.5", "anomaly")
scenario("ensemble_2", "RCP4.5", "anomaly")
scenario("ensemble_1", "RCP8.5", "anomaly")
scenario("ensemble_2", "RCP8.5", "anomaly")

scenario("ensemble_1", "RCP4.5", "historical stdanom")
scenario("ensemble_2", "RCP4.5", "historical stdanom")
scenario("ensemble_1", "RCP8.5", "historical stdanom")
scenario("ensemble_2", "RCP8.5", "historical stdanom")

scenario("ensemble_1", "RCP4.5", "ensemble stdanom")
scenario("ensemble_2", "RCP4.5", "ensemble stdanom")
scenario("ensemble_1", "RCP8.5", "ensemble stdanom")
scenario("ensemble_2", "RCP8.5", "ensemble stdanom")