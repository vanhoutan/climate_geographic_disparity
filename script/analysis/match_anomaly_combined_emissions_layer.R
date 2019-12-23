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
load(paste0("/Users/", dir, "/clim_geo_disp/output/previous results/BC-CO2_Combined_2000-2017.RData")) #BC + CO2
xlab = "BC + CO2 emission (g m-2)"

##################################################
### Combined BC-CO2 emission layer (1970-2018) ###
##################################################
load(paste0("/Users/", dir, "/clim_geo_disp/output/previous results/BC-CO2_Combined_1970-2018.RData")) #BC + CO2
bc_co2_adjusted = resample(bc_co2_adjusted, bco2, method = "bilinear") #use bilinear interpolation method to resample layer on 1 by 1 deg grid
bco2 = bc_co2_adjusted
xlab = "BC + CO2 emission (kg m-2)"
bco2*1000

##################################################################################################
### Combined CO2+BC+CH$+N2O emission layer (1970-2018) adjusted by average GWP20-100 yrs value ###
##################################################################################################
load(paste0("/Users/", dir, "/clim_geo_disp/output/BC_CO2_CH4_N2O_Combined_1970-2018.RData")) #load combined emissions data
bc_co2_ch4_n2o_adjusted = resample(bc_co2_ch4_n2o_adjusted, bco2, method = "bilinear") #use bilinear interpolation method to resample layer on 1 by 1 deg grid
bco2 = bc_co2_ch4_n2o_adjusted
bco2 = bco2 * 31556952 #31556952 seconds in one Gregorian calendar year (365.2425 days)
xlab = bquote('Emissions  ('*CO[2]* '+BC+' *CH[4]* '+' *N[2]* 'O: kg ' *m^-2~y^-1*')') #label A
xlab = bquote('Emissions  (kg ' *m^-2~y^-1*')') #label B

rm(bc_co2_adjusted, bc_co2_ch4_n2o_adjusted, bc_co2_unadjusted)

#############################################################################
### convert emissions to emissions per capita using NASA pop density data ###
#############################################################################
gpw_pop <- stack(paste0("/Users/", dir, "/Desktop/gpw/gpw_v4_population_density_rev11_1_deg.nc"), varname = "Population Density, v4.11 (2000, 2005, 2010, 2015, 2020): 1 degree")
# gpw_pop <- stack(paste0("/Users/", dir, "/Desktop/gpw/gpw_v4_population_density_rev11_30_min.nc", varname = "Population Density, v4.11 (2000, 2005, 2010, 2015, 2020): 30 arc-minutes")
# gpw_pop <- stack(paste0("/Users/", dir, "/Desktop/gpw/gpw_v4_population_density_rev11_15_min.nc", varname = "Population Density, v4.11 (2000, 2005, 2010, 2015, 2020): 15 arc-minutes")
# gpw_pop <- stack(paste0("/Users/", dir, "/Desktop/gpw/gpw_v4_population_density_rev11_2pt5_min.nc", varname = "Population Density, v4.11 (2000, 2005, 2010, 2015, 2020): 2.5 arc-minutes")

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
# bco2_adj_for_pop <- bco2/(gpw_pop+0.00001)
# bco2_adj_for_pop <- bco2/(gpw_pop+1)
bco2_adj_for_pop <- bco2/gpw_pop
bco2_adj_for_pop[!is.finite(bco2_adj_for_pop)] <- NA
bco2_adj_for_pop

# par(mfrow = c(3,1))
# plot(log10(gpw_pop+1), col = matlab.like(100), main = bquote('Avegrage population count 2000-2020,  log(pop ' *km^-2*'+1)'))
# plot(log10(bco2), col = matlab.like(100), main = bquote('Anthropogenic Emissions, ('*CO[2]* '+' *CH[4]* '+' *N[2]* 'O+BC: kg ' *m^-2~y^-1*')'))
# plot(log10(bco2_adj_for_pop), col = matlab.like(100), main = bquote('Anthropogenic Emissions per capita, ('*CO[2]* '+' *CH[4]* '+' *N[2]* 'O+BC: kg ' *m^-2~y^-1*')'))
# 
# pdf(paste0("/Users/", dir, "/Desktop/population_density_2000-2020.pdf", height = 5, width = 8.3)
# par(mfrow = c(1,1))
# plot(log10(gpw_pop+1), col = matlab.like(100), main = bquote('Avegrage population count 2000-2020,  log10(pop ' *km^-2*'+1)'), axes = F)
# # map(add = T, col = "lightgray", cex = 0.01)
# degAxis(1); degAxis(2, las = 2)
# dev.off()

# xlab = bquote('Emissions per capita  (kg ' *m^-2~y^-1*')')

rm(gpw_pop, gpw_pop1, gpw_pop2, gpw_pop3, gpw_pop4, gpw_pop5)

##############################################################
### Single BCE layer 1980-2017. No longer used in analysis ###
##############################################################
# bco2 <- raster(paste0("/Users/", dir, "/Desktop/climate/KV_climate/climate_impacts_2019/data/causes/GHG_ts_products/BCEMAN_sum_Jan 31.grd") #Just BC

# par(mfrow = c(2,1))
# ghg = color.palette(c("white", "red", "black"), space="rgb")
# plot(bco2, col = ghg(100), axes = F); map(add = T); degAxis(1); degAxis(2, las = 2)
# plot(log10(bco2), col = ghg(100), axes = F); map(add = T); degAxis(1); degAxis(2, las = 2)


##########################################################################
### Visualize MPI_ESM_MR based surface temp anomaly, pick RCP scenario ###
##########################################################################

# pdf("Anomaly.pdf", height = 10, width = 10.6)
# par(mfrow = c(3,2))
# change = color.palette(c("blue", "white", "red"), space="rgb")
# anomaly <- stack(paste0("/Users/", dir, "/Desktop/climate/KV_climate/climate_impacts_2019/data/impacts/temperature/anomally_products/MPI_ESM_future_anom_Jan 31.grd")[["MPI_rcp26"]]
# anomaly <- spatial_sync_raster(anomaly,bco2, method = "ngb", size_only = F, verbose = T)
# plot(anomaly, col = change(100), zlim = c(-20.5,20.5), main = "RCP26, Anomaly"); map(add = T)
# plot(abs(anomaly), col = change(100),  zlim = c(0,21), main = "RCP26, Absolute Anomaly"); map(add = T)
# 
# anomaly <- stack(paste0("/Users/", dir, "/Desktop/climate/KV_climate/climate_impacts_2019/data/impacts/temperature/anomally_products/MPI_ESM_future_anom_Jan 31.grd")[["MPI_rcp45"]]
# anomaly <- spatial_sync_raster(anomaly,bco2, method = "ngb", size_only = F, verbose = T)
# plot(anomaly, col = change(100), zlim = c(-20.5,20.5),main = "RCP45, Anomaly"); map(add = T)
# plot(abs(anomaly), col = change(100),  zlim = c(0,21), main = "RCP45, Absolute Anomaly"); map(add = T)
# 
# anomaly <- stack(paste0("/Users/", dir, "/Desktop/climate/KV_climate/climate_impacts_2019/data/impacts/temperature/anomally_products/MPI_ESM_future_anom_Jan 31.grd")[["MPI_rcp85"]]
# anomaly <- spatial_sync_raster(anomaly,bco2, method = "ngb", size_only = F, verbose = T)
# plot(anomaly, col = change(100), zlim = c(-20.5,20.5),main = "RCP85, Anomaly"); map(add = T)
# plot(abs(anomaly), col = change(100),  zlim = c(0,21), main = "RCP85, Absolute Anomaly"); map(add = T)
# dev.off()

######################################################
### select climate anomaly data, pick rcp scenario ###
######################################################

scenario = function(clim_anom, rcp){
  
  clim_anom = "ensemble_2"
  rcp = "RCP8.5"
  
  setwd(paste0("/Users/", dir, "/clim_geo_disp/data"))
  
  #MPI_ESM RCP-based anomalies - only single time period 2020-2100, can modify baseline and future time frame
  if (clim_anom == "original") {
    
    # anomaly <- stack(paste0("/Users/", dir, "/Desktop/climate/KV_climate/climate_impacts_2019/data/impacts/temperature/anomally_products/MPI_ESM_future_anom_Jan 31.grd")[["MPI_rcp26"]]
    # anomaly <- stack(paste0("/Users/", dir, "/Desktop/climate/KV_climate/climate_impacts_2019/data/impacts/temperature/anomally_products/MPI_ESM_future_anom_Jan 31.grd")[["MPI_rcp45"]]
    anomaly <- stack(paste0("/Users/", dir, "/Desktop/climate/KV_climate/climate_impacts_2019/data/impacts/temperature/anomally_products/MPI_ESM_future_anom_Jan 31.grd"))[["MPI_rcp85"]]
    
  }
  
  #CMIP5 ENSMN based anomalies
  if (clim_anom == "ensemble_1") anomaly = stack(paste0("CMIP5 ENSMN ", rcp, " anomaly (2006-2055)-(1956-2005).nc"), varname = "anomaly") #RCP8.5 or 4.5 anomaly (2050-2099)-(1956-2005)
  if (clim_anom == "ensemble_2") anomaly = stack(paste0("CMIP5 ENSMN ", rcp, " anomaly (2050-2099)-(1956-2005).nc"), varname = "anomaly") #RCP8.5 or 4.5 anomaly (2006-2055)-(1956-2005)
  
  #MPI_ESM_MR based anomalies
  if (clim_anom == "mpi_1") anomaly = stack(paste0("MPI-ESM-MR ", rcp, " anomaly (2006-2055)-(1956-2005).nc"), varname = "anomaly") #RCP8.5 or 4.5 anomaly (2050-2099)-(1956-2005)
  if (clim_anom == "mpi_2") anomaly = stack(paste0("MPI-ESM-MR ", rcp, " anomaly (2050-2099)-(1956-2005).nc"), varname = "anomaly") #RCP8.5 or 4.5 anomaly (2006-2055)-(1956-2005)
  
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
  raw_ratio$disparity <- (raw_ratio$anomaly - (slope*raw_ratio$BCE + min(raw_ratio$anomaly, na.rm = T)))/(sqrt((slope^2)-(1^2))) # so does this one
  # raw_ratio$disparity <- (raw_ratio$anomaly - (slope*raw_ratio$BCE + min(raw_ratio$anomaly, na.rm = T)))/(sqrt(abs((slope^2)-(1^2)))) # changed to abs(sqrt(()))
  # raw_ratio$disparity <- (raw_ratio$anomaly - (slope*raw_ratio$BCE + min(raw_ratio$anomaly, na.rm = T)))/(sqrt(abs((slope^2)-1))) # changed to abs(sqrt(()))
  # raw_ratio$disparity <- abs(rise*raw_ratio$BCE - run*raw_ratio$anomaly + min(raw_ratio$BCE, na.rm = T)) / sqrt(rise^2 + run^2)

  
  ########################################################################
  ### turn in to point file to detect overlap with regions of interest ###
  ########################################################################
  
  disparity <- st_as_sf(x = raw_ratio, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" )
  summary(disparity)
  
  ###############################
  ### contrasting color ideas ###
  ###############################
  
  # disp_col = "Darkorange"
  # opposite(disp_col)
  # low = opposite(disp_col)[1]
  # high = opposite(disp_col)[2]

  # low = viridis(3)[3]
  # high = viridis(3)[1]

  # low = matlab.like(10)[10]
  # high = matlab.like(10)[1]
  
  ratio = raw_ratio[which(complete.cases(raw_ratio$disparity)),]
  positive = round(dim((subset(ratio, disparity > 0)))[1]/dim(ratio)[1], 3)
  negative = round(dim((subset(ratio, disparity < 0)))[1]/dim(ratio)[1], 3)
  positive; negative
  
  if (clim_anom == "mpi_1") label = paste0("Model: MPI-ESM-MR \nExperiment: ", rcp, " \n21st Century Period: 2006-2055 \nHistorical Baseline: 1956-2005")
  if (clim_anom == "mpi_2") label = paste0("Model: MPI-ESM-MR \nExperiment: ", rcp, " \n21st Century Period: 2050-2099 \nHistorical Baseline: 1956-2005)")
  if (clim_anom == "ensemble_1") label = paste0("Experiment: ", rcp, " \n21st Century Period: 2006-2055")
  if (clim_anom == "ensemble_2") label = paste0("Experiment: ", rcp, " \n21st Century Period: 2050-2099")
  if (clim_anom == "original") label = paste0("Model: MPI-ESM-MR \nExperiment: ", rcp, " \n21st Century Period: 2020-2100 \nHistorical Baseline: 1880-2005")
  
  label = paste0(label, "\n+/- Disparity Ratio = ", positive, "/", negative)
  
  ### Set Universal Color Scale ###
  disparity_limits = c(-max(abs(raw_ratio$disparity), na.rm = T), max(abs(raw_ratio$disparity), na.rm = T)) 
  disparity_limits
  
  # input x y plot
  xy_plot <-
    ggplot(raw_ratio %>% 
             sample_frac(1)) +
    geom_point(aes(x = BCE, y = anomaly, color = disparity),
               size = 4, 
               alpha = 0.5, 
               shape = 20,
               show.legend = T) +
    geom_abline(
      intercept = min(raw_ratio$anomaly, na.rm = T),
      # intercept = 0,
      color = "black",
      slope = slope) +
    scale_color_gradientn(
      colours = c("cyan", "black", "red"),
      # colours = c("black", "cyan", "red"), 
      values = scales::rescale(c(-0.5, -0.05, 0, 0.05, 0.5)),
      # values = scales::rescale(c(0, 0.01, 0.05, 0.3, 0.5, 1)),
      limits = disparity_limits,
      name = "LCDI") + 
    scale_x_continuous(expand = c(0,0), limits = c(0, 4.92)) +
    # scale_x_continuous(expand = c(0,0), limits = c(0, 5000)) +
    scale_y_continuous(expand = c(0,0), limits = c(0, 10.5)) +
    xlab(xlab) +
    ylab(expression(paste('Surface Temperature Anomaly (',~degree,'C)', sep = ''))) + 
    # coord_fixed(ratio = 1/slope) +
    theme_pubr() +
    theme(legend.position = c(0.1, 0.85), 
          # title = element_text(colour = "white"),
          # axis.text = element_text(color = "white"),
          # axis.title = element_text(color = "white"),
          # axis.line = element_line(color = "white"),
          # panel.background = element_rect(fill = "gray10"), # bg of the panel
          # plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
          # panel.grid.major = element_blank(), # get rid of major grid
          # panel.grid.minor = element_blank(), # get rid of minor grid
          # legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          # legend.box.background = element_rect(fill = "transparent"), # get rid of legend panel bg
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
  
  xy_plot_main_text_blank <-
    ggplot(raw_ratio %>% sample_frac(0.6), aes(x = BCE, y = anomaly)) + 
    geom_blank() + 
    geom_abline(intercept = min(raw_ratio$anomaly, na.rm = T), slope = slope) +
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
  
  map_plot <-
    ggplot(raw_ratio) +
    geom_raster(aes(x = x, y = y, fill = disparity), show.legend = T) +
    geom_sf(data = world, fill = NA, size = 0.15, color = "lightgray") +
    scale_fill_gradientn(colours = c("cyan", "black", "red"), 
                         # values = scales::rescale(c(-0.5, -0.11, 0, 0.11, 0.5)),
                         limits = disparity_limits,
                         name = "") +
    scale_x_continuous(expand = c(-0.005, 0), "") +
    scale_y_continuous(expand = c(-0.005, 0), "") +
    ggtitle(label) +
    theme_pubr() + 
    theme(
      # axis.title.x = element_blank(),
      # axis.title.y = element_blank(),
      # text = element_text(size = 6.5),
      # axis.text.x = element_blank(),
      # axis.text.y = element_blank()
      legend.position = "right",
      legend.justification = c(1, 0))
  
  library(ggdark)
  
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
  
  pdf(paste0("/Users/", dir, "/Desktop/Figure_2a_blank.pdf"), height = 5, width = 5)
  print(xy_plot_main_text_blank)
  dev.off()

  pdf(paste0("/Users/", dir, "/Desktop/Figure_2a.pdf"), height = 5, width = 5)
  print(xy_plot_main_text)
  dev.off()
  
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
  
  png(paste0("/Users/", dir, "/Desktop/modified_disparity_legend.png"), height = 2, width = 4, units = "in", res = 500)
  legend <- cowplot::get_legend(map_plot_main_text)
  grid::grid.newpage()
  grid::grid.draw(legend)
  dev.off()
  
  setwd(paste0("/Users/", dir, "/Desktop"))
  
  if (clim_anom == "original") pdf("Disparity_2020:2100_MPI_original_xy.pdf", height = 5, width = 6)
  if (clim_anom == "mpi_1") pdf(paste0("Disparity_2006-2055_MPI_xy_", rcp , ".pdf"), height = 5, width = 6)
  if (clim_anom == "mpi_2") pdf(paste0("Disparity_2050-2099_MPI_xy_", rcp , ".pdf"), height = 5, width = 6)
  if (clim_anom == "ensemble_1") pdf(paste0("Disparity_2006-2055_Ensemble_XY_", rcp , ".pdf"), height = 6, width = 6)
  if (clim_anom == "ensemble_2") pdf(paste0("Disparity_2050-2099_Ensemble_XY_", rcp , ".pdf"), height = 6, width = 6)
  
  print(xy_plot)  
  
  dev.off()
  
  if (clim_anom == "original") pdf("Disparity_2020:2100_MPI_original_map.pdf", height = 3.5, width = 6)
  if (clim_anom == "mpi_1") pdf(paste0("Disparity_2005:2055_MPI_map_", rcp , ".pdf"), height = 3.5, width = 6)
  if (clim_anom == "mpi_2") pdf(paste0("Disparity_2050:2099_MPI_map_", rcp , ".pdf"), height = 3.5, width = 6)
  if (clim_anom == "ensemble_1") pdf(paste0("Disparity_2006-2055_Ensemble_Map_", rcp , ".pdf"), height = 6, width = 10)
  if (clim_anom == "ensemble_2") pdf(paste0("Disparity_2050-2099_Ensemble_Map_", rcp , ".pdf"), height = 6, width = 10)
  
  print(map_plot)  
  
  dev.off()
  
  # plotting inputs
  hist_carbon <- 
    ggplot(raw_ratio) + geom_histogram(aes(BCE)) +   
    ylab("") +
    xlab("Emissions (kg m-2)") +
    theme_pubr()
  
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
  
  map_carbon_main_text_b <-
    ggplot(raw_ratio %>% 
             sample_frac(1)) +
    geom_point(aes(x = log10(BCE+1), y = y, color = BCE),
               size = 4, 
               alpha = 0.5, 
               shape = 20,
               show.legend = T) +
    scale_color_gradientn(colours = c( "black", "cyan", "red"), 
                          values = scales::rescale(c(-0.5, -0.49, 0, 0.0001, 0.5)),
                          name = "")+
    scale_x_continuous(expand = c(0,0)) +
    ylim(-90,90) + 
    ylab(expression(paste('Latitude (',~degree,')', sep = ''))) + 
    xlab(bquote('log10(kg ' *m^-2~y^-1*'+1)')) + 
    theme_pubr() + 
    theme(legend.position = "none", 
          axis.title.y = element_text(margin = margin(t = 0, r = -10, b = 0, l = 0)))
  
  lat_mean = aggregate(raw_ratio$BCE, list(raw_ratio$y), mean)
  colnames(lat_mean) <- c("Lat", "BCE")
  
  map_carbon_main_text_b <- 
    ggplot(lat_mean, aes(x="", y=Lat, fill=BCE)) + 
    geom_raster() + 
    coord_cartesian(ylim = range(raw_ratio$y)) +
    theme_void(I(10)) + 
    theme(legend.position="none") + 
    scale_fill_gradientn(colours = c( "black", "cyan", "red"), name = "")
  
  hist_anomlay <- 
    ggplot(raw_ratio) + geom_histogram(aes(anomaly)) + 
    ylab("") + xlab("Temperature anomaly (deg C)") +
    theme_pubr()
  
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
                         limits = c(0,10.5),
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
  
  map_anomaly_main_text_b <-
    ggplot(raw_ratio %>% 
             sample_frac(1)) +
    geom_point(aes(x = anomaly, y = y, color = anomaly),
               size = 4, 
               alpha = 0.5, 
               shape = 20,
               show.legend = T) +
    scale_color_gradientn(colours = c( "black", "cyan", "red"), 
                          name = "")+
    scale_x_continuous(expand = c(0,0)) +
    # scale_y_continuous(expand = c(0,0)) +
    ylim(-90,90) + 
    ylab(expression(paste('Latitude (',~degree,')', sep = ''))) + 
    xlab(expression(paste('(',~degree,'C)', sep = ''))) + 
    theme_pubr() + 
    theme(legend.position = "none", 
          axis.title.y = element_text(margin = margin(t = 0, r = -10, b = 0, l = 0)))
  
  lat_mean = aggregate(raw_ratio$anomaly, list(raw_ratio$y), mean)
  colnames(lat_mean) <- c("Lat", "anomaly")
  
  map_anomaly_main_text_b <- 
    ggplot(lat_mean, aes(x="", y=Lat, fill=anomaly)) + 
    geom_raster() + 
    coord_cartesian(ylim = range(raw_ratio$y)) +
    theme_void(I(10)) + 
    theme(legend.position="none") + 
    scale_fill_gradientn(colours = c( "black", "cyan", "red"), name = "")
  
  # pdf(paste0("/Users/", dir, "/Desktop/clim_anomaly_legend.pdf"), height = 2, width = 1)
  # legend <- cowplot::get_legend(map_anomaly)
  # grid::grid.newpage()
  # grid::grid.draw(legend)
  # dev.off()
  
  # png(paste0("/Users/", dir, "/Desktop/emissions_legend.png"), units = "in", res = 500, height = 1, width = 3.2)
  # legend <- cowplot::get_legend(map_carbon_main_text)
  # grid::grid.newpage()
  # grid::grid.draw(legend)
  # dev.off()
  
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
  
  if (clim_anom == "original") pdf("Disparity_2020:2100_MPI_original_input.pdf", height = 8, width = 11)
  if (clim_anom == "mpi_1") pdf(paste0("Disparity_2005:2055_MPI_input_", rcp , ".pdf"), height = 8, width = 11)
  if (clim_anom == "mpi_2") pdf(paste0("Disparity_2050:2099_MPI_input_", rcp , ".pdf"), height = 8, width = 11)
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
  
  shape <- readOGR(dsn = paste0("/Users/", dir, "/clim_geo_disp/data/TEOW", layer = "wwf_terr_ecos"))  # read the shapefile in by name not the lack of .shp extension
  
  # shape <- ms_simplify(shape, keep = 0.001, keep_shapes = F) # simplify shapefile (saves computing time)
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
  shape_MEOW <- readOGR(dsn = paste0("/Users/", dir, "/clim_geo_disp/data/MEOW_2", layer = "WCMC-036-MEOW-PPOW-2007-2012-NoCoast"))  
  
  # shape_MEOW <- ms_simplify(shape_MEOW, keep = 0.001, keep_shapes = F) # simplify shapefile (saves computing time)
  shape_MEOW <- shape_MEOW %>% st_as_sf()  
  
  # clip out marine ecoregions overlapping on land
  # land <- ne_download(type = "land", category = 'physical', returnclass = "sf")
  load(paste0("/Users/", dir, "/clim_geo_disp/data/land_ocean_df.RData"))
  land <- land %>% st_set_precision(1000000) %>% lwgeom::st_make_valid()
  shape_MEOW <- st_difference(shape_MEOW, st_union(land))
  
  # find intersections with disparity
  intersection_realm <- st_intersection(disparity,shape_MEOW)
  rm(land, shape_MEOW)
  
  
  #############################
  ### Countries without EEZ ###
  #############################
  
  #shape <- readOGR(dsn = "./data/summarization/MEOW", layer = "meow_ecos") # read the shapefile in by name not the lack of .shp extension
  world <- ne_countries(scale = "large", returnclass = "sf") #worldwide country polygon
  
  # find intersections with disparity
  intersection_world <- st_intersection(disparity, world)
  rm(world)
  
  
  ##########################
  ### Countries with EEZ ###
  ##########################
  
  #EEZ land_union shapefile
  eez_land <- readOGR(dsn = paste0("/Users/", dir, "/clim_geo_disp/data/EEZ_land_union", layer = "EEZ_land_v2_201410"))  # read the shapefile in by name not the lack of .shp extension
  
  # eez_land <- ms_simplify(eez_land, keep = 0.05, keep_shapes = F) # simplify shapefile (saves computing time)
  
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
  #### Ocean vs. Land ####
  ########################
  
  # ocean <- ne_download(type = "ocean", category = 'physical', returnclass = "sf") 
  # land <- ne_download(type = "land", category = 'physical', returnclass = "sf") 
  load(paste0("/Users/", dir, "/clim_geo_disp/data/land_ocean_df.RData"))
  
  
  # find intersections with disparity
  land_intersection <- st_intersection(disparity,land)
  ocean_intersection <- st_intersection(disparity,ocean)
  
  # matchup column order
  ocean_intersection <- ocean_intersection %>% dplyr::select(BCE,anomaly,ratio,disparity,featurecla,scalerank,min_zoom,geometry)
  
  colnames(land_intersection)
  colnames(ocean_intersection)
  
  earth <- rbind(land_intersection,ocean_intersection)
  
  rm(ocean, land, land_intersection,ocean_intersection)
  
  setwd(paste0("/Users/", dir, "/Desktop"))
  save(intersection_biome, intersection_realm, intersection_world, intersection_land_eez, intersection_states, earth, 
       file = paste0("intersection_result_", clim_anom, "_", rcp, ".RData"))
  
}

# disparity = scenario("original")
scenario("ensemble_1", "RCP4.5")
scenario("ensemble_2", "RCP4.5")
scenario("ensemble_1", "RCP8.5")
scenario("ensemble_2", "RCP8.5")
scenario("mpi_1", "RCP8.5")
scenario("mpi_2", "RCP8.5")

# extract quantiles -------------------------------------------------------
load("~/Desktop/climate/climate_disp_2019-master/data_output/intersection_result.RData")
## TERRESTRIAL BIOME
#intersection_biome %>% group_by(BIOME) %>% mutate(median_disp = median(disparity)) %>% View() # test
# calculate quantiles 
intersection_biome_quantiles <- intersection_biome %>% group_by(BIOME) %>% 
  summarise(quant10_disp = quantile(disparity, 0.10, na.rm = TRUE), 
            quant50_disp = quantile(disparity, 0.50, na.rm = TRUE), 
            quant90_disp = quantile(disparity, 0.90, na.rm = TRUE)) 
# convert to tibble
intersection_biome_quantiles <- intersection_biome_quantiles %>% st_set_geometry(NULL) # drop geometry


## MARINE REALM
# calculate quantiles 
intersection_realm_quantiles <- intersection_realm %>% group_by(REALM) %>% 
  summarise(quant10_disp = quantile(disparity, 0.10, na.rm = TRUE), 
            quant50_disp = quantile(disparity, 0.50, na.rm = TRUE), 
            quant90_disp = quantile(disparity, 0.90, na.rm = TRUE)) 
# convert to tibble
intersection_realm_quantiles <- intersection_realm_quantiles %>% st_set_geometry(NULL) # drop geometry

## Land vs Sea
# calculate quantiles 
intersection_earth_quantiles <- earth %>% group_by(featurecla) %>% 
  summarise(quant10_disp = quantile(disparity, 0.10, na.rm = TRUE), 
            quant50_disp = quantile(disparity, 0.50, na.rm = TRUE), 
            quant90_disp = quantile(disparity, 0.90, na.rm = TRUE)) 
# convert to tibble
intersection_earth_quantiles <- intersection_earth_quantiles %>% st_set_geometry(NULL) # drop geometry

# Political boundaries
## US STATES
# calculate quantiles 
intersection_states_quantiles <- intersection_states %>% group_by(name) %>% 
  summarise(quant10_disp = quantile(disparity, 0.10, na.rm = TRUE), 
            quant50_disp = quantile(disparity, 0.50, na.rm = TRUE), 
            quant90_disp = quantile(disparity, 0.90, na.rm = TRUE)) 
# convert to tibble
intersection_states_quantiles <- intersection_states_quantiles %>% st_set_geometry(NULL) # drop geometry


# GLOBAL SUBREGIONS
# calculate quantiles 
intersection_subr_quantiles <- intersection_world %>% group_by(subregion) %>% 
  summarise(quant10_disp = quantile(disparity, 0.10, na.rm = TRUE), 
            quant50_disp = quantile(disparity, 0.50, na.rm = TRUE), 
            quant90_disp = quantile(disparity, 0.90, na.rm = TRUE)) 
# convert to tibble
intersection_subr_quantiles <- intersection_subr_quantiles %>% st_set_geometry(NULL) # drop geometry

# COUNTRIES
# calculate quantiles 
intersection_country_quantiles <- intersection_world %>% group_by(geounit) %>% 
  summarise(quant10_disp = quantile(disparity, 0.10, na.rm = TRUE), 
            quant50_disp = quantile(disparity, 0.50, na.rm = TRUE), 
            quant90_disp = quantile(disparity, 0.90, na.rm = TRUE)) 
# convert to tibble
intersection_country_quantiles <- intersection_country_quantiles %>% st_set_geometry(NULL) # drop geometry

# visualizations of quantile ranking --------------------------------------
### BIOMES

#### combine marine and terrestrial into one
#rename columns so ecoregion names match
colnames(intersection_realm_quantiles)[colnames(intersection_realm_quantiles)=="REALM"] <- "BIOME"

# create new column for land or sea
intersection_realm_quantiles$ecoregion <- "ocean"
intersection_biome_quantiles$ecoregion <- "land"

#merge into new df
intersection_allbiomes <- rbind(intersection_realm_quantiles, intersection_biome_quantiles)

#plot
intersection_allbiomes %>% 
  mutate(BIOME = fct_reorder(BIOME, quant50_disp)) %>% #I think they shuold be ordered by median (krt)
  ggplot()+
  geom_segment(aes(x = BIOME, xend = BIOME, y = quant10_disp, yend = quant90_disp), color = "lightgrey")+
  # geom_point(aes(x = BIOME, y = quant10_disp), color = "black")+
  # geom_point(aes(x = BIOME, y = quant50_disp, color = ecoregion))+
  # geom_point(aes(x = BIOME, y = quant90_disp), color = "darkgrey")+
  geom_point(aes(x = BIOME, y = quant10_disp, color = ecoregion))+
  geom_point(aes(x = BIOME, y = quant50_disp, color = ecoregion))+
  geom_point(aes(x = BIOME, y = quant90_disp, color = ecoregion))+
  scale_color_manual(values = c("orange", "darkturquoise"))+
  coord_flip()+
  xlab("biome")+
  ylab("disparity quantiles (10/50/90)")+
  # theme_themeo() + 
  theme_pubr()


### do marine and terrestrial separately as well
#### Terrestrial biome
terr_biome_rank <- intersection_biome_quantiles %>% 
  mutate(BIOME = fct_reorder(BIOME, quant10_disp)) %>% 
  ggplot()+
  geom_segment(aes(x = BIOME, xend = BIOME, y = quant10_disp, yend = quant90_disp), color = "lightgrey")+
  geom_point(aes(x = BIOME, y = quant10_disp),color = "black")+
  geom_point(aes(x = BIOME, y = quant50_disp),color = "#41ae76")+
  geom_point(aes(x = BIOME, y = quant90_disp),color = "darkgrey")+
  coord_flip()+
  xlab("biome")+
  ylab("disparity quantiles (10/50/90)")+
  # theme_themeo() + 
  theme_pubr()

#### marine realm
marine_biome_rank <- intersection_realm_quantiles %>% 
  mutate(BIOME = fct_reorder(BIOME, quant50_disp)) %>% 
  ggplot()+
  geom_segment(aes(x = BIOME, xend = BIOME, y = quant10_disp, yend = quant90_disp), color = "lightgrey")+
  geom_point(aes(x = BIOME, y = quant10_disp),color = "black")+
  geom_point(aes(x = BIOME, y = quant50_disp),color = "#6baed6")+
  geom_point(aes(x = BIOME, y = quant90_disp),color = "darkgrey")+
  coord_flip()+
  xlab("biome")+
  ylab("disparity quantiles (10/50/90)")+
  theme_themeo()

# POLITICAL BOUNDARIES
#### subregion
subr_rank <- intersection_subr_quantiles %>% 
  mutate(subregion = fct_reorder(subregion, quant10_disp)) %>% 
  ggplot()+
  geom_segment(aes(x = subregion, xend = subregion, y = quant10_disp, yend = quant90_disp), color = "lightgrey")+
  geom_point(aes(x = subregion, y = quant10_disp), color = "black")+
  geom_point(aes(x = subregion, y = quant50_disp), color = "#8c96c6")+
  geom_point(aes(x = subregion, y = quant90_disp), color = "darkgrey")+
  xlab("global subregion")+
  ylab("disparity quantiles (10/50/90)")+
  coord_flip()+
  theme_themeo()

#### country
country_rank <- intersection_country_quantiles %>% 
  mutate(geounit = fct_reorder(geounit, quant10_disp)) %>% 
  ggplot()+
  geom_segment(aes(x = geounit, xend = geounit, y = quant10_disp, yend = quant90_disp), color = "lightgrey")+
  geom_point(aes(x = geounit, y = quant10_disp), color = "black")+
  geom_point(aes(x = geounit, y = quant50_disp), color = "#fc8d59")+
  geom_point(aes(x = geounit, y = quant90_disp), color = "darkgrey")+
  xlab("country")+
  ylab("disparity quantiles (10/50/90)")+
  coord_flip()+
  theme_themeo()


#### US state
states_rank <- intersection_states_quantiles %>% 
  mutate(name = fct_reorder(name, quant10_disp)) %>% 
  ggplot()+
  geom_segment(aes(x = name, xend = name, y = quant10_disp, yend = quant90_disp), color = "lightgrey")+
  geom_point(aes(x = name, y = quant10_disp), color = "black")+
  geom_point(aes(x = name, y = quant50_disp), color = "#df65b0")+
  geom_point(aes(x = name, y = quant90_disp), color = "darkgrey")+
  xlab("state")+
  ylab("disparity quantiles (10/50/90)")+
  coord_flip()+
  theme_themeo()

# export csvs of quantile info for later analysis
write.csv(intersection_states_quantiles, "data_output/intersection_states_quantiles.csv")
write.csv(intersection_country_quantiles, "data_output/intersection_country_quantiles.csv")
write.csv(intersection_subr_quantiles, "data_output/intersection_subr_quantiles.csv")
write.csv(intersection_realm_quantiles, "data_output/intersection_realm_quantiles.csv")
write.csv(intersection_biome_quantiles, "data_output/intersection_biome_quantiles.csv")


######################################################### make new maps with fill = quant10
library(RColorBrewer)
##### terrestrial biome
# merge to bring in quantiles
intersection_biome <- left_join(intersection_biome, intersection_biome_quantiles, by = "BIOME")
shape <- left_join(shape, intersection_biome_quantiles, by = "BIOME")

orders_biome <- fct_reorder(intersection_biome$BIOME,intersection_biome$quant10_disp) %>% levels()
#colors_for_joy <- colorRampPalette(brewer.pal(8,"Spectral"))(20) %>% sample()

terr_biome_map <- shape %>% 
  mutate(BIOME = fct_relevel(BIOME, orders_biome %>% rev())) %>% 
  ggplot() +
  geom_sf(aes(group = BIOME, 
              fill = quant10_disp),
          color = "NA", show.legend = T)+
  scale_fill_distiller(palette = "BuGn")+
  theme_void() + 
  theme(panel.grid.major = element_line(colour = "white")) 

gridExtra::grid.arrange(terr_biome_map,
                        terr_biome_rank, 
                        ncol = 1)

##### marine realm
intersection_realm_quantiles$REALM <- intersection_realm_quantiles$BIOME #add realm back in for joining

# merge to bring in quantiles
intersection_realm <- left_join(intersection_realm, intersection_realm_quantiles, by = "REALM")
shape_MEOW <- left_join(shape_MEOW, intersection_realm_quantiles, by = "REALM")

orders_realm <- fct_reorder(intersection_realm$REALM,intersection_realm$quant10_disp) %>% levels()
#colors_for_joy <- colorRampPalette(brewer.pal(8,"Spectral"))(length(orders_from_joyplot_realm)) %>% sample()

marine_biome_map <- shape_MEOW %>% 
  mutate(REALM = fct_relevel(REALM,orders_realm %>% rev())) %>% 
  ggplot() +
  geom_sf( aes(group = REALM, fill = quant10_disp )
           ,color = "NA"
           ,show.legend = T )+
  scale_fill_distiller(palette = "Blues")+
  theme_void()+
  theme(panel.grid.major = element_line(colour = "white"))

gridExtra::grid.arrange(marine_biome_map,
                        marine_biome_rank, 
                        ncol = 1)

##### SUBREGION 
# merge to bring in quantiles
intersection_subr<- left_join(intersection_world, 
                              intersection_subr_quantiles, 
                              by = "subregion")

shape_subr <- left_join(shape_WORLD, 
                        intersection_subr_quantiles, 
                        by = "subregion")

orders_subr <- fct_reorder(intersection_subr$subregion,
                           intersection_subr$quant10_disp) %>% levels()
#colors_for_joy <- colorRampPalette(brewer.pal(8,"Spectral"))(24) %>% sample()

subr_map <- shape_subr %>% 
  mutate(subregion = fct_relevel(subregion,orders_subr %>% rev())) %>% 
  ggplot() +
  geom_sf( aes(group = subregion, 
               fill =  quant10_disp),
           color = "NA", show.legend = T)+
  scale_fill_distiller(palette = "BuPu")+
  theme_void()+
  theme(panel.grid.major = element_line(colour = "white"))

gridExtra::grid.arrange(subr_map, subr_rank, ncol = 1)

##### COUNTRY 
# merge to bring in quantiles
intersection_country<- left_join(intersection_world, intersection_country_quantiles, by = "geounit")
shape_country <- left_join(shape_WORLD, intersection_country_quantiles, by = "geounit")

orders_country <- fct_reorder(intersection_country$geounit,intersection_country$quant10_disp) %>% levels()
#colors_for_joy <- colorRampPalette(brewer.pal(8,"Spectral"))(241) %>% sample()

country_map <- shape_country %>% 
  mutate(geounit = fct_relevel(geounit,orders_country  %>% rev())) %>% 
  ggplot() +
  geom_sf( aes(group = geounit, fill =  quant10_disp)
           ,color = "NA", show.legend = T)+
  scale_fill_distiller(palette = "OrRd")+
  theme_void()+
  theme(panel.grid.major = element_line(colour = "white"))

gridExtra::grid.arrange(country_map, country_rank, ncol = 1)

###### STATES
# merge to bring in quantiles
intersection_states<- left_join(intersection_states, intersection_states_quantiles, by = "name")
states <- left_join(states, intersection_states_quantiles, by = "name")

orders_states  <- fct_reorder(intersection_states$name,intersection_states$quant10_disp) %>% levels()
#colors_for_joy <- colorRampPalette(brewer.pal(8,"Spectral"))(51) %>% sample()

states_map <- states %>% 
  mutate(subregion = fct_relevel(name,orders_states  %>% rev())) %>% 
  ggplot() +
  geom_sf( aes(group = name, fill =  quant10_disp)
           ,color = "NA", show.legend = T)+
  scale_fill_distiller(palette = "PuRd")+
  theme_void()+
  theme(panel.grid.major = element_line(colour = "white"))

gridExtra::grid.arrange(states_map, states_rank, ncol = 1)

#### this one will be weird
### OCEAN VS LAND

# merge to bring in quantiles
earth<- left_join(earth, intersection_earth_quantiles, by = "featurecla")

orders_earth <- fct_reorder(intersection_earth_quantiles$featurecla, intersection_earth_quantiles$quant10_disp) %>% levels()
#colors_for_joy <- colorRampPalette(brewer.pal(8,"Spectral"))(2) %>% sample()

earth %>% 
  #land %>% 
  #filter(BIOME == "Artic_North") %>% 
  mutate(subregion = fct_relevel(featurecla,orders_earth %>% rev())) %>% 
  ggplot() +
  geom_sf( aes(group = featurecla, fill =  quant10_disp),
           color = "white",
           size = .25,
           show.legend = F)+
  # scale_fill_manual(values = c("#1a9850", "#4575b4"))+
  theme_void()+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  
  theme(panel.grid.major = element_line(colour = NA),
        panel.background = element_rect(fill = '#4575b4', colour = '#4575b4'))



