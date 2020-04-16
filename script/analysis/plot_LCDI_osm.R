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
library(gridExtra)

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
load(paste0("/Users/", dir, "/clim_geo_disp/output/BC_CO2_CH4_N2O_Combined_1970-2018.RData")) #BC+CO2+CH4+N2O
load(paste0("/Users/", dir, "/clim_geo_disp/output/BC_CO2_CH4_N2O_NO2_Combined_1970-2018.RData")) #BC+CO2+CH4+N2O+NO2
load(paste0("/Users/", dir, "/clim_geo_disp/output/CO2_CH4_N2O_Combined_1970-2018.RData")) ##CO2+CH4+N2O

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

lcdi = function(clim_anom, rcp, variable){
  
  # clim_anom = c("ensemble_1", "ensemble_2")[2]
  # rcp = c("RCP4.5", "RCP8.5")[1]
  # variable = c("anomaly", "historical stdanom", "ensemble stdanom")[1]
  
  setwd(paste0("/Users/", dir, "/clim_geo_disp/data"))
  
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
  
  if (clim_anom == "ensemble_1") raw_ratio$period = "2006-2055"
  if (clim_anom == "ensemble_2") raw_ratio$period = "2050-2099"
  
  raw_ratio$rcp = rcp
  
  raw_ratio$label = paste0("+/- Disparity Ratio = ", positive, "/", negative)
  
  raw_ratio$slope = slope
  raw_ratio$intercept = min(raw_ratio$anomaly, na.rm = T)
  
  return(raw_ratio)
}

variable = c("anomaly", "historical stdanom", "ensemble stdanom")[2]

a = lcdi("ensemble_1", "RCP4.5", variable)
b = lcdi("ensemble_2", "RCP4.5", variable)
c = lcdi("ensemble_1", "RCP8.5", variable)
d = lcdi("ensemble_2", "RCP8.5", variable)

raw_ratio = rbind(a, b, c, d)

slope = raw_ratio[,c("period", "rcp", "slope", "intercept")]
slope = unique(slope)

### Set Universal Color Scale ###
disparity_limits = c(-max(abs(raw_ratio$disparity), na.rm = T), max(abs(raw_ratio$disparity), na.rm = T)) 
disparity_limits

label = raw_ratio[,c("period", "rcp", "label")]
label = unique(label)

# input x y plot
png("~/Desktop/LCDI_Scatter.png", height = 8, width = 10, units = "in", res = 300)
# pdf("~/Desktop/LCDI_Scatter.pdf", height = 8, width = 10)

# raw_ratio %>% 
#   sample_frac(0.01) %>% 
#   group_by(rcp, period) %>% 
#   do(gg = {
#     ggplot(., aes(x = BCE, y = anomaly, color = disparity)) + 
#       geom_point() + 
#       geom_abline(mapping = aes(intercept = intercept, slope = slope), color = "black", data = slope) + 
#       scale_color_gradientn(
#         colours = c("cyan", "black", "red"),
#         values = scales::rescale(c(-0.5, -0.15, 0, 0.15, 0.5)),
#         limits = c(-max(abs(raw_ratio$disparity), na.rm = T), max(abs(raw_ratio$disparity), na.rm = T)) ,
#         name = "LCDI") + 
#       facet_grid(period ~ rcp) + 
#       guides(fill = guide_colourbar(title.position = "right")) +
#       theme(legend.position = "right")
#   }) %>% 
#   .$gg %>% 
#   arrangeGrob(grobs = ., nrow = 2) %>%
#   grid.arrange()

xy_plot <- raw_ratio %>% 
  subset(period == "2050-2099") %>%
  subset(rcp == "RCP8.5") %>%
  sample_frac(1) %>% 
  ggplot() +
  geom_point(aes(x = BCE, y = anomaly, color = disparity), 
             # size = 6,
             alpha = 0.6,
             # shape = 21, 
             show.legend = T) +
  geom_abline(mapping = aes(intercept = intercept, slope = slope), color = "black", data = slope) + 
  facet_grid(period ~ rcp) +
  scale_color_gradientn(
    colours = c("cyan", "black", "red"),
    values = scales::rescale(c(-0.5, -0.05, 0, 0.05, 0.5)),
    limits = disparity_limits,
    name = "LCDI") + 
  xlab(xlab) +
  ylab(expression(paste('Surface Temperature Anomaly (',~degree,'C)', sep = ''))) + 
    theme_pubr(I(20)) +
    theme(legend.position = c("right")) + 
  geom_text(data = label, aes(label = label),
            x = Inf, y = Inf, hjust = 1.1, vjust = 1.3, color = "black", size = 5)

print(xy_plot)

dev.off()
  
# spatial plot
world <- ne_countries(scale = "small", returnclass = "sf") #worldwide country polygon

# png("~/Desktop/LCDI_Maps.png", height = 7, width = 14, units = "in", res = 300)
pdf("~/Desktop/LCDI_Maps.pdf", height = 7, width = 14)

map_plot <-
  ggplot(raw_ratio) +
  geom_raster(aes(x = x, y = y, fill = disparity), show.legend = T) +
  geom_sf(data = world, fill = NA, size = 0.15, color = "lightgray") +
  scale_fill_gradientn(colours = c("cyan", "black", "red"), 
                       values = scales::rescale(c(-0.5, -0.15, 0, 0.15, 0.5)),
                       limits = disparity_limits,
                       name = "LCDI") +
  facet_grid(period ~ rcp) + 
  scale_x_continuous(expand = c(-0.005, 0), "") +
  scale_y_continuous(expand = c(-0.005, 0), "") +
  theme_pubr(I(20)) +
  theme(legend.position = "right")

print(map_plot)
dev.off()

png("~/Desktop/CMIP5_Anomalies.png", height = 7, width = 14, units = "in", res = 300)
map_anomaly <-
  ggplot(raw_ratio) +
  geom_raster(aes(x = x, 
                  y = y,
                  fill = anomaly), # when inputs are raw
              show.legend = T) +
  geom_sf(data = world, fill = NA, size = .2, color = "lightgray")+
  scale_fill_gradientn(colours = c("black", "cyan", "red"), 
                       expression(paste('(',~degree,'C)', sep = ''))) +
  coord_sf() +
  facet_grid(period ~ rcp) + 
  scale_x_continuous(expand = c(-0.005, 0)) +
  scale_y_continuous(expand = c(-0.005, 0)) +
  theme_pubr(I(20)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        legend.position = "right")
print(map_anomaly)
dev.off()
