### Read in emissions/ anomaly data, calculate ratio and disparity as in "basic_index_2" ###
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
library(viridis)
library(ggdark)

load(paste0("/Users/", Sys.info()[7], "/clim_geo_disp/output/previous results/BC-CO2_Combined_2000-2017.RData")) #BC + CO2
load(paste0("/Users/", Sys.info()[7], "/clim_geo_disp/output/previous results/BC-CO2_Combined_1970-2018.RData")) #BC + CO2
bc_co2_adjusted = resample(bc_co2_adjusted, bco2, method = "bilinear") #use bilinear interpolation method to resample layer on 1 by 1 deg grid
bco2 = bc_co2_adjusted
bco2*1000
load(paste0("/Users/", Sys.info()[7], "/clim_geo_disp/output/BC_CO2_CH4_N2O_Combined_1970-2018.RData")) #BC + CO2
bc_co2_ch4_n2o_adjusted = resample(bc_co2_ch4_n2o_adjusted, bco2, method = "bilinear") #use bilinear interpolation method to resample layer on 1 by 1 deg grid
bco2 = bc_co2_ch4_n2o_adjusted
bco2 = bco2 * 31556952 #how many seconds in one Gregorian calendar year = 365.2425 days
xlab = "BC + CO2 + CH4 + N2O emission (kg/m-2)"

df = bco2@data@values
df = sort(df, decreasing = T)
df = as.data.frame(df)
df$id = 1
df$cum_emission = 100*cumsum(df$df)/sum(df$df)
df$cum_area = 100*cumsum(df$id)/dim(df)[1]

x = 90

df_sub = subset(df, cum_emission <= x)
frac = dim(df_sub)[1]/dim(df)[1]

p1 = ggplot(df %>% sample_frac(0.1), aes(cum_area, cum_emission, color = cum_emission)) + 
  geom_point(size = 1) + 
  scale_color_gradientn(colours = matlab.like(100), "%") + 
  # geom_vline(xintercept = frac*100, color = "gray", linetype = "dashed")+
  # geom_hline(yintercept = x, color = "gray", linetype = "dashed")+
  theme_pubr(I(20)) +
  # dark_theme_classic(I(20)) + 
  ylab("Emissions (%)") + 
  xlab("Total surface area (%)") + 
  theme(legend.position = "none",
        panel.grid.major = element_line(colour="lightgray", size = 0.1),
        panel.grid.minor = element_line(colour="lightgray", size = 0.1)) 
# annotate("text",
#            x = Inf,
#            y = -Inf,
#            hjust = 1,
#            vjust = -1,
#            size = 6,
#            # label = paste0(x, " deg C anomaly = ", round(frac, 3)*100, "% of total surface area"),
#            label = "1970-2018")

clim_anom = "ensemble_2"
rcp = "RCP8.5"
setwd(paste0("/Users/", Sys.info()[7], "/clim_geo_disp/data"))
anomaly = stack(paste0("CMIP5 ENSMN ", rcp, " anomaly (2050-2099)-(1956-2005).nc"), varname = "anomaly") #RCP8.5 or 4.5 anomaly (2006-2055)-(1956-2005)
anomaly <- spatial_sync_raster(anomaly, bco2, method = "ngb", size_only = F, verbose = T)
anomaly <- abs(anomaly) # make absolute or not absolute anomally

df = anomaly@data@values
df = sort(df, decreasing = T)
df = as.data.frame(df)
df$id = 1
df$anomaly = df$df
df$cum_area = 100*cumsum(df$id)/dim(df)[1]

x = 3

df_sub = subset(df, anomaly >= x)
frac = dim(df_sub)[1]/dim(df)[1]

p2 = ggplot(df %>% sample_frac(0.1), aes(cum_area, anomaly, color = anomaly)) + 
  geom_point(size = 1) + 
  scale_color_gradientn(colours = matlab.like(100), expression(paste('(',~degree,'C)', sep = ''))) + 
  # geom_vline(xintercept = frac*100, color = "gray", linetype = "dashed")+
  # geom_hline(yintercept = x, color = "gray", linetype = "dashed")+
  theme_pubr(I(20)) +
  # dark_theme_classic(I(20)) +
  ylab(expression(paste('Temperature Anomaly (',~degree,'C)', sep = ''))) + 
  xlab("Total surface area (%)") + 
  theme(legend.position = "none",
        panel.grid.major = element_line(colour="lightgray", size = 0.1),
        panel.grid.minor = element_line(colour="lightgray", size = 0.1)) 
# annotate("text",
#          x = Inf,
#          y = Inf,
#          hjust = 1,
#          vjust = 1.1,
#          size = 6,
#          # label = paste0(x, " deg C anomaly = ", round(frac, 3)*100, "% of total surface area"),
#          label = "RCP8.5 2050-2099")

pdf(paste0("/Users/", Sys.info()[7], "/Desktop/emissions_area.pdf"), height = 7, width = 14)
library(cowplot)
p = ggdraw() +
  draw_plot(p1, x = 0, y = 0, width = 0.5, height = 1) +
  draw_plot(p2, x = 0.5, y = 0, width = 0.5, height = 1) +
  draw_plot_label(label = c("a", "b"), size = 30,
                  x = c(0, 0.5), y = c(1, 1))  
print(p)
dev.off()

pdf(paste0("/Users/", Sys.info()[7], "/Desktop/emissions_area_h.pdf"), height = 5, width = 11)
gridExtra::grid.arrange(p1, p2, ncol = 2)
dev.off()

