rm(list = ls())

library(spatial.tools)
library(ncdf4)
library(tidyverse)
library(raster)
library(colorRamps)
library(ggpubr)
library(sm)
library(maps)

########################################################@@@@@@@@@@@@@@@@@@#################
### Modern-Era Retrospective Analysis for Research and Applications Version 2 (MERRA-2) ###
### BCEMAN (Anthropogenic Black Carbon)                                                 ###
### 1980-2018                                                                           ###
### 0.5° × 0.625°                                                                       ###
### kg m-2 s-1                                                                          ###
###########################################################################################

# BC <- list.files('/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/causes/GHGs', pattern = '\\.nc4$') # list all netcdf files in the folder
# BC <- paste0("/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/causes/GHGs/", BC) # add parent directories
# ghg <- c("OCEMAN","BCEMAN","SO2EMAN","SO4EMAN")[2] #BCEMAN = Black Carbon Anthropogenic Emissions
# 
# bc = raster::stack()
# 
# for (t in 1:length(BC)) {
# 
#   filename <- BC[t] # pull the first file,first year
#   year_month_read <- stack(filename, varname = ghg) # read as a raster
#   names(year_month_read) <- paste0(ghg,"_", gsub(".*Nx.\\s*|.nc4.*", "", filename)) # generate name: Emission_yearmonth
#   print(names(year_month_read)) 
#   bc <- stack(bc,year_month_read) # add to the timeseries stack
# 
# }
# 
# bc_mean = mean(bc)
# save(bc_mean, file = "/Users/ktanaka/clim_geo_disp/output/MERRA2_BC_Mean_1980-2018.RData")

load("/Users/ktanaka/clim_geo_disp/output/MERRA2_BC_Mean_1980-2018.RData")


#######################################################################################################
### Emission Database for Global Atmospheric Research Global Greenhouse Gas Emissions (EDGAR v5.0)  ###
### CO2_excl_short-cycle  1970-2018                                                                 ###
### CO2_org_short-cycle   1970-2015                                                                 ### 
### CH4                   1970-2015                                                                 ### 
### N2O                   1970-2015                                                                 ###
### 0.1°* x 0.1°                                                                                    ###
### kg m-2 s-1                                                                                      ### 
#######################################################################################################

################
### CO2 excl ###
################
var <- list.files('/Users/ktanaka/Desktop/edgar/v50_CO2_excl_short-cycle_org/v50_CO2_excl_short-cycle_org_C_TOTALS_nc/', pattern = '\\.nc$')
var <- paste0("/Users/ktanaka/Desktop/edgar/v50_CO2_excl_short-cycle_org/v50_CO2_excl_short-cycle_org_C_TOTALS_nc/", var) 

emissions <- "emi_co2"

ghg <- raster::stack()

for( t in 1:length(var)){ 
  
  filename <- var[t]
  year_month_read <- stack(filename, varname = emissions)
  names(year_month_read) <- paste0(emissions,"_", gsub(".*Nx.\\s*|.nc4.*", "", filename))
  print(names(year_month_read))
  ghg <- stack(ghg,year_month_read)
  
}

co2_excl_mean = mean(ghg)

###############
### CO2 org ###
###############
var <- list.files('/Users/ktanaka/Desktop/edgar/v50_CO2_org_short-cycle_C/v50_CO2_org_short-cycle_C_TOTALS_nc/', pattern = '\\.nc$')
var <- paste0("/Users/ktanaka/Desktop/edgar/v50_CO2_org_short-cycle_C/v50_CO2_org_short-cycle_C_TOTALS_nc/", var)

emissions <- "emi_co2"

ghg <- raster::stack() 

for( t in 1:length(var)){ 
  
  filename <- var[t]
  year_month_read <- stack(filename, varname = emissions) 
  names(year_month_read) <- paste0(emissions,"_", gsub(".*Nx.\\s*|.nc4.*", "", filename)) 
  print(names(year_month_read)) 
  ghg <- stack(ghg,year_month_read) 
  
}

co2_org_mean = mean(ghg)


###########
### CH4 ###
###########
var <- list.files('/Users/ktanaka/Desktop/edgar/v50_CH4_1970_2015/v50_CH4_TOTALS_nc', pattern = '\\.nc$') 
var <- paste0("/Users/ktanaka/Desktop/edgar/v50_CH4_1970_2015/v50_CH4_TOTALS_nc/", var) 

emissions <- "emi_ch4"

ghg <- raster::stack() 

for( t in 1:length(var)){
  
  filename <- var[t] 
  year_month_read <- stack(filename, varname = emissions) 
  names(year_month_read) <- paste0(emissions,"_", gsub(".*Nx.\\s*|.nc4.*", "", filename))
  print(names(year_month_read))
  ghg <- stack(ghg,year_month_read)
  
}

ch4_mean = mean(ghg)

###########
### N2O ###
###########
var <- list.files('/Users/ktanaka/Desktop/edgar/v50_N2O/v50_N2O_TOTALS_nc/', pattern = '\\.nc$')
var <- paste0("/Users/ktanaka/Desktop/edgar/v50_N2O/v50_N2O_TOTALS_nc/", var)

emissions <- "emi_n2o"

ghg <- raster::stack()

for( t in 1:length(var)){
  
  filename <- var[t]
  year_month_read <- stack(filename, varname = emissions)
  names(year_month_read) <- paste0(emissions,"_", gsub(".*Nx.\\s*|.nc4.*", "", filename))
  print(names(year_month_read))
  ghg <- stack(ghg,year_month_read)
  
}

n2o_mean = mean(ghg)

###########
### SO2 ###
###########

var <- list.files('/Users/ktanaka/Desktop/edgar/v432_SO2_TOTALS_nc/', pattern = '\\.nc$') # list all files in thel folder
var <- paste0("/Users/ktanaka/Desktop/edgar/v432_SO2_TOTALS_nc/", var) # add parent directories

emissions <- "emi_so2"

ghg <- raster::stack() # build an empty stack to put the desired emission metric in to

for( t in 1:length(var)){ # for each file that is year in a month
  
  filename <- var[t] # pull the first file,first year
  year_month_read <- stack(filename, varname = emissions) # read it in as a raster
  names(year_month_read) <- paste0(emissions,"_", gsub(".*Nx.\\s*|.nc4.*", "", filename)) # generate name: Emission_yearmonth
  print(names(year_month_read)) # print in the loop to keep an eye on progress
  ghg <- stack(ghg,year_month_read) # add to the timeseries stack
  
}

so2_mean = mean(ghg)

###############################################################################################
### combine EDGAR v5.0 data and multiply non-co2 data according to average GTP20-100 values ###
### April 10 2020, added SO2 with GTP(20) from doi:10.5194/acp-16-7451-2016                 ### 
###############################################################################################
co2_ch4_n2o_adjusted = sum(co2_excl_mean, co2_org_mean, ch4_mean*40.5, n2o_mean*290.5)
ge = readAll(co2_ch4_n2o_adjusted)
ge = raster::rotate(ge)
save(ge, file = "/Users/ktanaka/clim_geo_disp/output/CO2_CH4_N2O_Combined_1970-2018.RData")

co2_ch4_n2o_so2_adjusted = sum(co2_excl_mean, co2_org_mean, ch4_mean*40.5, n2o_mean*290.5, so2_mean*-92.625)
ge = readAll(co2_ch4_n2o_so2_adjusted)
ge = raster::rotate(ge)
save(ge, file = "/Users/ktanaka/clim_geo_disp/output/CO2_CH4_N2O_SO2_Combined_1970-2018.RData")

###############################################################################
### use bilinear interpolation method to resample BC on 0.1 by 0.1 deg grid ###
###############################################################################
bc_mean_0.1 = resample(bc_mean, raster::rotate(co2_ch4_n2o_adjusted), method = "bilinear") 

##################################################################################################
### multiply BC by average global temperature potenital value 20-100 years (Bond et al (2013)) ###
##################################################################################################
bc_mean_0.1_adj = bc_mean_0.1*400

#######################################
### combine MERRA-2 and EDGAR v5.0  ###
#######################################
bc_co2_ch4_n2o_adjusted = sum(raster::rotate(co2_ch4_n2o_adjusted), bc_mean_0.1_adj)
ge = readAll(bc_co2_ch4_n2o_adjusted)
save(ge, file = "/Users/ktanaka/clim_geo_disp/output/BC_CO2_CH4_N2O_Combined_1970-2018.RData")

bc_co2_ch4_n2o_so2_adjusted = sum(raster::rotate(co2_ch4_n2o_so2_adjusted), bc_mean_0.1_adj)
ge = readAll(bc_co2_ch4_n2o_so2_adjusted)
save(ge, file = "/Users/ktanaka/clim_geo_disp/output/BC_CO2_CH4_N2O_NO2_Combined_1970-2018.RData")

plot(ge, col = matlab.like(100))
plot(log10(ge), col = matlab.like(100))


### plot individual data ###
pdf("/Users/ktanaka/Desktop/edgar GHG 1970-2018.pdf", height = 8.5, width = 15)
par(mfrow = c(2,2), mar = c(4,4,4,6))
plot(raster::rotate(log10(co2_excl_mean + co2_org_mean)), col = matlab.like(100), main = bquote(''*CO[2]* ': log10(kg ' *m^-2~s^-1*')'), axes = F); maps::map(add = T); degAxis(1); degAxis(2, las = 2)
plot(log10(bc_mean_0.1), col = matlab.like(100), main = bquote('BC: log10(kg ' *m^-2~s^-1*')'), axes = F); maps::map(add = T); degAxis(1); degAxis(2, las = 2)
plot(raster::rotate(log10(ch4_mean)), col = matlab.like(100), main = bquote(''*CH[4]* ': log10(kg ' *m^-2~s^-1*')'), axes = F); maps::map(add = T); degAxis(1); degAxis(2, las = 2)
plot(raster::rotate(log10(n2o_mean)), col = matlab.like(100), main = bquote(''*N[2]* 'O: log10(kg ' *m^-2~s^-1*')'), axes = F); maps::map(add = T); degAxis(1); degAxis(2, las = 2)
dev.off()


xlab = bquote('Emissions  ('*CO[2]* '+BC+' *CH[4]* '+' *N[2]* 'O: kg ' *m^-2~y^-1*')')

co2 = raster::rotate(co2_excl_mean + co2_org_mean) %>% rasterToPoints() %>% data.frame()
world <- ne_countries(scale = "small", returnclass = "sf") #worldwide country polygon

co2 = ggplot(co2 %>% sample_frac(1)) +
  geom_raster(aes(x = x, y = y, fill = log10(layer)), show.legend = T) +
  geom_sf(data = world, fill = NA, size = .1, color = "gray") +
  scale_fill_gradientn(colours = c( "black", "cyan", "red"), 
                       name = bquote('log10(kg ' *m^-2~s^-1*')')) +
  coord_sf() +
  scale_x_continuous(expand = c(-0, 0)) +
  scale_y_continuous(expand = c(-0, 0)) +
  theme_pubr() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        legend.position = "right",
        legend.justification = c(1, 0))

bc = bc_mean_0.1 %>% rasterToPoints() %>% data.frame()

bc = ggplot(bc %>% sample_frac(1)) +
  geom_raster(aes(x = x, y = y, fill = log10(layer)), show.legend = T) +
  geom_sf(data = world, fill = NA, size = .1, color = "gray") +
  scale_fill_gradientn(colours = c( "black", "cyan", "red"), 
                       name = bquote('log10(kg ' *m^-2~s^-1*')')) +
  coord_sf() +
  scale_x_continuous(expand = c(-0, 0)) +
  scale_y_continuous(expand = c(-0, 0)) +
  theme_pubr() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        legend.position = "right",
        legend.justification = c(1, 0))

ch4 = raster::rotate(ch4_mean) %>% rasterToPoints() %>% data.frame()

ch4 = ggplot(ch4 %>% sample_frac(1)) +
  geom_raster(aes(x = x, y = y, fill = log10(layer)), show.legend = T) +
  geom_sf(data = world, fill = NA, size = .1, color = "gray") +
  scale_fill_gradientn(colours = c( "black", "cyan", "red"), 
                       name = bquote('log10(kg ' *m^-2~s^-1*')')) +
  coord_sf() +
  scale_x_continuous(expand = c(-0, 0)) +
  scale_y_continuous(expand = c(-0, 0)) +
  theme_pubr() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        legend.position = "right",
        legend.justification = c(1, 0))

n2o = raster::rotate(n2o_mean) %>% rasterToPoints() %>% data.frame()

n2o = ggplot(n2o %>% sample_frac(1)) +
  geom_raster(aes(x = x, y = y, fill = log10(layer)), show.legend = T) +
  geom_sf(data = world, fill = NA, size = .1, color = "gray") +
  scale_fill_gradientn(colours = c( "black", "cyan", "red"), 
                       name = bquote('log10(kg ' *m^-2~s^-1*')')) +
  coord_sf() +
  scale_x_continuous(expand = c(-0, 0)) +
  scale_y_continuous(expand = c(-0, 0)) +
  theme_pubr() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        legend.position = "right",
        legend.justification = c(1, 0))

so2 = raster::rotate(so2_mean) %>% rasterToPoints() %>% data.frame()

so2 = ggplot(so2 %>% sample_frac(1)) +
  geom_raster(aes(x = x, y = y, fill = log10(layer)), show.legend = T) +
  geom_sf(data = world, fill = NA, size = .1, color = "gray") +
  scale_fill_gradientn(colours = c( "black", "cyan", "red"), 
                       name = bquote('log10(kg ' *m^-2~s^-1*')')) +
  coord_sf() +
  scale_x_continuous(expand = c(-0, 0)) +
  scale_y_continuous(expand = c(-0, 0)) +
  theme_pubr() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        legend.position = "right",
        legend.justification = c(1, 0))

unified = bc_co2_ch4_n2o_adjusted %>% rasterToPoints() %>% data.frame()

unified = ggplot(unified %>% sample_frac(1)) +
  geom_raster(aes(x = x, y = y, fill = log10(layer)), show.legend = T) +
  geom_sf(data = world, fill = NA, size = .1, color = "gray") +
  scale_fill_gradientn(colours = c( "black", "cyan", "red"), 
                       name = bquote('log10(kg ' *m^-2~s^-1*')')) +
  coord_sf() +
  scale_x_continuous(expand = c(-0, 0)) +
  scale_y_continuous(expand = c(-0, 0)) +
  theme_pubr() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        legend.position = "right",
        legend.justification = c(1, 0))

pdf("~/Desktop/Figure_s.pdf", height = 8, width = 16)
p = ggdraw() +
  draw_plot(co2,    x = 0,   y = 0.66, width = 0.5, height = 0.33) +
  draw_plot(bc,     x = 0.5, y = 0.66, width = 0.5, height = 0.33) +
  draw_plot(ch4,    x = 0,   y = 0.33,   width = 0.5, height = 0.33) +
  draw_plot(n2o,    x = 0.5, y = 0.33,   width = 0.5, height = 0.33) +
  draw_plot(so2,    x = 0.5, y = 0,   width = 0.5, height = 0.33) +
  draw_plot_label(label = c("a", "c", "b", "d", "e"), size = 25, 
                  x = c(0, 0, 0.5, 0.5, 0), y = c(1, 0.5, 1, 0.5, 1))  
print(p)
dev.off()



pdf("/Users/ktanaka/Desktop/BC+CO2+CH4+N2O.pdf", height = 5, width = 8.34)
par(mfrow = c(1,1))
plot(log10(bc_co2_ch4_n2o_adjusted), col = matlab.like(100), main = bquote('Emissions  ('*CO[2]* '+BC+' *CH[4]* '+' *N[2]* 'O: kg ' *m^-2~s^-1*')'), axes = F); maps::map(add = T); degAxis(1); degAxis(2, las = 2)
dev.off()

pdf("/Users/ktanaka/Dropbox/PAPER climate geographic disparities/figures/supplemental/edgar co2 1970-2018_presentation.pdf", height = 5, width = 8.34)
par(mfrow = c(1,1), fg = 'white', col.axis = 'white', col.main="white", col.lab = 'white')
plot(raster::rotate(log10(co2_excl_mean + co2_org_mean)), col = matlab.like(100), main = "CO2 1970-2018 log10(kg m-2 s-1)"); maps::map(add = T)
dev.off()

pdf("/Users/ktanaka/Dropbox/PAPER climate geographic disparities/figures/supplemental/edgar ch4 1970-2015_presentation.pdf", height = 5, width = 8.34)
par(mfrow = c(1,1), fg = 'white', col.axis = 'white', col.main="white", col.lab = 'white')
plot(raster::rotate(log10(ch4_mean)), col = matlab.like(100), main = "CH4 1970-2015 log10(kg m-2 s-1)"); maps::map(add = T)
dev.off()

pdf("/Users/ktanaka/Dropbox/PAPER climate geographic disparities/figures/supplemental/edgar n2o 1970-2015_presentation.pdf", height = 5, width = 8.34)
par(mfrow = c(1,1), fg = 'white', col.axis = 'white', col.main="white", col.lab = 'white')
plot(raster::rotate(log10(n2o_mean)), col = matlab.like(100), main = "N2O 1970-2015 log10(kg m-2 s-1)"); maps::map(add = T)
dev.off()

pdf("/Users/ktanaka/Dropbox/PAPER climate geographic disparities/figures/supplemental/merra2 bc 1978-2018_presentation.pdf", height = 5, width = 8.34)
par(mfrow = c(1,1), fg = 'white', col.axis = 'white', col.main="white", col.lab = 'white')
plot(log10(bc_mean_0.1_adj), col = matlab.like(100), main = "BC 1980-2018 log10(kg m-2 s-1)"); maps::map(add = T)
dev.off()

pdf("/Users/ktanaka/Dropbox/PAPER climate geographic disparities/figures/supplemental/BC+CO2+CH4+N2O.pdf", height = 6, width = 10)
par(mfrow = c(1,1))
plot(log10(bc_co2_ch4_n2o_adjusted), col = matlab.like(100), main = "MERRA2 (BC 1980-2018) + EDGAR (CO2, CH4, N2O 1970-2018) log10(kg m-2 s-1)"); maps::map(add = T)
dev.off()

pdf("/Users/ktanaka/Dropbox/PAPER climate geographic disparities/figures/supplemental/BC+CO2+CH4+N2O_presentation.pdf", height = 5, width = 8.34)
par(mfrow = c(1,1), fg = 'white', col.axis = 'white', col.main="white", col.lab = 'white')
plot(log10(bc_co2_ch4_n2o_adjusted), col = matlab.like(100), main = "BC (1980-2018) + CO2 (1970-2018) + CH4 + N2O (1970-2015) log10(kg m-2 s-1)"); maps::map(add = T)
dev.off()


#plot surface distribution plot
ghg1 = as.data.frame(rasterToPoints(co2_excl_mean + co2_org_mean))
ghg2 = as.data.frame(rasterToPoints(ch4_mean))
ghg3 = as.data.frame(rasterToPoints(n2o_mean))
ghg4 = as.data.frame(rasterToPoints(bc_mean))

ghg1$GHG = "CO2: 1970-2018"
ghg2$GHG = "CH4: 1970-2015"
ghg3$GHG = "N2O: 1970-2015"
ghg4$GHG = "BC: 1980-2018"

ghg = rbind(ghg4, ghg3, ghg2, ghg1)

ghg = ghg[,c("layer", "GHG")]

library("ggsci")

# pdf("/Users/ktanaka/Desktop/GHG_histgram.pdf", height = 6, width = 7)

fancy_scientific <- function(l) {
  # turn in to character string in scientific notation
  l <- format(l, scientific = TRUE)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "'\\1'e", l)
  # turn the 'e+' into plotmath format
  l <- gsub("e", "%*%10^", l)
  # return this as an expression
  parse(text=l)
}

ggplot(ghg %>% sample_frac(0.01), aes(x = log10(layer), fill = GHG)) + 
  geom_density(alpha = 0.8) + 
  # xlim(-45, -15) +
  scale_y_continuous(labels = fancy_scientific) + 
  xlab("log10(kg m-2 s-1)") + ylab("") + 
  scale_fill_tron(name = "") +
  # scale_fill_viridis_d(name = "", begin = 0.5, end = 1) +
  ggdark::dark_theme_classic() + 
  ggtitle("Surface emissions (0.1° x 0.1°)") + 
  theme(legend.position = c(0.8,0.8),
        text = element_text(size = 20))

dev.off()

