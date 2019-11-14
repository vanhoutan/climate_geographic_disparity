rm(list = ls())

library(spatial.tools)
library(ncdf4)
library(tidyverse)  # data tidying
library(raster)
library(colorRamps)
library(ggpubr)
library(sm)
library(maps)

##########################################################
### read MERRA2 BC layers, then stack and average them ###
##########################################################

#MERRA2 BC resolution  0.5° × 0.625° kg m-2 s-1

# list all files in the MERRA model folder
BC <- list.files('/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/causes/GHGs', pattern = '\\.nc4$')
# BC = BC[226:441] #BC data Jan 2000 - December 2017
BC <- paste0("/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/causes/GHGs/", BC) # add parent directories

ghg <- c("OCEMAN","BCEMAN","SO2EMAN","SO4EMAN")[2] #BCEMAN = Black Carbon Anthropogenic Emissions

bc = raster::stack()

for (t in 1:length(BC)) {

  filename <- BC[t] # pull the first file,first year

  year_month_read <- stack(filename, varname = ghg) # read it in as a raster
  names(year_month_read) <- paste0(ghg,"_", gsub(".*Nx.\\s*|.nc4.*", "", filename)) # generate name: Emission_yearmonth
  print(names(year_month_read)) # print in the loop to keep an eye on progress
  bc <- stack(bc,year_month_read) # add to the timeseries stack

}

bc_mean = mean(bc)

save.image("/Users/ktanaka/clim_geo_disp/output/MERRA2_BC_Mean_1980-2018.RData")


load("/Users/ktanaka/clim_geo_disp/output/MERRA2_BC_Mean_1980-2018.RData")


##################################################
### EDGAR v5.0 Global Greenhouse Gas Emissions ###
##################################################

#gridmaps are expressed in 0.1°* x 0.1° kg m-2 s-1 

################
### CO2 excl ###
################
GHG_ts <- list.files('/Users/ktanaka/Desktop/edgar/v50_CO2_excl_short-cycle_org/v50_CO2_excl_short-cycle_org_C_TOTALS_nc/', pattern = '\\.nc$')
GHG_ts <- paste0("/Users/ktanaka/Desktop/edgar/v50_CO2_excl_short-cycle_org/v50_CO2_excl_short-cycle_org_C_TOTALS_nc/", GHG_ts) 

emissions <- "emi_co2"

ghg <- raster::stack() # build an empty stack to put the desired emission metric in to

for( t in 1:length(GHG_ts)){ # for each file that is year in a month
  
  filename <- GHG_ts[t] # pull the first file,first year
  year_month_read <- stack(filename, varname = emissions) # read it in as a raster
  names(year_month_read) <- paste0(emissions,"_", gsub(".*Nx.\\s*|.nc4.*", "", filename)) # generate name: Emission_yearmonth
  print(names(year_month_read)) # print in the loop to keep an eye on progress
  ghg <- stack(ghg,year_month_read) # add to the timeseries stack
  
}

co2_excl_mean = mean(ghg)

###############
### CO2 org ###
###############
GHG_ts <- list.files('/Users/ktanaka/Desktop/edgar/v50_CO2_org_short-cycle_C/v50_CO2_org_short-cycle_C_TOTALS_nc/', pattern = '\\.nc$')
GHG_ts <- paste0("/Users/ktanaka/Desktop/edgar/v50_CO2_org_short-cycle_C/v50_CO2_org_short-cycle_C_TOTALS_nc/", GHG_ts)

emissions <- "emi_co2"

ghg <- raster::stack() # build an empty stack to put the desired emission metric in to

for( t in 1:length(GHG_ts)){ # for each file that is year in a month
  
  filename <- GHG_ts[t] # pull the first file,first year
  year_month_read <- stack(filename, varname = emissions) # read it in as a raster
  names(year_month_read) <- paste0(emissions,"_", gsub(".*Nx.\\s*|.nc4.*", "", filename)) # generate name: Emission_yearmonth
  print(names(year_month_read)) # print in the loop to keep an eye on progress
  ghg <- stack(ghg,year_month_read) # add to the timeseries stack
  
}

co2_org_mean = mean(ghg)


###########
### CH4 ###
###########

GHG_ts <- list.files('/Users/ktanaka/Desktop/edgar/v50_CH4_1970_2015/v50_CH4_TOTALS_nc', pattern = '\\.nc$') # list all files in thel folder
GHG_ts <- paste0("/Users/ktanaka/Desktop/edgar/v50_CH4_1970_2015/v50_CH4_TOTALS_nc/", GHG_ts) # add parent directories

emissions <- "emi_ch4"

ghg <- raster::stack() # build an empty stack to put the desired emission metric in to

for( t in 1:length(GHG_ts)){ # for each file that is year in a month
  
  filename <- GHG_ts[t] # pull the first file,first year
  
  # year_month_read <- stack(filename, varname = emissions[gas]) # read it in as a raster
  # names(year_month_read) <- paste0(emissions[gas],"_", gsub(".*Nx.\\s*|.nc4.*", "", filename)) # generate name: Emission_yearmonth
  #plot(year_month_read, col = viridis::viridis(10), main = names(year_month_read))
  
  year_month_read <- stack(filename, varname = emissions) # read it in as a raster
  names(year_month_read) <- paste0(emissions,"_", gsub(".*Nx.\\s*|.nc4.*", "", filename)) # generate name: Emission_yearmonth
  print(names(year_month_read)) # print in the loop to keep an eye on progress
  ghg <- stack(ghg,year_month_read) # add to the timeseries stack
  
}

ch4_mean = mean(ghg)

###############
### N2O ###
###############

GHG_ts <- list.files('/Users/ktanaka/Desktop/edgar/v50_N2O/v50_N2O_TOTALS_nc/', pattern = '\\.nc$') # list all files in thel folder
GHG_ts <- paste0("/Users/ktanaka/Desktop/edgar/v50_N2O/v50_N2O_TOTALS_nc/", GHG_ts) # add parent directories

emissions <- "emi_n2o"

ghg <- raster::stack() # build an empty stack to put the desired emission metric in to

for( t in 1:length(GHG_ts)){ # for each file that is year in a month
  
  filename <- GHG_ts[t] # pull the first file,first year
  
  # year_month_read <- stack(filename, varname = emissions[gas]) # read it in as a raster
  # names(year_month_read) <- paste0(emissions[gas],"_", gsub(".*Nx.\\s*|.nc4.*", "", filename)) # generate name: Emission_yearmonth
  #plot(year_month_read, col = viridis::viridis(10), main = names(year_month_read))
  
  year_month_read <- stack(filename, varname = emissions) # read it in as a raster
  names(year_month_read) <- paste0(emissions,"_", gsub(".*Nx.\\s*|.nc4.*", "", filename)) # generate name: Emission_yearmonth
  print(names(year_month_read)) # print in the loop to keep an eye on progress
  ghg <- stack(ghg,year_month_read) # add to the timeseries stack
  
}

n2o_mean = mean(ghg)



#################################################################################
### sum layers, multiply non-co2 layers according to average GTP20-100 values ###
#################################################################################
ghg_mean = sum(co2_excl_mean, co2_org_mean, ch4_mean*40.5, n2o_mean*290.5)

###############################################################################
### use bilinear interpolation method to resample BC on 0.1 by 0.1 deg grid ###
###############################################################################
bc_mean_0.1 = resample(bc_mean, raster::rotate(ghg_mean), method = "bilinear") 

##################################################################################################
### multiply BC by average global temperature potenital value 20-100 years (Bond et al (2013)) ###
##################################################################################################
bc_mean_0.1_adj = bc_mean_0.1*400

######################
### sum everything ###
######################
bc_co2_ch4_n2o_adjusted = sum(raster::rotate(ghg_mean), bc_mean_0.1_adj)
plot(bc_co2_ch4_n2o_adjusted, col = matlab.like(100))
plot(log10(bc_co2_ch4_n2o_adjusted), col = matlab.like(100))



### plot individual layers ###

pdf("/Users/ktanaka/Dropbox/PAPER climate geographic disparities/figures/supplemental/Edgar GHG 1970-2018.pdf", height = 10, width = 15)
par(mfrow = c(2,2))
plot(raster::rotate(log10(co2_excl_mean)), col = matlab.like(100), main = "EDGAR CO2 EXCL 1970-2018 log10(kg m-2 s-1)"); maps::map(add = T)
plot(raster::rotate(log10(co2_org_mean)), col = matlab.like(100), main = "EDGAR CO2 ORG 1970-2015 log10(kg m-2 s-1)"); maps::map(add = T)
plot(raster::rotate(log10(ch4_mean)), col = matlab.like(100), main = "EDGAR CH4 EXCL 1970-2015 log10(kg m-2 s-1)"); maps::map(add = T)
plot(raster::rotate(log10(n2o_mean)), col = matlab.like(100), main = "EDGAR N2O 1970-2015 log10(kg m-2 s-1)"); maps::map(add = T)
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


###################################
### save unified emission layer ###
###################################

bc_co2_ch4_n2o_adjusted = readAll(bc_co2_ch4_n2o_adjusted)

save(bc_co2_ch4_n2o_adjusted, file = "/Users/ktanaka/clim_geo_disp/output/BC_CO2_CH4_N2O_Combined_1970-2018.RData")


#plot surface distribution plot
ghg1 = as.data.frame(rasterToPoints(co2_excl_mean + co2_org_mean))
ghg2 = as.data.frame(rasterToPoints(ch4_mean))
ghg3 = as.data.frame(rasterToPoints(n2o_mean))
ghg4 = as.data.frame(rasterToPoints(bc_mean))

ghg1$GHG = "CO2 1970-2018"
ghg2$GHG = "CH4 1970-2015"
ghg3$GHG = "N2O 1970-2015"
ghg4$GHG = "BC 1980-2018"

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

ggplot(ghg %>% sample_frac(0.1), aes(x = log(layer), fill = GHG)) + 
  geom_density(alpha = 0.5) + 
  # xlim(-42, -15) + 
  scale_y_continuous(labels = fancy_scientific) + 
  xlab("log10(kg m-2 s-1)") + ylab("") + 
  # scale_fill_tron(name = "") +
  scale_fill_viridis_d(name = "") +
  theme_pubr() + 
  ggtitle("Surface emissions (1970-2018: 0.1° x 0.1° res)") + 
  theme(legend.position = c(0.8,0.8),
        # title = element_text(colour = "white"),
        # axis.text = element_text(color = "white"),
        # axis.title = element_text(color = "white"),
        # axis.line = element_line(color = "white"),
        # legend.text = element_text(color = "white", size = 20),
        # panel.background = element_rect(fill = "transparent"), # bg of the panel
        # plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        # panel.grid.major = element_blank(), # get rid of major grid
        # panel.grid.minor = element_blank(), # get rid of minor grid
        # legend.background = element_rect(fill = "transparent"), # get rid of legend bg
        # legend.box.background = element_rect(fill = "transparent"), # get rid of legend panel bg
        text = element_text(size = 15))

# par(mfrow = c(1,4), fg = 'white', col.axis = 'white', col.main = "white", col.lab = 'white')
# hist(log10(co2_excl_mean + co2_org_mean), breaks = 50, col = "white", main = "Distribution of surface CO2", xlab = "log10(kg/m-2y-1)", ylab = "", xlim = c(-22, -1), ylim = c(0, 800000))
# hist(log10(ch4_mean), breaks = 50, col = "white", main = "Distribution of surface Ch4",xlab = "log10(kg/m-2y-1)", ylab = "", xlim = c(-22, -1), ylim = c(0, 800000))
# hist(log10(n2o_mean), breaks = 50, col = "white", main = "Distribution of surface N2O",xlab = "log10(kg/m-2y-1)", ylab = "", xlim = c(-22, -1), ylim = c(0, 800000))
# hist(log10(bc_mean), breaks = 50, col = "white", main = "Distribution of surface BC",xlab = "log10(kg/m-2y-1)", ylab = "", xlim = c(-22, -1), ylim = c(0, 800000))

dev.off()

