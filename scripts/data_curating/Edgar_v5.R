library(raster)
library(rasterVis)

rm(list = ls())

scale_x_longitude <- function(xmin=-180, xmax=180, step=1, ...) {
  xbreaks <- seq(xmin,xmax,step)
  xlabels <- unlist(lapply(xbreaks, function(x) ifelse(x < 0, parse(text=paste0(x,"^o", "*W")), ifelse(x > 0, parse(text=paste0(x,"^o", "*E")),x))))
  return(scale_x_continuous("", breaks = xbreaks, labels = xlabels, expand = c(0, 0), ...))
}
scale_y_latitude <- function(ymin=-90, ymax=90, step=0.5, ...) {
  ybreaks <- seq(ymin,ymax,step)
  ylabels <- unlist(lapply(ybreaks, function(x) ifelse(x < 0, parse(text=paste0(x,"^o", "*S")), ifelse(x > 0, parse(text=paste0(x,"^o", "*N")),x))))
  return(scale_y_continuous("", breaks = ybreaks, labels = ylabels, expand = c(0, 0), ...))
}    

setwd("~/Dropbox (MBA)/PAPER climate geographic disparities/figures/supplemental")

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

pdf("Edgar_CH4_1970-2015.pdf", width = 10, height = 6)

ghg = mean(ghg)
ghg = raster::rotate(ghg)

rasterVis::gplot(ghg) + 
  geom_point(aes(color = log10(value))) +
  # coord_equal() +
  coord_quickmap(xlim = c(-180, 180),
                 ylim = c(-90, 90)) +
  theme_bw() + 
  borders(xlim = c(-180, 180),
          ylim = c(-180, 180),
          fill = NA, size = 0.1) +
  scale_x_longitude(xmin = -180, xmax = 180, step = 60) +
  scale_y_latitude(ymin = -180, ymax = 180, step = 60) +
  scale_color_gradientn(colours = c( "black","cyan", "red"), name = "log10(kg m-2)") + 
  ggtitle("CH4_1970-2015")

dev.off()

################
### CO2 excl ###
################
GHG_ts <- list.files('/Users/ktanaka/Desktop/edgar/v50_CO2_excl_short-cycle_org/v50_CO2_excl_short-cycle_org_C_TOTALS_nc/', pattern = '\\.nc$') # list all files in thel folder
GHG_ts <- paste0("/Users/ktanaka/Desktop/edgar/v50_CO2_excl_short-cycle_org/v50_CO2_excl_short-cycle_org_C_TOTALS_nc/", GHG_ts) # add parent directories

emissions <- "emi_co2"

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

pdf("Edgar_CO2_excl_1970-2018.pdf", width = 10, height = 6)

ghg = mean(ghg)
ghg = raster::rotate(ghg)

rasterVis::gplot(ghg) + 
  geom_point(aes(color = log10(value))) +
  # coord_equal() +
  coord_quickmap(xlim = c(-180, 180),
                 ylim = c(-90, 90)) +
  theme_bw() + 
  borders(xlim = c(-180, 180),
          ylim = c(-180, 180),
          fill = NA, size = 0.1) +
  scale_x_longitude(xmin = -180, xmax = 180, step = 60) +
  scale_y_latitude(ymin = -180, ymax = 180, step = 60) +
  scale_color_gradientn(colours = c( "black","cyan", "red"), name = "kg m-2") + 
  ggtitle("CO2_excl_1970-2018")

dev.off()

###############
### CO2 org ###
###############

GHG_ts <- list.files('/Users/ktanaka/Desktop/edgar/v50_CO2_org_short-cycle_C/v50_CO2_org_short-cycle_C_TOTALS_nc/', pattern = '\\.nc$') # list all files in thel folder
GHG_ts <- paste0("/Users/ktanaka/Desktop/edgar/v50_CO2_org_short-cycle_C/v50_CO2_org_short-cycle_C_TOTALS_nc/", GHG_ts) # add parent directories

emissions <- "emi_co2"

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

pdf("Edgar_CO2_org_1970-2015.pdf", width = 10, height = 6)

ghg = mean(ghg)
ghg = raster::rotate(ghg)

rasterVis::gplot(ghg) + 
  geom_point(aes(color = log10(value))) +
  coord_equal() +
  coord_quickmap(xlim = c(-180, 180),
                 ylim = c(-90, 90)) +
  theme_bw() + 
  borders(xlim = c(-180, 180),
          ylim = c(-180, 180),
          fill = NA, size = 0.1) +
  scale_x_longitude(xmin = -180, xmax = 180, step = 60) +
  scale_y_latitude(ymin = -180, ymax = 180, step = 60) +
  scale_color_gradientn(colours = c( "black","cyan", "red"), name = "kg m-2") + 
  ggtitle("CO2_org_1970-2015")

dev.off()

###########
### N2O ###
###########

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

ghg = mean(ghg)

pdf("Edgar_N2O_1970-2015.pdf", width = 10, height = 6)

ghg = mean(ghg)
ghg = raster::rotate(ghg)

rasterVis::gplot(ghg) + 
  geom_point(aes(color = log10(value))) +
  # coord_equal() +
  coord_quickmap(xlim = c(-180, 180),
                 ylim = c(-90, 90)) +
  theme_bw() + 
  borders(xlim = c(-180, 180),
          ylim = c(-180, 180),
          fill = NA, size = 0.1) +
  scale_x_longitude(xmin = -180, xmax = 180, step = 60) +
  scale_y_latitude(ymin = -180, ymax = 180, step = 60) +
  scale_color_gradientn(colours = c( "black","cyan", "red"), name = "kg m-2") + 
  ggtitle("N2O_org_1970-2015")

dev.off()

###########
### SO2 ###
###########

GHG_ts <- list.files('/Users/ktanaka/Desktop/edgar/v432_SO2_TOTALS_nc/', pattern = '\\.nc$') # list all files in thel folder
GHG_ts <- paste0("/Users/ktanaka/Desktop/edgar/v432_SO2_TOTALS_nc/", GHG_ts) # add parent directories

emissions <- "emi_so2"

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

pdf("Edgar_SO2_1970-2012.pdf", width = 10, height = 6)

ghg = mean(ghg)
ghg = raster::rotate(ghg)

rasterVis::gplot(ghg) + 
  geom_point(aes(color = log10(value))) +
  # coord_equal() +
  coord_quickmap(xlim = c(-180, 180),
                 ylim = c(-90, 90)) +
  theme_bw() + 
  borders(xlim = c(-180, 180),
          ylim = c(-180, 180),
          fill = NA, size = 0.1) +
  scale_x_longitude(xmin = -180, xmax = 180, step = 60) +
  scale_y_latitude(ymin = -180, ymax = 180, step = 60) +
  scale_color_gradientn(colours = c( "black","cyan", "red"), name = "kg m-2") + 
  ggtitle("SO2_1970-2012")

dev.off()

