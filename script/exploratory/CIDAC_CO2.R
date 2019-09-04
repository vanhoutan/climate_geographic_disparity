library(raster)
library(tidyverse)
library(viridis)
library(rasterVis)
library(magick)

year <- 1751:2013
#dev.new()

#plot(1751,0,xlim = c(1751,2013), ylim = c(0,10000))

for(each_year in 1:length(year)){
  
  year_name <- paste0('/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/causes/CIDAC_CO2/gridcar/gridfiles/gridcar.', year[each_year])
  
  one_year <- read.table(year_name) %>% 
    as.matrix() %>% 
    as.vector() %>% 
    matrix(nrow = 180, ncol = 360, byrow = T) %>% 
    raster(xmn= -180, xmx= 180,
           ymn= -90, ymx= 90, 
           crs=CRS("+proj=longlat +datum=WGS84"))
  
  png(paste0("/Users/ktanaka/Desktop/","CO2_", year[each_year],".png"),
      width = 800, 
      height = 800
  )
  
  levelplot((one_year + 1 ), 
            zscaleLog = T,
            xlab = year[each_year] %>% as.character() ,
            ylab = NULL) %>% print()
  
  #ann_sum <- cellStats(one_year, sum)
  
  #points(year[each_year],ann_sum, cex = .2)
  
  print(year[each_year])
  dev.off()
  
}

# Step 2: List those Plots, Read them in, and then make animation
list.files(path = "/Users/tgagne/Desktop/CO2_gif/", 
           pattern = "*.png", 
           full.names = T) %>% 
  map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=10) %>% # animates, can opt for number of loops
  image_write("/Users/tgagne/Desktop/CO2_gif/ndwi_aug_hgm.gif") # write to current dir


## Using ImageMagick: Set working dir first to images
system(command= "convert /Users/tgagne/Desktop/CO2_gif/* -delay 100 -loop 0 /Users/tgagne/Desktop/CO2_gif/cdec_snow_IM.gif")


# play with -delay as necessary, this is much faster than the fps in "magick"

# To Resize Use
system(command= "convert -size 1100x850 cdec_snow.gif -resize 800x600 cdec_snow_small.gif")

## OR
system(command= "convert cdec_snow.gif -resize 50% cdec_snow_small.gif")

## TO Slow down use existing Gif

system(command = "convert -delay 40 cdec_snow.gif cdec_snow.gif")


plot(1751,0,xlim = c(1751,2015))

for(each_year in 1:length(year)){
  
  year_name <- paste0('/Users/ktanaka/Desktop/climate/KV_climate/climate_impacts_2019/data/causes/CIDAC_CO2/gridcar/gridfiles/gridcar.',year[each_year])
  
  one_year <- read.table(year_name) %>% sum()
  
  
  
}








