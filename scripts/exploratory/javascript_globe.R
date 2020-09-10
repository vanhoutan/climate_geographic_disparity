# 3D globe visualizations
library(rgdal)
library(threejs)
library(RColorBrewer)
library(rworldmap)
library(geosphere)
library(gpclib)
library(animation)


test_mega_df <- future_anomally %>%  rasterToPoints() %>% 
  as.data.frame()

test_mega_df <- raw_ratio
test_mega_df$layer <- test_mega_df$ratio

# World map
worldMap <- getMap()
world.points <- fortify(worldMap)
world.points$region <- world.points$id
world.df <- world.points[,c("long","lat","group", "region")]


rotate_map <- function(angle = -74){
  ggplot() + 
    geom_tile(data = test_mega_df[1:50000,], aes(x = x, y = y, fill = layer), alpha = 0.8) +
    scale_fill_distiller(palette = "Spectral", direction = 1)+
    geom_path(data = world.df, aes(x = long, y = lat, group = group)) +
    scale_y_continuous(breaks = (-2:2) * 30) +
    scale_x_continuous(breaks = (-4:4) * 45) +
    coord_map("ortho", orientation=c(61, angle, 0))
}


saveGIF({
  ani.options(nmax = 360)
  for(i in seq(0,360)){
    print(rotate_map(i))
    print(i)
  }
}, interval = 0.1, outdir="~", movie.name = "temp_anomalies.gif")

globe <- ggplot(test_mega_df,aes(x,y,fill = layer))+
  geom_tile()+
  scale_fill_distiller(palette = "Spectral", direction = 1)+
  coord_map("ortho", orientation=c(61, 20, 0))

library(animation)

saveGIF({
  ani.options(nmax = 360)
  for(i in seq(0,360)){
    print(rotate_map(globe))
    print(i)
  }
}, interval = 0.1, outdir="/Users/ewengallic/Documents/Projets/carte_pole", movie.name = "temp_anomalies.gif")

var_to_map <- test_mega_df$layer *-1

test_mega_df$var_to_map <- as.numeric( 
  cut(var_to_map, breaks=seq(from = min(var_to_map), to = max(var_to_map), length = 200),                                      
      #breaks=quantile(var_to_map, probs=c(seq(from = 0, to = 1, length = 20))),
      include.lowest=TRUE))

col_ramp <- colorRampPalette(brewer.pal(11,"Spectral") %>%  rev()) 
col <- (col_ramp(200))[test_mega_df$var_to_map]

globejs(lat=test_mega_df$y, long=test_mega_df$x,
        val=test_mega_df$var_to_map^1.01,    # Bar height 
        #val= 0,
        color=col,
        pointsize=10,
        atmosphere=F,
        renderer = 'auto',
        fov = 50,
        rotationlong = 2.2,
        bg = "white",
        bodycolor = "white",
        emissive = "white")



