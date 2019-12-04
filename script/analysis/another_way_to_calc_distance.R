library(sf)
library(sp)
data(meuse)

# create new line - please note the small changes relative to your code : 
# x = first column and cbind to have a matrix instead of a data.frame
newline = cbind(x = seq(from = 178000, to = 181000, length.out = 100),
                y = seq(from = 330000, to = 334000, length.out = 100))

# transform the points and lines into spatial objects
meuse <- st_as_sf(meuse, coords = c("x", "y"))
newline <- st_linestring(newline)

# Compute the distance - works also for non straight lines !
st_distance(meuse, newline) [1:10]
## [1] 291.0 285.2 409.8 548.0 647.6 756.0 510.0 403.8 509.4 684.8


# Ploting to check that your line is were you expect it
plot_sf(meuse)
plot(meuse, add = TRUE, pch = 20)
plot(newline, add = TRUE)

newline = cbind(x = c(178000, 181000), 
                y = c(330000, 334000))

# transform the points and lines into spatial objects
meuse <- st_as_sf(meuse, coords = c("x", "y"), crs = 31370)
newline <- st_linestring(newline)

# Compute the distance - works also for non straight lines !
st_distance(meuse, newline) [1:10]
## [1] 291.0 285.2 409.8 548.0 647.6 756.0 510.0 403.8 509.4 684.8


# Ploting to check that your line is were you expect it
plot_sf(meuse)
plot(meuse, add = TRUE, pch = 20)
plot(newline, add = TRUE)

distances <- st_distance(meuse, newline)
newline = data.frame(x = c(178000, 181000), 
                     y = c(330000, 334000))

segments <- as.data.frame(st_coordinates(meuse))
segments <- data.frame(
  segments, 
  X2 = segments$X - distances/sqrt(2), 
  Y2 = segments$Y + distances/sqrt(2)
)    

library(ggplot2)
ggplot() +
  geom_point(data = segments, aes(X,Y), color = ) +
  geom_line(data = newline, aes(x,y)) +
  geom_segment(data = segments, aes(x = X, y = Y, xend = X2, yend = Y2), 
               color = "orangered", alpha = 0.5) +
  coord_equal() + xlim(c(177000, 182000))

library(sp)
library(rgeos)

newline = cbind(x = c(178000, 181000), 
                y = c(330000, 334000))

plot_sf(meuse)
plot(meuse, add = TRUE, pch = 20)
plot(newline, add = TRUE)

spline <- as(st_as_sfc(st_as_text(st_linestring(newline))), "Spatial") # there is probably a more straighforward solution...
position <- gProject(spline, as(meuse, "Spatial"))
position <-  coordinates(gInterpolate(spline, position))
colnames(position) <- c("X2", "Y2")

segments <- data.frame(st_coordinates(meuse), position)

x = cbind(segments$X , segments$Y )
y = cbind(segments$X2 ,segments$Y2)

segments$d = pointDistance(x, y, lonlat = F, )

library(ggplot2)
ggplot() +
  geom_point(data = segments, aes(X,Y, color = d)) +
  geom_line(data = as.data.frame(newline), aes(x,y)) +
  geom_segment(data = segments, aes(x = X, y = Y, xend = X2, yend = Y2, color = d), alpha = 0.5) +
  coord_equal() + xlim(c(177000, 182000)) + 
  scale_color_viridis_c()

