rm(list = ls())

dir = Sys.info()[7]

setwd('/Users/ktanaka/Downloads/anthromes_1_NetCDF')

fn <- 'anthromes_v1.nc'

library(raster)
library(ncdf4)
library(rgdal)
library(rgeos)

ncfile <- nc_open(fn)
print(ncfile) # see all the object's structure and attributes

YourBrick <- brick(fn)
YourBrick

r <- raster(YourBrick, layer=1) # Select which data is needed to be saved

r

class(r)

plot(r, axes = T)

gpw_pop <- stack(paste0("/Users/", dir, "/Desktop/gpw/gpw_v4_population_density_rev11_1_deg.nc"), varname = "Population Density, v4.11 (2000, 2005, 2010, 2015, 2020): 1 degree")

r = resample(r, gpw_pop, method = "ngb") #use bilinear interpolation method to resample layer on 1 by 1 deg grid
plot(r, axes = T)

anthrome <- rasterToPolygons(r, dissolve = F, digits = 0)

save(anthrome, file = "/Users/ktanaka/Desktop/anthrome_1.RData")

p <- rasterToPolygons(r, dissolve = F, digits = 0)

p

p <- ms_simplify(p, keep = 0.001, keep_shapes = F) # simplify shapefile (saves computing time)

p <- p %>% st_as_sf() 

intersection_anthromes <- st_intersection(disparity,p)

anthromes <- intersection_anthromes %>% 
  group_by(layer) %>% 
  summarise(median = median(disparity, na.rm = T),
            mean = mean(disparity, na.rm = T),
            q_10 = quantile(disparity, 0.1, na.rm = T),
            q_90 = quantile(disparity, 0.9, na.rm = T),
            sd = sd(disparity, na.rm = T), 
            n = n()) %>%
  mutate(se = sd/sqrt(n),
         lower.ci = median - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = median + qt(1 - (0.05 / 2), n - 1) * se)

anthromes = anthromes %>% st_set_geometry(NULL) # drop geometry
colnames(anthromes) = c("unit", "median", "mean", "q_10", "q_90", "sd", "n", "se", "lower.ci", "upper.ci"); anthromes$type = "Land"

anthromes %>% 
  mutate(unit = fct_reorder(factor(unit), q_10)) %>% 
  ggplot() +
  geom_segment(aes(
    color = q_10, 
    x = unit, 
    xend = unit,
    y = q_10, 
    yend = q_90),
    size = 1) +
  geom_point(aes(
    color = q_10,
    x = unit,
    y = median),
    size = 1) +
  coord_flip() +
  scale_colour_gradientn(
    colours = c("cyan", 
                "black",
                "red"),
    # values = scales::rescale(c(-0.5, -0.01, 0.0, 0.01, 0.5)),
    values = scales::rescale(c(-0.5, -0.04, 0.1, 0.2, 0.5)),
    name = "LCDI") + 
  xlab("") +
  ylab("") +
  theme_pubr() + 
  scale_y_continuous(breaks=c(-4, -2, 0, 2, 4)) + 
  theme(
    strip.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    # strip.background = element_blank(),
    # title = element_text(colour = "white"),
    # axis.text = element_text(color = "white"),
    # axis.title = element_text(color = "white"),
    # axis.line = element_line(color = "white"),
    # legend.text = element_text(color = "white", size = 20),
    # panel.background = element_rect(fill = "gray20"), # bg of the panel
    # plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    # panel.grid.major = element_blank(), # get rid of major grid
    # panel.grid.minor = element_blank(), # get rid of minor grid
    # legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    # legend.box.background = element_rect(fill = "transparent"), # get rid of legend panel bg
    text = element_text(size = 10))


spplot(p,axes=T)

# Save as shapefile
writeOGR(p, dsn='/Users/ktanaka/Desktop/anthromes_v1', layer='anthromes_v1', driver="ESRI Shapefile",overwrite_layer=T)
