# shape <- readOGR(dsn = "/Users/ktanaka/clim_geo_disp/data/TEOW", layer = "wwf_terr_ecos")  # read the shapefile in by name not the lack of .shp extension
# shape <- ms_simplify(shape, keep = 0.001, keep_shapes = F) # simplify shapefile (saves computing time)
# save(shape, file = "/Users/ktanaka/clim_geo_disp/data/teow_0.001.RData")
load("/Users/ktanaka/clim_geo_disp/data/teow_0.001.RData")
pdf("/Users/ktanaka/Desktop/biome.pdf", bg ="transparent", width = 20, height = 10)
par(fg = 'white', col.axis = 'white', col.main="white", col.lab = 'white')
spplot(shape, zcol = "ECO_NAME", colorkey = F)
dev.off()


# shape <- readOGR(dsn = "/Users/ktanaka/clim_geo_disp/data/MEOW_2", layer = "WCMC-036-MEOW-PPOW-2007-2012-NoCoast")
# shape <- ms_simplify(shape, keep = 0.01, keep_shapes = F) # simplify shapefile (saves computing time)
save(shape, file = "/Users/ktanaka/clim_geo_disp/data/meow_0.01.RData")
load("/Users/ktanaka/clim_geo_disp/data/meow_0.01.RData")
pdf("/Users/ktanaka/Desktop/realm.pdf", bg ="transparent", width = 10, height = 5)
par(fg = 'white', col.axis = 'white', col.main="white", col.lab = 'white')
# spplot(shape, zcol = "REALM", key=list(lines=TRUE, col="transparent"))
spplot(shape, zcol = "REALM", colorkey = F)
dev.off()

shape <- ne_countries(scale = "small", returnclass = "sf") #worldwide country polygon
pdf("/Users/ktanaka/Desktop/geographical.pdf", bg ="transparent", width = 30, height = 30)
par(fg = 'white', col.axis = 'white', col.main="white", col.lab = 'white')
plot(shape[57])
dev.off()

shape <- ne_countries(scale = "small", returnclass = "sf") #worldwide country polygon
pdf("/Users/ktanaka/Desktop/world.pdf", bg ="transparent", width = 20, height = 10)
par(fg = 'white', col.axis = 'white', col.main="white", col.lab = 'white')
plot(shape[12])
dev.off()

shape <- ne_states(country = "United States of America", returnclass = "sf")
pdf("/Users/ktanaka/Desktop/us.pdf", bg ="transparent", width = 20, height = 10)
par(fg = 'white', col.axis = 'white', col.main="white", col.lab = 'white')
plot(shape[9])
dev.off()
