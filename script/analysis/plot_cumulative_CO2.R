library(ggplot2)
library(colorRamps)
library(ggpubr)

rm(list = ls())

df <- readr::read_csv("/Users/ktanaka/clim_geo_disp/data/annual-co-emissions-by-region.csv")

df$`Annual CO₂ emissions (tonnes )` = df$`Annual CO₂ emissions (tonnes )`

co2 = NULL

for (i in 1751:2017) {
  
  # i = 2017
  
  d = subset(df, Year == i)
  sum = sum(d$`Annual CO₂ emissions (tonnes )`)
  d = data.frame(i, sum)  
  
  co2 = rbind(co2, d)
  
}

colnames(co2) = c("Year", "CO2")

pre2000 = subset(co2, Year %in% c(1751:1999))
post2000 = subset(co2, Year %in% c(2000:2017))

sum(pre2000$CO2)
sum(post2000$CO2)

sum(post2000$CO2)/(sum(post2000$CO2) + sum(pre2000$CO2))

co2$Period = ifelse(co2$Year %in% c(1751:1999), "Pre-2000", "Post-2000")

# df = subset(co2, Year %in% c(1800, 1850, 1900, 1950, 1975, 2000, 2017))
# plot(df, pch = 20, bty = "l")

par(mar = c(2,5,1,1))
plot(co2$Year, co2$CO2, axes = F, pch = 20, cex = 2, 
     ann = F, col = matlab.like(dim(co2)[1]))
axis(1, at = seq(1750, 2020, by = 10))
axis(2, las = 2, at = seq(0, 900000000, by = 10000000))
legend("topleft", "Annual CO2 emissions 1971-2017 (1000 tonnes )", bty = "n")

pdf("/Users/ktanaka/Dropbox/PAPER climate geographic disparities/figures/supplemental/Annual_CO2_Emissions_1751-2017.pdf", height = 5, width = 5.5)
ggplot(co2, aes(Year, CO2, color = Period)) + 
  geom_point() +
  ylab("1000 tonnes") + xlab("") + 
  scale_x_continuous(breaks = seq(1750, 2020, by = 50)) +
  scale_y_continuous() +
  scale_color_manual(values = c(matlab.like(10)[1], matlab.like(10)[10])) + 
  geom_text(aes(x = 1751, y = max(co2$CO2), 
                label = "Total CO2 emissions 1751-1999: 2.6e+12 tons", 
                color = "Pre-2000"), show.legend = F, hjust = 0, fontface = "bold",
            data = data.frame()) + 
  geom_text(aes(x = 1751, y = max(co2$CO2)*0.95, 
                label = "Total CO2 emissions 2000-2017: 1.4e+12 tons", 
                color = "Post-2000"), show.legend = F, hjust = 0, fontface = "bold",
            data = data.frame()) +
  theme_pubr(I(10)) + 
  theme(legend.position = "none")
dev.off()
