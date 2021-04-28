library(ggplot2)
library(colorRamps)
library(ggpubr)

rm(list = ls())

df <- readr::read_csv("~/climate_geographic_disparity/data/annual-co-emissions-by-region.csv")

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

sub = subset(co2, Year %in% c(1970:2015))

sum_total = sum(co2$CO2)
sum_sub = sum(sub$CO2)

sum_sub/sum_total

co2$Period = ifelse(co2$Year %in% c(1970:2015), "A", "B")

# df = subset(co2, Year %in% c(1800, 1850, 1900, 1950, 1975, 2000, 2017))
# plot(df, pch = 20, bty = "l")

pdf("/Users/ktanaka/Dropbox/PAPER climate geographic disparities/figures/supplemental/Annual_CO2_Emissions_1751-2017_1.pdf", 
    height = 4, width = 6)

par(mar = c(2,5,1,1))
plot(co2$Year, co2$CO2/1000000000, axes = F, pch = 20, cex = 2,
     ann = F, col = matlab.like(dim(co2)[1]))
axis(1, at = seq(1750, 2020, by = 10))
axis(2, las = 2, at = seq(0, max(co2$CO2)/1000000000, by = 10 ))
legend("topleft", "Annual CO2 emissions 1751-2017 (billion tonnes )", bty = "n")

dev.off()


pdf("/Users/ktanaka/Dropbox/PAPER climate geographic disparities/figures/supplemental/Annual_CO2_Emissions_1751-2017_2.pdf", height = 4, width = 6)
ggplot(co2, aes(Year, CO2/1000000000, color = Period)) + 
  geom_point() +
  ylab("billion tonnes") + xlab("") + 
  scale_x_continuous(breaks = seq(1750, 2020, by = 50)) +
  scale_y_continuous() +
  scale_color_manual(values = c("red", "blue")) + 
  geom_text(aes(x = 1751, y = max(co2$CO2/1000000000), 
                label = paste0("Total CO2 emissions 1751-2017: ", round(sum_total/1000000000, 1), " billion tonnes"), 
                color = "A"), show.legend = F, hjust = 0, fontface = "bold",
            data = data.frame()) + 
  geom_text(aes(x = 1751, y = max(co2$CO2/1000000000)*0.95, 
                label = paste0("Total CO2 emissions 1970-2015: ", round(sum_sub/1000000000, 1), " billion tonnes"), 
                color = "B"), show.legend = F, hjust = 0, fontface = "bold",
            data = data.frame()) +
  theme_pubr(I(10)) + 
  theme(legend.position = "none")
dev.off()
