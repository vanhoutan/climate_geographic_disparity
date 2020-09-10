library(ggplot2)
library(dplyr)
library(sf)
library(forcats)
library(ggpubr)
library(maps)

rm(list = ls())

data = c("_merra2_edgar_ghg", "_merra2_edgar_co2", "_merra2_odiac")[1]

rcp = c("RCP4.5", "RCP8.5")[2]
period = c("2006-2055", "2050-2099")[2]
scale = c("scaled", "unscaled")[2]

load(paste0("~/clim_geo_disp/output/intersection_result_", period, "_", rcp, data, ".Rdata"))

df = subset(intersection_world, geounit %in% c("China", "Greenland", "India"))

library(plyr)
median <- ddply(df, "geounit", summarise, med = median(disparity))
q10 <- ddply(df, "geounit", summarise, q10 = quantile(disparity, 0.1))
q90 <- ddply(df, "geounit", summarise, q90 = quantile(disparity, 0.9))

pdf('/Users/ktanaka/Desktop/quantile_comparisons.pdf', height = 6, width = 6)

ggplot(df, aes(x=disparity, fill = geounit)) + 
  # geom_histogram(alpha = 0.5) +
  geom_vline(data = median, aes(xintercept = med, color = geounit), size = 2)+
  geom_vline(data=q10, aes(xintercept=q10, color=geounit),linetype="dashed", size = 2)+
  geom_vline(data=q90, aes(xintercept=q90, color=geounit),linetype="dashed", size = 2)+
  facet_wrap(~geounit, ncol = 1, scales = "fixed") +
  geom_density(alpha = 0.8, adjust = 1/3) +
  # theme_pubr(I(20)) +
  dark_theme_classic(I(20)) + 
  xlab("") +
  ylab("") +
  scale_fill_discrete(name = "") + 
  scale_color_discrete(name = "") + 
  guides(fill = guide_legend(override.aes = list(linetype = 0, alpha = 1)),
         color = guide_legend(override.aes = list(linetype = 0))) + 
  theme(
    legend.position = c(0.15, 0.95),
    # legend.position = "none",
    strip.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    text = element_text(size = 15))

dev.off()

