####################################################
### calculate euclidean distance from slope line ###
####################################################

rm(list = ls())

load("~/Desktop/temp_emissions.RData")

# rise
rise <- (max(raw_ratio$anomaly, na.rm = T) - min(raw_ratio$anomaly, na.rm = T))

# run
run <- (max(raw_ratio$BCE, na.rm = T)  - min(raw_ratio$BCE, na.rm = T))

# slope
slope = rise / run

round(slope, 2)

raw_ratio$becker_disparity <- (raw_ratio$anomaly - (slope*raw_ratio$BCE + min(raw_ratio$anomaly, na.rm = T)))/(sqrt(slope^2-1)) # Becker formula
raw_ratio$textbook_disparity <- abs(rise*raw_ratio$BCE - run*raw_ratio$anomaly + min(raw_ratio$BCE, na.rm = T)) / sqrt(rise^2 + run^2) #Textbook formula e,g, https://www.intmath.com/plane-analytic-geometry/perpendicular-distance-point-line.php

limits_A = c(-max(abs(raw_ratio$becker_disparity), na.rm = T), max(abs(raw_ratio$becker_disparity), na.rm = T)) 
limits_B = c(min(raw_ratio$textbook_disparity, na.rm = T), max(raw_ratio$textbook_disparity, na.rm = T)) 

becker <- ggplot(raw_ratio) + 
  geom_point(aes(x = BCE, y = anomaly, color = becker_disparity), size = 4, alpha = 0.5, slope = 20, show.legend = T) +
  geom_abline(intercept = min(raw_ratio$anomaly, na.rm = T), color = "black", slope = slope) +
  scale_color_gradientn(colours = c("cyan", "black", "red"), values = scales::rescale(c(-0.5, -0.05, 0, 0.05, 0.5)), limits = limits_A, name = "LCDI") + 
  scale_x_continuous(expand = c(0,0), limits = c(0, 4.92)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 10.5)) +
  xlab("Emissions") +
  ylab('Temp Anomaly') + 
  coord_fixed(ratio = 1/slope) +
  theme_pubr() +
  theme(legend.position = c(0.1, 0.85), text = element_text(size = 15))

textbook <- ggplot(raw_ratio) +
  geom_point(aes(x = BCE, y = anomaly, color = textbook_disparity), size = 4, alpha = 0.5, slope = 20, show.legend = T) +
  geom_abline( intercept = 0, color = "black", slope = slope) +
  scale_color_gradientn(colours = c("black", "cyan", "red"), values = scales::rescale(c(0, 0.01, 0.05, 0.3, 0.5, 1)), limits = limits_B, name = "LCDI") + 
  scale_x_continuous(expand = c(0,0), limits = c(0, 4.92)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 10.5)) +
  xlab("Emissions") +
  ylab('Temp Anomaly') + 
  coord_fixed(ratio = 1/slope) +
  theme_pubr() +
  theme(legend.position = c(0.1, 0.85), text = element_text(size = 15))

pdf("~/Desktop/comarison.pdf", height = 5, width = 10)
cowplot::plot_grid(becker, textbook)
dev.off()

