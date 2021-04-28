lcdi <- read_csv("~/climate_geographic_disparity/data/LCDI_RCP8.5_2050-2099.csv")

bins <- 100
cols <- c("cyan", "black", "red")
colGradient <- colorRampPalette(cols)
cut.cols <- colGradient(bins)

ggplot(lcdi, aes(lcdi, fill = cut(lcdi, bins))) +
  geom_histogram(show.legend = FALSE) + 
  scale_fill_manual(values = cut.cols) + 
  ggthemes::theme_few()
