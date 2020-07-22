rm(list = ls())

mpi <- read_csv("~/clim_geo_disp/data/UNDP_Indices/mpi.csv") #UNDP MPI 1990-2018
load("~/clim_geo_disp/data/Disparity_Countries_with_EEZ.RData") #LCDI for 162 UN member with EEZ

un = subset(df3, outcome == "RCP8.5 2050-2099  ") %>% 
  group_by(unit) %>% 
  summarise(lcdi = mean(mean))

colnames(un)[1] = "Country"

mpi$mpi_mean = rowMeans(mpi[,c(2:30)])

mpi = mpi[,c(1, 31)]

mpi$Country = gsub("United States", "USA", mpi$Country, fixed = T)

disp = merge(un, mpi)

class(disp$mpi_mean)

pdf("~/Desktop/UNDP_CLIM_DISP.pdf", height = 10, width = 10)

disp %>% 
  ggplot(aes(lcdi, mpi_mean, label = Country)) + 
  ggrepel::geom_text_repel(aes(color = Country)) +
  geom_point(aes(color = Country)) +
  stat_smooth(method = "lm", se = F) + 
  # xlim(-max(disp$lcdi), max(disp$lcdi)) +
  # ylim(0,1) + 
  xlab("LCDI") + ylab("UNDP_MPI_1990-2018") +
  ggthemes::theme_few() +
  theme(legend.position = "none") 

dev.off()


# undp = NULL
# 
# for (i in 1990:2018) {
#   
#   # i = 1990
#   y = i-1988
#   
#   df = mpi[,c(1,y)]
#   
#   colnames(df) = c("Country", "MPI")
#   
#   df$Year = i
#   
#   undp = rbind(undp, df)
#   
# }