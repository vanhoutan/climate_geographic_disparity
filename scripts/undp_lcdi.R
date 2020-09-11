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
mpi$Country = gsub("Bahamas", "The Bahamas", mpi$Country, fixed = T)
mpi$Country = gsub("Bolivia (Plurinational State of)", "Bolivia", mpi$Country, fixed = T)
mpi$Country = gsub("Brunei Darussalam", "Brunei", mpi$Country, fixed = T)
mpi$Country = gsub("Congo (Democratic Republic of the)", "DR Congo", mpi$Country, fixed = T)
mpi$Country = gsub("C√¥te d'Ivoire", "Cote d'Ivoire", mpi$Country, fixed = T)
mpi$Country = gsub("Czechia", "Czech Republic", mpi$Country, fixed = T)
mpi$Country = gsub("Eswatini (Kingdom of)", "Eswatini", mpi$Country, fixed = T)
mpi$Country = gsub("Gambia", "The Gambia", mpi$Country, fixed = T)
mpi$Country = gsub("Hong Kong, China (SAR)", "Hong Kong", mpi$Country, fixed = T)
mpi$Country = gsub("Iran (Islamic Republic of)", "Iran", mpi$Country, fixed = T)
mpi$Country = gsub("Korea (Republic of)", "South Korea", mpi$Country, fixed = T)
mpi$Country = gsub("Lao People's Democratic Republic", "Laos", mpi$Country, fixed = T)
mpi$Country = gsub("North Macedonia", "Macedonia", mpi$Country, fixed = T)
mpi$Country = gsub("Micronesia (Federated States of)", "Micronesia", mpi$Country, fixed = T)
mpi$Country = gsub("Micronesia", "Moldova", mpi$Country, fixed = T)
mpi$Country = gsub("Russian Federation", "The", mpi$Country, fixed = T)
mpi$Country = gsub("Syrian Arab Republic", "Syria", mpi$Country, fixed = T)
mpi$Country = gsub("Tanzania (United Republic of)", "Tanzania", mpi$Country, fixed = T)
mpi$Country = gsub("Venezuela (Bolivarian Republic of)", "Venezuela", mpi$Country, fixed = T)

kyle_fix = read_csv("~/clim_geo_disp/data/MPI_LCDI_KV_10Sep.csv")
kyle_fix = kyle_fix$COUNTRY
mpi = mpi %>% subset(Country %in% kyle_fix)

# kyle_fix = data_frame(Country = kyle_fix$UNDP_Nations, kyle_fix = 1)
# View(merge(mpi, kyle_fix, all = T))

disp = merge(un, mpi)

class(disp$mpi_mean)

region = read_csv("~/clim_geo_disp/data/country_region.csv")
region = data.frame(Country = region$name,
                    # Region = region$subregion
                    Region = region$continent
)

disp = merge(disp, region)

pdf("~/Desktop/UNDP_CLIM_DISP.pdf", height = 10, width = 10)

disp %>% 
  ggplot(aes(lcdi, mpi_mean, label = Country)) + 
  ggrepel::geom_text_repel(aes(color = Region), show.legend = F) +
  geom_point(aes(color = Region)) +
  stat_smooth(method = "lm", se = F) + 
  # xlim(-max(disp$lcdi), max(disp$lcdi)) +
  # ylim(0,1) + 
  # scale_color_viridis_d() + 
  xlab("LCDI") + ylab("UNDP_MPI_1990-2018") +
  ggthemes::theme_few() +
  theme(legend.position = "right") + 
  guides(color = guide_legend(ncol = 1))

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