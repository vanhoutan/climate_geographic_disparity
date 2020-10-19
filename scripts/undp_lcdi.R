library(dplyr)
library(readr)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(rgeos)
library(sf)
library(rnaturalearth)
library(RColorBrewer)

rm(list = ls())

###########################
### import LCDI results ###
###########################
load("~/clim_geo_disp/data/Disparity_Countries_with_EEZ.RData") #LCDIs for 192 UN members with EEZ

lcdi_scenario = c("RCP4.5 2006-2055  ", "RCP4.5 2050-2099  ", "RCP8.5 2006-2055  ", "RCP8.5 2050-2099  ")[4] ##change this number for different scenario

lcdi = df3 %>% 
  subset(outcome %in% lcdi_scenario) %>% #select LCDI scenario
  group_by(unit) %>% 
  summarise(lcdi = mean(mean))
rm(df3)
colnames(lcdi)[1] = "Country"

#######################
### import UNDP-MPI ###
#######################

## MPI = multi-dimensional poverty index
## If change the MPI source file for 2019 or 2020, then change this link
## this list is the INCOME INDEX only for each country 
## it is used in the MPI calculation, but it is only one factor of the MPI
undp <- read_csv("~/clim_geo_disp/data/UNDP_Indices/mpi.csv") #UNDP mpi 1990-2018
## update the list length to 32 if bring in 2020 data, this is 1990-2018
undp$undp_mean = rowMeans(undp[,c(2:30)])
undp = undp[,c(1, 31)]

#reconcile country names
undp$Country = gsub("United States", "USA", undp$Country, fixed = T)
undp$Country = gsub("United Kingdom", "UK", undp$Country, fixed = T)
undp$Country = gsub("Bahamas", "The Bahamas", undp$Country, fixed = T)
undp$Country = gsub("Bolivia (Plurinational State of)", "Bolivia", undp$Country, fixed = T)
undp$Country = gsub("Brunei Darussalam", "Brunei", undp$Country, fixed = T)
undp$Country = gsub("Congo (Democratic Republic of the)", "DR Congo", undp$Country, fixed = T)
undp$Country = gsub("C√¥te d'Ivoire", "Cote d'Ivoire", undp$Country, fixed = T)
undp$Country = gsub("Côte d'Ivoire", "Cote d'Ivoire", undp$Country, fixed = T)
undp$Country = gsub("Czechia", "Czech Republic", undp$Country, fixed = T)
undp$Country = gsub("Eswatini (Kingdom of)", "Eswatini", undp$Country, fixed = T)
undp$Country = gsub("Gambia", "The Gambia", undp$Country, fixed = T)
undp$Country = gsub("Hong Kong, China (SAR)", "Hong Kong", undp$Country, fixed = T)
undp$Country = gsub("Iran (Islamic Republic of)", "Iran", undp$Country, fixed = T)
undp$Country = gsub("Korea (Republic of)", "South Korea", undp$Country, fixed = T)
undp$Country = gsub("Lao People's Democratic Republic", "Laos", undp$Country, fixed = T)
undp$Country = gsub("North Macedonia", "Macedonia", undp$Country, fixed = T)
undp$Country = gsub("Micronesia (Federated States of)", "Micronesia", undp$Country, fixed = T)
undp$Country = gsub("Micronesia", "Moldova", undp$Country, fixed = T)
undp$Country = gsub("Russian Federation", "The", undp$Country, fixed = T)
undp$Country = gsub("Syrian Arab Republic", "Syria", undp$Country, fixed = T)
undp$Country = gsub("Tanzania (United Republic of)", "Tanzania", undp$Country, fixed = T)
undp$Country = gsub("Venezuela (Bolivarian Republic of)", "Venezuela", undp$Country, fixed = T)

#import list of reconciled country names done by KVH
kyle_fix = read_csv("~/clim_geo_disp/data/mpi_LCDI_KV_10Sep.csv")
kyle_fix = kyle_fix$COUNTRY
undp = undp %>% subset(Country %in% kyle_fix)
undp_lcdi = merge(lcdi, undp); rm(lcdi, undp, kyle_fix)

###########################################
### import continent and subregion data ###
###########################################
region = read_csv("~/clim_geo_disp/data/country_region.csv")
region = data.frame(Country = region$name,
                    # Region = region$subregion
                    Region = region$continent
)

#reconcile country names
region$Country = gsub("United States of America", "USA", region$Country, fixed = T)
region$Country = gsub("United Kingdom", "UK", region$Country, fixed = T)
region$Country = gsub("Bosnia and Herz.", "Bosnia and Herzegovina", region$Country, fixed = T)
region$Country = gsub("Central African Rep.", "Central African Republic", region$Country, fixed = T)
region$Country = gsub("Côte d'Ivoire", "Cote d'Ivoire", region$Country, fixed = T)
region$Country = gsub("Dem. Rep. Congo", "DR Congo", region$Country, fixed = T)
region$Country = gsub("Dominican Rep.", "Dominican Republic", region$Country, fixed = T)
region$Country = gsub("Eq. Guinea", "Equatorial Guinea", region$Country, fixed = T)
region$Region = gsub("Seven seas (open ocean)", "Africa", region$Region, fixed = T)

############################
### merge into single df ###
############################
undp_lcdi_region = merge(undp_lcdi, region)

#########################################
### filter labels based on MPI values ###
#########################################
mpi_hi = undp_lcdi_region %>% top_n(8, undp_mean) # top number
mpi_lw = undp_lcdi_region %>% top_n(-8, undp_mean) # bottom number
mpi = rbind(mpi_hi, mpi_lw); mpi

## pdf(paste0("~/Desktop/UNDP_LCDI_", Sys.Date(), ".pdf"), height = 10, width = 10)

undp_lcdi_region %>%
  ggplot(aes(lcdi, undp_mean, label = Country)) + 
  geom_point(aes(color = Region)) +
  
  # label every country
  # ggrepel::geom_text_repel(aes(color = Region), show.legend = F) +
  
  # # label outlier countries + alpha
  # stat_dens2d_filter(geom = "text_repel", aes(color = Region), keep.fraction = 0.1) + 
  # ggrepel::geom_text_repel(data = subset(undp_lcdi_region, Country %in% c("USA", "China")), aes(color = Region), show.legend = F) +
  
  # label mpi outlier countries
  ggrepel::geom_text_repel(data = mpi, aes(color = Region), show.legend = F) +
  ggrepel::geom_text_repel(data = subset(undp_lcdi_region, Country %in% c("USA", "China", "Belgium")), aes(color = Region), show.legend = F) +
  
  stat_smooth(method = "lm", se = F, color = "gray") + 
  coord_fixed(ratio = 10) + 
  scale_color_manual(values = c("#CC004C", "#FCB711", "#0DB14B", "#0089D0", "#6460AA", "#F37021")) + # NBC logo colors
  xlab(paste0("Local Climate Disparity Index ", lcdi_scenario)) + ylab("UNDP multidimensional poverty index (1990-2018)") +
  ggthemes::theme_few() +
  theme(legend.position = "right") + 
  guides(color = guide_legend(ncol = 1))

dev.off()

#####################
### plot polygons ###
#####################
shape <- ne_countries(scale = "small", returnclass = "sf") #worldwide country polygon
shape <- shape[57] %>% st_as_sf()  
shape <- shape %>% filter(!subregion %in% c("Seven seas (open ocean)",""))
shape <- shape %>% filter(!subregion %in% c("Antarctica",""))

pdf("~/Desktop/unit-subregions.pdf", width = 10, height = 8)
shape %>% ggplot() + 
  geom_sf(aes(group = subregion, fill = subregion), color = "NA", show.legend = T) + 
  theme_void() +
  # guides(fill = guide_legend(nrow = 5), "") + 
  theme(legend.position = "none")
dev.off()

shape <- ne_countries(scale = "small", returnclass = "sf") #worldwide country polygon
shape <- shape[55] %>% st_as_sf()  
shape <- shape %>% filter(!continent %in% c("Seven seas (open ocean)",""))
shape <- shape %>% filter(!continent %in% c("Antarctica",""))

pdf("~/Desktop/unit-continents.pdf", width = 10, height = 8)
shape %>% ggplot() + 
  geom_sf(aes(group = continent, fill = continent), color = "NA", show.legend = T) + 
  theme_void() +
#  scale_fill_discrete("") + 
  scale_fill_manual(values = c("#CC004C", "#FCB711", "#0DB14B", "#0089D0", "#6460AA", "#F37021")) + 
  guides(fill = guide_legend(nrow = 1), "") + 
  theme(legend.position = "top")
dev.off()

# undp = NULL
#
# for (i in 1990:2018) {
#
#   # i = 1990
#   y = i-1988
#
#   df = undp[,c(1,y)]
#
#   colnames(df) = c("Country", "undp")
#
#   df$Year = i
#
#   undp = rbind(undp, df)
#
# }