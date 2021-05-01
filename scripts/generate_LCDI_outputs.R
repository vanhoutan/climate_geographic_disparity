# summarize LCDI for countries_with_EEZ

library(ggplot2)
library(dplyr)
library(sf)
library(forcats)
library(ggpubr)
library(maps)
library(cowplot)
library(readr)
library(ggdark)

rm(list = ls())

dir = Sys.info()[7]

pre_combine = function(rcp, period, variable){
  
  rcp = c("RCP4.5", "RCP8.5")[2]
  period = c("2006-2055", "2050-2099")[2]
  variable = c("anomaly", "historical stdanom", "ensemble stdanom")[1]
  scale = c("scaled", "unscaled")[2]
  
  load(paste0("~/climate_geographic_disparity/outputs/ocean_land/intersection_result_", period, "_", rcp, "_", variable, "_merra2_edgar_ghg.Rdata"))
  
  intersection_biome$Outcome = paste0(rcp, " ", period)
  intersection_realm$Outcome = paste0(rcp, " ", period)
  intersection_land_eez$Outcome = paste0(rcp, " ", period)
  intersection_world$Outcome = paste0(rcp, " ", period)
  intersection_states$Outcome = paste0(rcp, " ", period)
  intersection_anthromes$Outcome = paste0(rcp, " ", period)
  
  #shorten geographical region names
  table(intersection_world$subregion)
  intersection_world$subregion = gsub("South-Eastern", "SE.", intersection_world$subregion, fixed = T)
  intersection_world$subregion = gsub("Northern", "N.", intersection_world$subregion, fixed = T)
  intersection_world$subregion = gsub("Australia and New Zealand", "AU & NZ", intersection_world$subregion, fixed = T)
  intersection_world$subregion = gsub("Southern", "S.", intersection_world$subregion, fixed = T)
  intersection_world$subregion = gsub("Tropical", "Trop.", intersection_world$subregion, fixed = T)
  intersection_world$subregion = gsub("Central", "C.", intersection_world$subregion, fixed = T)
  intersection_world$subregion = gsub("Eastern", "E.", intersection_world$subregion, fixed = T)
  intersection_world$subregion = gsub("Western", "W.", intersection_world$subregion, fixed = T)
  intersection_world$subregion = gsub("South", "S.", intersection_world$subregion, fixed = T)
  intersection_world$subregion = gsub("Mediterranean", "Medit.", intersection_world$subregion, fixed = T)
  
  #shorten realm names
  table(intersection_realm$REALM)
  intersection_realm$REALM = gsub("Northern", "N.", intersection_realm$REALM, fixed = T)
  # intersection_realm$REALM = gsub("Atlantic", "Atl", intersection_realm$REALM, fixed = T)
  intersection_realm$REALM = gsub("Southern", "S.", intersection_realm$REALM, fixed = T)
  intersection_realm$REALM = gsub("Tropical", "Trop.", intersection_realm$REALM, fixed = T)
  intersection_realm$REALM = gsub("Central", "C.", intersection_realm$REALM, fixed = T)
  intersection_realm$REALM = gsub("Eastern", "E.", intersection_realm$REALM, fixed = T)
  intersection_realm$REALM = gsub("Western", "W.", intersection_realm$REALM, fixed = T)
  intersection_realm$REALM = gsub("Temperate", "Temp.", intersection_realm$REALM, fixed = T)
  intersection_realm$REALM = gsub("Mediterranean", "Medit.", intersection_realm$REALM, fixed = T)
  
  
  #shorten biome names (see table s05)
  table(intersection_biome$BIOME)
  intersection_biome$BIOME = gsub("Tropical_and_Subtropical_Moist_Broadleaf_Forests", "Trop Moist Forest", intersection_biome$BIOME, fixed = T)
  intersection_biome$BIOME = gsub("Tropical_and_Subtropical_Dry_Broadleaf_Forests", "Trop Dry Forest", intersection_biome$BIOME, fixed = T)
  intersection_biome$BIOME = gsub("Tropical_and_Subtropical_Coniferous_Forests", "Conif Forest", intersection_biome$BIOME, fixed = T)
  intersection_biome$BIOME = gsub("Tropical_and_Subtropical_Grasslands_Savannas_and_Shrublands", "Trop Grassland", intersection_biome$BIOME, fixed = T)
  intersection_biome$BIOME = gsub("Temperate_Conifer_Forests", "Temp Conif Forest", intersection_biome$BIOME, fixed = T)
  intersection_biome$BIOME = gsub("Temperate_Broadleaf_and_Mixed_Forests", "Temp Decid Forest", intersection_biome$BIOME, fixed = T)
  intersection_biome$BIOME = gsub("Temperate_Grasslands_Savannas_and_Shrublands", "Temp Grassland", intersection_biome$BIOME, fixed = T)
  intersection_biome$BIOME = gsub("Flooded_Grasslands_and_Savannas", "Flooded Grassland", intersection_biome$BIOME, fixed = T)
  intersection_biome$BIOME = gsub("Montane_Grasslands_and_Shrublands", "Mont Grassland", intersection_biome$BIOME, fixed = T)
  intersection_biome$BIOME = gsub("Mediterranean_Forests_Woodlands_and_Scrub", "Med Forest", intersection_biome$BIOME, fixed = T)
  intersection_biome$BIOME = gsub("Deserts_and_Xeric_Shrublands", "Desert", intersection_biome$BIOME, fixed = T)
  intersection_biome$BIOME = gsub("Boreal_Forests_Taiga", "Boreal Forest", intersection_biome$BIOME, fixed = T)
  intersection_biome$BIOME = gsub("Polar_Artic", "Polar Artic", intersection_biome$BIOME, fixed = T)
  intersection_biome$BIOME = gsub("Temperate", "Temp", intersection_biome$BIOME, fixed = T)
  intersection_biome$BIOME = gsub("Tropical", "Trop", intersection_biome$BIOME, fixed = T)
  intersection_biome$BIOME = gsub("Subtropical", "Subtrop.", intersection_biome$BIOME, fixed = T)
  
  
  exclude_list = c("Area en controversia (disputed - Peruvian point of view)", 
                   "Area of overlap Australia/Indonesia", 
                   "Conflict zone China/Japan/Taiwan", 
                   "Conflict zone Japan/Russia",
                   "Conflict zone Japan/South Korea",
                   "Disputed Barbados/Trinidad & Tobago",
                   "Disputed Kenya/Somalia",
                   "Disputed Western Sahara/Mauritania",
                   "Joint development area Australia/East Timor",
                   "Joint regime Colombia/Jamaica",
                   "Joint regime Japan/Korea",
                   "Joint regime Nigeria/Sao Tome and Principe",
                   "Protected zone Australia/Papua New Guinea", 
                   "Spratly Islands", 
                   "Antarctica", 
                   "Gaza Strip")
  
  intersection_land_eez = intersection_land_eez[ ! intersection_land_eez$Country %in% exclude_list, ]
  
  intersection_land_eez$Country = gsub("&", "and", intersection_land_eez$Country)
  intersection_land_eez$Country = gsub(" Is.", " Islands", intersection_land_eez$Country, fixed = T)
  intersection_land_eez$Country = gsub(" I.", " Island", intersection_land_eez$Country, fixed = T)
  intersection_land_eez$Country = gsub("Congo, DRC", "DR Congo", intersection_land_eez$Country, fixed = T)
  intersection_land_eez$Country = gsub("Bonaire, Sint-Eustasius, Saba", "Netherlands", intersection_land_eez$Country, fixed = T)
  intersection_land_eez$Country = gsub("United States ", "US ", intersection_land_eez$Country, fixed = T)
  intersection_land_eez$Country = gsub("US Virgin Islands", "Virgin Islands, US", intersection_land_eez$Country, fixed = T)
  intersection_land_eez$Country = gsub("St. ", "Saint ", intersection_land_eez$Country, fixed = T)
  
  Australia = sov.expand("Australia", regex = F)
  China = sov.expand("China", regex = F); China = gsub("China:", "", China, fixed = T)
  Denmark = sov.expand("Denmark", regex = F)
  Finland = sov.expand("Finland", regex = F); Finland = gsub("Finland:", "", Finland, fixed = T)
  France = sov.expand("France", regex = F)
  Netherlands = sov.expand("Netherlands", regex = F)
  New_Zealand = sov.expand("New Zealand", regex = F)
  Norway = sov.expand("Norway", regex = F); Norway = gsub("Norway:", "", Norway, fixed = T)
  UK = sov.expand("UK", regex = F)
  USA = sov.expand("USA", regex = F)
  
  Australia = c(Australia, "Heard Island and McDonald Islands")
  New_Zealand = c(New_Zealand, "Cook Islands")
  
  UK = c(UK, "South Georgia and the South Sandwich Islands", "United Kingdom", "Saint Helena, Ascension en Tristan da Cunha", "Anguilla")
  USA = c(USA, "United States", "Northern Marinana Islands-Guam")
  
  sov_list = rbind(paste0(Australia, sep = ", ", collapse = ""), 
                   paste0(China, sep = ", ", collapse = ""), 
                   paste0(Denmark, sep = ", ", collapse = ""), 
                   paste0(Finland, sep = ", ", collapse = ""), 
                   paste0(France, sep = ", ", collapse = ""), 
                   paste0(Netherlands, sep = ", ", collapse = ""), 
                   paste0(New_Zealand, sep = ", ", collapse = ""), 
                   paste0(Norway, sep = ", ", collapse = ""), 
                   paste0(UK, sep = ", ", collapse = ""), 
                   paste0(USA, sep = ", ", collapse = ""))
  
  sov_list = as.data.frame(sov_list)
  readr::write_csv(sov_list, "~/climate_geographic_disparity/data/Sov_list.csv")
  
  intersection_land_eez$Country = ifelse(intersection_land_eez$Country %in% Australia, "Australia",  intersection_land_eez$Country)
  intersection_land_eez$Country = ifelse(intersection_land_eez$Country %in% China, "China",  intersection_land_eez$Country)
  intersection_land_eez$Country = ifelse(intersection_land_eez$Country %in% Denmark, "Denmark",  intersection_land_eez$Country)
  intersection_land_eez$Country = ifelse(intersection_land_eez$Country %in% Finland, "Finland",  intersection_land_eez$Country)
  intersection_land_eez$Country = ifelse(intersection_land_eez$Country %in% France, "France",  intersection_land_eez$Country)
  intersection_land_eez$Country = ifelse(intersection_land_eez$Country %in% Netherlands, "Netherlands",  intersection_land_eez$Country)
  intersection_land_eez$Country = ifelse(intersection_land_eez$Country %in% New_Zealand, "New Zealand",  intersection_land_eez$Country)
  intersection_land_eez$Country = ifelse(intersection_land_eez$Country %in% Norway, "Norway",  intersection_land_eez$Country)
  intersection_land_eez$Country = ifelse(intersection_land_eez$Country %in% UK, "UK",  intersection_land_eez$Country)
  intersection_land_eez$Country = ifelse(intersection_land_eez$Country %in% USA, "USA",  intersection_land_eez$Country)
  
  names(table(intersection_land_eez$Country))
  
  if (scale == "scaled") {
    
    intersection_biome$disparity = scale(intersection_biome$disparity)
    intersection_realm$disparity = scale(intersection_realm$disparity)
    intersection_states$disparity = scale(intersection_states$disparity)
    intersection_world$disparity = scale(intersection_world$disparity)
    intersection_land_eez$disparity = scale(intersection_land_eez$disparity)
    earth$disparity = scale(earth$disparity)
    
  }
  
  # #just some convenient category for countries
  # g1 = names(table(subset(intersection_world, continent == "Africa")$geounit))
  # g2 = names(table(subset(intersection_world, continent == "Antarctica")$geounit))
  # g3 = names(table(subset(intersection_world, continent == "Asia")$geounit))
  # g4 = names(table(subset(intersection_world, continent == "Europe")$geounit))
  # g5 = names(table(subset(intersection_world, continent == "North America")$geounit))
  # g6 = names(table(subset(intersection_world, continent == "Oceania")$geounit))
  # g7 = names(table(subset(intersection_world, continent == "South America")$geounit))
  # g8 = names(table(subset(intersection_world, continent == "Seven seas (open ocean)")$geounit))
  # 
  # #just some convenient category for countries
  # g1 = names(table(subset(intersection_world, continent == "Africa")$geounit))
  # g2 = names(table(subset(intersection_world, continent == "Americas")$geounit))
  # g3 = names(table(subset(intersection_world, continent == "Antarctica")$geounit))
  # g4 = names(table(subset(intersection_world, continent == "Asia")$geounit))
  # g5 = names(table(subset(intersection_world, continent == "Europe")$geounit))
  # g6 = names(table(subset(intersection_world, continent == "Oceania")$geounit))
  # g7 = names(table(subset(intersection_world, continent == "Seven seas (open ocean)")$geounit))
  
  #just some convenient category for countries
  g1 = names(table(subset(intersection_world, income_grp == "1. High income: OECD")$geounit))
  g2 = names(table(subset(intersection_world, income_grp == "2. High income: nonOECD")$geounit))
  g3 = names(table(subset(intersection_world, income_grp == "3. Upper middle income")$geounit))
  g4 = names(table(subset(intersection_world, income_grp == "4. Lower middle income")$geounit))
  g5 = names(table(subset(intersection_world, income_grp == "5. Low income")$geounit))
  
  #just some convenient category for states
  s1 = names(table(subset(intersection_states, region == "Northeast")$name))
  s2 = names(table(subset(intersection_states, region == "Midwest")$name))
  s3 = names(table(subset(intersection_states, region == "South")$name))
  s4 = names(table(subset(intersection_states, region == "West")$name))
  
  # alpha <- 0.05
  # 
  # country2 <- intersection_world %>% 
  #   group_by(geounit) %>% 
  #   summarize(mean = mean(disparity, na.rm = T),
  #             lower.ci = mean(disparity, na.rm = T) - qt(1- alpha/2, (n() - 1))*sd(disparity)/sqrt(n()),
  #             upper.ci = mean(disparity, na.rm = T) + qt(1- alpha/2, (n() - 1))*sd(disparity)/sqrt(n()))
  
  #terrestrial biome
  biome <- intersection_biome %>% 
    group_by(BIOME) %>% 
    summarise(median = median(disparity, na.rm = T),
              mean = mean(disparity, na.rm = T),
              q_10 = quantile(disparity, 0.1, na.rm = T),
              q_90 = quantile(disparity, 0.9, na.rm = T),
              sd = sd(disparity, na.rm = T), 
              n = n()) %>%
    mutate(se = sd/sqrt(n),
           lower.ci = median - qt(1 - (0.05 / 2), n - 1) * se,
           upper.ci = median + qt(1 - (0.05 / 2), n - 1) * se)
  
  #marine realm
  realm <- intersection_realm %>% 
    group_by(REALM) %>% 
    summarise(median = median(disparity, na.rm = T),
              mean = mean(disparity, na.rm = T),
              q_10 = quantile(disparity, 0.1, na.rm = T),
              q_90 = quantile(disparity, 0.9, na.rm = T),
              sd = sd(disparity, na.rm = T), 
              n = n()) %>%
    mutate(se = sd/sqrt(n),
           lower.ci = median - qt(1 - (0.05 / 2), n - 1) * se,
           upper.ci = median + qt(1 - (0.05 / 2), n - 1) * se)
  
  # #land vs. sea
  # earth <- earth %>% 
  #   group_by(featurecla) %>% 
  #   summarise(median = median(disparity, na.rm = T),
  #             mean = mean(disparity, na.rm = T),
  #             q_10 = quantile(disparity, 0.1, na.rm = T),
  #             q_90 = quantile(disparity, 0.9, na.rm = T),
  #             sd = sd(disparity, na.rm = T), 
  #             n = n()) %>%
  #   mutate(se = sd/sqrt(n),
  #          lower.ci = median - qt(1 - (0.05 / 2), n - 1) * se,
  #          upper.ci = median + qt(1 - (0.05 / 2), n - 1) * se)
  
  #US states
  states <- intersection_states %>% 
    group_by(name) %>% 
    summarise(median = median(disparity, na.rm = T),
              mean = mean(disparity, na.rm = T),
              q_10 = quantile(disparity, 0.1, na.rm = T),
              q_90 = quantile(disparity, 0.9, na.rm = T),
              sd = sd(disparity, na.rm = T), 
              n = n()) %>%
    mutate(se = sd/sqrt(n),
           lower.ci = median - qt(1 - (0.05 / 2), n - 1) * se,
           upper.ci = median + qt(1 - (0.05 / 2), n - 1) * se)
  
  # global subregions 
  subr <- intersection_world %>% 
    group_by(subregion) %>% 
    summarise(median = median(disparity, na.rm = T),
              mean = mean(disparity, na.rm = T),
              q_10 = quantile(disparity, 0.1, na.rm = T),
              q_90 = quantile(disparity, 0.9, na.rm = T),
              sd = sd(disparity, na.rm = T), 
              n = n()) %>%
    mutate(se = sd/sqrt(n),
           lower.ci = median - qt(1 - (0.05 / 2), n - 1) * se,
           upper.ci = median + qt(1 - (0.05 / 2), n - 1) * se)
  
  # countries
  country <- intersection_world %>% 
    group_by(geounit) %>% 
    summarise(median = median(disparity, na.rm = T),
              mean = mean(disparity, na.rm = T),
              q_10 = quantile(disparity, 0.1, na.rm = T),
              q_90 = quantile(disparity, 0.9, na.rm = T),
              sd = sd(disparity, na.rm = T),
              n = n()) %>%
    mutate(se = sd/sqrt(n),
           lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
           upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)
  
  # countries with EEZ
  eez <- intersection_land_eez %>% 
    group_by(Country) %>% 
    summarise(median = median(disparity, na.rm = T),
              mean = mean(disparity, na.rm = T),
              q_10 = quantile(disparity, 0.1, na.rm = T),
              q_90 = quantile(disparity, 0.9, na.rm = T),
              sd = sd(disparity, na.rm = T), 
              n = n()) %>%
    mutate(se = sd/sqrt(n),
           lower.ci = median - qt(1 - (0.05 / 2), n - 1) * se,
           upper.ci = median + qt(1 - (0.05 / 2), n - 1) * se)
  
  # Anthropogenic biomes
  anthrome <- intersection_anthromes %>% 
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
  
  biome = biome %>% st_set_geometry(NULL) # drop geometry
  realm = realm %>% st_set_geometry(NULL) # drop geometry
  # earth = earth %>% st_set_geometry(NULL) # drop geometry
  states = states %>% st_set_geometry(NULL) # drop geometry
  subr = subr %>% st_set_geometry(NULL) # drop geometry
  country = country %>% st_set_geometry(NULL) # drop geometry
  eez = eez %>% st_set_geometry(NULL) # drop geometry
  anthrome = anthrome %>% st_set_geometry(NULL) # drop geometry
  
  colnames(biome) = c("unit", "median", "mean", "q_10", "q_90", "sd", "n", "se", "lower.ci", "upper.ci"); biome$type = "Land"
  colnames(realm) = c("unit", "median", "mean", "q_10", "q_90", "sd", "n", "se", "lower.ci", "upper.ci"); realm$type = "Ocean"
  colnames(subr) = c("unit", "median", "mean", "q_10", "q_90", "sd", "n", "se", "lower.ci", "upper.ci"); subr$type = "Global_Subregions"
  colnames(country) = c("unit", "median", "mean", "q_10", "q_90", "sd", "n", "se", "lower.ci", "upper.ci"); country$type = "Countries_without_EEZ"
  colnames(eez) = c("unit", "median", "mean", "q_10", "q_90", "sd", "n", "se", "lower.ci", "upper.ci"); eez$type = "Countries_with_EEZ"
  colnames(states) = c("unit", "median", "mean", "q_10", "q_90", "sd", "n", "se", "lower.ci", "upper.ci"); states$type = "US_States"
  # colnames(earth) = c("unit", "median", "mean", "q_10", "q_90", "sd", "n", "se", "lower.ci", "upper.ci"); earth$type = "Land_Sea"
  colnames(anthrome) = c("unit", "median", "mean", "q_10", "q_90", "sd", "n", "se", "lower.ci", "upper.ci"); anthrome$type = "Anthromes"
  
  df = rbind(biome, realm, states, subr, country, eez, 
             # earth, 
             anthrome)
  rm(biome, realm, 
     # earth, 
     states, subr, country, eez, anthrome)
  
  intersection_biome =    intersection_biome[,c("disparity")]; intersection_biome$type = "Land"
  intersection_realm =    intersection_realm[,c("disparity")]; intersection_realm$type = "Ocean"
  intersection_subr =     intersection_world[,c("disparity")]; intersection_subr$type = "Global_Subregions"
  intersection_world =    intersection_world[,c("disparity")]; intersection_world$type = "Countries_without_EEZ"
  intersection_land_eez = intersection_land_eez[,c("disparity")]; intersection_land_eez$type = "Countries_with_EEZ"
  intersection_states =   intersection_states[,c("disparity")]; intersection_states$type = "US_States"
  # intersection_earth = rbind(intersection_biome[,c("disparity")], intersection_realm[,c("disparity")]); intersection_earth$type = "Land_Sea"
  intersection_anthromes = intersection_anthromes[,c("disparity")]; intersection_anthromes$type = "Anthropogenic_Biomes"
  
  baseline = rbind(intersection_biome, intersection_realm, intersection_subr, intersection_world, intersection_land_eez, intersection_states, intersection_anthromes)
  rm(intersection_biome, intersection_realm, intersection_subr, intersection_world, intersection_land_eez, intersection_states, intersection_anthromes)
  
  df = df[!(df$unit == "Seven seas (open ocean)"),]
  
  df$unit = gsub("Tropical_and_Subtropical", "Trop_Subtrop", df$unit, fixed = T)
  df$unit = gsub("Mediterranean_", "Medrn_", df$unit, fixed = T)
  df$unit = gsub("Northern ", "N. ", df$unit, fixed = T)
  df$unit = gsub("Southern ", "S. ", df$unit, fixed = T)
  df$unit = gsub("Eastern ", "E. ", df$unit, fixed = T)
  df$unit = gsub("Western ", "W. ", df$unit, fixed = T)
  
  df$outcome = paste(rcp, period, " ") 
  
  return(df)
  
}

variable = c("anomaly", "historical stdanom", "ensemble stdanom")[2]

# https://psl.noaa.gov/ipcc/ocn/overview.html

# anomaly: the difference in the mean climate in the future time period (RCP8.5 or RCP4.5) compared to the historical reference period. 

# historical stdanom: the difference in the mean climate in the future time period (RCP8.5 or RCP4.5) compared to the historical reference period, but the anomalies are standardized (normalized) by the de-trended inter-annual standard deviation in the historical period (average of all the models or single model).

# ensemble stdanom: Standard Anomaly (ensemble spread): the anomalies are standardized by the standard deviation of the mean climate (for the historical period) of all the models. 

rcp45_mid = pre_combine("RCP4.5", "2006-2055", variable)
rcp45_end = pre_combine("RCP4.5", "2050-2099", variable)
rcp85_mid = pre_combine("RCP8.5", "2006-2055", variable)
rcp85_end = pre_combine("RCP8.5", "2050-2099", variable)

df = rbind(rcp45_mid, rcp45_end, rcp85_mid, rcp85_end)
df = subset(df, type == "Countries_with_EEZ")
df$type = ifelse(df$type %in% c("Countries_with_EEZ"), "Nation_states", df$type) 

variable = gsub(" ", "_", variable)

save(df, file = paste0("~/climate_geographic_disparity/outputs/LCDI_Nation_States_with_EEZ_", variable, ".RData")) #LCDIs for 192 UN members with EEZ

