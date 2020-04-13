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

data = c("_merra2_edgar_ghg", "_merra2_edgar_co2", "_merra2_odiac")[1]

pre_combine = function(rcp, period, variable){
  
  # rcp = c("RCP4.5", "RCP8.5")[2]
  # period = c("2006-2055", "2050-2099")[2]
  # variable = c("anomaly", "historical stdanom", "ensemble stdanom")[1]
  scale = c("scaled", "unscaled")[2]
  
  load(paste0("~/clim_geo_disp/output/intersection_result_", period, "_", rcp, "_", variable, data, ".Rdata"))
  
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
  readr::write_csv(sov_list, "/Users/ktanaka/Desktop/Sov_list.csv")
  
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

rcp45_mid = pre_combine("RCP4.5", "2006-2055", "anomaly")
rcp45_end = pre_combine("RCP4.5", "2050-2099", "anomaly")
rcp85_mid = pre_combine("RCP8.5", "2006-2055", "anomaly")
rcp85_end = pre_combine("RCP8.5", "2050-2099", "anomaly")

plot_ranking_tobether = function(h, w, col_size, segment_size, font_size, var){
  
  segment_size = 1
  col_size = 4
  font_size = 10
  var = "q_10"
  
  df = rbind(rcp45_mid, rcp45_end, rcp85_mid, rcp85_end)
  df = subset(df, type != "Countries_without_EEZ")
  df$type = ifelse(df$type %in% c("Land", "Ocean"), "Ecoregions", df$type)
  df$type = ifelse(df$type %in% c("Global_Subregions"), "Political_regions", df$type)
  df$type = ifelse(df$type %in% c("Countries_with_EEZ"), "Nation_states", df$type) # choose between "Countries_without_EEZ" or "Countries_with_EEZ"
  
  df1 = subset(df, type == "Ecoregions")
  df2 = subset(df, type == "Political_regions")
  df3 = subset(df, type == "Nation_states")
  df4 = subset(df, type == "US_States")
  df5 = subset(df, type == "Anthromes")
  
  df1 = aggregate(df1[, 2:5], list(df1$unit), mean)
  df2 = aggregate(df2[, 2:5], list(df2$unit), mean)
  df3 = aggregate(df3[, 2:5], list(df3$unit), mean)
  df4 = aggregate(df4[, 2:5], list(df4$unit), mean)
  df5 = aggregate(df5[, 2:5], list(df5$unit), mean)
  
  colnames(df1)[1] = "unit"
  colnames(df2)[1] = "unit"
  colnames(df3)[1] = "unit"
  colnames(df4)[1] = "unit"
  colnames(df5)[1] = "unit"
  
  df1$category = "Ecoregions"
  df2$category = "Political_regions"
  df3$category = "Nation_states"
  df4$category = "US_states"
  df5$category = "Anthropogenic_biome"
  
  if (var == "median") {
    
    df1$Disparity = df1$median
    df2$Disparity = df2$median
    df3$Disparity = df3$median
    df4$Disparity = df4$median
    df5$Disparity = df5$median

  } else {
    
    df1$Disparity = df1$q_10
    df2$Disparity = df2$q_10
    df3$Disparity = df3$q_10
    df4$Disparity = df4$q_10
    df5$Disparity = df5$q_10
    
  }
  
  Ecoregion_median =         median(df1$Disparity)
  Political_regions_median = median(df2$Disparity)
  Nation_states_median =     median(df3$Disparity)
  US_states_median =         median(df4$Disparity)
  Anthromes_median =         median(df5$Disparity)
  
  top = df1 %>% top_n(8, Disparity)
  bottom = df1 %>% top_n(-8, Disparity)
  df1 = tbl_df(bind_rows(top, bottom))
  
  top = df2 %>% top_n(8, Disparity)
  bottom = df2 %>% top_n(-8, Disparity)
  df2 = tbl_df(bind_rows(top, bottom))
  
  top = df3 %>% top_n(25, Disparity)
  bottom = df3 %>% top_n(-25, Disparity)
  df3 = tbl_df(bind_rows(top, bottom))
  
  top = df4 %>% top_n(25, Disparity)
  bottom = df4 %>% top_n(-25, Disparity)
  df4 = tbl_df(bind_rows(top, bottom))
  
  top = df5 %>% top_n(9, Disparity)
  bottom = df5 %>% top_n(-9, Disparity)
  df5 = tbl_df(bind_rows(top, bottom))
  
  scientific <- function(x){
    
    library(ggplot2)
    library(scales)
    library(ggthemes)
    
    ifelse(x == 0, "0", parse(text = gsub("[+]", "", gsub("e", " %*% 10^", scientific_format()(x)))))
  
  }
  
  df = rbind(df1, df2, df3, df4, df5)
  
  disparity_limits = c(-max(abs(df$Disparity)), max(abs(df$Disparity)))
  # disparity_limits = c(min(df$Disparity), max(df$Disparity))

  category_list = c("Ecoregions", "Political_regions", "Nation_states", "US_states", "Anthropogenic_biome")
  
  plot_list = list()
  
  for (i in 1:length(category_list)) {
    
    if (category_list[i] == "Ecoregions") m = Ecoregion_median
    if (category_list[i] == "Political_regions") m = Political_regions_median
    if (category_list[i] == "Nation_states") m = Nation_states_median
    if (category_list[i] == "US_states") m = US_states_median
    if (category_list[i] == "Anthropogenic_biome") m = Anthromes_median
    
    p = subset(df, category == category_list[i]) %>% 
      mutate(unit = fct_reorder(unit, Disparity)) %>% 
      ggplot() +
      geom_segment(aes(
        color = Disparity, 
        x = unit, 
        xend = unit,
        y = q_10, 
        yend = q_90),
        size = segment_size) +
      geom_point(aes(
        color = Disparity,
        x = unit,
        y = median),
        size = col_size, 
        shape = 20) +
      coord_flip() +
      geom_hline(yintercept = m, 
                 # linetype = "dashed", 
                 color = "lightgrey", 
                 size = 1) + 
      scale_colour_gradientn(
        colours = c("cyan", 
                    "black",
                    "red"),
        # values = scales::rescale(c(-0.5, -0.01, 0.0, 0.01, 0.5)),
        values = scales::rescale(c(-0.5, -0.04, 0.1, 0.2, 0.5)),
        limits = disparity_limits,
        name = "LCDI") + 
      xlab("") +
      ylab("") +
      theme_pubr() + 
      scale_y_continuous(breaks = pretty(disparity_limits), 
                         limits = range(pretty(disparity_limits))*1.1) + 
      theme(
        strip.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        # strip.background = element_blank(),
        # title = element_text(colour = "white"),
        # axis.text = element_text(color = "white"),
        # axis.title = element_text(color = "white"),
        # axis.line = element_line(color = "white"),
        # legend.text = element_text(color = "white", size = 20),
        # panel.background = element_rect(fill = "gray20"), # bg of the panel
        # plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        # panel.grid.major = element_blank(), # get rid of major grid
        # panel.grid.minor = element_blank(), # get rid of minor grid
        # legend.background = element_rect(fill = "transparent"), # get rid of legend bg
        # legend.box.background = element_rect(fill = "transparent"), # get rid of legend panel bg
        text = element_text(size = font_size))
    # annotate("text",
    #          x = -Inf,
    #          y = Inf,
    #          hjust = 1,
    #          vjust = -0.2,
    #          label =  "\n Future period = 2006-2055 & 2050-2099 \n Experiment = rcp 4.5 & 8.5")
    
    
    
    if (i %in% c(3,4)) {
      
      pdf(paste0("/Users/ktanaka/Desktop/Figure_3_", data, "_", var, "_", category_list[i], ".pdf"), height = 8, width = 6)
      p = p + theme(legend.position = "right")
      print(p)
      dev.off()    
      
    } else {
      
      pdf(paste0("/Users/ktanaka/Desktop/Figure_3_", data, "_", var, "_", category_list[i], ".pdf"), height = 4, width = 6)
      p = p + theme(legend.position = "right")
      print(p)
      dev.off()      
    }
    
    

    
    if (i == 4) {
      
      p = p + theme(legend.position = c(0.9, 0.1))
      
    } else {
      
      p = p + theme(legend.position = "none")
      
    }
    
    plot_list[[i]] = p
    
  }
  
  pdf(paste0("/Users/ktanaka/Desktop/Figure_3_", data, "_", var, ".pdf"), h = h, w = w)
  
  # p = ggdraw() +
  #   draw_plot(plot_list[[3]], x = 0, y = 0, width = 0.3, height = 0.98) +
  #   draw_plot(plot_list[[2]], x = 0.32, y = 0, width = 0.3, height = 0.44) +
  #   draw_plot(plot_list[[1]], x = 0.32, y = .43, width = 0.3, height = 0.55) +
  #   draw_plot(plot_list[[4]], x = 0.65, y = 0, width = 0.34, height = 0.98) +
  #   draw_plot_label(label = c("a", "b", "c", "d"), size = 40,
  #                   x = c(0, 0.32, 0.32, 0.65), y = c(1, 1, 0.44, 1))  
  
  p = ggdraw() +
    draw_plot(plot_list[[3]], x = 0,      y = 0,   width = 0.333, height = 1) +
    draw_plot(plot_list[[2]], x = 0.333,  y = 0.69, width = 0.333, height = 0.31) +
    draw_plot(plot_list[[1]], x = 0.333,  y = 0.38, width = 0.333, height = 0.31) +
    draw_plot(plot_list[[5]], x = 0.333,  y = 0,   width = 0.333, height = 0.38) +
    draw_plot(plot_list[[4]], x = 0.666,  y = 0,   width = 0.333, height = 1) + 
    draw_plot_label(label = c("a", "b", "c", "d", "e"), size = 40,
                    x = c(0, 0.32, 0.32, 0.32, 0.65), y = c(1, 1, 0.7, 0.4, 1))  
  
  print(p)
  
  dev.off()
  
}

plot_ranking_tobether(12, 20, 4, 2, 18, "median")
plot_ranking_tobether(16, 20, 4, 2, 18, "q_10")
plot_ranking_tobether(16, 20, 4, 2, 18, "q_10")


plot_ranking = function(var, h, w, col_size, segment_size, font_size, n, stat) {
  
  segment_size = 1
  col_size = 2
  n = 5
  font_size = 10
  stat = "q_10"
  var = "Nation_states"
  
  df = rbind(rcp45_mid, rcp45_end, rcp85_mid, rcp85_end)
  df = subset(df, type != "Countries_without_EEZ")
  df$type = ifelse(df$type %in% c("Land", "Ocean"), "Ecoregions", df$type)
  df$type = ifelse(df$type %in% c("Global_Subregions"), "Political_regions", df$type)
  df$type = ifelse(df$type %in% c("Countries_with_EEZ"), "Nation_states", df$type) # choose between "Countries_without_EEZ" or "Countries_with_EEZ"

  df1 = subset(df, type == "Ecoregions")
  df2 = subset(df, type == "Political_regions")
  df3 = subset(df, type == "Nation_states")
  df4 = subset(df, type == "US_States")
  
  t1 = df1
  t2 = df2
  t3 = df3
  t4 = df4
  
  colnames(t1) = c("Unit", "Median", "Mean", "10th quantile", "90th quantile", "SD", "n", "SE", "lower.CI", "upper.CI", "type", "Scenario")
  colnames(t2) = c("Unit", "Median", "Mean", "10th quantile", "90th quantile", "SD", "n", "SE", "lower.CI", "upper.CI", "type", "Scenario")
  colnames(t3) = c("Unit", "Median", "Mean", "10th quantile", "90th quantile", "SD", "n", "SE", "lower.CI", "upper.CI", "type", "Scenario")
  colnames(t4) = c("Unit", "Median", "Mean", "10th quantile", "90th quantile", "SD", "n", "SE", "lower.CI", "upper.CI", "type", "Scenario")
  
  df1 = aggregate(df1[, 2:5], list(df1$unit), mean)
  df2 = aggregate(df2[, 2:5], list(df2$unit), mean)
  df3 = aggregate(df3[, 2:5], list(df3$unit), mean)
  df4 = aggregate(df4[, 2:5], list(df4$unit), mean)
  
  colnames(df1)[1] = "unit"
  colnames(df2)[1] = "unit"
  colnames(df3)[1] = "unit"
  colnames(df4)[1] = "unit"
  
  df1$category = "Ecoregions"
  df2$category = "Political_regions"
  df3$category = "Nation_states"
  df4$category = "US_States"
  
  df = rbind(df1, df2, df3, df4)
  
  if (stat == "median") {
    df$Disparity = df$median #rank by median
  } else {
    df$Disparity = df$q_10 #rank by 10th quantile
  }
  
  disparity_limits = c(-max(abs(df$Disparity)), max(abs(df$Disparity)))
  
  df = subset(df, category %in% var)
  
  # if (var %in% c("Nation_states", "US_states")) {
  # 
  #   top = df %>% top_n(n, Disparity) #25 or 10
  #   bottom = df %>% top_n(-n, Disparity)
  #   df = tbl_df(bind_rows(top, bottom))
  # 
  # }
  # 
  # if (var %in% c("Nation_states", "US_states")) {
  # 
  #   top = df %>% top_n(n, Disparity) #25 or 10
  #   bottom = df %>% top_n(-n, Disparity)
  #   df = tbl_df(bind_rows(top, bottom))
  # 
  # }
  if (var %in% c("Nation_states", "US_States")) {
    
    df <- within(df, group <- as.integer(cut(Disparity*-1, quantile(Disparity*-1, probs=0:6/6), include.lowest = TRUE)))

  }
  
  p = subset(df) %>% 
    mutate(unit = fct_reorder(unit, Disparity)) %>% 
    ggplot() +
    geom_hline(yintercept = median(df$Disparity),
               # linetype = "dashed",
               color = "darkgray",
               size = 0.5) +
    geom_segment(aes(
      color = Disparity, 
      x = unit, 
      xend = unit,
      y = q_10, 
      yend = q_90),
      size = segment_size) +
    # geom_text(aes(unit, q_10, label = unit), hjust = 0, vjust = -0.8, size = font_size) +
    geom_point(aes(
      color = Disparity,
      x = unit,
      y = median),
      size = col_size) +
    # ylim(c(-max(abs(df$median)), max(abs(df$median)))*1.5) +
    # ylim(disparity_limits*1.5) +
    coord_flip() +
    scale_colour_gradientn(
      colours = c("cyan", 
                  "black",
                  "red"),
      values = scales::rescale(c(-0.5, -0.04, 0.1, 0.2, 0.5)),
      # values = scales::rescale(c(-0.5, -0.11, 0, 0.11, 0.5)),
      limits = disparity_limits,
      name = "LCDI") + 
    xlab("") +
    ylab("") +
    theme_pubr() +
    # dark_theme_classic() +
    theme(
      legend.position = c(0.01,1),
      legend.justification = c(0,1),
      # legend.position = "right",
      # title = element_text(colour = "white"),
      # axis.text = element_text(color = "white"),
      # axis.title = element_text(color = "white"),
      # axis.line = element_line(color = "white"),
      # axis.title.y = element_blank(),
      # axis.text.y = element_blank(),
      # axis.ticks.y = element_blank(),
      # axis.text.x = element_text(size = 20),
      # legend.text = element_text(color = "white", size = 20),
      # panel.background = element_rect(fill = "gray20"), # bg of the panel
      # plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
      # panel.grid.major = element_blank(), # get rid of major grid
      # panel.grid.minor = element_blank(), # get rid of minor grid
      # legend.background = element_rect(fill = "transparent"), # get rid of legend bg
      # legend.box.background = element_rect(fill = "transparent"), # get rid of legend panel bg
      text = element_text(size = font_size)) 
  
  # theme(
  #   # axis.text.y = element_blank(),
  #   axis.ticks.y = element_blank(), 
  #   legend.justification = c(-0.1, 1), 
  #   legend.position = c(0, 1)) 
  
  # ggtitle("\n Period = 2006-2055 & 2050-2099 \n Experiment = rcp 4.5 & 8.5")
  
  # annotate("text",
  #          x = -Inf,
  #          y = Inf,
  #          hjust = 1,
  #          vjust = -0.5,
  #          label =  "\nPeriod = 2006-2055 & 2050-2099\nExperiment = rcp 4.5 & 8.5")
  
  if (var %in% c("Nation_states")) {
    
    p = p + facet_wrap(~group, scales = "free_y", ncol = 2, dir = "v")
    
  }
  
  print(p)
  
  pdf(paste0("/Users/ktanaka/Desktop/Rank_", var, data, ".pdf"), height = h, width = w)
  print(p)
  dev.off()
  
  # write_csv(t1, paste0("/Users/ktanaka/Desktop/Ecoretions_summary.csv"))
  # write_csv(t2, paste0("/Users/ktanaka/Desktop/Political_regions_summary.csv"))
  # write_csv(t3, paste0("/Users/ktanaka/Desktop/Nation_states_summary.csv"))
  # write_csv(t4, paste0("/Users/ktanaka/Desktop/US_states_summary.csv"))

}

plot_ranking(var = "Ecoregions",        h = 5, w = 6, col_size = 3, segment_size = 1, font_size = 8, n = 5, stat = "q_10")
plot_ranking(var = "Political_regions", h = 4, w = 6, col_size = 3, segment_size = 1, font_size = 8, n = 5, stat = "q_10")
plot_ranking(var = "Nation_states",     h = 5, w = 6, col_size = 3, segment_size = 1, font_size = 8, n = 5, stat = "q_10")
plot_ranking(var = "Nation_states",     h = 12, w = 10, col_size = 3, segment_size = 1, font_size = 10, n = 5, stat = "q_10")
plot_ranking(var = "US_states",         h = 8, w = 6, col_size = 3, segment_size = 1, font_size = 8, n = 5, stat = "q_10")