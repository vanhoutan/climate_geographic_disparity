library(ggplot2)
library(dplyr)
library(sf)
library(forcats)
library(ggpubr)
library(maps)

rm(list = ls())

data = c("_merra2_edgar_ghg", "_merra2_edgar_co2", "_merra2_odiac")[1]

pre_combine = function(rcp, period){
  
  # rcp = c("RCP4.5", "RCP8.5")[1]
  # period = c("2006-2055", "2050-2099")[1]
  scale = c("scaled", "unscaled")[2]
  
  load(paste0("~/clim_geo_disp/output/intersection_result_", period, "_", rcp, data, ".Rdata"))
  
  intersection_biome$Outcome = paste0(rcp, " ", period)
  intersection_realm$Outcome = paste0(rcp, " ", period)
  intersection_land_eez$Outcome = paste0(rcp, " ", period)
  intersection_world$Outcome = paste0(rcp, " ", period)
  intersection_states$Outcome = paste0(rcp, " ", period)
  
  #shorten geographical region names
  table(intersection_world$subregion)
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
  
  
  #shorten biome names, see https://doi.org/10.1016/j.rse.2012.08.013
  table(intersection_biome$BIOME)
  intersection_biome$BIOME = gsub("Tropical_and_Subtropical_Moist_Broadleaf_Forests", "Moist forests", intersection_biome$BIOME, fixed = T)
  intersection_biome$BIOME = gsub("Tropical_and_Subtropical_Dry_Broadleaf_Forests", "Dry forests", intersection_biome$BIOME, fixed = T)
  intersection_biome$BIOME = gsub("Tropical_and_Subtropical_Coniferous_Forests", "Conifer forests", intersection_biome$BIOME, fixed = T)
  intersection_biome$BIOME = gsub("Tropical_and_Subtropical_Grasslands_Savannas_and_Shrublands", "Savannas/shrublands", intersection_biome$BIOME, fixed = T)
  intersection_biome$BIOME = gsub("Temperate_Conifer_Forests", "Temperate Conifer Forests", intersection_biome$BIOME, fixed = T)
  intersection_biome$BIOME = gsub("Temperate_Broadleaf_and_Mixed_Forests", "Temperate forests", intersection_biome$BIOME, fixed = T)
  intersection_biome$BIOME = gsub("Temperate_Grasslands_Savannas_and_Shrublands", "Pampas", intersection_biome$BIOME, fixed = T)
  intersection_biome$BIOME = gsub("Flooded_Grasslands_and_Savannas", "Pantanal", intersection_biome$BIOME, fixed = T)
  intersection_biome$BIOME = gsub("Montane_Grasslands_and_Shrublands", "Montane grass/shrublands", intersection_biome$BIOME, fixed = T)
  intersection_biome$BIOME = gsub("Mediterranean_Forests_Woodlands_and_Scrub", "Medit. forests", intersection_biome$BIOME, fixed = T)
  intersection_biome$BIOME = gsub("Deserts_and_Xeric_Shrublands", "Deserts/xeric shrublands", intersection_biome$BIOME, fixed = T)
  intersection_biome$BIOME = gsub("Boreal_Forests_Taiga", "Boreal forests", intersection_biome$BIOME, fixed = T)
  intersection_biome$BIOME = gsub("Polar_Artic", "Polar Artic", intersection_biome$BIOME, fixed = T)
  intersection_biome$BIOME = gsub("Temperate", "Temp.", intersection_biome$BIOME, fixed = T)
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
  intersection_land_eez$Country = gsub("Congo, DRC", "Democratic Republic of the Congo", intersection_land_eez$Country, fixed = T)
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
  
  #land vs. sea
  earth <- earth %>% 
    group_by(featurecla) %>% 
    summarise(median = median(disparity, na.rm = T),
              mean = mean(disparity, na.rm = T),
              q_10 = quantile(disparity, 0.1, na.rm = T),
              q_90 = quantile(disparity, 0.9, na.rm = T),
              sd = sd(disparity, na.rm = T), 
              n = n()) %>%
    mutate(se = sd/sqrt(n),
           lower.ci = median - qt(1 - (0.05 / 2), n - 1) * se,
           upper.ci = median + qt(1 - (0.05 / 2), n - 1) * se)
  
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
  
  biome = biome %>% st_set_geometry(NULL) # drop geometry
  realm = realm %>% st_set_geometry(NULL) # drop geometry
  earth = earth %>% st_set_geometry(NULL) # drop geometry
  states = states %>% st_set_geometry(NULL) # drop geometry
  subr = subr %>% st_set_geometry(NULL) # drop geometry
  country = country %>% st_set_geometry(NULL) # drop geometry
  eez = eez %>% st_set_geometry(NULL) # drop geometry
  
  colnames(biome) = c("unit", "median", "mean", "q_10", "q_90", "sd", "n", "se", "lower.ci", "upper.ci"); biome$type = "Land"
  colnames(realm) = c("unit", "median", "mean", "q_10", "q_90", "sd", "n", "se", "lower.ci", "upper.ci"); realm$type = "Ocean"
  colnames(subr) = c("unit", "median", "mean", "q_10", "q_90", "sd", "n", "se", "lower.ci", "upper.ci"); subr$type = "Global_Subregions"
  colnames(country) = c("unit", "median", "mean", "q_10", "q_90", "sd", "n", "se", "lower.ci", "upper.ci"); country$type = "Countries_without_EEZ"
  colnames(eez) = c("unit", "median", "mean", "q_10", "q_90", "sd", "n", "se", "lower.ci", "upper.ci"); eez$type = "Countries_with_EEZ"
  colnames(states) = c("unit", "median", "mean", "q_10", "q_90", "sd", "n", "se", "lower.ci", "upper.ci"); states$type = "US_States"
  colnames(earth) = c("unit", "median", "mean", "q_10", "q_90", "sd", "n", "se", "lower.ci", "upper.ci"); earth$type = "Land_Sea"
  
  df = rbind(biome, realm, states, subr, country, eez, earth); rm(biome, realm, earth, states, subr, country, eez)
  
  intersection_biome = intersection_biome[,c("disparity")]; intersection_biome$type = "Land"
  intersection_realm = intersection_realm[,c("disparity")]; intersection_realm$type = "Ocean"
  intersection_subr = intersection_world[,c("disparity")]; intersection_subr$type = "Global_Subregions"
  intersection_world = intersection_world[,c("disparity")]; intersection_world$type = "Countries_without_EEZ"
  intersection_land_eez = intersection_land_eez[,c("disparity")]; intersection_land_eez$type = "Countries_with_EEZ"
  intersection_states = intersection_states[,c("disparity")]; intersection_states$type = "US_States"
  intersection_earth = rbind(intersection_biome[,c("disparity")], intersection_realm[,c("disparity")]); intersection_earth$type = "Land_Sea"
  
  baseline = rbind(intersection_biome, intersection_realm, intersection_subr, intersection_world, intersection_land_eez, intersection_states, intersection_earth)
  rm(intersection_biome, intersection_realm, intersection_subr, intersection_world, intersection_land_eez, intersection_states, intersection_earth)
  
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

rcp45_mid = pre_combine("RCP4.5", "2006-2055")
rcp45_end = pre_combine("RCP4.5", "2050-2099")
rcp85_mid = pre_combine("RCP8.5", "2006-2055")
rcp85_end = pre_combine("RCP8.5", "2050-2099")

plot_ranking = function(var, h, w, col_size, segment_size, stat) {
  
  df = rbind(rcp45_mid, rcp45_end, rcp85_mid, rcp85_end)
  df$type = ifelse(df$type %in% c("Land", "Ocean"), "Ecoregions", df$type)
  df$type = ifelse(df$type %in% c("Global_Subregions"), "Political_regions", df$type)
  df$type = ifelse(df$type %in% c("Countries_without_EEZ"), "Nation_states", df$type) # choose between "Countries_without_EEZ" or "Countries_with_EEZ"
  
  if (stat == "median") {
    colnames(df)[2] = "Disparity" #rank by median
  }
  if (stat == "q_10") {
    colnames(df)[4] = "Disparity" #rank by 10th quantile
  }
  if (stat == "q_90") {
    colnames(df)[5] = "Disparity" #rank by 90th quantile
  }
  
  # segment_size = 1
  # col_size = 2
  # var = "Nation_states"
  
  disparity_limits = c(-max(abs(df$Disparity)), max(abs(df$Disparity)))
  # df = subset(df, type %in% c("Land", "Ocean"))
  # df = subset(df, type %in% c("Global_Subregions"))
  # df = subset(df, type %in% c("Countries_without_EEZ"))
  # df = subset(df, type %in% c("US_States"))
  
  df = subset(df, type %in% var)
  # disparity_limits = c(min(df$Disparity), max(df$Disparity))
  
  df =  df %>% 
    group_by(unit) %>% 
    summarise(median = median(Disparity, na.rm = T),
              mean = mean(Disparity, na.rm = T),
              sd = sd(Disparity, na.rm = T), 
              n = n()) %>%
    mutate(se = sd/sqrt(n),
           lower.ci = median - qt(1 - (0.05 / 2), n - 1) * se,
           upper.ci = median + qt(1 - (0.05 / 2), n - 1) * se)
  
  if (var %in% c("Nation_states")) {
    
    top = df %>% top_n(25, median) #25 or 10
    bottom = df %>% top_n(-25, median)
    df = tbl_df(bind_rows(top, bottom))
    
  }
  
  # top = df %>% top_n(25, median) #25 or 10
  # bottom = df %>% top_n(-25, median)
  # df = tbl_df(bind_rows(top, bottom))
  
  p = subset(df, n > 0) %>% 
    mutate(unit = fct_reorder(unit, median)) %>% 
    ggplot() +
    geom_hline(yintercept = median(df$median), 
               # linetype = "dashed", 
               color = "lightgrey", 
               size = 1) + 
    geom_segment(aes(
      color = median, 
      x = unit, xend = unit,
      y = lower.ci, yend = upper.ci),
      size = segment_size) +
    geom_point(aes(
      color = median,
      x = unit,
      y = median),
      size = col_size) +
    # ylim(c(-max(abs(df$median)), max(abs(df$median)))*1.1) +
    coord_flip() +
    scale_colour_gradientn(
      colours = c("cyan", 
                  "black",
                  "red"),
      values = scales::rescale(c(-0.5, -0.04, 0.1, 0.2, 0.5)),
      # values = scales::rescale(c(-0.5, -0.11, 0, 0.11, 0.5)),
      limits = disparity_limits,
      name = "") + 
    xlab("") +
    ylab("") +
    theme_pubr() + 
    
    theme(
      # legend.position = c(0.9, 0.25), 
      # legend.position = "none",
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
      text = element_text(size = 15))   
  
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
  
  print(p)
  
  pdf(paste0("/Users/ktanaka/Desktop/Rank_", var, data, ".pdf"), height = h, width = w)
  print(p)
  dev.off()
  
}

plot_ranking("Ecoregions", 8, 9, 1, 2, "median")
plot_ranking("Political_regions", 7, 5, 1, 2, "median")
plot_ranking("Nation_states", 9, 5, 1, 2, "median")
plot_ranking("US_States", 9, 5, 1, 2, "median")

plot_ranking_tobether = function(h, w, col_size, segment_size, font_size, var){
  
  df = rbind(rcp45_mid, rcp45_end, rcp85_mid, rcp85_end)
  df$type = ifelse(df$type %in% c("Land", "Ocean"), "Ecoregions", df$type)
  df$type = ifelse(df$type %in% c("Global_Subregions"), "Political_regions", df$type)
  df$type = ifelse(df$type %in% c("Countries_without_EEZ"), "Nation_states", df$type) # choose between "Countries_without_EEZ" or "Countries_with_EEZ"
  
  if (var == "median") {
    colnames(df)[2] = "Disparity" #rank by median
  }
  if (var == "q_10") {
    colnames(df)[4] = "Disparity" #rank by 10th quantile
  }
  if (var == "q_90") {
    colnames(df)[5] = "Disparity" #rank by 90th quantile
  }
  
  # col_size = 1
  # segment_size = 2
  # font_size = 12
  
  ### Set Universal Color Scale ###
  # disparity_limits = c(min(df$Disparity), max(df$Disparity)) 
  disparity_limits = c(-max(abs(df$Disparity)), max(abs(df$Disparity)))
  
  df1 = subset(df, type == "Ecoregions")
  df2 = subset(df, type == "Political_regions")
  df3 = subset(df, type == "Nation_states")
  df4 = subset(df, type == "US_States")
  
  df1 =  df1 %>% 
    group_by(unit) %>% 
    summarise(median = median(Disparity, na.rm = T),
              mean = mean(Disparity, na.rm = T),
              sd = sd(Disparity, na.rm = T), 
              n = n()) %>%
    mutate(se = sd/sqrt(n),
           lower.ci = median - qt(1 - (0.05 / 2), n - 1) * se,
           upper.ci = median + qt(1 - (0.05 / 2), n - 1) * se)
  
  df2 =  df2 %>% 
    group_by(unit) %>% 
    summarise(median = median(Disparity, na.rm = T),
              mean = mean(Disparity, na.rm = T),
              sd = sd(Disparity, na.rm = T), 
              n = n()) %>%
    mutate(se = sd/sqrt(n),
           lower.ci = median - qt(1 - (0.05 / 2), n - 1) * se,
           upper.ci = median + qt(1 - (0.05 / 2), n - 1) * se)
  
  df3 =  df3 %>% 
    group_by(unit) %>% 
    summarise(median = median(Disparity, na.rm = T),
              mean = mean(Disparity, na.rm = T),
              sd = sd(Disparity, na.rm = T), 
              n = n()) %>%
    mutate(se = sd/sqrt(n),
           lower.ci = median - qt(1 - (0.05 / 2), n - 1) * se,
           upper.ci = median + qt(1 - (0.05 / 2), n - 1) * se)
  
  df4 =  df4 %>% 
    group_by(unit) %>% 
    summarise(median = median(Disparity, na.rm = T),
              mean = mean(Disparity, na.rm = T),
              sd = sd(Disparity, na.rm = T), 
              n = n()) %>%
    mutate(se = sd/sqrt(n),
           lower.ci = median - qt(1 - (0.05 / 2), n - 1) * se,
           upper.ci = median + qt(1 - (0.05 / 2), n - 1) * se)
  
  df1$category = "Ecoregions"
  df2$category = "Political_regions"
  df3$category = "Nation_states"
  df4$category = "US_states"
  
  top = df1 %>% top_n(10, median)
  bottom = df1 %>% top_n(-10, median)
  df1 = tbl_df(bind_rows(top, bottom))
  
  top = df2 %>% top_n(10, median)
  bottom = df2 %>% top_n(-10, median)
  df2 = tbl_df(bind_rows(top, bottom))
  
  top = df3 %>% top_n(20, median)
  bottom = df3 %>% top_n(-20, median)
  df3 = tbl_df(bind_rows(top, bottom))
  
  top = df4 %>% top_n(20, median)
  bottom = df4 %>% top_n(-20, median)
  df4 = tbl_df(bind_rows(top, bottom))
  
  a = df1
  b = df2
  c = df3
  d = df4
  
  median_line = b %>% 
    group_by(category) %>% 
    summarise(Median = median(median))
  
  scientific <- function(x){
    
    library(ggplot2)
    library(scales)
    library(ggthemes)
    
    ifelse(x==0, "0", parse(text = gsub("[+]", "", gsub("e", " %*% 10^", scientific_format()(x)))))
  }
  
  p1 = a %>% 
    mutate(unit = fct_reorder(unit, median)) %>% 
    ggplot() +
    geom_segment(aes(
      color = median, 
      x = unit, xend = unit,
      y = lower.ci, yend = upper.ci),
      size = segment_size) +
    geom_point(aes(
      color = median,
      x = unit,
      y = median),
      size = col_size) +
    coord_flip() +
    facet_wrap(.~ category) + 
    geom_hline(yintercept = median(a$median), 
               # linetype = "dashed", 
               color = "lightgrey", 
               size = 1) + 
    scale_colour_gradientn(
      colours = c("cyan", 
                  "black",
                  "red"),
      values = scales::rescale(c(-0.5, -0.04, 0.1, 0.2, 0.5)),
      limits = disparity_limits,
      name = "") + 
    xlab("") +
    ylab("") +
    theme_pubr() + 
    # scale_y_continuous(label = scientific) + 
    theme(
      legend.position = "none",
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
  
  
  p2 = b %>% 
    mutate(unit = fct_reorder(unit, median)) %>% 
    ggplot() +
    geom_segment(aes(
      color = median, 
      x = unit, xend = unit,
      y = lower.ci, yend = upper.ci),
      size = segment_size) +
    geom_point(aes(
      color = median,
      x = unit,
      y = median),
      size = col_size) +
    coord_flip() +
    geom_hline(data = median_line, 
               aes(yintercept = Median), 
               # linetype = "dashed", 
               color = "lightgrey", 
               size = 1) + 
    scale_colour_gradientn(
      colours = c("cyan", 
                  "black",
                  "red"),
      values = scales::rescale(c(-0.5, -0.04, 0.1, 0.2, 0.5)),
      limits = disparity_limits,
      name = "") + 
    xlab("") +
    ylab("") +
    theme_pubr() + 
    # scale_y_continuous(label = scientific) + 
    theme(
      legend.position = "none",
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
  
  
  p3 = c %>% 
    mutate(unit = fct_reorder(unit, median)) %>% 
    ggplot() +
    geom_segment(aes(
      color = median, 
      x = unit, xend = unit,
      y = lower.ci, yend = upper.ci),
      size = segment_size) +
    geom_point(aes(
      color = median,
      x = unit,
      y = median),
      size = col_size) +
    coord_flip() +
    facet_wrap(.~ category) + 
    geom_hline(yintercept = median(c$median), 
               # linetype = "dashed", 
               color = "lightgrey", 
               size = 1) + 
    scale_colour_gradientn(
      colours = c("cyan", 
                  "black",
                  "red"),
      values = scales::rescale(c(-0.5, -0.04, 0.1, 0.2, 0.5)),
      limits = disparity_limits,
      name = "") + 
    xlab("") +
    ylab("") +
    theme_pubr() + 
    # scale_y_continuous(label = scientific) + 
    theme(
      legend.position = "none",
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
  
  p4 = d %>% 
    mutate(unit = fct_reorder(unit, median)) %>% 
    ggplot() +
    geom_segment(aes(
      color = median, 
      x = unit, xend = unit,
      y = lower.ci, yend = upper.ci),
      size = segment_size) +
    geom_point(aes(
      color = median,
      x = unit,
      y = median),
      size = col_size) +
    coord_flip() +
    facet_wrap(.~ category) + 
    geom_hline(yintercept = median(c$median), 
               # linetype = "dashed", 
               color = "lightgrey", 
               size = 1) + 
    scale_colour_gradientn(
      colours = c("cyan", 
                  "black",
                  "red"),
      values = scales::rescale(c(-0.5, -0.04, 0.1, 0.2, 0.5)),
      limits = disparity_limits,
      name = "") + 
    xlab("") +
    ylab("") +
    theme_pubr() + 
    # scale_y_continuous(label = scientific) + 
    theme(
      legend.position = "right",
      legend.justification = c(1, 0),
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
  
  
  # pdf(paste0("/Users/ktanaka/Desktop/Combined_ranking_1", data, ".pdf"), h = h, w = w)
  # gridExtra::grid.arrange(p1, p2, ncol = 1)
  # dev.off()
  # 
  # pdf(paste0("/Users/ktanaka/Desktop/Combined_ranking_2", data, ".pdf"), h = h, w = w)
  # gridExtra::grid.arrange(p3, p4, ncol = 2)
  # dev.off()
  
  pdf(paste0("/Users/ktanaka/Desktop/Figure_3_", data, "_", var, ".pdf"), h = h, w = w)
  
  p = ggdraw() +
    draw_plot(p3, x = 0, y = 0, width = 0.3, height = 1) +
    draw_plot(p2, x = 0.35, y = 0, width = 0.3, height = .49) +
    draw_plot(p1, x = 0.35, y = .5, width = 0.3, height = .49) +
    draw_plot(p4, x = 0.7, y = 0, width = 0.3, height = 1) +
    draw_plot_label(label = c("A", "B", "C", "D"), size = 20,
                    x = c(0, 0.35, 0.35, 0.7), y = c(1, 1, 0.5, 1))  
  
  print(p)
  
  dev.off()
  
}

plot_ranking_tobether(12, 16, 1, 4, 12, "median")
plot_ranking_tobether(12, 16, 1, 4, 12, "q_10")
plot_ranking_tobether(12, 16, 1, 4, 12, "q_90")



