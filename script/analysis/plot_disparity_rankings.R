library(ggplot2)
library(dplyr)
library(sf)
library(forcats)
library(ggpubr)
library(maps)

rm(list = ls())

pre_combine = function(rcp, period){
  
  # rcp = c("RCP4.5", "RCP8.5")[1]
  # period = c("2006-2055", "2050-2099")[1]
  scale = c("scaled", "unscaled")[2]
  
  ### rco4.5, 2006-2055 ###
  load(paste0("~/clim_geo_disp/output/intersection_result_", period, "_", rcp, ".RData"))
  
  intersection_biome$Outcome = paste0(rcp, " ", period)
  intersection_realm$Outcome = paste0(rcp, " ", period)
  intersection_land_eez$Outcome = paste0(rcp, " ", period)
  intersection_states$Outcome = paste0(rcp, " ", period)
  
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
  
  colnames(biome) = c("unit", "median", "mean", "sd", "n", "se", "lower.ci", "upper.ci"); biome$type = "Land"
  colnames(realm) = c("unit", "median", "mean", "sd", "n", "se", "lower.ci", "upper.ci"); realm$type = "Ocean"
  colnames(subr) = c("unit", "median", "mean", "sd", "n", "se", "lower.ci", "upper.ci"); subr$type = "Global_Subregions"
  colnames(country) = c("unit", "median", "mean", "sd", "n", "se", "lower.ci", "upper.ci"); country$type = "Countries_without_EEZ"
  colnames(eez) = c("unit", "median", "mean", "sd", "n", "se", "lower.ci", "upper.ci"); eez$type = "Countries_with_EEZ"
  colnames(states) = c("unit", "median", "mean", "sd", "n", "se", "lower.ci", "upper.ci"); states$type = "US_States"
  colnames(earth) = c("unit", "median", "mean", "sd", "n", "se", "lower.ci", "upper.ci"); earth$type = "Land_Sea"
  
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

df = rbind(rcp45_mid, rcp45_end, rcp85_mid, rcp85_end)
df$type = ifelse(df$type %in% c("Land", "Ocean"), "Ecoregions", df$type)
df$type = ifelse(df$type %in% c("Global_Subregions"), "Political_regions", df$type)
df$type = ifelse(df$type %in% c("Countries_with_EEZ"), "Nation_states", df$type)

colnames(df)[2] = "Disparity"

plot_ranking = function(var, h, w) {
  
  disparity_limits = c(-2.27, 2.27) #this is -max(disparity); max(disparity) from rcp4.5 2006-2055 scenario
  
  # df = subset(df, type %in% c("Land", "Ocean"))
  # df = subset(df, type %in% c("Global_Subregions"))
  # df = subset(df, type %in% c("Countries_without_EEZ"))
  # df = subset(df, type %in% c("US_States"))
  
  # var = "Nation_states"
  
  df = subset(df, type %in% var)
  
  colnames(df)[2] = "Disparity"
  
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
    
    # top = df %>% top_n(25, median)
    # bottom = df %>% top_n(-25, median)
    # df = tbl_df(bind_rows(top, bottom))
    
  }
  
  
  p = subset(df, n > 0) %>% 
    mutate(unit = fct_reorder(unit, median)) %>% 
    ggplot() +
    geom_segment(aes(
      color = median, # color = Ecoregion,
      x = unit, xend = unit,
      y = lower.ci, yend = upper.ci),
      size = 0.5) +
    geom_point(aes(
      color = median, # color = Ecoregion
      x = unit,
      y = median),
      size = 1.5) +
    coord_flip() +
    geom_hline(yintercept = median(df$median), 
               linetype = "dashed", 
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
    theme_classic2() + 
    # ylim(c(-max(abs(df$median)), max(abs(df$median)))*1.1) +
    theme(
      # axis.text.y = element_blank(),
      axis.ticks.y = element_blank(), 
      legend.justification = c(-0.1, 1), 
      # legend.position = c(0, 1),
      legend.position = "none") + 
    ggtitle("\n Future period = 2006-2055 & 2050-2099 \n Experiment = rcp 4.5 & 8.5")
    # annotate("text",
    #          x = Inf, 
    #          y = -Inf, 
    #          hjust = 0,
    #          vjust = 0.8,
    #          label =  "\n Future period = 2006-2055 & 2050-2099 \n Experiment = rcp 4.5 & 8.5")
  
  print(p)
  
  pdf(paste0("/Users/ktanaka/Dropbox/PAPER climate geographic disparities/figures/drafts/Rank_", var, ".pdf"), height = h, width = w)
  print(p)
  dev.off()
  
}

plot_ranking("Ecoregions", 6, 8)
plot_ranking("Political_regions", 6, 8)
plot_ranking("Nation_states", 25, 8)
plot_ranking("US_States", 7, 8)

plot_ranking_tobether = function(h, w, col_size, segment_size){
  
  # col_size = 1
  # segment_size = 2
  
  ### Set Universal Color Scale ###
  disparity_limits = c(-2.27, 2.27) #this is -max(disparity); max(disparity) from rcp4.5 2006-2055 scenario

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
  
  top = df1 %>% top_n(14, median)
  bottom = df1 %>% top_n(-14, median)
  df1 = tbl_df(bind_rows(top, bottom))
  
  top = df3 %>% top_n(25, median)
  bottom = df3 %>% top_n(-25, median)
  df3 = tbl_df(bind_rows(top, bottom))
  
  # top = df4 %>% top_n(22, median)
  # bottom = df4 %>% top_n(-22, median)
  # df4 = tbl_df(bind_rows(top, bottom))
  
  a = df3
  b = rbind(df1, df2)
  c = df4
  
  median_line = b %>% 
    group_by(category) %>% 
    summarise(Median = median(median))
  
  p1 = a %>% 
    mutate(unit = fct_reorder(unit, median)) %>% 
    ggplot() +
    geom_segment(aes(
      color = median, 
      x = unit, xend = unit,
      y = lower.ci, yend = upper.ci),
      size = segment_size) +
    # geom_point(aes( 
    #   color = median, 
    #   x = unit, 
    #   y = median), 
    #   size = col_size) +
    coord_flip() +
    facet_wrap(.~ category) + 
    geom_hline(yintercept = median(a$median), 
               linetype = "dashed", 
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
    theme_classic2() +
    # ylim(c(-max(abs(a$median)), max(abs(a$median)))*1.1) +
    theme(
      axis.ticks.y = element_blank(), 
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      # legend.justification = c(-0.1, 1), 
      # legend.position = c(0, 1),
      legend.position = "none") 
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
    # geom_point(aes( 
    #   color = median, 
    #   x = unit, 
    #   y = median), 
    #   size = col_size) +
    coord_flip() +
    geom_hline(data = median_line, 
               aes(yintercept = Median), 
               linetype = "dashed", 
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
    theme_classic2() +
    facet_wrap(.~ category, scales = "free_y", ncol = 1) + 
    # ylim(c(-max(abs(a$median)), max(abs(a$median)))*1.1) +
    theme(
      axis.ticks.y = element_blank(), 
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      # legend.justification = c(-0.1, 1), 
      # legend.position = c(0, 1),
      legend.position = "none") 
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
    # geom_point(aes( 
    #   color = median, 
    #   x = unit, 
    #   y = median), 
    #   size = col_size) +
    coord_flip() +
    facet_wrap(.~ category) + 
    geom_hline(yintercept = median(c$median), 
               linetype = "dashed", 
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
    theme_classic2() +
    # ylim(c(-max(abs(a$median)), max(abs(a$median)))*1.1) +
    theme(
      axis.ticks.y = element_blank(), 
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      # legend.justification = c(-0.1, 1), 
      # legend.position = c(0, 1),
      legend.position = "none") 
  # annotate("text",
  #          x = -Inf, 
  #          y = Inf, 
  #          hjust = 1,
  #          vjust = -0.2, 
  #          label =  "\n Future period = 2006-2055 & 2050-2099 \n Experiment = rcp 4.5 & 8.5")
  
  
  pdf("/Users/ktanaka/Desktop/Combined_plot.pdf", h = h, w = w)
  gridExtra::grid.arrange(p1, p2, p3, ncol = 3)
  dev.off()

  
}

plot_ranking_tobether(10, 30, 2.5, 1)
  
