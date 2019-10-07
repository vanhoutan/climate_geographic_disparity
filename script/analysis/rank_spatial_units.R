library(ggplot2)
library(dplyr)
library(sf)
library(forcats)
library(ggpubr)
library(maps)

rm(list = ls())

model_run = c("esn_1", "esn_2", "mpi_orgiginal")[1]
scale = c("scaled", "unscaled")[1]
rcp = c("RCP4.5", "RCP8.5")[1]

if (model_run == "mpi_original") {
  
  load("~/clim_geo_disp/output/intersection_result_BCO2_99perecentile.RData") 
  
} else {
  
  load(paste0("~/clim_geo_disp/output/intersection_result_", model_run, "_", rcp, ".RData"))
  
  if (model_run == "esn_1") period = "2005-2055"
  if (model_run == "esn_2") period = "2050-2099"
  
}

# if (anom == "CMIP5_Ensemble") load(paste0("~/clim_geo_disp/output/intersection_result_BCO2_ESM_", period, ".RData"))
# if (anom == "MPI_ESM") load(paste0("~/clim_geo_disp/output/intersection_result_BCO2_MPI_", period, ".RData"))
# if (anom == "MPI_Org") load("~/clim_geo_disp/output/intersection_result_BCO2_99perecentile.RData") # BC + CO2 with values aggregated at 99th percentile

# process process process -------------------------------------------------

# load("~/clim_geo_disp/output/intersection_result_BCO2.RData")
# load("~/clim_geo_disp/output/intersection_result_rcp85_untrimmed.RData")

# rm(land_intersection, ocean_intersection)

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

# plot --------------------------------------------------------------------

rank_individual_unit <- function(var) {
  
  if (var == "Ecoregions") {
    
    baseline = subset(baseline, type %in% c("Land", "Ocean"))
    df = subset(df, type %in% c("Land", "Ocean"))
    df$type = "Ecoregions"
    
    top = df %>% 
      # group_by(Region) %>% 
      top_n(25, median)
    
    bottom = df %>% 
      # group_by(Region) %>% 
      top_n(-25, median)
    
    df_sub = tbl_df(bind_rows(top, bottom))
    df_sub$type = "Ecoregions"
    
  }
  
  if (var == "Political_regions") {
    
    baseline = subset(baseline, type %in% c("Global_Subregions"))
    df = subset(df, type %in% c("Global_Subregions"))
    df$type = "Global_Subregions"
    
    top = df %>% 
      # group_by(Region) %>% 
      top_n(25, median)
    
    bottom = df %>% 
      # group_by(Region) %>% 
      top_n(-25, median)
    
    df_sub = tbl_df(bind_rows(top, bottom))
    df_sub$type = "Global_Subregions"
    
  }
  
  if (var == "Countries_without_EEZ") {
    
    baseline = subset(baseline, type %in% c("Countries_without_EEZ"))
    df = subset(df, type %in% c("Countries_without_EEZ"))
    df$type = "Countries_without_EEZ"
    
    top = df %>% 
      # group_by(Region) %>% 
      top_n(11, median)
    
    bottom = df %>% 
      # group_by(Region) %>% 
      top_n(-11, median)
    
    df_sub = tbl_df(bind_rows(top, bottom))
    df_sub$type = "Countries_without_EEZ"
    
  }
  
  if (var == "Countries_with_EEZ") {
    
    baseline = subset(baseline, type %in% c("Countries_with_EEZ"))
    df = subset(df, type %in% c("Countries_with_EEZ"))
    df$type = "Countries_with_EEZ"
    
    top = df %>% 
      # group_by(Region) %>% 
      top_n(50, median)
    
    bottom = df %>% 
      # group_by(Region) %>% 
      top_n(-50, median)
    
    df_sub = tbl_df(bind_rows(top, bottom))
    df_sub$type = "Countries_with_EEZ"
    
  }
  
  if (var == "US_States") {
    
    baseline = subset(baseline, type %in% c("US_States"))
    df = subset(df, type %in% c("US_States"))
    df$type = "US_States"
    
    top = df %>% 
      # group_by(Region) %>% 
      top_n(11, median)
    
    bottom = df %>% 
      # group_by(Region) %>% 
      top_n(-11, median)
    
    df_sub = tbl_df(bind_rows(top, bottom))
    df_sub$type = "US_States"
  }
  
  if (var == "Earth") {
    
    baseline = subset(baseline, type %in% c("Land_Sea"))
    df = subset(df, type %in% c("Land_Sea"))
    df$type = "Earth"
    
  }
  
  baseline <- baseline %>% 
    summarise(median = median(disparity, na.rm = T),
              mean = mean(disparity, na.rm = T),
              sd = sd(disparity, na.rm = T), 
              n = n()) %>%
    mutate(se = sd/sqrt(n),
           lower.ci = median - qt(1 - (0.05 / 2), n - 1) * se,
           upper.ci = median + qt(1 - (0.05 / 2), n - 1) * se)
  
  if (var %in% c("Countries_without_EEZ", "Countries_with_EEZ")) {
    
    p1 = subset(df_sub, n > 0) %>% 
      mutate(unit = fct_reorder(unit, median)) %>% 
      ggplot() +
      geom_text(aes(
        # color = median,
        x = unit,
        y = median,
        label = unit),
        hjust = -0.2,
        vjust = -0.1) +
      geom_point(aes(
        color = median, 
        x = unit, 
        y = median), 
        size = 4) +
      coord_flip() +
      geom_hline(yintercept = baseline$mean, 
                 linetype="dashed", 
                 color = "lightgrey", 
                 size = 1) + 
      # scale_colour_gradient2(low = "Black", 
      #                        mid = "white", 
      #                        high = "Red", 
      #                        midpoint = baseline$mean,
      #                        limits = c(-max(abs(df_sub$median)), max(abs(df_sub$median))),
      #                        name = "") + 
      scale_colour_gradientn(
        colours = c("cyan", 
                    "black",
                    "red"),
        values = scales::rescale(c(-0.5, -0.1, 0, 0.1, 0.5)),
        limits = c(-max(abs(df$median)), max(abs(df$median))), name = "") + 
      ylim(c(-max(abs(df$median)), max(abs(df$median)))*1.1) +
      xlab("") +
      ylab("Disparity (95% CI)") +
      theme_pubr() + 
      theme(
        axis.text.y = element_blank(),
        axis.ticks = element_blank(), 
        legend.justification = c(-0.1, 1), 
        legend.position = c(0, 1)) + 
      annotate("text",
               x = -Inf, 
               y = Inf, 
               hjust = 1,
               vjust = -0.2, 
               label = paste0("\n Temperature Anomaly = ", model_run, "\n Future Period = ", period, "\n Experiment = ", rcp))
    
    p2 = subset(df, n > 0) %>% 
      mutate(unit = fct_reorder(unit, median)) %>% 
      ggplot() +
      geom_segment(aes(
        color = median,
        x = unit, xend = unit,
        y = lower.ci, yend = upper.ci),
        size = 0.5) +
      geom_point(aes(
        color = median, 
        x = unit, 
        y = median),
        size = 1.5) +
      coord_flip() +
      geom_hline(yintercept = baseline$mean, 
                 linetype = "dashed", 
                 color = "lightgrey", 
                 size = 1) + 
      # annotate("rect",
      #          xmin = 0, xmax = 10,
      #          ymin = -9, ymax = 9,
      #          alpha = .1, 
      #          fill = "black") +
      # annotate("rect",
      #          xmin = nrow(df)-10, xmax = nrow(df),
      #          ymin = -9, ymax = 9,
      #          alpha = .2, 
      #          fill = "red") +
      # scale_colour_gradient2(low = "Black", 
      #                        mid = "white", 
      #                        high = "Red", 
      #                        midpoint = baseline$mean,
      #                        limits = c(-max(abs(df_sub$median)), max(abs(df_sub$median))),
      #                        name = "") + 
      scale_colour_gradientn(
        colours = c("cyan", 
                    "black",
                    "red"),
        values = scales::rescale(c(-0.5, -0.1, 0, 0.1, 0.5)),
        limits = c(-max(abs(df$median)), max(abs(df$median))), name = "") + 
      # ylim(-10, 10) +
      xlab("") +
      ylab("Disparity (95% CI)") +
      theme_pubr() + 
      theme(axis.text.y = element_blank(),
            axis.ticks = element_blank(), 
            legend.justification = c(0,1), 
            legend.position = "none") 
    
    p3 = subset(df_sub, n > 0) %>% 
      mutate(unit = fct_reorder(unit, median)) %>% 
      ggplot() +
      # geom_segment(aes(
      #   color = median,
      #   x = unit, xend = unit,
      #   y = lower.ci, yend = upper.ci),
      #   size = 0.5) +
      geom_point(aes(
        color = median, 
        x = unit, 
        y = median),
        size = 1.5) +
      coord_flip() +
      geom_hline(yintercept = baseline$mean, 
                 linetype = "dashed", 
                 color = "lightgrey", 
                 size = 1) + 
      # annotate("rect",
      #          xmin = 0, xmax = 10,
      #          ymin = -9, ymax = 9,
      #          alpha = .1, 
      #          fill = "black") +
      # annotate("rect",
      #          xmin = nrow(df)-10, xmax = nrow(df),
      #          ymin = -9, ymax = 9,
      #          alpha = .2, 
      #          fill = "red") +
      # scale_colour_gradient2(low = "Black", 
    #                        mid = "white", 
    #                        high = "Red", 
    #                        midpoint = baseline$mean,
    #                        limits = c(-max(abs(df_sub$median)), max(abs(df_sub$median))),
    #                        name = "") + 
    scale_colour_gradientn(
      colours = c("cyan", 
                  "black",
                  "red"),
      values = scales::rescale(c(-0.5, -0.1, 0, 0.1, 0.5)),
      limits = c(-max(abs(df$median)), max(abs(df$median))), name = "") + 
      # ylim(-10, 10) +
      xlab("") +
      ylab("") +
      theme_pubr() + 
      theme(axis.ticks = element_blank(), 
            legend.justification = c(0,1), 
            legend.position = "none") 
    
    pdf(paste0("/Users/ktanaka/Dropbox/PAPER climate geographic disparities/figures/drafts/Rank_", var, "_", period, "_", rcp, ".pdf"), height = 16, width = 8)
    print(p3)
    dev.off()

  } else {
    
    p1 = subset(df_sub, n > 0) %>% 
      mutate(unit = fct_reorder(unit, median)) %>% 
      ggplot() +
      geom_text(aes(
        x = unit,
        y = median,
        label = unit),
        hjust = -0.15,
        vjust = -0.1) +
      geom_point(aes( 
        color = median, 
        x = unit, 
        y = median), 
        size = 4) +
      geom_hline(yintercept = baseline$mean, 
                 linetype = "dashed", 
                 color = "lightgrey", 
                 size = 1) + 
      coord_flip() +
      # scale_colour_gradient2(low = "Black", 
      #                        mid = "white", 
      #                        high = "Red", 
      #                        midpoint = baseline$mean,
      #                        limits = c(-max(abs(df_sub$median)), max(abs(df_sub$median))),
      #                        name = "") + 
      scale_colour_gradientn(
        colours = c("cyan", 
                    "black",
                    "red"),
        values = scales::rescale(c(-0.5, -0.1, 0, 0.1, 0.5)),
        limits = c(-max(abs(df$median)), max(abs(df$median))), name = "") + 
      # facet_wrap(.~type, scales = "free_x", ncol = 7) +
      ylim(c(-max(abs(df$median)), max(abs(df$median)))*1.1) +
      xlab("") +
      ylab("Disparity (95% CI)") +
      theme_pubr() + 
      theme(
        axis.text.y = element_blank(),
        axis.ticks = element_blank(), 
        legend.justification = c(-0.1, 1), 
        legend.position = c(0, 1)) + 
      annotate("text",
               x = -Inf, 
               y = Inf, 
               hjust = 1,
               vjust = -0.2, 
               label =  paste0("\n Temperature Anomaly = ", model_run, "\n Future Period = ", period, "\n Experiment = ", rcp))
    
    p2 = subset(df, n > 0) %>% 
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
        size = 1) +
      geom_hline(yintercept = baseline$mean, 
                 linetype = "dashed", 
                 color = "lightgrey", 
                 size = 1) + 
      coord_flip() +
      # scale_colour_gradient2(low = "Black", 
      #                        mid = "white", 
      #                        high = "Red", 
      #                        midpoint = baseline$mean,
      #                        limits = c(-max(abs(df_sub$median)), max(abs(df_sub$median))),
      #                        name = "") + 
      scale_colour_gradientn(
        colours = c("cyan", 
                    "black",
                    "red"),
        values = scales::rescale(c(-0.5, -0.1, 0, 0.1, 0.5)),
        limits = c(-max(abs(df$median)), max(abs(df$median))), name = "") + 
      # facet_wrap(.~type, scales = "free_x", ncol = 7) +
      xlab("") +
      ylab("Disparity (95% CI)") +
      theme_pubr() + 
      theme(
        # axis.text.y = element_blank(),
        axis.ticks = element_blank(), 
        legend.justification = c(-0.1, 1), 
        legend.position = c(0, 1)) + 
      annotate("text",
               x = -Inf, 
               y = Inf, 
               hjust = 1,
               vjust = -0.2, 
               label =  paste0("\n Temperature Anomaly = ", model_run, "\n Future Period = ", period, "\n Experiment = ", rcp))
    
    p3 = subset(df, n > 0) %>% 
      mutate(unit = fct_reorder(unit, median)) %>% 
      ggplot() +
      # geom_segment(aes(
      #   color = median, # color = Ecoregion,
      #   x = unit, xend = unit,
      #   y = lower.ci, yend = upper.ci),
      #   size = 0.5) +
      geom_point(aes( 
        color = median, # color = Ecoregion
        x = unit, 
        y = median), 
        size = 1) +
      geom_hline(yintercept = baseline$mean, 
                 linetype = "dashed", 
                 color = "lightgrey", 
                 size = 1) + 
      coord_flip() +
      # scale_colour_gradient2(low = "Black", 
      #                        mid = "white", 
      #                        high = "Red", 
      #                        midpoint = baseline$mean,
      #                        limits = c(-max(abs(df_sub$median)), max(abs(df_sub$median))),
      #                        name = "") + 
      scale_colour_gradientn(
        colours = c("cyan", 
                    "black",
                    "red"),
        values = scales::rescale(c(-0.5, -0.1, 0, 0.1, 0.5)),
        limits = c(-max(abs(df$median)), max(abs(df$median))), name = "") + 
      # facet_wrap(.~type, scales = "free_x", ncol = 7) +
      xlab("") +
      ylab("") +
      theme_pubr() + 
      theme(
        # axis.text.y = element_blank(),
        axis.ticks = element_blank(), 
        legend.justification = c(-0.1, 1), 
        legend.position = c(0, 1)) + 
      annotate("text",
               x = -Inf, 
               y = Inf, 
               hjust = 1,
               vjust = -0.2, 
               label =  paste0("\n Future Period = ", period, "\n Experiment = ", rcp))
    
    pdf(paste0("/Users/ktanaka/Dropbox/PAPER climate geographic disparities/figures/drafts/Rank_", var, "_", period, "_", rcp, ".pdf"), height = 16, width = 8)
    
    # gridExtra::grid.arrange(p1, p2, ncol = 2)
    print(p3)
    dev.off()
    
  }
  
}

rank_individual_unit("Ecoregions")
rank_individual_unit("Political_regions")
# rank_individual_unit("Countries_without_EEZ")
rank_individual_unit("Countries_with_EEZ")
rank_individual_unit("US_States")
# rank_individual_unit("Earth")

rank_all_unit <- function() {
  
  #bundle ecoregion group
  df$type[df$type %in% c("Land", "Ocean")] = "Ecoregion"  
  df$type[df$type %in% c("Land_Sea")] = "Earth"  
  
  #rename main units
  df$type[df$type == c("Ecoregion")] = "1. Ecoregion"  
  df$type[df$type == c("Global_Subregions")] = "2. Subregions"  
  df$type[df$type == c("Countries_with_EEZ")] = "3. Countries"  
  df$type[df$type == c("US_States")] = "4. US_States"  
  
  #remove earth
  df = subset(df, type != "Countries_without_EEZ" & type != "Earth")
  
  xxx = 10
  
  #keep only top and bottom xxx ecoregions 
  top = subset(df, type == "1. Ecoregion") %>% top_n(xxx, median)
  bottom = subset(df, type == "1. Ecoregion") %>% top_n(-xxx, median)
  frac = rbind(bottom, top)
  df = rbind(subset(df, type != "1. Ecoregion"), frac)
  
  #keep only top and bottom xxx subregions 
  top = subset(df, type == "2. Subregions") %>% top_n(xxx, median)
  bottom = subset(df, type == "2. Subregions") %>% top_n(-xxx, median)
  frac = rbind(bottom, top)
  df = rbind(subset(df, type != "2. Subregions"), frac)
  
  #keep only top and bottom xxx countries 
  top = subset(df, type == "3. Countries") %>% top_n(xxx, median)
  bottom = subset(df, type == "3. Countries") %>% top_n(-xxx, median)
  frac = rbind(bottom, top)
  df = rbind(subset(df, type != "3. Countries"), frac)
  
  #keep only top and bottom xxx US states 
  top = subset(df, type == "4. US_States") %>% top_n(xxx, median)
  bottom = subset(df, type == "4. US_States") %>% top_n(-xxx, median)
  frac = rbind(bottom, top)
  df = rbind(subset(df, type != "4. US_States"), frac)
  
  baseline <- baseline %>% 
    summarise(median = median(disparity, na.rm = T),
              mean = mean(disparity, na.rm = T),
              sd = sd(disparity, na.rm = T), 
              n = n()) %>%
    mutate(se = sd/sqrt(n),
           lower.ci = median - qt(1 - (0.010 / 2), n - 1) * se,
           upper.ci = median + qt(1 - (0.010 / 2), n - 1) * se)
  
  
  p1 = df %>% mutate(unit = fct_reorder(unit, median)) %>% 
    ggplot() +
    # geom_segment(aes(
    #   color = median, # color = Ecoregion,
    #   x = unit, xend = unit,
    #   y = lower.ci, yend = upper.ci),
    #   size = 0.1) +
    # geom_text(aes(
    #   # color = median, # color = Ecoregion
    #   x = unit,
    #   y = median, 
    #   label = unit), 
    #   hjust = -0.1, 
  #   vjust = 0) +
  geom_point(aes( 
    color = median,
    x = unit, 
    y = median), 
    size = 3) +
    geom_hline(yintercept = baseline$mean,
               linetype = "dashed",
               color = "lightgrey",
               size = 1) +
    coord_flip() +
    scale_colour_gradientn(
      # colours = c("Black", 
      #             # "White",
      #             "White",
      #             # "White",
      #             "Red"),
      # midpoint = baseline$mean,
      colours = c("cyan", 
                  "black",
                  "red"),
      values = scales::rescale(c(-0.5, -0.1, 0, 0.1, 0.5)),
      limits = c(-max(abs(df$median)), max(abs(df$median))), name = "") + 
    facet_wrap(.~type, scales = "free_y", ncol = 1) +
    ylim(c(-max(abs(df$median)), max(abs(df$median)))*1.1) +
    xlab("") +
    ylab("Disparity (95% CI)") +
    theme_classic2() +
    theme(
      # axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      text = element_text(size = 6.5), 
      # legend.justification = c(-0.1, 1),
      legend.position = "right") +
    annotate("text",
             x = -Inf,
             y = Inf,
             hjust = 1,
             vjust = -0.2,
             size = 1,
             label = paste0("\n Model = ", anom, "\n Future Period = ", period))
  
  p2 = df %>% mutate(unit = fct_reorder(unit, median)) %>% 
    ggplot() +
    geom_point(aes( 
      color = median,
      x = unit, 
      y = median), 
      size = 2) +
    geom_segment(aes(
      color = median, # color = Ecoregion,
      x = unit, xend = unit,
      y = lower.ci, yend = upper.ci),
      size = 0.5) +
    geom_hline(yintercept = baseline$mean,
               linetype = "dashed",
               color = "lightgrey",
               size = 1) +
    coord_flip() +
    scale_colour_gradientn(
      colours = c("cyan", 
                  "black",
                  "red"),
      values = scales::rescale(c(-0.5, -0.1, 0, 0.1, 0.5)),
      limits = c(-max(abs(df$median)), max(abs(df$median))), name = "") + 
    # ylim(c(-max(abs(df$median)), max(abs(df$median)))*1.1) +
    xlab("") +
    ylab("Disparity (95% CI)") +
    theme_pubr() +
    theme(
      # axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      # legend.justification = c(-0.1, 1),
      legend.position = "right") +
    annotate("text",
             x = -Inf,
             y = Inf,
             hjust = 1,
             vjust = -0.2,
             label = paste0("\n Model = ", anom, "\n Future Period = ", period))
  
  p3 = df %>% mutate(unit = fct_reorder(unit, median)) %>% 
    ggplot() +
    geom_point(aes( 
      color = median,
      x = unit, 
      y = median), 
      size = 2) +
    # geom_segment(aes(
    #   color = median, # color = Ecoregion,
    #   x = unit, xend = unit,
    #   y = lower.ci, yend = upper.ci),
    #   size = 0.5) +
    geom_hline(yintercept = baseline$mean,
               linetype = "dashed",
               color = "lightgrey",
               size = 1) +
    scale_colour_gradientn(
      colours = c("cyan", 
                  "black",
                  "red"),
      values = scales::rescale(c(-0.5, -0.1, 0, 0.1, 0.5)),
      limits = c(-max(abs(df$median)), max(abs(df$median))), name = "") + 
    # ylim(c(-max(abs(df$median)), max(abs(df$median)))*1.1) +
    xlab("") +
    ylab("Disparity (95% CI)") +
    theme_pubr() +
    theme(
      # axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      # legend.justification = c(-0.1, 1),
      legend.position = c(0.05,0.9),
      axis.text.x = element_text(angle = 45, hjust = 1)) +
    annotate("text",
             x = Inf,
             y = -Inf,
             hjust = 1,
             vjust = -0.2,
             label = paste0("\n Model = ", anom, "\n Future Period = ", period))
  
  pdf(paste0("/Users/ktanaka/Dropbox/PAPER climate geographic disparities/figures/drafts/Rank_Combined_", anom, "_", period, "_Short.pdf"),
      height = 13, width = 10)
  print(p1)
  dev.off()
  
  pdf(paste0("/Users/ktanaka/Dropbox/PAPER climate geographic disparities/figures/drafts/Rank_Combined_", anom, "_", period, "_Full.pdf"),
      height = 13, width = 10)
  print(p2)
  dev.off()
  
  pdf(paste0("/Users/ktanaka/Dropbox/PAPER climate geographic disparities/figures/drafts/Rank_Combined_", anom, "_", period, "_Test.pdf"),
      height = 10, width = 20)
  print(p3)
  dev.off()
  
}
rank_all_unit()


##################
### ecoregions ###
##################

colnames(realm)[colnames(realm)=="REALM"] <- "BIOME"
realm$Ecoregion <- "Ocean"
biome$Ecoregion <- "Land"
ecoregion <- rbind(realm, biome)

baseline = rbind(intersection_biome[,c("BIOME", "disparity")], intersection_realm[,c("BIOME", "disparity")])

baseline <- baseline %>% 
  summarise(median = median(disparity, na.rm = T),
            mean = mean(disparity, na.rm = T),
            sd = sd(disparity, na.rm = T), 
            n = n()) %>%
  mutate(se = sd/sqrt(n),
         lower.ci = median - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = median + qt(1 - (0.05 / 2), n - 1) * se)

png(paste0("/Users/ktanaka/Dropbox/PAPER climate geographic disparities/figures/fig_drafts/Rank_1_Ecoregion_", anom, "_", period, ".png"), 
    height = 12, width = 8, res = 500, units = "in")

ecoregion %>% 
  mutate(BIOME = fct_reorder(BIOME, median)) %>% 
  ggplot() +
  geom_segment(aes(
    color = median, # color = Ecoregion,
    x = BIOME, xend = BIOME, 
    y = lower.ci, yend = upper.ci), 
    size = 0.1) + 
  geom_text(aes(
    color = median, # color = Ecoregion,
    x = BIOME, 
    y = median, 
    label = BIOME), 
    hjust = 0.5, 
    vjust = -0.5) +
  geom_point(aes( 
    color = median, # color = Ecoregion
    x = BIOME, 
    y = median)) +
  geom_hline(yintercept = baseline$mean, 
             linetype = "dashed", 
             color = "lightgrey", 
             size = 1) + 
  # annotate("rect",
  #          xmin = 0, xmax = length(unique(baseline$BIOME)),
  #          ymin = baseline$lower.ci, ymax = baseline$upper.ci,
  #          alpha = 1) +
  # scale_color_manual(values = c("orange", "darkturquoise")) +
  scale_colour_gradient2(low = "Black",
                         mid = "Gray",
                         high = "Red",
                         midpoint = baseline$mean,
                         limits = c(-max(abs(ecoregion$median)), max(abs(ecoregion$median))),
                         name = "") +
  coord_flip() +
  # facet_wrap(.~Ecoregion, scales = "free_y", ncol = 1) +
  ylim(-3, 3) +
  xlab("") +
  ylab("Disparity (Median with 95% CI)") +
  theme_pubr(I(20)) + 
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(), 
        legend.justification = c(-0.1, 1), 
        legend.position = c(0, 1.05)) + 
  ggtitle("Ecoregion")  + 
  annotate("text",
           x = -Inf, 
           y = Inf, 
           hjust = 1,
           vjust = -0.2, 
           label = paste0("\n Temperature Anomaly = ", anom, "\n Future Period = ", period))

dev.off()


##########################
### Global Sub Regions ###
##########################

baseline <- intersection_world %>% 
  summarise(median = median(disparity, na.rm = T),
            mean = mean(disparity, na.rm = T),
            sd = sd(disparity, na.rm = T), 
            n = n()) %>%
  mutate(se = sd/sqrt(n),
         lower.ci = median - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = median + qt(1 - (0.05 / 2), n - 1) * se)

png(paste0("/Users/ktanaka/Dropbox/PAPER climate geographic disparities/figures/fig_drafts/Rank_2_Subregion_", anom, "_", period, ".png"), 
    height = 12, width = 8, res = 500, units = "in")

subr[!(subr$subregion == "Seven seas (open ocean)"),] %>% 
  mutate(subregion = fct_reorder(subregion, median)) %>% 
  ggplot() +
  geom_segment(aes(
    color = median,
    x = subregion, xend = subregion, 
    y = lower.ci, yend = upper.ci),
    size = 0.1) + 
  geom_text(aes(
    color = median,
    x = subregion, 
    y = median, 
    label = subregion),
    hjust = 0.5, 
    vjust = -0.5) +
  geom_point(aes(
    color = median, 
    x = subregion, 
    y = median)) +
  coord_flip() +
  geom_hline(yintercept = baseline$median, 
             linetype="dashed", 
             color = "lightgrey", 
             size = 1) + 
  # annotate("rect", 
  #          xmin = 0, xmax = length(unique(baseline$subregion)),
  #          ymin = baseline$lower.ci, ymax = baseline$upper.ci,
  #          alpha = .2) +
  scale_colour_gradient2(low = "Black", 
                         mid = "Gray", 
                         high = "Red", 
                         midpoint = baseline$mean,
                         limits = c(-max(abs(subr$median)), max(abs(subr$median))),
                         name = "") + 
  ylim(-2,2) +
  xlab("") +
  ylab("Disparity (95% CI)") +
  theme_pubr(I(20)) + 
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(), 
        legend.justification = c(-0.1, 1), 
        legend.position = c(0, 1.05)) + 
  ggtitle("Subregion")  + 
  annotate("text",
           x = -Inf, 
           y = Inf, 
           hjust = 1,
           vjust = -0.2, 
           label = paste0("\n Temperature Anomaly = ", anom, "\n Future Period = ", period))
dev.off()


#########################
### Countries w/o EEZ ###
#########################

baseline <- intersection_world %>% 
  summarise(median = median(disparity, na.rm = T),
            mean = mean(disparity, na.rm = T),
            sd = sd(disparity, na.rm = T), 
            n = n()) %>%
  mutate(se = sd/sqrt(n),
         lower.ci = median - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = median + qt(1 - (0.05 / 2), n - 1) * se)

# country = country %>% 
#   mutate(Region = ifelse(geounit %in% g1, "Africa", 
#                          ifelse(geounit %in% g2, "Antarctica",
#                                 ifelse(geounit %in% g3, "Asia",
#                                        ifelse(geounit %in% g4, "Europe",
#                                               ifelse(geounit %in% g5, "North America",
#                                                      ifelse(geounit %in% g6, "Oceania",
#                                                             ifelse(geounit %in% g7, "South America",
#                                                                    ifelse(geounit %in% g8, "Open Ocean", "")))))))))
# 
# country = country %>% 
#   mutate(Region = ifelse(geounit %in% g1, "Africa", 
#                          ifelse(geounit %in% g2, "Americas",
#                                 ifelse(geounit %in% g3, "Antarctica",
#                                        ifelse(geounit %in% g4, "Asia",
#                                               ifelse(geounit %in% g5, "Europe",
#                                                      ifelse(geounit %in% g6, "Oceania",
#                                                             ifelse(geounit %in% g7, "Seven seas (open ocean)", "Others"))))))))

country = country %>% 
  mutate(Region = ifelse(geounit %in% g1, "1. High income: OECD", 
                         ifelse(geounit %in% g2, "2. High income: nonOECD",
                                ifelse(geounit %in% g3, "3. Upper middle income",
                                       ifelse(geounit %in% g4, "4. Lower middle income",
                                              ifelse(geounit %in% g5, "5. Low income", "Others"))))))

country_top = country %>% 
  # group_by(Region) %>% 
  top_n(11, median)

country_bottom = country %>% 
  # group_by(Region) %>% 
  top_n(-11, median)

country_sub = tbl_df(bind_rows(country_top, country_bottom))

png(paste0("/Users/ktanaka/Dropbox/PAPER climate geographic disparities/figures/fig_drafts/Rank_3_Country_without_EEZ_", anom, "_", period, ".png"), 
    height = 9, width = 18, res = 500, units = "in")

p1 = subset(country_sub, n > 0) %>% 
  mutate(geounit = fct_reorder(geounit, median)) %>% 
  ggplot() +
  geom_segment(aes(
    color = median,
    x = geounit, xend = geounit,
    y = lower.ci, yend = upper.ci),
    size = 0.1) +
  geom_text(aes(
    color = median,
    x = geounit, 
    y = median, 
    label = geounit),
    hjust = 0.5, 
    vjust = -0.5) +
  geom_point(aes(
    color = median, 
    x = geounit, 
    y = median)) +
  coord_flip() +
  geom_hline(yintercept = baseline$mean, 
             linetype="dashed", 
             color = "lightgrey", 
             size = 1) + 
  # annotate("rect",
  #          xmin = 0, xmax = length(unique(baseline$geounit)),
  #          ymin = baseline$lower.ci, ymax = baseline$upper.ci,
  #          alpha = .2) +
  scale_colour_gradient2(low = "Black", 
                         mid = "Gray", 
                         high = "Red", 
                         midpoint = baseline$mean,
                         limits = c(-max(abs(country_sub$median)), max(abs(country_sub$median))),
                         name = "") + 
  ylim(-9, 9) +
  # facet_wrap(.~ Region, scales = "free_y", ncol = 1) +
  xlab("") +
  ylab("Disparity (95% CI)") +
  theme_pubr(I(20)) + 
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(), 
        legend.justification = c(-0.1, 1), 
        legend.position = c(0, 1.05)) + 
  ggtitle("Top & Bottom 10 Countries without EEZ") + 
  annotate("text",
           x = -Inf, 
           y = Inf, 
           hjust = 1,
           vjust = -0.2, 
           label = paste0("\n Temperature Anomaly = ", anom, "\n Future Period = ", period))

p2 = subset(country, n > 0) %>% 
  mutate(geounit = fct_reorder(geounit, median)) %>% 
  ggplot() +
  geom_segment(aes(
    color = median,
    x = geounit, xend = geounit,
    y = lower.ci, yend = upper.ci),
    size = 0.1) +
  # geom_text(aes(
  #   color = median,
  #   x = geounit, 
  #   y = median, 
  #   label = geounit),
  #   hjust = 0.5, 
  #   vjust = -0.5) +
  geom_point(aes(
    color = median, 
    x = geounit, 
    y = median)) +
  coord_flip() +
  geom_hline(yintercept = baseline$mean, 
             linetype="dashed", 
             color = "lightgrey", 
             size = 1) + 
  annotate("rect",
           xmin = 0, xmax = 10,
           ymin = -9, ymax = 9,
           alpha = .1, 
           fill = "black") +
  annotate("rect",
           xmin = nrow(country)-10, xmax = nrow(country),
           ymin = -9, ymax = 9,
           alpha = .2, 
           fill = "red") +
  scale_colour_gradient2(low = "Black", 
                         mid = "Gray", 
                         high = "Red", 
                         midpoint = baseline$mean,
                         limits = c(-max(abs(country_sub$median)), max(abs(country_sub$median))),
                         name = "") + 
  ylim(-9, 9) +
  # facet_wrap(.~ Region, scales = "free_y", ncol = 1) +
  xlab("") +
  ylab("Disparity (95% CI)") +
  theme_pubr(I(20)) + 
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(), 
        legend.justification = c(0,1), 
        legend.position = "none") + 
  ggtitle("Full ranking")

gridExtra::grid.arrange(p1, p2, ncol = 2)

dev.off()

##########################
### Countries with EEZ ###
##########################

baseline <- intersection_land_eez %>% 
  summarise(median = median(disparity, na.rm = T),
            mean = mean(disparity, na.rm = T),
            sd = sd(disparity, na.rm = T), 
            n = n()) %>%
  mutate(se = sd/sqrt(n),
         lower.ci = median - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = median + qt(1 - (0.05 / 2), n - 1) * se)

eez = eez %>% 
  mutate(Region = ifelse(Country %in% g1, "1. High income: OECD", 
                         ifelse(Country %in% g2, "2. High income: nonOECD",
                                ifelse(Country %in% g3, "3. Upper middle income",
                                       ifelse(Country %in% g4, "4. Lower middle income",
                                              ifelse(Country %in% g5, "5. Low income", "Others"))))))

eez_top = eez %>% 
  # group_by(Region) %>% 
  top_n(11, median)

eez_bottom = eez %>% 
  # group_by(Region) %>% 
  top_n(-11, median)

eez_sub = tbl_df(bind_rows(eez_top, eez_bottom))

png(paste0("/Users/ktanaka/Dropbox/PAPER climate geographic disparities/figures/fig_drafts/Rank_4_Country_with_EEZ_", anom, "_", period, ".png"), 
    height = 9, width = 18, res = 500, units = "in")

p1 = subset(eez_sub, n > 0) %>% 
  mutate(Country = fct_reorder(Country, median)) %>% 
  ggplot() +
  geom_segment(aes(
    color = median,
    x = Country, xend = Country,
    y = lower.ci, yend = upper.ci),
    size = 0.1) +
  geom_text(aes(
    color = median,
    x = Country, 
    y = median, 
    label = Country),
    hjust = 0.5, 
    vjust = -0.5) +
  geom_point(aes(
    color = median, 
    x = Country, 
    y = median)) +
  coord_flip() +
  geom_hline(yintercept = baseline$median, 
             linetype="dashed", 
             color = "lightgrey", 
             size = 1) + 
  # annotate("rect",
  #          xmin = 0, xmax = length(unique(baseline$Country)),
  #          ymin = baseline$lower.ci, ymax = baseline$upper.ci,
  #          alpha = .2) +
  scale_colour_gradient2(low = "Black", 
                         mid = "Gray", 
                         high = "Red", 
                         midpoint = baseline$mean,
                         limits = c(-max(abs(eez_sub$median)), max(abs(eez_sub$median))),
                         name = "") + 
  ylim(-11, 11) +
  # facet_wrap(.~ Region, scales = "free_y", ncol = 1) +
  xlab("") +
  ylab("Disparity (95% CI)") +
  theme_pubr(I(20)) + 
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(), 
        legend.justification = c(-0.1, 1), 
        legend.position = c(0, 1.05)) + 
  ggtitle("Top & Bottom 10 Countries with EEZ") + 
  annotate("text",
           x = -Inf, 
           y = Inf, 
           hjust = 1,
           vjust = -0.2, 
           label = paste0("\n Temperature Anomaly = ", anom, "\n Future Period = ", period))

p2 = subset(eez, n > 0) %>% 
  mutate(Country = fct_reorder(Country, median)) %>% 
  ggplot() +
  geom_segment(aes(
    color = median,
    x = Country, xend = Country,
    y = lower.ci, yend = upper.ci),
    size = 0.1) +
  geom_point(aes(
    color = median, 
    x = Country, 
    y = median)) +
  annotate("rect",
           xmin = 0, xmax = 10,
           ymin = -9, ymax = 9,
           alpha = .2, 
           fill = "black") +
  annotate("rect",
           xmin = nrow(eez)-10, xmax = nrow(eez),
           ymin = -9, ymax = 9,
           alpha = .2, 
           fill = "red") +
  coord_flip() +
  geom_hline(yintercept = baseline$median, 
             linetype="dashed", 
             color = "lightgrey", 
             size = 1) + 
  scale_colour_gradient2(low = "Black", 
                         mid = "Gray", 
                         high = "Red", 
                         midpoint = baseline$mean,
                         limits = c(-max(abs(eez_sub$median)), max(abs(eez_sub$median))),
                         name = "") + 
  ylim(-11, 11) +
  xlab("") +
  ylab("Disparity (95% CI)") +
  theme_pubr(I(20)) + 
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(), 
        legend.justification = c(-0.1, 1), 
        legend.position = "none") + 
  ggtitle("Full Ranking")

gridExtra::grid.arrange(p1, p2, ncol = 2)

dev.off()


############################################
### compare country with and without EEZ ###
############################################

eez$area = "with_EEZ"
country$area = "without_EEZ"
colnames(country)[1] = "Country"
nation = rbind(country, eez)

png(paste0("/Users/ktanaka/Dropbox/PAPER climate geographic disparities/figures/fig_drafts/Rank_5_Country_with_without_EEZ_", anom, "_", period, ".png"), 
    height = 10, width = 10, res = 500, units = "in")

subset(nation, 
       # Region %in% c('1. High income: OECD') &
       # area == "with_EEZ" & 
       # area == "without_EEZ" & 
       n > 0) %>% 
  mutate(Country = fct_reorder(Country, median)) %>% 
  ggplot(aes(
    x = Country, 
    y = median, 
    color = area)) +
  geom_point(position=position_dodge(width = 0.5)) +
  geom_errorbar(
    aes(
      ymin = lower.ci,
      ymax = upper.ci),
    width = 0,
    size = 0.1,
    position=position_dodge(width = 0.5)) +
  coord_flip() +
  geom_hline(yintercept = baseline$median, 
             linetype="dashed", 
             color = "lightgrey",
             size = 1) + 
  # annotate("rect", 
  #          xmin = 0, 
  #          xmax = length(unique(baseline$Country)),
  #          ymin = baseline$lower.ci, 
  #          ymax = baseline$upper.ci,
  #          alpha = .2) +
  scale_color_manual(values = c("orange", "darkturquoise"), "") +
  # facet_wrap(.~ Region, scales = "free_y", ncol = 1) +
  ylim(-12, 12) +
  xlab("") +
  ylab("Disparity (Median with 95% CI)") +
  theme_pubr(I(20)) + 
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(), 
        legend.justification = c(-0.1, 1), 
        legend.position = c(0, 1.05)) + 
  ggtitle("Countries with and without EEZ") + 
  annotate("text",
           x = -Inf, 
           y = Inf, 
           hjust = 1,
           vjust = -0.2, 
           label = paste0("\n Temperature Anomaly = ", anom, "\n Future Period = ", period))

dev.off()

#################
### US states ###
#################

baseline <- intersection_states %>% 
  summarise(median = median(disparity, na.rm = T), 
            mean = mean(disparity, na.rm = T), 
            sd = sd(disparity, na.rm = T), 
            n = n()) %>%
  mutate(se = sd/sqrt(n),
         lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)

png(paste0("/Users/ktanaka/Dropbox/PAPER climate geographic disparities/figures/fig_drafts/Rank_6_US_States_", anom, "_", period, ".png"), 
    height = 10, width = 10, res = 500, units = "in")

subset(states, 
       # Region %in% c('1. High income: OECD') &
       # area == "with_EEZ" & 
       # area == "without_EEZ" & 
       n > 0) %>% 
  mutate(name = fct_reorder(name, median)) %>% 
  ggplot() +
  geom_segment(aes(x = name,
                   xend = name,
                   y = lower.ci,
                   yend = upper.ci,
                   color = median), size = 0.1) +
  geom_text(aes(x = name,
                y = median,
                label = name,
                color = median),
            hjust = -0.1,
            vjust = 0) +
  geom_point(aes(x = name, 
                 y = median, 
                 color = median)) +
  coord_flip() +
  geom_hline(yintercept = baseline$mean, linetype="dashed", color = "lightgrey", size = 1) + 
  annotate("rect", 
           xmin = 0, 
           xmax = length(unique(baseline$name)),
           ymin = baseline$lower.ci, 
           ymax = baseline$upper.ci,
           alpha = .2) +
  scale_colour_gradient2(low = "Black", 
                         mid = "Gray", 
                         high = "Red2", 
                         midpoint = baseline$mean,
                         limits = c(-max(abs(states$median)), max(abs(states$median))),
                         name = "") + 
  ylim(-22,22) +
  xlab("") +
  ylab("Disparity (95% CI)") +
  theme_pubr(I(20)) + 
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(), 
        legend.justification = c(-0.1, 1), 
        legend.position = c(0, 1.05)) + 
  ggtitle("US States") + 
  annotate("text",
           x = -Inf, 
           y = Inf, 
           hjust = 1,
           vjust = -0.2, 
           label = paste0("\n Temperature Anomaly = ", anom, "\n Future Period = ", period))

dev.off()

