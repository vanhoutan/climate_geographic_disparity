library(ggplot2)
library(dplyr)
library(sf)
library(forcats)
library(ggpubr)

load("~/Desktop/climate/climate_disp_2019-master/data_output/intersection_result_rcp85_untrimmed.RData")

intersection_biome$disparity = scale(intersection_biome$disparity)
intersection_realm$disparity = scale(intersection_realm$disparity)
intersection_states$disparity = scale(intersection_states$disparity)
intersection_world$disparity = scale(intersection_world$disparity)
intersection_land_eez$disparity = scale(intersection_land_eez$disparity)
earth$disparity = scale(earth$disparity)

#just some convenient category for countries
g1 = names(table(subset(intersection_world, continent == "Africa")$geounit))
g2 = names(table(subset(intersection_world, continent == "Antarctica")$geounit))
g3 = names(table(subset(intersection_world, continent == "Asia")$geounit))
g4 = names(table(subset(intersection_world, continent == "Europe")$geounit))
g5 = names(table(subset(intersection_world, continent == "North America")$geounit))
g6 = names(table(subset(intersection_world, continent == "Oceania")$geounit))
g7 = names(table(subset(intersection_world, continent == "South America")$geounit))
g8 = names(table(subset(intersection_world, continent == "Seven seas (open ocean)")$geounit))

#just some convenient category for countries
g1 = names(table(subset(intersection_world, continent == "Africa")$geounit))
g2 = names(table(subset(intersection_world, continent == "Americas")$geounit))
g3 = names(table(subset(intersection_world, continent == "Antarctica")$geounit))
g4 = names(table(subset(intersection_world, continent == "Asia")$geounit))
g5 = names(table(subset(intersection_world, continent == "Europe")$geounit))
g6 = names(table(subset(intersection_world, continent == "Oceania")$geounit))
g7 = names(table(subset(intersection_world, continent == "Seven seas (open ocean)")$geounit))

#just some convenient category for countries
g1 = names(table(subset(intersection_world, income_grp == "1. High income: OECD")$geounit))
g2 = names(table(subset(intersection_world, income_grp == "2. High income: nonOECD")$geounit))
g3 = names(table(subset(intersection_world, income_grp == "3. Upper middle income")$geounit))
g4 = names(table(subset(intersection_world, income_grp == "4. Lower middle income")$geounit))
g5 = names(table(subset(intersection_world, income_grp == "5. Low income")$geounit))

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

biome <- biome %>% st_set_geometry(NULL) # drop geometry
realm <- realm %>% st_set_geometry(NULL) # drop geometry
earth <- earth %>% st_set_geometry(NULL) # drop geometry
states <- states %>% st_set_geometry(NULL) # drop geometry
subr <- subr %>% st_set_geometry(NULL) # drop geometry
country <- country %>% st_set_geometry(NULL) # drop geometry
eez <- eez %>% st_set_geometry(NULL) # drop geometry

# ecoregions
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

ecoregion %>% 
  mutate(BIOME = fct_reorder(BIOME, median)) %>% 
  ggplot() +
  geom_segment(aes(x = BIOME, 
                   xend = BIOME, 
                   y = lower.ci, 
                   yend = upper.ci, 
                   color = Ecoregion), size = 1) + 
  geom_text(aes(x = BIOME, 
                y = median, 
                label = BIOME),
            hjust = -0.05, 
            vjust = -0.5) + 
  geom_hline(yintercept = baseline$mean, 
             linetype = "dashed", 
             color = "lightgrey", 
             size = 1) + 
  annotate("rect", 
           xmin = 0, 
           xmax = length(unique(baseline$BIOME)),
           ymin = baseline$lower.ci, 
           ymax = baseline$upper.ci,
           alpha = .2) +
  geom_point(aes(x = BIOME, 
                 y = median, 
                 color = Ecoregion)) +
  scale_color_manual(values = c("orange", "darkturquoise")) +
  coord_flip() +
  # facet_wrap(.~Ecoregion, scales = "free_y") +
  ylim(max(abs(ecoregion$median))*-1,max(abs(ecoregion$median))) + 
  xlab("") +
  ylab("Disparity (Median with 95% CI)") +
  theme_pubr() + 
  theme(axis.text.y = element_blank(),
        legend.justification = c(0, 1), 
        legend.position = c(0, 1))

# Terrestorial biome
baseline <- intersection_biome %>% 
  summarise(median = median(disparity, na.rm = T),
            mean = mean(disparity, na.rm = T),
            sd = sd(disparity, na.rm = T), 
            n = n()) %>%
  mutate(se = sd/sqrt(n),
         lower.ci = median - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = median + qt(1 - (0.05 / 2), n - 1) * se)

biome %>% 
  mutate(BIOME = fct_reorder(BIOME, median)) %>% 
  ggplot() +
  geom_segment(aes(x = BIOME, 
                   xend = BIOME, 
                   y = lower.ci, 
                   yend = upper.ci, 
                   color = Ecoregion), size = 1) + 
  geom_text(aes(x = BIOME, 
                y = median, 
                label = BIOME),
            hjust = -0.05, 
            vjust = -0.5) + 
  geom_hline(yintercept = baseline$mean, 
             linetype="dashed", 
             color = "lightgrey", size = 1) + 
  annotate("rect", 
           xmin = 0, 
           xmax = length(unique(baseline$BIOME)),
           ymin = baseline$lower.ci, 
           ymax = baseline$upper.ci,
           alpha = .2) +
  geom_point(aes(x = BIOME, y = median, color = Ecoregion)) +
  scale_color_manual(values = c("orange")) +
  coord_flip() +
  ylim(max(abs(biome$median))*-1,max(abs(biome$median))) + 
  xlab("") +
  ylab("Disparity (Median with 95% CI)") +
  theme_pubr() + 
  theme(axis.text.y = element_blank(),
        legend.justification = c(0,1), 
        legend.position = c(0,1))

# Marine realm
baseline <- intersection_realm %>% 
  summarise(median = median(disparity, na.rm = T),
            mean = mean(disparity, na.rm = T),
            sd = sd(disparity, na.rm = T), 
            n = n()) %>%
  mutate(se = sd/sqrt(n),
         lower.ci = median - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = median + qt(1 - (0.05 / 2), n - 1) * se)

realm %>% 
  mutate(BIOME = fct_reorder(BIOME, median)) %>% 
  ggplot() +
  geom_segment(aes(x = BIOME, 
                   xend = BIOME, 
                   y = lower.ci, 
                   yend = upper.ci,
                   color = median),
               size = 1) + 
  geom_text(aes(x = BIOME, 
                y = median, 
                label = BIOME,
                color = median),
            hjust = -1,
            vjust = 0.3) +
  geom_point(aes(x = BIOME, 
                 y = median, 
                 color = median)) +
  coord_flip() +
  geom_hline(yintercept = baseline$mean, linetype="dashed", color = "lightgrey", size = 1) + 
  annotate("rect", 
           xmin = 0, 
           xmax = length(unique(baseline$median)),
           ymin = baseline$lower.ci, 
           ymax = baseline$upper.ci,
           alpha = .2) +
  scale_colour_gradient2(low = "Black",
                         mid = "Gray",
                         high = "Red2",
                         midpoint = baseline$median,
                         name = "",
                         limits = c(-max(abs(realm$median)), max(abs(realm$median)))) +
  ylim(-3,3) +
  xlab("") +
  ylab("Normalized disparity (95% CI)") +
  theme_pubr() +
  theme(
    axis.text.y = element_blank(),
    legend.justification = c(0,1), 
    legend.position = c(0,1))

# Global Sub Regions
baseline <- intersection_world %>% 
  summarise(median = median(disparity, na.rm = T),
            mean = mean(disparity, na.rm = T),
            sd = sd(disparity, na.rm = T), 
            n = n()) %>%
  mutate(se = sd/sqrt(n),
         lower.ci = median - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = median + qt(1 - (0.05 / 2), n - 1) * se)

subr %>% 
  mutate(subregion = fct_reorder(subregion, median)) %>% 
  ggplot() +
  geom_segment(aes(x = subregion, 
                   xend = subregion, 
                   y = lower.ci, 
                   yend = upper.ci,
                   color = median), size = 1) + 
  geom_text(aes(x = subregion, 
                y = median, 
                label = subr$subregion,
                color = median),
            hjust = -1,
            vjust = 0.3) +
  geom_point(aes(x = subregion, 
                 y = median, 
                 color = median)) +
  coord_flip() +
  geom_hline(yintercept = baseline$median, linetype="dashed", color = "lightgrey", size = 1) + 
  annotate("rect", 
           xmin = 0, 
           xmax = length(unique(baseline$subregion)),
           ymin = baseline$lower.ci, 
           ymax = baseline$upper.ci,
           alpha = .2) +
  scale_colour_gradient2(low = "Black", 
                         mid = "Gray", 
                         high = "Red2", 
                         midpoint = baseline$median, 
                         name = "", 
                         limits = c(-max(abs(subr$median)), max(abs(subr$median)))) + 
  ylim(-3,3) +
  xlab("") +
  ylab("Normalized disparity (95% CI)") +
  theme_pubr() +
  theme(
    axis.text.y = element_blank(),
    legend.justification = c(0,1), 
    legend.position = c(0,1))

# Countries
baseline <- intersection_world %>% 
  summarise(median = median(disparity, na.rm = T), 
            sd = sd(disparity, na.rm = T), 
            n = n()) %>%
  mutate(se = sd/sqrt(n),
         lower.ci = median - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = median + qt(1 - (0.05 / 2), n - 1) * se)

country = country %>% 
  mutate(Region = ifelse(geounit %in% g1, "Africa", 
                         ifelse(geounit %in% g2, "Antarctica",
                                ifelse(geounit %in% g3, "Asia",
                                       ifelse(geounit %in% g4, "Europe",
                                              ifelse(geounit %in% g5, "North America",
                                                     ifelse(geounit %in% g6, "Oceania",
                                                            ifelse(geounit %in% g7, "South America",
                                                                   ifelse(geounit %in% g8, "Open Ocean", "")))))))))

country = country %>% 
  mutate(Region = ifelse(geounit %in% g1, "Africa", 
                         ifelse(geounit %in% g2, "Americas",
                                ifelse(geounit %in% g3, "Antarctica",
                                       ifelse(geounit %in% g4, "Asia",
                                              ifelse(geounit %in% g5, "Europe",
                                                     ifelse(geounit %in% g6, "Oceania",
                                                            ifelse(geounit %in% g7, "Seven seas (open ocean)", "Others"))))))))

country = country %>% 
  mutate(Region = ifelse(geounit %in% g1, "1. High income: OECD", 
                         ifelse(geounit %in% g2, "2. High income: nonOECD",
                                ifelse(geounit %in% g3, "3. Upper middle income",
                                       ifelse(geounit %in% g4, "4. Lower middle income",
                                              ifelse(geounit %in% g5, "5. Low income", "Others"))))))
country %>% 
  mutate(geounit = fct_reorder(geounit, median)) %>% 
  ggplot() +
  geom_segment(aes(x = geounit, 
                   xend = geounit, 
                   y = lower.ci, 
                   yend = upper.ci, 
                   color = Region), size = 1) + 
  geom_text(aes(x = geounit, 
                y = median, 
                label = country$geounit, 
                color = Region),
            hjust = -0.5, 
            vjust = -0.5) + 
  geom_hline(yintercept = baseline$median, 
             linetype="dashed", 
             color = "lightgrey", size = 1) + 
  annotate("rect", 
           xmin = 0, 
           xmax = length(unique(baseline$geounit)),
           ymin = baseline$lower.ci, 
           ymax = baseline$upper.ci,
           alpha = .2) +
  geom_point(aes(x = geounit, y = median, color = Region)) +
  scale_color_manual(values = pals::parula(length(unique(country$Region)))) +
  coord_flip() +
  ylim(max(abs(country$median))*-1,max(abs(country$median))) + 
  xlab("") +
  ylab("Disparity (95% CI)") +
  theme_pubr() + 
  facet_wrap(.~ Region, scales = "free_y", ncol = 1) +
  theme(
        # legend.justification = c(1,0), 
        # legend.position = c(1,0),
        legend.position = "none",
    axis.text.y = element_blank())

# Countries with EEZ
baseline <- intersection_land_eez %>% 
  summarise(median = median(disparity, na.rm = T), 
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

eez %>% 
  mutate(Country = fct_reorder(Country, median)) %>% 
  ggplot() +
  geom_segment(aes(x = Country, 
                   xend = Country, 
                   y = lower.ci, 
                   yend = upper.ci, 
                   color = Region), 
               size = 1) + 
  geom_text(aes(x = Country, 
                y = median, 
                label = eez$Country),
            hjust = -0.05, 
            vjust = -0.5) + 
  geom_hline(yintercept = baseline$median, 
             linetype="dashed", 
             color = "lightgrey", 
             size = 1) + 
  annotate("rect", 
           xmin = 0, 
           xmax = length(unique(baseline$Country)),
           ymin = baseline$lower.ci, 
           ymax = baseline$upper.ci,
           alpha = .2) +
  geom_point(aes(x = Country, y = median, color = Region)) +
  scale_color_manual(values = pals::parula(length(unique(eez$Region)))) +
  coord_flip() +
  xlab("") +
  ylab("Disparity (Median with 95% CI)") +
  theme_pubr() +
  facet_wrap(.~ Region, scales = "free_y", ncol = 1) +
  theme(
    # legend.justification = c(1,0), 
    # legend.position = c(1,0),
    legend.position = "none",
    axis.text.y = element_blank())

eez$area = "with_EEZ"
country$area = "without_EEZ"
colnames(country)[1] = "Country"
nation = rbind(country, eez)

nation %>% 
  mutate(Country = fct_reorder(Country, median)) %>% 
  ggplot() +
  geom_segment(aes(x = Country, 
                   xend = Country, 
                   y = lower.ci, 
                   yend = upper.ci, 
                   color = area), size = 1) + 
  geom_hline(yintercept = baseline$median, 
             linetype="dashed", 
             color = "lightgrey",
             size = 1) + 
  # geom_text(aes(x = Country,
  #               y = median,
  #               label = nation$Country),
  #           hjust = -0.3,
  #           vjust = 0) +
  annotate("rect", 
           xmin = 0, 
           xmax = length(unique(baseline$Country)),
           ymin = baseline$lower.ci, 
           ymax = baseline$upper.ci,
           alpha = .2) +
  geom_point(aes(x = Country, y = median, color = area)) + 
  geom_jitter(aes(x = Country, y = median, color = area), width = 1) + 
  scale_color_manual(values = c("orange", "darkturquoise")) +
  coord_flip() +
  xlab("") +
  ylim(-5, 5) + 
  ylab("Disparity (Median with 95% CI)") +
  theme_pubr() + 
  # facet_wrap(.~ area) + 
  theme(
    # axis.text.y = element_blank(),
        legend.justification = c(0,1), 
        legend.position = c(0,1))

#States
baseline <- intersection_states %>% 
  summarise(median = median(disparity, na.rm = T), 
            mean = mean(disparity, na.rm = T), 
            sd = sd(disparity, na.rm = T), 
            n = n()) %>%
  mutate(se = sd/sqrt(n),
         lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)

states %>% 
  mutate(name = fct_reorder(name, median)) %>% 
  ggplot() +
  geom_segment(aes(x = name, 
                   xend = name, 
                   y = lower.ci, 
                   yend = upper.ci,
                   color = median), size = 1) + 
  geom_text(aes(x = name,
                y = median,
                label = name,
                color = median),
            hjust = -1,
            vjust = 0.3) +
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
                         midpoint = baseline$median, 
                         name = "", 
                         limits = c(-max(abs(states$median)), max(abs(states$median)))) + 
  ylim(-3,3) +
  xlab("") +
  ylab("Normalized disparity (95% CI)") +
  theme_pubr() +
  theme(
    axis.text.y = element_blank(),
    legend.justification = c(0,1), 
    legend.position = c(0,1))
