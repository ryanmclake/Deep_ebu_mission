

library(tidyverse)
library(lubridate)

install.packages("psych")          # Install psych package
library("psych")  

dt1_ebu <- dt1 %>% filter(Reservoir == "FCR") %>% mutate(year = lubridate::year(DateTime),
                          month = lubridate::month(DateTime)) %>%
  select(year, month, Ebu_rate, Site, Depth_m) %>%
  group_by(year) %>%
  na.omit(.) %>%
  summarize(geo_ebu_mean = geometric.mean(Ebu_rate, na.rm = T),
            ari_ebu_mean = mean(Ebu_rate, na.rm = T),
            ari_ebu_median = median(Ebu_rate, na.rm = T),
            min_ebu = min(Ebu_rate),
            max_ebu = max(Ebu_rate),
            sd_ebu = sd(Ebu_rate, na.rm = T),
            N_ebu = length(Ebu_rate),
            month_ebu = length(unique(month)),
            avg_depth = geometric.mean(Depth_m))
  
  
dt1_diff <- dt1 %>% filter(Reservoir == "FCR") %>% mutate(year = lubridate::year(DateTime),
                          month = lubridate::month(DateTime)) %>%
  select(year, month, Diff_rate, Site, Depth_m) %>%
  group_by(year) %>%
  na.omit(.) %>%
  summarize(geo_diff_mean = geometric.mean(Diff_rate, na.rm = T),
            ari_diff_mean = mean(Diff_rate, na.rm = T),
            min_diff = min(Diff_rate),
            max_diff = max(Diff_rate),
            sd_diff = sd(Diff_rate, na.rm = T),
            N_diff = length(Diff_rate),
            month_ebu = length(unique(month)),
            avg_depth = geometric.mean(Depth_m))


chaos_ar <- c(2, 63.1, 66.6, 65.4, 69.9, 62.7, 14.7)
chaos_geo <- c(1.5, 54.1, 54.6, 57.6, 59, 59.9, 14.2)

mean(chaos_ar)
geometric.mean(chaos_geo)
