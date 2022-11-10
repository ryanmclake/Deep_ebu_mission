
library(tidyverse)
library(lubridate)
library(ggplot2)
library(zoo)
se <- function(x) sd(x) / sqrt(length(x))
#=============================================#

#======== Kz calculation - gradient flux method =====
# this method of estimating Kz uses water temperature profiles and
# applies the gradient-flux method

# Kz = sum(ak * Vk)/ Gi * Ai

# top of the equation is the temporal increase in temperature * volume in the box
# for each box below the specified depth e.g. for Kz at 4m it is the sum 
# of the ak* volume for 4-5 and 5-6m... etc.

# ak = dT/dt in each box below k=1
# V = volume of box i

# Gi = vertical temperature gradient at the upper boundary of box i
# Gi = changeT(i-1) / changeD
# Ai is area at upper boundary of box i

#  daily temperature profiles, wide format
dailyT <- temps %>% rename(Date = Date2, depth = Depth_m, temp = temp_c) %>%
  mutate(depth = ifelse(depth == 0.1, 0, depth)) %>%
  pivot_wider(names_from = depth,
              values_from = temp,
              names_prefix = "wtr_") %>%
  select(-wtr_3.3, -wtr_6.3, -wtr_8.9, -wtr_5.3) %>%
  na.omit(.)

# also need bathymetry
depths <-  data.frame(depths = seq(0, 10, 0.1)) # this is the depths for the lake I looked at 
depths_boundaries <- seq(0, 10, 0.1) # where I calculated Kz for
depths_measurements <- c(0,1.6,3.8,5,6.2,8,9) # the depths of the thermistors

bathy <- read.delim(file = "bathymetry for analysis.txt") %>%
  full_join(depths,., by = "depths") %>%
  arrange(depths) %>%
  
  #interpolate the areas - you may not need/want these steps Ryan!
  mutate(areas_int = ifelse(depths == 9, 0, 
                            na.approx(areas, na.rm = F))) %>%
  # then extract the boxes needed anc calculate the volume based on these areas
  filter(depths %in% depths_boundaries) %>%
  # calculate the volume of each of these trapeziums
  # (a+b)/2 * h
  mutate(volume = ((lead(areas_int) + areas_int)/2) * (lead(depths) - depths))

# ==== top of the equation ====

# calculate ak = dT/dt

#  change from previous value
# for daily values divide by number of s in a day = 86400
dT.dt <- function(var) {
  abs_change <-  var - lag(var)
  # divide by number of seconds in a day
  dT_dt <- abs_change/86400
  
  return(dT_dt)
}

# calculate dT/dt for the water temperatures (numeric)
dT_dt <- dailyT %>%
  ungroup(.) %>%
  mutate_if(is.numeric, dT.dt)

# then multiply the dT_dt by the volume of the box (Vi)
# dT/dt * Vk

# function to look-up the depth volume for each column and multiply by this value
dT.dt.V <- function(column) {
  # subtract 0.5 to get the right layer, substitute() does something to get the colname
  # e.g.for the temperatures measured at 1m this requires the volume layer between 0.5 and 1.5 m
  V <- bathy$volume[which(bathy$depths == as.numeric(gsub("wtr_", 
                                                          "", 
                                                          substitute(column))))]
  dT_dt_V <- column * V
  return(dT_dt_V)
}

dT_dt_Vk <- dT_dt %>%
  mutate(wtr_0 = dT.dt.V(wtr_0),
         wtr_1.6 = dT.dt.V(wtr_1.6),
         wtr_3.8 = wtr_3.8*5976.76214,
         wtr_5 = dT.dt.V(wtr_5),
         wtr_6.2 = dT.dt.V(wtr_6.2),
         wtr_8 = dT.dt.V(wtr_8),
         wtr_9 = dT.dt.V(wtr_9))

# for each depth you add up that layer plus the ones below
sum_dT_dt_Vk <- 
  dT_dt_Vk %>%
  select(Date, wtr_6.2, wtr_8) %>%
  # writing over the columns but should be okay because don't then use
  # that column in the next mutate
  mutate(wtr_6.2 = wtr_6.2 + wtr_8, # box = 4 m
         wtr_8 = wtr_8)                # box = 6 m

#=========================================#
#### bottom of the equation ####
# temperature gradient between the boxes
# gradient between each layer divided by the height of the layer (1m)
# Gi <- data.frame(date = dailyT$Date,
#                  
#                  # dailyT[,2:6] is wtr_1:wtr_5
#                  # dailyT[,3:7] is wtr_2:wtr_6
#                  # so the calc is 1 - 2; 2 - 3; 3 - 4 etc..
#                  (dailyT[,2:7] - dailyT[,3:8])/1) # then divide by height of layer (ie 1m)

Gi <- dailyT %>% mutate(wtr_0 = wtr_0-wtr_1.6/1.6,
                        wtr_1.6 = wtr_1.6-wtr_3.8/2.2,
                        wtr_3.8 = wtr_3.8-wtr_5/1.2,
                        wtr_5 = wtr_5-wtr_6.2/1.2,
                        wtr_6.2 = wtr_6.2-wtr_8/1.8,
                        wtr_8 = wtr_8-wtr_9/1) %>%
  select(-wtr_9)

# use the boundary values as column names
# only 1.5 - 5.5m
# multply the temperature gradient at the boundary by the area of that boundary
Gi.Ai <- function(column) {
  # get the area for the right boundary
  A <- bathy$areas_int[which(bathy$depths == gsub("wtr_", "", substitute(column)))]
  Gi_Ai <- column * A
  return(Gi_Ai)
}

# areas at boundaries
Gi_Ai <- Gi %>%
  as_tibble() %>%
  mutate_if(is.numeric, Gi.Ai) %>%
  # only valid for the deeper layers
  select(Date, wtr_6.2, wtr_8)

#=====================================#

# final calculation of Kz using the gradient heatflux method 
# kz = sum_dT_dt_V / Gi_Ai

Kz_ghf <- data.frame(Date = Gi_Ai$Date,
                     #calculate Kz for each layer [-date]
                     sum_dT_dt_Vk[,-1] / Gi_Ai[,-1])

colnames(Kz_ghf) <- c("Date", paste0("Kz_", gsub("wtr_", "", colnames(Gi_Ai[-1]))))

# check the output
Kz_ghf %>%
  pivot_longer(cols = Kz_6.2:Kz_8,
               names_to = "depth",
               names_prefix = "Kz_", 
               values_to = "Kz") %>%
  ggplot(.) +
  geom_point(aes(x=Date, y= log10(Kz), colour = depth)) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b%y") +
  theme(axis.text.x = element_text(angle = 90))

# Kz values cannot be less than the rate of molecular diffusion
rate_diffusion <- 1.4e-7

Kz_ghf <- Kz_ghf %>%
  # if value is less than molecular diffusion set to rate of diffusion
  mutate_if(is.numeric, 
            ~ifelse(. < rate_diffusion, # value is < rate of diffusion
                    rate_diffusion,
                    .)) 
# check the output
p <- Kz_ghf %>%
  select(-Kz_6.2) %>%
  mutate(year = year(Date)) %>%
  mutate(month = month(Date)) %>%
  mutate(week = week(Date)) %>%
  ungroup(.) %>%
  mutate(Kz_8 = ifelse(is.infinite(Kz_8),NA, Kz_8),
         Kz_8 = ifelse(Kz_8 <= 1.4e-7, NA, Kz_8)) %>%
  dplyr::mutate_at(vars(Kz_8),funs(imputeTS::na_interpolation(., option = "linear"))) %>%
  filter(month %in% c("5","6","7","8","9","10")) %>%
  pivot_longer(cols = Kz_8,
               names_to = "depth",
               names_prefix = "Kz_", 
               values_to = "Kz")

p_17_ts <- p %>% filter(year == "2017") %>%
  group_by(week, year) %>%
  summarize(Kz = mean(Kz, na.rm = F)) %>%
  ggplot(., aes(x = week, y = log(Kz)))+
  geom_line()+
  geom_point(color = "dodgerblue4", size = 5)+
  labs(title = "2017 Kz 8-9m")

p_18_ts <- p %>% filter(year == "2018") %>%
  group_by(week, year) %>%
  summarize(Kz = mean(Kz, na.rm = F))%>%
  ggplot(., aes(x = week, y = log(Kz)))+
  geom_line()+
  geom_point(color = "dodgerblue4", size = 5)+
  labs(title = "2018 Kz 8-9m")

p_box <- p %>% 
  group_by(week, year) %>%
  summarize(Kz = mean(Kz, na.rm = F))%>%
  ggplot(., aes(x = week, y = log(Kz), group = year))+
  geom_boxplot(aes(fill = as.character(year)))



# write the Kz values
write.table(Kz_ghf, sep = "\t", row.names = F, quote = F,
            file = "Kz (gradient heat flux method).txt")

#============================================#