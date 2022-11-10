
library(tidyverse)
library(lubridate)
library(ggplot2)
library(zoo)

#======== Kz - stability frequency =======
# calculating Kz from temperature profiles
dailyT <- read.delim(file = "daily_thermistor.txt") %>%
  mutate(Date = ymd(Date)) %>%
  pivot_wider(names_from = depth,
              values_from = temp,
              names_prefix = "wtr_")
# hourlyT <-read.delim(file = "hourly_thermistor.txt") %>%
#   mutate(datetime = ymd_hms(Hour)) %>%
#   pivot_wider(names_from = depth,
#               values_from = temp,
#               names_prefix = "wtr_") %>%
#   select(-Hour)

# equation based on eq 10 in Saloranta et al (2007) parameterisation of vertical
# diffusion and heat fluxes. Based on stability frequency as per
# Hondzo & Stefan (1993)

# K = alpha_k*(N2)^ -0.43
# alpha_k = 0.00706 *(A)^0.56 - default parameterisation during ice-free periods

#caluclate N2 - Brunt-vaisala frequency
N2_daily <- rLakeAnalyzer::ts.buoyancy.freq(dailyT, at.thermo = F, seasonal = F)
# N2_hourly <- rLakeAnalyzer::ts.buoyancy.freq(hourlyT, at.thermo = F, seasonal = F)


# alpha-k

A <- 0.031 # lake area in km2
alpha_k <- 0.00706 * (A) ^ 0.56

# set the lower limit for kz as the 
N2min = 7*10^-5
k_max <- alpha_k * (N2min) ^ -0.43



# use alpha k and n2 to calculate K at each depth
# method only valid for stability values great than N2min, so Kz cannot be 
# greater than K_max
K_daily <- N2_daily %>%
  mutate(K_1.5 = alpha_k * (N2_1.5) ^ -0.43,
         K_2.5 = alpha_k * (N2_2.5) ^ -0.43,
         K_3.5 = alpha_k * (N2_3.5) ^ -0.43,
         K_4.5 = alpha_k * (N2_4.5) ^ -0.43,
         K_5.5 = alpha_k * (N2_5.5) ^ -0.43) %>%
  mutate_at(c("K_1.5", "K_2.5", "K_3.5","K_4.5","K_5.5"),
            funs(ifelse(.>= k_max, NA, .))) %>%
  mutate_at(c("K_1.5", "K_2.5", "K_3.5","K_4.5","K_5.5"),
            funs(./10000))

# use alpha k and n2 to calculate K at each depth
# K_hourly <- N2_hourly %>%
#   mutate(K_1.5 = alpha_k * (N2_1.5) ^ -0.43,
#          K_2.5 = alpha_k * (N2_2.5) ^ -0.43,
#          K_3.5 = alpha_k * (N2_3.5) ^ -0.43,
#          K_4.5 = alpha_k * (N2_4.5) ^ -0.43,
#          K_5.5 = alpha_k * (N2_5.5) ^ -0.43) %>%
#   mutate_at(c("K_1.5", "K_2.5", "K_3.5","K_4.5","K_5.5"),
#             funs(ifelse(.>= k_max, NA, .))) %>%
#   mutate_at(c("K_1.5", "K_2.5", "K_3.5","K_4.5","K_5.5"),
#             funs(./10000))

# output of values is in cm2 s-1 (I think)
# want to convert this to m2 s-1 --> divide by 10000

ggplot(K_daily, aes(x=Date, y = K_5.5)) +
  geom_line() +
  #scale_y_log10() +
  #geom_hline(yintercept = log10(k_max), linetype = "dashed") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %y") +
  labs(x= "Date") +
  coord_cartesian(xlim = c(ymd("2019-01-01"),
                           ymd("2019-12-31"))) +
  theme_bw()


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
dailyT <- read.delim(file = "daily_thermistor.txt") %>%
  mutate(Date = ymd(Date)) %>%#,
  pivot_wider(names_from = depth,
              values_from = temp,
              names_prefix = "wtr_")

# also need bathymetry
depths <-  data.frame(depths = seq(0.5, 6.5, 0.5)) # this is the depths for the lake I looked at 
depths_boundaries <- seq(0.5, 6.5, 1) # where I calculated Kz for
depths_measurements <- seq(1, 6, 1) # the depths of the thermistors

bathy <- read.delim(file = "bathymetry for analysis.txt") %>%
  full_join(depths,., by = "depths") %>%
  arrange(depths) %>%
  
  #interpolate the areas - you may not need/want these steps Ryan!
  mutate(areas_int = ifelse(depths == 6.5, 0, 
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
  mutate_if(is.numeric, dT.dt)

# then multiply the dT_dt by the volume of the box (Vi)
# dT/dt * Vk

# function to look-up the depth volume for each column and multiply by this value
dT.dt.V <- function(column) {
  # subtract 0.5 to get the right layer, substitute() does something to get the colname
  # e.g.for the temperatures measured at 1m this requires the volume layer between 0.5 and 1.5 m
  V <- bathy$volume[which(bathy$depths == as.numeric(gsub("wtr_", 
                                                          "", 
                                                          substitute(column))) - 0.5)]
  dT_dt_V <- column * V
  return(dT_dt_V)
}

dT_dt_Vk <- dT_dt %>%
  mutate_if(is.numeric, dT.dt.V)

# for each depth you add up that layer plus the ones below
sum_dT_dt_Vk <- 
  dT_dt_Vk %>%
  select(Date, wtr_4, wtr_5, wtr_6) %>%
  # writing over the columns but should be okay because don't then use
  # that column in the next mutate
  mutate(wtr_4 = wtr_4 + wtr_5 + wtr_6, # box = 4 m
         wtr_5 = wtr_5 + wtr_6,         # box = 5 m
         wtr_6 = wtr_6)                # box = 6 m

#=========================================#
#### bottom of the equation ####
# temperature gradient between the boxes
# gradient between each layer divided by the height of the layer (1m)
Gi <- data.frame(date = dailyT$Date,
                 
                 # dailyT[,2:6] is wtr_1:wtr_5
                 # dailyT[,3:7] is wtr_2:wtr_6
                 # so the calc is 1 - 2; 2 - 3; 3 - 4 etc..
                 (dailyT[,2:6] - dailyT[,3:7])/1) # then divide by height of layer (ie 1m)

# use the boundary values as column names
# only 1.5 - 5.5m
colnames(Gi) <- c("Date", paste0("wtr_", depths_boundaries[-c(1,7)])) 

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
  select(Date, wtr_3.5, wtr_4.5, wtr_5.5)

#=====================================#

# final calculation of Kz using the gradient heatflux method 
# kz = sum_dT_dt_V / Gi_Ai

Kz_ghf <- data.frame(Date = Gi_Ai$Date,
                     #calculate Kz for each layer [-date]
                     sum_dT_dt_Vk[,-1] / Gi_Ai[,-1])

colnames(Kz_ghf) <- c("Date", paste0("Kz_", gsub("wtr_", "", colnames(Gi_Ai[-1]))))

# check the output
Kz_ghf %>%
  pivot_longer(cols = Kz_3.5:Kz_5.5,
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
Kz_ghf %>%
  pivot_longer(cols = Kz_3.5:Kz_5.5,
               names_to = "depth",
               names_prefix = "Kz_", 
               values_to = "Kz") %>%
  ggplot(.) +
  geom_point(aes(x=Date, y= log10(Kz), colour = depth)) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b%y") +
  theme(axis.text.x = element_text(angle = 90))

# write the Kz values
write.table(Kz_ghf, sep = "\t", row.names = F, quote = F,
            file = "Kz (gradient heat flux method).txt")
#============================================#