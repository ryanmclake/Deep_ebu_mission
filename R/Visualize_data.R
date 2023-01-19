
#dt1 = ebullition data
#dt2 = hobo data (only to site 45)
#dt3 = turbulence from FCR sed
#dt4 = site locations
#dt5 = secchi data
#dt6 = YSI data
#dt7 = CTD data

library(tidyverse)
library(lubridate)
library(hms)

#Visual time series of 17 and 18 Ebullition data
dt1 %>% filter(Transect =="T5") %>%
  filter(Site %in% c("T5e2","T5e3")) %>%
  ggplot(.,)+
  geom_point(aes(x = DateTime, y = Ebu_rate, color = Site))


ebu_17 <- dt1 %>% filter(Transect =="T5") %>%
  filter(Site %in% c("T5e2","T5e3")) %>%
  mutate(year = lubridate::year(DateTime),
         week = lubridate::week(DateTime)) %>% 
  filter(year == "2017") %>%
  group_by(week) %>%
  summarize(Ebu_rate = mean(Ebu_rate, na.rm = F),
            Diff_rate = mean(Diff_rate, na.rm = F))

ebu_18 <- dt1 %>% filter(Transect =="T5") %>%
  filter(Site %in% c("T5e2","T5e3")) %>%
  mutate(year = lubridate::year(DateTime),
         week = lubridate::week(DateTime)) %>% 
  filter(year == "2018") %>%
  group_by(week) %>%
  summarize(Ebu_rate = mean(Ebu_rate, na.rm = F),
            Diff_rate = mean(Diff_rate, na.rm = F))

# Run a Welches t.test --> KISS
t.test(ebu_17$Ebu_rate, ebu_18$Ebu_rate, var.equal=FALSE)
### SIGNIFICANTLY DIFFERENT ### pval = 4.5 e-6

ysi_17 <- dt6 %>% filter(Reservoir == "FCR") %>% filter(Site == 50) %>% filter(Depth_m != -0.1) %>% filter(Depth_m <= 9.0) %>%
  select(DateTime, Depth_m, Temp_C, DO_mgL) %>%
  mutate(year = lubridate::year(DateTime)) %>%
  filter(year == "2017") %>%
  group_by(DateTime, Depth_m) %>%
  summarize(temp = mean(Temp_C),
            do = mean(DO_mgL)) %>%
  filter(Depth_m %in% c(0.1, 1.6, 3.8, 5, 6.2, 8, 9)) %>%
  na.omit(.)%>%
  rename(Date = DateTime)


ctd_17 <- dt7 %>% filter(Reservoir == "FCR") %>% filter(Site == 50) %>% filter(Depth_m != -0.1) %>% filter(Depth_m <= 11) %>%
  select(Date, Depth_m, Temp_C, DO_mgL) %>%
  mutate(year = lubridate::year(Date)) %>%
  filter(year == "2017") 

# filter out depths in the CTD cast that are closest to these specified values.
df.final<-data.frame()
ctd1<-ctd_17 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 0.1)))
ctd2<-ctd_17 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1.6)))
ctd3<-ctd_17 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 3.8)))
ctd4<-ctd_17 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5)))
ctd5<-ctd_17 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 6.2)))
ctd6<-ctd_17 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 8)))
ctd7<-ctd_17 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 9)))
ctd8<-ctd_17 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 11)))

# Bind each of the data layers together.
df.final = rbind(ctd1,ctd2,ctd3,ctd4,ctd5,ctd6,ctd7,ctd8)
# Re-arrange the data frame by date
ctd_17 <- arrange(df.final, Date)
# Round each extracted depth to the nearest 10th. 
ctd_17$Depth_m <- round(as.numeric(ctd_17$Depth_m), digits = 0.5)

ctd_17 <- ctd_17 %>% mutate(Depth_m = ifelse(Depth_m < 0.9, 0.1, Depth_m)) %>%
  mutate(Depth_m = ifelse(Depth_m >= 9.1, 9.5, Depth_m)) %>%
  select(-year) %>%
  rename(temp = Temp_C,
         do = DO_mgL)

temp_17 <- bind_rows(ctd_17, ysi_17) %>%
  arrange(Date, Depth_m) %>%
  mutate(Date2 = date(Date)) %>%
  group_by(Date2, Depth_m) %>%
  summarize(temp_c = mean(temp),
            DO = mean(do)) %>%
  mutate(week = lubridate::week(Date2))


# CTD and YSI 2018


ysi_18 <- dt6 %>% filter(Reservoir == "FCR") %>% filter(Site == 50) %>% filter(Depth_m != -0.1) %>% filter(Depth_m <= 9) %>%
  select(DateTime, Depth_m, Temp_C, DO_mgL) %>%
  mutate(year = lubridate::year(DateTime)) %>%
  filter(year == "2018") %>%
  group_by(DateTime, Depth_m) %>%
  summarize(temp = mean(Temp_C),
            do = mean(DO_mgL)) %>%
  filter(Depth_m %in% c(0.1, 1.6, 3.8, 5, 6.2, 8, 9)) %>%
  na.omit(.)%>%
  rename(Date = DateTime)

ggplot(ysi_18, aes(x = temp, y = -Depth_m, color = as.character(Date)))+
  geom_line()+
  labs(title = "YSI Temp Profiles")+
  theme(legend.position = "none")

ctd_18 <- dt7 %>% filter(Reservoir == "FCR") %>% filter(Site == 50) %>% filter(Depth_m != -0.1) %>% filter(Depth_m <= 9.5) %>%
  select(Date, Depth_m, Temp_C, DO_mgL) %>%
  mutate(year = lubridate::year(Date)) %>%
  filter(year == "2018") 

# filter out depths in the CTD cast that are closest to these specified values.
df.final<-data.frame()
ctd1<-ctd_18 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 0.1)))
ctd2<-ctd_18 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1.6)))
ctd3<-ctd_18 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 3.8)))
ctd4<-ctd_18 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5)))
ctd5<-ctd_18 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 6.2)))
ctd6<-ctd_18 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 8)))
ctd7<-ctd_18 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 9)))
ctd8<-ctd_18 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 11)))

# Bind each of the data layers together.
df.final = rbind(ctd1,ctd2,ctd3,ctd4,ctd5,ctd6,ctd7,ctd8)
# Re-arrange the data frame by date
ctd_18 <- arrange(df.final, Date)
# Round each extracted depth to the nearest 10th. 
ctd_18$Depth_m <- round(as.numeric(ctd_18$Depth_m), digits = 0.5)

ctd_18 <- ctd_18 %>% mutate(Depth_m = ifelse(Depth_m < 0.9, 0.1, Depth_m)) %>%
  mutate(Depth_m = ifelse(Depth_m == 9.3, 9.5, Depth_m)) %>%
  mutate(Depth_m = ifelse(Depth_m == 9.2, 9.5, Depth_m)) %>%
  select(-year) %>%
  rename(temp = Temp_C,
         do = DO_mgL)

temp_18 <- bind_rows(ctd_18, ysi_18) %>%
  arrange(Date, Depth_m) %>%
  mutate(Date2 = date(Date)) %>%
  group_by(Date2, Depth_m) %>%
  summarize(temp_c = mean(temp),
            DO = mean(do)) %>%
  mutate(week = lubridate::week(Date2))







temp_18_2 <- temp_18 %>%
  mutate(week = lubridate::week(Date2)) %>%
  group_by(week, Depth_m) %>%
  filter(Depth_m == 9.5) %>%
  summarize(temp = mean(temp_c),
            DO = mean(DO))

temp_17_2 <- temp_17 %>% ungroup(.) %>%
  mutate(week = lubridate::week(Date2)) %>%
  group_by(week, Depth_m) %>%
  filter(Depth_m == 9.5) %>%
  summarize(temp = mean(temp_c),
            DO = mean(DO))

temps <- bind_rows(temp_17, temp_18) %>%
  select(-DO, -week)


dat_17 <- temp_17_2 %>%
  filter(Depth_m == 9.5) %>%
  left_join(., ebu_17, by = "week") %>%
  left_join(., p_17_ts, by = "week") %>%
  select(week, Ebu_rate, temp, Kz, DO) %>%
  filter(week >= 23) %>%
  filter(week <= 43) %>%
  mutate(Ebu_rate = ifelse(Ebu_rate == 0, NA, Ebu_rate)) %>%
  ungroup(.) %>%
  dplyr::mutate_at(vars(c("Ebu_rate","temp","Kz", "DO")),funs(imputeTS::na_interpolation(., option = "linear"))) %>%
  mutate(year = 2017) %>%
  na.omit(.)




dat_17%>%
  ggplot(., aes(x = temp, y = Ebu_rate))+geom_point()





dat_18 <- temp_18_2 %>%
  filter(Depth_m == 9.5) %>%
  left_join(., ebu_18, by = "week") %>%
  left_join(., p_18_ts, by = "week") %>%
  select(week, Ebu_rate, temp, Kz, DO) %>%
  filter(week >= 23) %>%
  filter(week <= 43) %>%
  ungroup(.) %>%
  dplyr::mutate_at(vars(c("Ebu_rate","temp","Kz", "DO")),funs(imputeTS::na_interpolation(., option = "linear"))) %>%
  mutate(year = 2018) %>%
  na.omit(.)

dat <- bind_rows(dat_17, dat_18) %>%
  mutate(Ebu_rate = log(Ebu_rate) %>% as.vector(),
         temp = log(temp) %>% as.vector(),
         DO = log(DO) %>% as.vector(),
         Kz = log(Kz*1000000) %>% as.vector())

year_17 <- dat %>% filter(year == 2017) %>%
  mutate(ebu_lag = lag(Ebu_rate)) %>%
  filter(week >= 23) %>%
  filter(week <= 43)

year_18 <- dat %>% filter(year == 2018) %>%
  mutate(ebu_lag = lag(Ebu_rate))%>%
  filter(week >= 23) %>%
  filter(week <= 43)

tsm_17 = glm(Ebu_rate ~ ebu_lag + DO + Kz, data = year_17)
summary(tsm_17)

tsm_18 = glm(Ebu_rate ~ ebu_lag + Kz + DO, data = year_18)
summary(tsm_18)
  
year_18%>%
  ggplot(., aes(x = Kz, y = Ebu_rate))+geom_point()

year_17%>%
  ggplot(., aes(x = week, y = Ebu_rate))+geom_point()





