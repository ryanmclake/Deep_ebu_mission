
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
  select(DateTime, Depth_m, Temp_C) %>%
  mutate(year = lubridate::year(DateTime)) %>%
  filter(year == "2017") %>%
  group_by(DateTime, Depth_m) %>%
  summarize(temp = mean(Temp_C)) %>%
  filter(DateTime != "2017-04-24 11:22:00") %>% 
  filter(Depth_m %in% c(0.1, 1.6, 3.8, 5, 6.2, 8, 9)) %>%
  na.omit(.)%>%
  rename(Date = DateTime)
  
  ggplot(ysi_17, aes(x = temp, y = -Depth_m, color = as.character(DateTime)))+
  geom_line()+
  labs(title = "YSI Temp Profiles")+
  theme(legend.position = "none")

ctd_17 <- dt7 %>% filter(Reservoir == "FCR") %>% filter(Site == 50) %>% filter(Depth_m != -0.1) %>% filter(Depth_m <= 9.0) %>%
  select(Date, Depth_m, Temp_C) %>%
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

# Bind each of the data layers together.
df.final = rbind(ctd1,ctd2,ctd3,ctd4,ctd5,ctd6,ctd7)
# Re-arrange the data frame by date
ctd_17 <- arrange(df.final, Date)
# Round each extracted depth to the nearest 10th. 
ctd_17$Depth_m <- round(as.numeric(ctd_17$Depth_m), digits = 0.5)

ctd_17 <- ctd_17 %>% mutate(Depth_m = ifelse(Depth_m < 0.9, 0.1, Depth_m)) %>%
  select(-year) %>%
  rename(temp = Temp_C)

temp_17 <- bind_rows(ctd_17, ysi_17) %>%
  arrange(Date, Depth_m) %>%
  mutate(Date2 = date(Date)) %>%
  group_by(Date2, Depth_m) %>%
  summarize(temp_c = mean(temp))


# CTD and YSI 2018


ysi_18 <- dt6 %>% filter(Reservoir == "FCR") %>% filter(Site == 50) %>% filter(Depth_m != -0.1) %>% filter(Depth_m <= 9.0) %>%
  select(DateTime, Depth_m, Temp_C) %>%
  mutate(year = lubridate::year(DateTime)) %>%
  filter(year == "2018") %>%
  group_by(DateTime, Depth_m) %>%
  summarize(temp = mean(Temp_C)) %>%
  filter(Depth_m %in% c(0.1, 1.6, 3.8, 5, 6.2, 8, 9)) %>%
  na.omit(.)%>%
  rename(Date = DateTime)

ggplot(ysi_18, aes(x = temp, y = -Depth_m, color = as.character(Date)))+
  geom_line()+
  labs(title = "YSI Temp Profiles")+
  theme(legend.position = "none")

ctd_18 <- dt7 %>% filter(Reservoir == "FCR") %>% filter(Site == 50) %>% filter(Depth_m != -0.1) %>% filter(Depth_m <= 9.0) %>%
  select(Date, Depth_m, Temp_C) %>%
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

# Bind each of the data layers together.
df.final = rbind(ctd1,ctd2,ctd3,ctd4,ctd5,ctd6,ctd7)
# Re-arrange the data frame by date
ctd_18 <- arrange(df.final, Date)
# Round each extracted depth to the nearest 10th. 
ctd_18$Depth_m <- round(as.numeric(ctd_18$Depth_m), digits = 0.5)

ctd_18 <- ctd_18 %>% mutate(Depth_m = ifelse(Depth_m < 0.9, 0.1, Depth_m)) %>%
  select(-year) %>%
  rename(temp = Temp_C)

temp_18 <- bind_rows(ctd_18, ysi_18) %>%
  arrange(Date, Depth_m) %>%
  mutate(Date2 = date(Date)) %>%
  group_by(Date2, Depth_m) %>%
  summarize(temp_c = mean(temp))

temps <- bind_rows(temp_17, temp_18)





