
#Visual time series of 17 and 18
ebu50 <- dt1 %>% filter(Transect =="T5") %>%
  filter(Site %in% c("T5e2","T5e3")) %>%
  ggplot(.,)+
  geom_point(aes(x = DateTime, y = Ebu_rate, color = Site))

ebu50

ebu_17 <- dt1 %>% 
  mutate(year = lubridate::year(DateTime)) %>% filter(year == "2017")
ebu_18 <- dt1 %>% 
  mutate(year = lubridate::year(DateTime)) %>% filter(year == "2018")


# Run a Welches t.test --> KISS
t.test(ebu_17$Ebu_rate, ebu_18$Ebu_rate, var.equal=FALSE)

