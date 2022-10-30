

ebu50 <- dt1 %>% filter(Transect =="T5") %>%
  filter(Site %in% c("T5e2","T5e3")) %>%
  ggplot(.,)+
  geom_point(aes(x = DateTime, y = Ebu_rate, color = Site))
