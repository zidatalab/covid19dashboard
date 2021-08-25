wasistdavomrki <- rki_vacc %>% 
  select(geo, date, metric, value) %>% 
  group_by(geo, metric) %>% 
  arrange(date) %>% 
  mutate(valuediff=c(value[1], diff(value)),
         vorhanden=ifelse(valuediff==0 | is.na(value),
                          NA,
                          1)) %>% 
  ungroup() %>% 
  group_by(geo) %>% 
  filter(!is.na(vorhanden)) %>% 
  ungroup() %>%
  group_by(geo, metric) %>% 
  summarise(enddate=max(date),
            begindate=min(date))

mindate <- min(wasistdavomrki$begindate)
maxdate <- max(wasistdavomrki$enddate)

for (mygeo in unique(wasistdavomrki$geo)) {
  ggplot(wasistdavomrki %>% 
           filter(geo==mygeo) %>% 
           arrange(enddate, begindate) %>%
           mutate(metric = factor(metric, metric))) +
    geom_segment(aes(x=begindate,
                     xend=enddate,
                     y=metric, yend=metric,
                     color=metric)) +
    scale_x_date(limits = c(mindate, maxdate)) +
    theme(legend.position = "none")
  ggsave(paste0("~/Bilder/wasistdavomrki_", mygeo, ".png"))
}


