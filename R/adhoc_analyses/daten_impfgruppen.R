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
            begindate=min(date)) %>% 
  ungroup()

alle_bl_metric <- expand_grid(geo=unique(wasistdavomrki$geo),
                              metric=unique(wasistdavomrki$metric))

wasistdavomrki <- alle_bl_metric %>% 
  left_join(wasistdavomrki, by=c("geo", "metric")) %>% 
  group_by(metric) %>% 
  mutate(metricfirstdate=min(begindate, na.rm=TRUE),
         metriclastdate=max(enddate, na.rm=TRUE)) %>% 
  ungroup()

mindate <- min(wasistdavomrki$begindate, na.rm = TRUE)
maxdate <- max(wasistdavomrki$enddate, na.rm=TRUE)

for (mygeo in unique(wasistdavomrki$geo)) {
  ggplot(wasistdavomrki %>% 
           filter(geo==mygeo) %>% 
           arrange(metriclastdate, metricfirstdate) %>%
           mutate(metric = factor(metric, metric))) +
    geom_segment(aes(x=begindate,
                     xend=enddate,
                     y=metric, yend=metric,
                     color=metric)) +
    scale_x_date(limits = c(mindate, maxdate)) +
    theme(legend.position = "none")
  ggsave(paste0("~/Bilder/wasistdavomrki_", mygeo, ".png"))
}


