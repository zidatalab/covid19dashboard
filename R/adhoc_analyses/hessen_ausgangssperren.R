ausgangssperren <- c(6631, 6531, 6533, 6535, 6632)

hessen_as <- rki %>%
  filter(IdBundesland==6) %>%
  filter(Meldedatum>=as_date("2020-09-01")-7) %>%
  group_by(Meldedatum, IdLandkreis) %>%
  summarise(AnzahlFall=sum(AnzahlFall, na.rm=TRUE)) %>%
  mutate(IdLandkreis=as.integer(IdLandkreis),
         lockdown_bund=ifelse(Meldedatum>="2020-12-17", 1, 0),
         lockdown_light=ifelse(Meldedatum>="2020-11-02", 1, 0),
         feiertag=case_when(
           Meldedatum>="2020-12-24" & Meldedatum<="2020-12-26" ~ 1,
           Meldedatum=="2020-12-31" | Meldedatum=="2021-01-01" ~ 1,
           TRUE ~ 0
         ), 
         ausgangssperre=case_when(
           IdLandkreis==6631 & Meldedatum>="2020-12-12" ~ 1,
           IdLandkreis==6531 & Meldedatum>="2020-12-13" ~ 1,
           IdLandkreis==6533 & Meldedatum>="2020-12-12" ~ 1,
           IdLandkreis==6535 & Meldedatum>="2020-12-17" ~ 1,
           IdLandkreis==6632 & Meldedatum>="2021-01-12" ~ 1,
           TRUE ~ 0
         )) %>%
  left_join(., kreise_regstat_alter %>%
              mutate(einwohnende=ag_1+ag_2+ag_3+ag_4+ag_5+ag_6,
                     id=floor(id/1000)) %>%
              select(id, einwohnende),
            by=c("IdLandkreis"="id")) %>%
  group_by(IdLandkreis) %>%
  mutate(lockdown_bund_lag7=lag(lockdown_bund, 7, 0),
         lockdown_light_lag7=lag(lockdown_light, 7, 0),
         ausgangssperre_lag7=lag(ausgangssperre, 7, 0),
         STI=round((AnzahlFall+lag(AnzahlFall, 1)+lag(AnzahlFall, 2)+lag(AnzahlFall, 3)+
                lag(AnzahlFall, 4)+lag(AnzahlFall, 5)+lag(AnzahlFall, 6))/einwohnende*100000),
         loggrowth=log1p(AnzahlFall)-log1p(lag(AnzahlFall, 7)),
         wtag=as.character(wday(Meldedatum, label=TRUE, week_start = 4)))
  
lmsti <- lm(loggrowth ~ 1 + feiertag + ausgangssperre + lockdown_light + lockdown_bund, data=hessen_as)
summary(lmsti)

ggplot(hessen_as,
       aes(x=Meldedatum, y=STI)) +
  geom_line(aes(col=factor(ausgangssperre))) +
  facet_wrap(.~factor(IdLandkreis)) +
  geom_smooth(se=FALSE)

### gesamt brd
brd <- rki %>%
  filter(Meldedatum>=as_date("2020-09-01")-7) %>%
  group_by(Meldedatum, IdLandkreis) %>%
  summarise(AnzahlFall=sum(AnzahlFall, na.rm=TRUE)) %>%
  mutate(IdLandkreis=as.integer(IdLandkreis),
         lockdown_bund=ifelse(Meldedatum>="2020-12-17", 1, 0),
         lockdown_light=ifelse(Meldedatum>="2020-11-02", 1, 0),
         feiertag=case_when(
           Meldedatum>="2020-12-24" & Meldedatum<="2020-12-26" ~ 1,
           Meldedatum=="2020-12-31" | Meldedatum=="2021-01-01" ~ 1,
           TRUE ~ 0
         )) %>%
  left_join(., kreise_regstat_alter %>%
              mutate(einwohnende=ag_1+ag_2+ag_3+ag_4+ag_5+ag_6,
                     id=floor(id/1000)) %>%
              select(id, einwohnende),
            by=c("IdLandkreis"="id")) %>%
  group_by(IdLandkreis) %>%
  mutate(lockdown_bund_lag7=lag(lockdown_bund, 7, 0),
         lockdown_light_lag7=lag(lockdown_light, 7, 0),
         STI=round((AnzahlFall+lag(AnzahlFall, 1)+lag(AnzahlFall, 2)+lag(AnzahlFall, 3)+
                      lag(AnzahlFall, 4)+lag(AnzahlFall, 5)+lag(AnzahlFall, 6))/einwohnende*100000),
         loggrowth=log1p(AnzahlFall)-log1p(lag(AnzahlFall, 7)),
         wtag=as.character(wday(Meldedatum, label=TRUE, week_start = 4)))

lmsti <- lm(loggrowth ~ 1 + feiertag + lockdown_light + lockdown_bund, data=brd)
summary(lmsti)
