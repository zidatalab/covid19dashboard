# ausgangssperren <- c(6631, 6531, 6533, 6535, 6632)

hessen_as <- rki %>%
  filter(IdBundesland==6) %>%
  filter(Meldedatum>=as_date("2020-09-01")-7) %>%
  group_by(Meldedatum, IdLandkreis) %>%
  summarise(AnzahlFall=sum(AnzahlFall, na.rm=TRUE),
            Landkreis=Landkreis[1]) %>%
  mutate(IdLandkreis=as.integer(IdLandkreis),
         lockdown_bund=ifelse(Meldedatum>="2020-12-17", 1, 0),
         lockdown_light=ifelse(Meldedatum>="2020-11-02", 1, 0),
         feiertag=case_when(
           Meldedatum>="2020-12-24" & Meldedatum<="2021-01-03" ~ 1,
           # Meldedatum=="2020-12-31" | Meldedatum=="2021-01-01" ~ 1,
           TRUE ~ 0
         ), 
         ausgangssperre=case_when(
           IdLandkreis==6413 & Meldedatum>="2020-12-12" & Meldedatum<="2021-01-05" ~ 1,
           IdLandkreis==6431 & Meldedatum>="2020-12-21" & Meldedatum<="2021-01-04" ~ 1,
           IdLandkreis==6432 & Meldedatum>="2020-12-21" & Meldedatum<="2021-01-05" ~ 1,
           IdLandkreis==6433 & Meldedatum>="2020-12-12" & Meldedatum<="2020-12-20" ~ 1,
           IdLandkreis==6435 & Meldedatum>="2020-12-12" & Meldedatum<="2021-01-14" ~ 1,
           IdLandkreis==6437 & Meldedatum>="2020-12-15" & Meldedatum<="2021-01-06" ~ 1,
           IdLandkreis==6438 & Meldedatum>="2020-12-12" & Meldedatum<="2021-01-05" ~ 1,
           IdLandkreis==6439 & Meldedatum>="2020-12-16" & Meldedatum<="2020-12-23" ~ 1,
           IdLandkreis==6440 & Meldedatum>="2020-12-15" & Meldedatum<="2021-01-05" ~ 1,
           IdLandkreis==6531 & Meldedatum>="2020-12-13" ~ 1,
           IdLandkreis==6533 & Meldedatum>="2020-12-12" ~ 1,
           IdLandkreis==6535 & Meldedatum>="2020-12-17" ~ 1,
           IdLandkreis==6631 & Meldedatum>="2020-12-12" ~ 1,
           IdLandkreis==6632 & Meldedatum>="2021-01-12" & Meldedatum<="2021-01-18" ~ 1,
           IdLandkreis==6632 & Meldedatum>="2020-12-16" & Meldedatum<="2021-01-04" ~ 1,
           IdLandkreis==6634 & Meldedatum>="2021-01-17" & Meldedatum<="2021-01-05" ~ 1,
           IdLandkreis==6635 & Meldedatum>="2020-12-21" & Meldedatum<="2021-01-04" ~ 1,
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
         feiertag_lag7=lag(feiertag, 7, 0),
         STI=round((AnzahlFall+lag(AnzahlFall, 1)+lag(AnzahlFall, 2)+lag(AnzahlFall, 3)+
                lag(AnzahlFall, 4)+lag(AnzahlFall, 5)+lag(AnzahlFall, 6))/einwohnende*100000),
         loggrowth=log1p(AnzahlFall)-log1p(lag(AnzahlFall, 7)),
         stigrowth=log(STI)-log(lag(STI, 7)),
         wtag=as.character(wday(Meldedatum, label=TRUE, week_start = 4)))
  
lmloggrowth <- lm(loggrowth ~ 1 + feiertag + feiertag_lag7  + ausgangssperre_lag7 + lockdown_light_lag7 + lockdown_bund_lag7, data=hessen_as)
summary(lmloggrowth)
library(broom)
tidy(lmloggrowth) %>% mutate(estimate=exp(estimate))
# cor(hessen_as%>%select(loggrowth, feiertag, feiertag_lag7, ausgangssperre_lag7,lockdown_bund_lag7,lockdown_light_lag7), use="complete")

ggplot(hessen_as,
       aes(x=Meldedatum, y=STI)) +
  geom_line(aes(col=factor(ausgangssperre))) +
  facet_wrap(.~factor(IdLandkreis)) +
  geom_smooth(se=FALSE)

ggplot(hessen_as,
       aes(x=Meldedatum, y=stigrowth)) +
  geom_line(aes(col=factor(ausgangssperre))) +
  facet_wrap(.~factor(IdLandkreis)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = as_date("2020-11-02"))

ggplot(hessen_as,
       aes(x=Meldedatum, y=STI)) +
  geom_line(aes(col=factor(ausgangssperre), group=1)) +
  facet_wrap(.~Landkreis) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = as_date("2020-11-02"), linetype="dashed") +
  geom_vline(xintercept = as_date("2020-12-17"), linetype="dashed") +
  scale_color_zi(name = "Ausgangssperre", labels = c("ohne", "mit")) +
  # scale_x_date(breaks = "2 month",date_labels = "%d.%m.") +
  theme_zi()

### gesamt brd
brd <- rki %>%
  filter(Meldedatum>=as_date("2020-09-01")-7) %>%
  group_by(Meldedatum, IdLandkreis) %>%
  summarise(AnzahlFall=sum(AnzahlFall, na.rm=TRUE)) %>%
  mutate(IdLandkreis=as.integer(IdLandkreis),
         lockdown_bund=ifelse(Meldedatum>="2020-12-17", 1, 0),
         lockdown_light=ifelse(Meldedatum>="2020-11-02", 1, 0),
         feiertag=case_when(
           Meldedatum>="2020-12-24" & Meldedatum<="2021-01-03" ~ 1,
           # Meldedatum=="2020-12-31" | Meldedatum=="2021-01-01" ~ 1,
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
         feiertag_lag7=lag(feiertag, 7, 0),
         STI=round((AnzahlFall+lag(AnzahlFall, 1)+lag(AnzahlFall, 2)+lag(AnzahlFall, 3)+
                      lag(AnzahlFall, 4)+lag(AnzahlFall, 5)+lag(AnzahlFall, 6))/einwohnende*100000),
         loggrowth=log1p(AnzahlFall)-log1p(lag(AnzahlFall, 7)),
         wtag=as.character(wday(Meldedatum, label=TRUE, week_start = 4)))

lmloggrowth <- lm(loggrowth ~ 1 + feiertag + feiertag_lag7 + lockdown_light_lag7 + lockdown_bund_lag7, data=brd)
summary(lmloggrowth)
library(broom)
tidy(lmloggrowth) %>% mutate(estimate=exp(estimate))

# library(lme4)
# lmmloggrowth <- lmer(loggrowth ~ 1 + feiertag + feiertag_lag7 + lockdown_light_lag7 + lockdown_bund_lag7 + (1|IdLandkreis), data=brd)
# summary(lmmloggrowth)
# library(MuMIn)
# r.squaredGLMM(lmmloggrowth)
# r.squaredGLMM(lmloggrowth)
# library(rstanarm) # ?