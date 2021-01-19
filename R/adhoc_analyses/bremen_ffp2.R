bremen_ffp2_gesamt <- rki %>%
  filter(IdBundesland%in%c(2,3,4) & grepl("SK", Landkreis, fixed = TRUE)) %>%
  filter(Meldedatum>=as_date("2020-09-01")-7) %>%
  group_by(Meldedatum, IdLandkreis) %>%
  summarise(AnzahlFall=sum(AnzahlFall, na.rm=TRUE),
            Landkreis=Landkreis[1],
            IdBundesland=IdBundesland[1]) %>%
  mutate(IdLandkreis=as.integer(IdLandkreis),
         lockdown_bund=ifelse(Meldedatum>="2020-12-17", 1, 0),
         lockdown_light=ifelse(Meldedatum>="2020-11-02", 1, 0),
         feiertag=case_when(
           Meldedatum>="2020-12-24" & Meldedatum<="2021-01-03" ~ 1,
           # Meldedatum=="2020-12-31" | Meldedatum=="2021-01-01" ~ 1,
           TRUE ~ 0
         ), 
         ffp2=case_when(
           IdBundesland==4 & Meldedatum>="2020-11-13" & Meldedatum<="2020-12-03" ~ 1,
           Meldedatum>="2020-12-15" & Meldedatum<="2021-01-06" ~ 1,
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
         ffp2_lag7=lag(ffp2, 7, 0),
         feiertag_lag7=lag(feiertag, 7, 0),
         STI=round((AnzahlFall+lag(AnzahlFall, 1)+lag(AnzahlFall, 2)+lag(AnzahlFall, 3)+
                lag(AnzahlFall, 4)+lag(AnzahlFall, 5)+lag(AnzahlFall, 6))/einwohnende*100000),
         loggrowth=log1p(AnzahlFall)-log1p(lag(AnzahlFall, 7)),
         stigrowth=log(STI)-log(lag(STI, 7)),
         wtag=as.character(wday(Meldedatum, label=TRUE, week_start = 4)))
  
lmloggrowth <- lm(loggrowth ~ 1 + feiertag + feiertag_lag7 + ffp2_lag7 + lockdown_light_lag7 + lockdown_bund_lag7,
                  data=bremen_ffp2_gesamt)
summary(lmloggrowth)
library(broom)
tidy(lmloggrowth) %>% mutate(estimate=exp(estimate))

ggplot(bremen_ffp2_gesamt,
       aes(x=Meldedatum, y=STI)) +
  geom_line(aes(col=factor(ffp2), group=1)) +
  facet_wrap(.~Landkreis) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = as_date("2020-11-02"), linetype="dashed") +
  geom_vline(xintercept = as_date("2020-12-17"), linetype="dashed") +
  scale_color_zi(name = "FFP2-Maskenausgabe", labels = c("ohne", "mit")) +
  theme_zi() +
  labs(subtitle="STI gesamt")

### bremen 60+

bremen_ffp2_60plus <- rki %>%
  filter(IdBundesland%in%c(2,3,4)) %>%
  filter(Meldedatum>=as_date("2020-09-01")-7 & grepl("SK", Landkreis, fixed = TRUE) & Altersgruppe%in%c("A60-A79", "A80+")) %>%
  group_by(Meldedatum, IdLandkreis) %>%
  summarise(AnzahlFall=sum(AnzahlFall, na.rm=TRUE),
            Landkreis=Landkreis[1],
            IdBundesland=IdBundesland[1]) %>%
  mutate(IdLandkreis=as.integer(IdLandkreis),
         lockdown_bund=ifelse(Meldedatum>="2020-12-17", 1, 0),
         lockdown_light=ifelse(Meldedatum>="2020-11-02", 1, 0),
         feiertag=case_when(
           Meldedatum>="2020-12-24" & Meldedatum<="2021-01-03" ~ 1,
           # Meldedatum=="2020-12-31" | Meldedatum=="2021-01-01" ~ 1,
           TRUE ~ 0
         ), 
         ffp2=case_when(
           IdBundesland==4 & Meldedatum>="2020-11-13" & Meldedatum<="2020-12-03" ~ 1,
           Meldedatum>="2020-12-15" & Meldedatum<="2021-01-06" ~ 1,
           TRUE ~ 0
         )) %>%
  left_join(., kreise_regstat_alter %>%
              mutate(einwohnende=ag_5+ag_6,
                     id=floor(id/1000)) %>%
              select(id, einwohnende),
            by=c("IdLandkreis"="id")) %>%
  group_by(IdLandkreis) %>%
  mutate(lockdown_bund_lag7=lag(lockdown_bund, 7, 0),
         lockdown_light_lag7=lag(lockdown_light, 7, 0),
         ffp2_lag7=lag(ffp2, 7, 0),
         feiertag_lag7=lag(feiertag, 7, 0),
         STI=round((AnzahlFall+lag(AnzahlFall, 1)+lag(AnzahlFall, 2)+lag(AnzahlFall, 3)+
                      lag(AnzahlFall, 4)+lag(AnzahlFall, 5)+lag(AnzahlFall, 6))/einwohnende*100000),
         loggrowth=log1p(AnzahlFall)-log1p(lag(AnzahlFall, 7)),
         stigrowth=log(STI)-log(lag(STI, 7)),
         wtag=as.character(wday(Meldedatum, label=TRUE, week_start = 4)))

lmloggrowth <- lm(loggrowth ~ 1 + feiertag + feiertag_lag7 + ffp2_lag7 + lockdown_light_lag7 + lockdown_bund_lag7,
                  data=bremen_ffp2_60plus)
summary(lmloggrowth)
library(broom)
tidy(lmloggrowth) %>% mutate(estimate=exp(estimate))

ggplot(bremen_ffp2_60plus,
       aes(x=Meldedatum, y=STI)) +
  geom_line(aes(col=factor(ffp2), group=1)) +
  facet_wrap(.~Landkreis) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = as_date("2020-11-02"), linetype="dashed") +
  geom_vline(xintercept = as_date("2020-12-17"), linetype="dashed") +
  scale_color_zi(name = "FFP2-Maskenausgabe", labels = c("ohne", "mit")) +
  theme_zi() +
  labs(subtitle="STI 60+")

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