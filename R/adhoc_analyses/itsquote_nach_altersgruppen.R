library(tidyverse)
library(lubridate)

busselancet_altersgruppen_hospital <- tibble("Hosp059"=2896, "Hosp6079"=1621+2158, "Hosp80"=3346)

busselancet_altersgruppen_hospital_anteile <- busselancet_altersgruppen_hospital/sum(busselancet_altersgruppen_hospital)

conn <- DBI::dbConnect(RPostgres::Postgres(),
                       host   = Sys.getenv("DBHOST"),
                       dbname = Sys.getenv("DBNAME"),
                       user      =  Sys.getenv("DBUSER"),
                       password        = Sys.getenv("DBPASSWORD"),
                       port     = 5432,
                       sslmode = 'require')
rki <- tbl(conn,"rki")
# divi_all <- tbl(conn, "divi_all") %>% collect() %>% mutate(daten_stand=as_date(daten_stand))
rki_cases_infected <- rki %>% group_by(Meldedatum,Altersgruppe) %>% 
  summarise(AnzahlFall=sum(AnzahlFall,na.rm = T), .groups="drop") %>% 
  arrange(Meldedatum, Altersgruppe) %>% collect() %>%
  mutate(Altersgruppe=str_remove_all(Altersgruppe,"A"),
         Altersgruppe=ifelse(Altersgruppe %in% c("60-79","80+"),
                             Altersgruppe,"0-59")) %>%
  group_by(Meldedatum, Altersgruppe) %>% 
  summarise("Fälle"=sum(AnzahlFall , na.rm = T), .groups="drop") %>% 
  arrange(Meldedatum, Altersgruppe) %>% 
  pivot_wider(id_cols = Meldedatum,
              names_from = Altersgruppe,
              values_from = c("Fälle"),
              values_fill = list("Fälle"=0)) %>% ungroup() %>%
  mutate(Meldedatum=lubridate::as_date(Meldedatum)) %>%
  arrange(Meldedatum) %>%
  mutate(cases059=cumsum(`0-59`),
         cases6079=cumsum(`60-79`),
         cases80=cumsum(`80+`)) %>%
  mutate(Infected059=cases059-lag(cases059,15),
         Infected6079=cases6079-lag(cases6079,15),
         Infected80=cases80-lag(cases80,15),
         Infected2=Infected059+Infected6079+Infected80) # %>%
  # filter(row_number()>15) %>%
  # # left_join(vorwarndata %>% select(date, Infected), by=c("Meldedatum"="date")) %>%
  # left_join(divi_all %>% filter(id==0) %>% select(faelle_covid_aktuell, daten_stand) %>% collect(), by=c('Meldedatum'='daten_stand')) %>%
  # mutate(icucases_lagged=lag(faelle_covid_aktuell, 8)) # %>% filter(row_number()>n()-100)
cases <- rki_cases_infected %>% filter(Meldedatum=="2020-04-20") %>%
  select(cases059, cases6079, cases80)
divi_abg <- 6785 # divi tagesbericht 20.4.2020
# verteilung_cases_20apr <- cases_20apr/sum(sapply(cases_20apr, as.integer))
busse_auf_divi <- busselancet_altersgruppen_hospital_anteile*divi_abg
hospquote <- busse_auf_divi/cases

cases <- rki_cases_infected %>% filter(Meldedatum=="2020-10-13") %>%
  select(cases059, cases6079, cases80)
divi_abg <- 18278  # divi tagesbericht 13.10.2020
# verteilung_cases_20apr <- cases_20apr/sum(sapply(cases_20apr, as.integer))
busse_auf_divi <- busselancet_altersgruppen_hospital_anteile*divi_abg
hospquote <- busse_auf_divi/cases

# icu_days <- 10.1 # aok/divi paper lancet
# share_icu <- (449+17838)/303258 # divi intensivregister and rki daily report 6. oktober 2020
# altersgruppen_bund <- tibble("unter 20"=18.4, "20 bis 40"=24.6,	"40 bis 60"=28.4,
#                              "60 bis 80"=21.7,	"80+"=6.8)/100 # destatis 2019 https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Bevoelkerungsstand/Tabellen/liste-altersgruppen.html




## with restriktor
# library(restriktor)
# its_infected_altersgruppen <- rki_divi_n_alter %>% select(Infected059, Infected6079, Infected80, faelle_covid_aktuell, Infected2, icucases_lagged) %>%
#   drop_na()
# its_infected_altersgruppen <- as.data.frame(sapply(its_infected_altersgruppen, as.integer))
# regrmodel_noage <- lm(icucases_lagged ~ 0 + Infected2, data=its_infected_altersgruppen) # 
# regrmodel_age <- lm(icucases_lagged ~ 0 + Infected059 + Infected6079 + Infected80, data=its_infected_altersgruppen) # 
# brd_timeseries <- tbl(conn,"brd_timeseries")
# vorwarndata <- brd_timeseries %>% filter(id==0) %>% collect()  %>%
#   mutate(
#     cases_rm=floor(zoo::rollmean(cases, 7, fill=NA)),
#     cases=ifelse(is.na(cases_rm), cases, cases_rm),
#     Infected=cases-lag(cases,15)) %>% # Wg. 10 Tage infektiös und symptomatisch + 5 Tage asymptomatisch
#   mutate(Rt=(cases-lag(cases,10))/lag(Infected,10)) %>% filter(!is.na(Infected) & !is.na(Rt))  %>%
#   mutate(date=date(date),
#          Neue_faelle=cases-lag(cases),
#          Neue_faelle_Anstieg = Neue_faelle/lag(Neue_faelle),
#          Vorwarnzeit= log(16000/Neue_faelle)/log(Neue_faelle_Anstieg),
#          Situation = case_when(Vorwarnzeit<0 ~ "grün",
#                                (Vorwarnzeit>18 )  ~ "orange",
#                                (Vorwarnzeit>=0 & Vorwarnzeit<18)   ~ "rot"),
#          Situation=factor(Situation,levels=c("grün","orange","rot"),ordered=T),
#          show_val=wday(date)==3) %>% filter(date>=date("2020-03-02"))
# coef(regrmodel_age)
# regrmodel_age_restr <- restriktor(regrmodel_age, constraints = ' Infected059  < 1;
#                                   Infected6079 < 1;
#                                   Infected80 < 1;
#                                   Infected059 > 0;
#                                   Infected6079 > 0;
#                                   Infected80 > 0')
# summary(regrmodel_age_restr)