##### Packages
library(DT)
library(DBI)
library(forcats)
library(EpiEstim)
library(plotly)
library(zicolors)
library(deSolve)
library(jsonlite)
library(readxl)
library(data.table)
library(dplyr)
library(glue)
library(lubridate)
library(tidyverse)
library(tidyr)
library(stringr)
library(ggplot2)
library(dtplyr)
library(zoo)
require(ISOcodes)
library(openxlsx)
library(patchwork)

## load rki data
conn <- DBI::dbConnect(RPostgres::Postgres(),
                       host   = Sys.getenv("DBHOST"),
                       dbname = Sys.getenv("DBNAME"),
                       user      =  Sys.getenv("DBUSER"),
                       password        = Sys.getenv("DBPASSWORD"),
                       port     = 5432,
                       sslmode = 'require')
rki <- tbl(conn,"rki") %>% collect()

## read all xlsx

datumsliste <- as_date("2021-03-24")+7*0:14

filenames <- list.files("data/klinische_aspekte_old", pattern="*.xlsx", full.names=TRUE)
hospitalisierung_faelle <- c(lapply(filenames[1:3], 
                                    read.xlsx,
                                    sheet=2, 
                                    startRow=1),
                             lapply(filenames[4:8], 
                                    read.xlsx,
                                    sheet=2, 
                                    startRow=2),
                             lapply(filenames[9:11], 
                                    read.xlsx,
                                    sheet=2, 
                                    startRow=5),
                             lapply(filenames[12:14], 
                                    read.xlsx,
                                    sheet=2, 
                                    startRow=7),
                             lapply(filenames[15], 
                                    read.xlsx,
                                    sheet=2, 
                                    startRow=8))

hospitalisierung_angabe <- lapply(filenames[1:15], 
                                    read.xlsx,
                                    sheet=1, 
                                    startRow=2)

## vergleich nachmeldungen

nl <- length(hospitalisierung_faelle)
agg_hospfaelle <- tibble()
for (i in seq(nl)) {
  hfaelle_i <- hospitalisierung_faelle[[i]]
  if (i<=3) {
    hfaelle_i <- hfaelle_i %>% 
      rename(`A00..04`=`0.-.4.Jährige`,
             `A05..14`=`5.-.14.Jährige`,
             `A15..34`=`15.-.34.Jährige`,
             `A35..59`=`35.-.59.Jährige`,
             `A60..79`=`60.-.79.Jährige`,
             `A80+`=`80+.Jährige`)
  }
  agg_hospfaelle <- bind_rows(agg_hospfaelle,
                              hfaelle_i %>% 
                                mutate(Meldejahr=as.double(Meldejahr),
                                       Meldewoche=as.double(Meldewoche)) %>% 
                                mutate(alle_ag_sum=rowSums(across(`A00..04`:`A80+`), na.rm = TRUE)) %>% 
                                select(Meldejahr, Meldewoche, alle_ag_sum) %>% 
                                mutate(pubdate=datumsliste[i],
                                       pubJahrKW=year(pubdate)*100+isoweek(pubdate)-1,
                                       JahrKW=Meldejahr*100+Meldewoche)
                              )
}

agg_hospfaelle_delay <- agg_hospfaelle %>% 
  select(-Meldejahr, -Meldewoche, -pubdate) %>% 
  filter(JahrKW>202100 & JahrKW>=min(pubJahrKW) & JahrKW<=max(pubJahrKW)-5) %>%
  group_by(JahrKW) %>%
  arrange(pubJahrKW) %>% 
  mutate(gemeldet6weeks=alle_ag_sum/max(alle_ag_sum[pubJahrKW<=JahrKW+5]),
         weekdelay=pubJahrKW-JahrKW+1)
  
  # pivot_wider(id_cols=JahrKW, names_from = pubJahrKW, values_from = alle_ag_sum)

ggplot(agg_hospfaelle_delay %>% 
         filter(weekdelay<=6), aes(x=weekdelay, y=gemeldet6weeks, group=weekdelay)) +
  geom_boxplot() +
  ylim(0, 1)
  # geom_line()

wdelay <- 2 # so viele weglassen

## vergleich anteil mit angabe

nl <- length(hospitalisierung_angabe)
agg_hospangabe <- tibble()
for (i in seq(nl)) {
  agg_hospangabe <- bind_rows(agg_hospangabe,
                              hospitalisierung_angabe[[i]] %>% 
                                mutate(Meldejahr=as.double(Meldejahr),
                                       Meldewoche=as.double(MW)) %>% 
                                select(Meldejahr, Meldewoche, `Fälle.gesamt`, `Anzahl.mit.Angaben.zur.Hospitalisierung`) %>% 
                                mutate(pubdate=datumsliste[i],
                                       pubJahrKW=year(pubdate)*100+isoweek(pubdate)-1,
                                       JahrKW=Meldejahr*100+Meldewoche) %>% 
                                filter(JahrKW==pubJahrKW-wdelay)
  )
}
agg_hospangabe <- agg_hospangabe %>% 
  mutate(AnteilmitAngabe=`Anzahl.mit.Angaben.zur.Hospitalisierung`/`Fälle.gesamt`)

ggplot(agg_hospangabe, aes(x=Meldewoche, y=AnteilmitAngabe)) +
  geom_bar(stat="identity") +
  ylim(0, 1)

## anteil AG hosp. an fällen jeweils

nl <- length(hospitalisierung_faelle)
agg_hospag <- tibble()
for (i in seq(nl)) {
  hfaelle_i <- hospitalisierung_faelle[[i]]
  if (i<=3) {
    hfaelle_i <- hfaelle_i %>% 
      rename(`A00..04`=`0.-.4.Jährige`,
             `A05..14`=`5.-.14.Jährige`,
             `A15..34`=`15.-.34.Jährige`,
             `A35..59`=`35.-.59.Jährige`,
             `A60..79`=`60.-.79.Jährige`,
             `A80+`=`80+.Jährige`)
  }
  agg_hospag <- bind_rows(agg_hospag,
                          hfaelle_i %>% 
                            mutate(Meldejahr=as.double(Meldejahr),
                                   Meldewoche=as.double(Meldewoche)) %>% 
                            # mutate(alle_ag_sum=rowSums(across(`A00..04`:`A80+`), na.rm = TRUE)) %>% 
                            select(Meldejahr, Meldewoche, `A00..04`:`A80+`) %>% 
                            mutate(pubdate=datumsliste[i],
                                   pubJahrKW=year(pubdate)*100+isoweek(pubdate)-1,
                                   JahrKW=Meldejahr*100+Meldewoche) %>% 
                            filter(JahrKW==pubJahrKW-wdelay)
  )
}

agg_hospag_hospangabe <- agg_hospag %>% 
  left_join(agg_hospangabe %>% select(JahrKW, `Fälle.gesamt`),
            by="JahrKW")

rki_agg_kw_ag <- rki %>% 
  mutate(Jahr=year(Meldedatum),
         KW=isoweek(Meldedatum),
         JahrKW=ifelse(KW!=53, Jahr*100+KW, 202053)) %>% 
  group_by(JahrKW, Altersgruppe) %>% 
  summarise(AnzahlFaelle=sum(AnzahlFall[NeuerFall>=0]),
            .groups="drop")

agg_anteil_ag <- agg_hospag_hospangabe %>% 
  pivot_longer(cols=`A00..04`:`A80+`, names_to = "Altersgruppe",
               values_to="AnzahlHosp") %>% 
  select(Meldejahr, Meldewoche, JahrKW, `Fälle.gesamt`, 
         Altersgruppe, AnzahlHosp) %>% 
  mutate(Altersgruppe=str_replace(Altersgruppe, fixed(".."), "-A")) %>% 
  left_join(rki_agg_kw_ag,
            by=c("JahrKW", "Altersgruppe")) %>% 
  mutate(Anteil_an_FaelleGesamt=AnzahlHosp/`Fälle.gesamt`,
         Anteil_an_FaelleAG=AnzahlHosp/AnzahlFaelle) %>% 
  mutate(Altersgruppe=str_replace_all(Altersgruppe, fixed("A"), ""))

ggplot(agg_anteil_ag, aes(x=JahrKW, y=Anteil_an_FaelleGesamt)) +
  geom_line() +
  facet_wrap(~ Altersgruppe, ncol = 1, scales = "free_y") #

anteile_hosp_ag_plot <- ggplot(agg_anteil_ag, aes(x=Meldewoche, y=Anteil_an_FaelleAG)) +
  geom_line() +
  theme(axis.title.x = element_blank()) +
  theme_zi() +
  scale_y_continuous(labels=scales::percent_format(1)) +
  scale_x_continuous(breaks=c(10,15,20)) +
  labs(subtitle = "Anteil Hospitalisierungen an gemeldeten Fällen") +
  facet_wrap(~ Altersgruppe, ncol = 6,  scales = "free_y") #,  scales = "free_y"

# fallzahlen_ag_plot <- ggplot(agg_anteil_ag %>% 
#                                pivot_longer(cols = c(`AnzahlFaelle`, "AnzahlHosp"),
#                                             names_to="Fälle",
#                                             values_to = "Anzahl"),
#                              aes(x=JahrKW, y=Anzahl, fill=Fälle)) +
#   geom_bar(stat="identity") +
#   facet_wrap(~ Altersgruppe, ncol = 1) # 
hospzahlen_ag_plot <- ggplot(agg_anteil_ag,
                             aes(x=Meldewoche, y=AnzahlHosp)) +
  geom_bar(stat="identity") +
  theme(axis.title.x = element_blank()) +
  theme_zi() +
  ylab("hosp. Fälle") +
  scale_y_continuous(labels=comma_format(big.mark = ".",
                                         decimal.mark = ","),
                     breaks=c(0, 1000, 2000)) +
  scale_x_continuous(breaks=c(10,15,20)) +
  labs(subtitle = "hospitalisierte Fälle") +
  facet_wrap(~ Altersgruppe, ncol = 6)

fallzahlen_ag_plot <- ggplot(agg_anteil_ag,
                             aes(x=Meldewoche, y=AnzahlFaelle)) +
  geom_bar(stat="identity") +
  theme_zi() +
  ylab("Infektionsfälle") +
  scale_y_continuous(labels=comma_format(big.mark = ".",
                                         decimal.mark = ",")) +
  scale_x_continuous(breaks=c(10,15,20)) +
  labs(subtitle = "gemeldete Fälle insgesamt") +
  facet_wrap(~ Altersgruppe, ncol = 6)

patchwork_plot <- anteile_hosp_ag_plot / hospzahlen_ag_plot / fallzahlen_ag_plot

ggsave("R/adhoc_analyses/hospitalisierungen_altersgruppen.png",
       patchwork_plot,
       width = 24, height = 24*10/16,
       units="cm")

##### owid uk
owid_covid_data <- read_csv("data/owid-covid-data.csv")
weekly_hospital_admissions_covid <- read_csv("data/weekly-hospital-admissions-covid.csv")

owid_ukde <- owid_covid_data %>% 
  filter(location %in% c("United Kingdom", "Germany", "Israel", "Belgium",
                         "Netherlands", "Portugal", "Denmark", "Italy", "Spain") &
           year(date)==2021) %>% 
  select(location, new_cases, date) %>% 
  mutate(KW=isoweek(date)) %>% 
  group_by(location, KW) %>% 
  summarise(Faelle=sum(new_cases),
            .groups="drop") %>% 
  filter(KW>=2 & KW<25)
hosp_ukde <- weekly_hospital_admissions_covid %>% 
  filter(Entity %in% c("United Kingdom", "Germany", "Israel", "Belgium",
                       "Netherlands", "Portugal", "Denmark", "Italy", "Spain")) %>% 
  mutate(KW=isoweek(Day)) %>% 
  filter(year(Day)==2021) %>% 
  group_by(Entity, KW) %>% 
  summarise(Hosp_Faelle=round(sum(`Weekly new hospital admissions`)),
            .groups="drop") %>% 
  filter(KW>=2 & KW<25)
anteil_hospfaelle_ukde <- owid_ukde %>% 
  left_join(hosp_ukde,
            by=c("KW", "location"="Entity")) %>% 
  mutate(anteil_hosp=Hosp_Faelle/Faelle)

ggplot(anteil_hospfaelle_ukde %>% 
         pivot_longer(
           cols=c("Faelle", "Hosp_Faelle", "anteil_hosp"),
           names_to="Merkmal",
           values_to="Wert"
         ), aes(x=KW, y=Wert)) +
  geom_line() +
  ylim(0, NA) +
  facet_wrap(location ~ Merkmal, scales="free_y", ncol=3)

ggsave("R/adhoc_analyses/hospitalisierungen_vergleich_international.png",
       width = 18, height = 36,
       units="cm")
