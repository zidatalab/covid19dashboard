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

datumsliste <- as_date("2021-04-14")+7*0:11

filenames <- list.files("data/klinische_aspekte_old", pattern="*.xlsx", full.names=TRUE)
hospitalisierung_faelle <- c(lapply(filenames[1:5], 
                                    read.xlsx,
                                    sheet=2, 
                                    startRow=2),
                             lapply(filenames[6:8], 
                                    read.xlsx,
                                    sheet=2, 
                                    startRow=5),
                             lapply(filenames[9:11], 
                                    read.xlsx,
                                    sheet=2, 
                                    startRow=7),
                             lapply(filenames[12], 
                                    read.xlsx,
                                    sheet=2, 
                                    startRow=8))

hospitalisierung_angabe <- lapply(filenames[1:12], 
                                    read.xlsx,
                                    sheet=1, 
                                    startRow=2)

## vergleich nachmeldungen

nl <- length(hospitalisierung_faelle)
agg_hospfaelle <- tibble()
for (i in seq(nl)) {
  agg_hospfaelle <- bind_rows(agg_hospfaelle,
                              hospitalisierung_faelle[[i]] %>% 
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
  agg_hospag <- bind_rows(agg_hospag,
                          hospitalisierung_faelle[[i]] %>% 
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
         Anteil_an_FaelleAG=AnzahlHosp/AnzahlFaelle)

ggplot(agg_anteil_ag, aes(x=JahrKW, y=Anteil_an_FaelleGesamt)) +
  geom_line() +
  facet_wrap(~ Altersgruppe, ncol = 3, scales = "free_y") # 

ggplot(agg_anteil_ag, aes(x=JahrKW, y=Anteil_an_FaelleAG)) +
  geom_line() +
  facet_wrap(~ Altersgruppe, ncol = 3,  scales = "free_y") #
