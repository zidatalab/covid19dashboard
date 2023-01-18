library(lubridate)
library(dplyr)
library(stringr)
library(readr)
library(tidyverse)
library(openxlsx)
library(readxl)
library(janitor)

conn <- DBI::dbConnect(RPostgres::Postgres(),
                       host   = Sys.getenv("DBHOST"),
                       dbname = Sys.getenv("DBNAME"),
                       user      =  Sys.getenv("DBUSER"),
                       password        = Sys.getenv("DBPASSWORD"),
                       port     = 5432,
                       sslmode = 'require')

# RepoStand <- as_date((tbl(conn,"Stand") %>% collect())$Datum)
# DashboardStand <- as_date((tbl(conn,"Dashboardstand") %>% collect())$Datum)
# 
# if (DashboardStand<RepoStand) {
#   #rmarkdown::render('../Start_old.Rmd')
#   # source("functions_newvwz.R", encoding = "UTF-8")
#   Dashboardstand <- tibble(
#     "Datum"=as.character(date(now())),
#     "Zeit"=paste0(str_pad(format(hour(now())),2, pad = "0"),
#                   ":",
#                   str_pad(format(minute(now())),2, pad = "0"),
#                   " Uhr"))
#   DBI::dbWriteTable(conn,"Dashboardstand",Dashboardstand,overwrite=TRUE)
#   test_new_vacc <- tryCatch(
#     {
#       mytemp <- tempfile()
#       test_vacc_link <- "https://raw.githubusercontent.com/ard-data/2020-rki-impf-archive/master/data/9_csv_v2/metric_dosen_astrazeneca_kumulativ.csv"
#       download.file(test_vacc_link, mytemp, method = "curl")
#       test_vacc_data <- read_csv(mytemp)
#       write_csv(test_vacc_data, "../data/test_vacc_ard_new.csv")
#       test_vacc_data
#     },
#     error=function(e) {
#       # read old data
#       test_vacc_data <- read_csv("../data/test_vacc_ard_new.csv")
#       return(test_vacc_data)
#     }
#   )
#   write_csv(test_new_vacc, "../data/test_vacc_ard_old.csv")
#   
# 
# }

test_new_kbv_vacc <- tbl(conn,"kbvcovidvacczi") %>% 
  summarise(maxdate=max(vacc_date)) %>%
  collect() %>% 
  pull(maxdate)

test_kbv_aggr_vacc <- tbl(conn,"kbv_impfstoff_kreise") %>% 
  summarise(maxdate=max(vacc_date)) %>%
  collect() %>% 
  pull(maxdate)

rki_vacc_laender <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Impfungen_in_Deutschland/master/Aktuell_Deutschland_Bundeslaender_COVID-19-Impfungen.csv") %>% 
  mutate(BundeslandId_Impfort=as.integer(BundeslandId_Impfort),
         Bundesland=case_when(
           BundeslandId_Impfort == 1 ~ "Schleswig-Holstein",
           BundeslandId_Impfort == 2 ~ "Hamburg",
           BundeslandId_Impfort == 3 ~ "Niedersachsen",
           BundeslandId_Impfort == 4 ~ "Bremen",
           BundeslandId_Impfort == 6 ~ "Hessen",
           BundeslandId_Impfort == 7 ~ "Rheinland-Pfalz",
           BundeslandId_Impfort == 8 ~ "Baden-Württemberg",
           BundeslandId_Impfort == 9 ~ "Bayern",
           BundeslandId_Impfort == 10 ~ "Saarland",
           BundeslandId_Impfort == 11 ~ "Berlin",
           BundeslandId_Impfort == 12 ~ "Brandenburg",
           BundeslandId_Impfort == 13 ~ "Mecklenburg-Vorpommern",
           BundeslandId_Impfort == 14 ~ "Sachsen",
           BundeslandId_Impfort == 15 ~ "Sachsen-Anhalt",
           BundeslandId_Impfort == 16 ~ "Thüringen",
           BundeslandId_Impfort == 5 ~ "Nordrhein-Westfalen",
           BundeslandId_Impfort == 17 ~ "Bundesressorts",
           TRUE ~ "ERROR"
         ),
         Impfdatum=as_date(Impfdatum)) %>% 
  mutate(Impfstoff=case_when(
    Impfstoff=="Comirnaty" ~ "Comirnaty",
    Impfstoff=="Comirnaty bivalent (Original/Omikron)" ~ "Comirnaty-Omikron",
    Impfstoff=="Comirnaty Original/Omicron BA.1" ~ "Comirnaty-Omikron",
    Impfstoff=="Comirnaty Original/Omicron BA.4-5" ~ "Comirnaty-Omikron",
    Impfstoff=="Comirnaty-Kleinkinder" ~ "Comirnaty",
    Impfstoff=="Spikevax bivalent (Original/Omikron)" ~ "Moderna-Omikron",
    Impfstoff=="Spikevax bivalent Original/Omicron BA.1" ~ "Moderna-Omikron",
    Impfstoff=="Spikevax bivalent Original/Omicron BA.4-5" ~ "Moderna-Omikron",
    Impfstoff=="Spikevax" ~ "Moderna",
    Impfstoff=="Vaxzevria" ~ "AstraZeneca",
    Impfstoff=="Jcovden" ~ "Janssen",
    Impfstoff=="Nuvaxovid" ~ "Novavax",
    Impfstoff=="Valneva" ~ "Valneva",
    Impfstoff=="VidPrevtyn Beta" ~ "Sanofi",
    TRUE ~ "ERROR"
  )) %>% 
  select(vacc_date=Impfdatum, BundeslandId_Impfort,
         Bundesland,
         vacc_series=Impfserie,
         Impfstoff,
         anzahl_alleorte=Anzahl) # %>% 
# group_by(vacc_date, LandkreisId, vacc_series, Altersgruppe) %>% 
# summarise(anzahl_alleorte=sum(anzahl_alleorte), .groups = "drop")

test_new_rki_vacc <- max(rki_vacc_laender$vacc_date)

test_old_rki_vacc <- tbl(conn,"kbv_rki_impfstoffe_laender") %>% 
  filter(!is.na(anzahl_alleorte)) %>% 
  summarise(maxdate=max(vacc_date)) %>%
  collect() %>% 
  pull(maxdate)

if (test_new_kbv_vacc>test_kbv_aggr_vacc | test_new_rki_vacc>test_old_rki_vacc) {
  kbv_impfstoff <- tbl(conn,"kbvcovidvacczi") %>% 
    collect()
  kbv_age <- tbl(conn,"kbvcovidagegroup") %>%
    collect()
  
  kreise_plz <- read_csv2("../data/plz_kreis_bl.csv") %>% 
    select(PLZ, Kreis2016, Kreis2016name, KV, KVname, BL, BLname) %>% 
    bind_rows(tibble(PLZ="0", Kreis2016=0, Kreis2016name="unbekannt",
                     BL=0, BLname="unbekannt",
                     KV=0, KVname="unbekannt"))
  
  rki_vacc_kreise <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Impfungen_in_Deutschland/master/Aktuell_Deutschland_Landkreise_COVID-19-Impfungen.csv") %>% 
    mutate(LandkreisId=ifelse(LandkreisId_Impfort=="u" | 
                                LandkreisId_Impfort=="17000", 
                              "0",
                              LandkreisId_Impfort),
           LandkreisId=as.integer(LandkreisId),
           Impfdatum=as_date(Impfdatum)) %>% 
    select(vacc_date=Impfdatum, LandkreisId, vacc_series=Impfschutz,
           Altersgruppe,
           anzahl_alleorte=Anzahl) %>% 
    group_by(vacc_date, LandkreisId, vacc_series, Altersgruppe) %>% 
    summarise(anzahl_alleorte=sum(anzahl_alleorte))
  
  kbv_impfstoff_plz <- kbv_impfstoff %>% 
    # mutate(anzahl=as.integer(anzahl),
    #        arzt_plz=as.integer(arzt_plz),
    #        vacc_series=as.integer(vacc_series)) %>% 
    # mutate(vacc_series=ifelse(vacc_series==2 & vacc_product=="Ad26.COV2.S", 1, vacc_series)) %>% 
    select(vacc_date, PLZ=arzt_plz, 
           vacc_product,
           vacc_series, anzahl_praxen=anzahl) %>% 
    group_by(vacc_date, PLZ, vacc_series, vacc_product) %>% 
    summarise(anzahl_praxen=sum(anzahl_praxen, na.rm=TRUE),
              .groups="drop") %>% 
    mutate(PLZ=ifelse(PLZ%in%setdiff(unique(PLZ), unique(kreise_plz$PLZ)), 
                      "0", 
                      PLZ))
  
  kbv_age_plz <- kbv_age %>% 
    # mutate(anzahl=as.integer(anzahl),
    #        arzt_plz=as.integer(arzt_plz),
    #        vacc_series=as.integer(vacc_series)) %>% 
    select(vacc_date, PLZ=arzt_plz, 
           Altersgruppe=altersgruppe,
           vacc_series, anzahl_praxen=anzahl) %>% 
    group_by(vacc_date, PLZ, vacc_series, Altersgruppe) %>% 
    summarise(anzahl_praxen=sum(anzahl_praxen, na.rm=TRUE),
              .groups="drop") %>% 
    mutate(PLZ=ifelse(PLZ%in%setdiff(unique(PLZ), unique(kreise_plz$PLZ)), 
                      "0", 
                      PLZ)) %>% 
    mutate(Altersgruppe=case_when(
      Altersgruppe=="0" ~ "18-59",
      Altersgruppe=="1" ~ "60+",
      Altersgruppe=="2" ~ "12-17",
      Altersgruppe=="3" ~ "5-11",
      Altersgruppe=="4" ~ "0-4",
      TRUE ~ "u"
    ))
  
  kbv_age_kreise_kv_bl <- kbv_age_plz %>% 
    left_join(kreise_plz, by="PLZ") %>% 
    group_by(vacc_date, Kreis2016, vacc_series, Altersgruppe) %>% 
    summarise(anzahl_praxen=sum(anzahl_praxen, na.rm=TRUE),
              Kreis2016name=Kreis2016name[1],
              kv=KV[1],
              kvname=KVname[1],
              BundeslandId=BL[1],
              Bundesland=BLname[1],
              .groups="drop")
  
  kbv_impfstoff_kreise_kv_bl <- kbv_impfstoff_plz %>% 
    left_join(kreise_plz, by="PLZ") %>% 
    group_by(vacc_date, Kreis2016, vacc_series, vacc_product) %>% 
    summarise(anzahl_praxen=sum(anzahl_praxen, na.rm=TRUE),
              Kreis2016name=Kreis2016name[1],
              kv=KV[1],
              kvname=KVname[1],
              BundeslandId=BL[1],
              Bundesland=BLname[1],
              .groups="drop")
  
  kbv_impfstoff_laender <- kbv_impfstoff_kreise_kv_bl %>% 
    mutate(Impfstoff=case_when(
      vacc_product %in% c("BNT162b2", 
                          "BNT162b2-Kinder",
                          "BNT162b2-KK") ~ "Comirnaty",
      vacc_product %in% c("BNT162b2-VA", "BNT162b2-Kinder-VA",
                          "BNT162b2-BA.1", "BNT162b2-BA.4/5") ~ "Comirnaty-Omikron",
      vacc_product=="AZD1222" ~ "AstraZeneca",
      vacc_product=="VAT00008" ~ "Sanofi",
      vacc_product=="mRNA-1273" ~ "Moderna",
      vacc_product %in% c("mRNA-1273-VA", "mRNA-1273-BA.4/5",
                          "mRNA-1273-BA.1") ~ "Moderna-Omikron",
      vacc_product=="Ad26.COV2.S" ~ "Janssen",
      vacc_product=="NVX-CoV2373" ~ "Novavax",
      vacc_product=="VLA2001" ~ "Valneva",
      TRUE ~ "ERROR"
    )) %>% 
    group_by(vacc_date, vacc_series, Impfstoff, Bundesland) %>% 
    summarise(anzahl_praxen=sum(anzahl_praxen), .groups = "drop")
  
  kbv_rki_impfstoff_laender <- 
    full_join(
      kbv_impfstoff_laender,
      rki_vacc_laender,
      by=c("vacc_date",
           "vacc_series",
           "Impfstoff",
           "Bundesland")) %>% 
    mutate(KW=isoweek(vacc_date),
           Jahr=year(vacc_date),
           Monat=month(vacc_date),
           Jahr=isoyear(vacc_date),
           JahrKW=100*Jahr+KW)
  
  kbv_rki_age_kreise <- 
    full_join(
      kbv_age_kreise_kv_bl,
      rki_vacc_kreise %>% 
        mutate(Altersgruppe=case_when(
          Altersgruppe=="05-11" ~ "5-11",
          Altersgruppe=="00-04" ~ "0-4",
          TRUE ~ Altersgruppe
        )),
      by=c("vacc_date",
           "Kreis2016"="LandkreisId",
           "Altersgruppe",
           "vacc_series")) %>% 
    mutate(across(c(anzahl_praxen, anzahl_alleorte), ~replace(., is.na(.), 0))) %>% 
    group_by(Kreis2016) %>% 
    mutate(across(Kreis2016name:Bundesland, ~max(., na.rm=TRUE))) %>% 
    ungroup() %>% 
    mutate(KW=isoweek(vacc_date),
           Jahr=year(vacc_date),
           Monat=month(vacc_date),
           Jahr=isoyear(vacc_date),
           JahrKW=100*Jahr+KW)
  
  moderna_auffr <- kbv_rki_impfstoff_laender %>% 
    filter((vacc_series==3 | vacc_series==4) & Impfstoff=="Moderna") %>% 
    group_by(Bundesland, KW, Jahr, JahrKW) %>% 
    summarise(anzahl_moderna_auffr_praxen=sum(anzahl_praxen, na.rm=TRUE),
              anzahl_moderna_auffr_alleorte=sum(anzahl_alleorte, na.rm=TRUE),
              .groups="drop")
  
  write_csv(moderna_auffr, "../data/moderna_auffr.csv")
  
  viertimpfungen_praxen <- kbv_rki_impfstoff_laender %>% 
    filter(vacc_series>=4) %>% 
    group_by(vacc_date, Bundesland) %>% 
    summarise(viert_praxen=sum(anzahl_praxen),
              .groups="drop")
  
  write_csv(viertimpfungen_praxen, "../data/impfdax/viertimpfungen_praxen.csv")

  DBI::dbWriteTable(conn, "kbv_rki_impfstoffe_laender", 
                    kbv_rki_impfstoff_laender, overwrite=TRUE)
  DBI::dbWriteTable(conn, "kbv_impfstoff_kreise", kbv_impfstoff_kreise_kv_bl %>% 
                      mutate(KW=isoweek(vacc_date)), overwrite=TRUE)
  DBI::dbWriteTable(conn, "kbv_rki_altersgruppen_kreise", kbv_rki_age_kreise,
                    overwrite=TRUE)
  DBI::dbWriteTable(conn, "rki_impfstoffe_laender", rki_vacc_laender,
                    overwrite=TRUE)
  DBI::dbWriteTable(conn, "rki_altersgruppen_kreise", rki_vacc_kreise,
                    overwrite=TRUE)

}

# test_new_vacc <- tryCatch(
#   {
#     mytemp <- tempfile()
#     test_vacc_link <- "https://raw.githubusercontent.com/ard-data/2020-rki-impf-archive/master/data/9_csv_v3/metric_dosen_astrazeneca_kumulativ.csv"
#     download.file(test_vacc_link, mytemp, method = "curl")
#     test_vacc_data <- read_csv(mytemp)
#     write_csv(test_vacc_data, "../data/test_vacc_ard_new.csv")
#     test_vacc_data
#   },
#   error=function(e) {
#     # read old data
#     test_vacc_data <- read_csv("../data/test_vacc_ard_new.csv")
#     return(test_vacc_data)
#   }
# )
# test_old_vacc <- read_csv("../data/test_vacc_ard_old.csv")

# if (max(test_new_vacc$date)>max(test_old_vacc$date)) {
  # rmarkdown::render('../Start_old.Rmd')
  # source("functions_newvwz.R", encoding = "UTF-8")
  # Dashboardstand <- tibble(
  #   "Datum"=as.character(date(now())),
  #   "Zeit"=paste0(str_pad(format(hour(now())),2, pad = "0"),
  #                 ":",
  #                 str_pad(format(minute(now())),2, pad = "0"),
  #                 " Uhr"))
  # DBI::dbWriteTable(conn,"Dashboardstand",Dashboardstand,overwrite=TRUE)
  # write_csv(test_new_vacc, "../data/test_vacc_ard_old.csv")
  # vacc_zahlen <- read_csv("../data/vacc_zahlen_ard.csv")
  # DBI::dbWriteTable(conn, "rki_excel_impfdaten", vacc_zahlen, overwrite=TRUE)
# }

source("generatedata_impfindex.R")
lieferungen <- read_csv("../data/tabledata/impfstoff_lieferungen_bmg.csv")
DBI::dbWriteTable(conn, "bmg_impfstofflieferungen", lieferungen, overwrite=TRUE)

rki_hosp_bl <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Hospitalisierungen_in_Deutschland/master/Aktuell_Deutschland_COVID-19-Hospitalisierungen.csv")
# hosp_erweiterung <- expand_grid(Datum=as_date("2020-02-22")+days(0:7),
#                                 Bundesland=unique(rki_hosp_bl$Bundesland),
#                                 Altersgruppe=unique(rki_hosp_bl$Altersgruppe),
#                                 Neue_Hospitalisierung_Faelle=0,
#                                 `7T_Hospitalisierung_Faelle`=0)
rki_hosp_bl <- rki_hosp_bl %>% 
  mutate(ST_neueFaelle_estimate=round(`7T_Hospitalisierung_Faelle`/7))
DBI::dbWriteTable(conn, "rki_hospitalisierung_faelle_bl", rki_hosp_bl, overwrite=TRUE)

# push tables for corona report
alm_ev <- read_csv("../data/almev.csv")
DBI::dbWriteTable(conn, "alm_ev", alm_ev, overwrite=TRUE)

wtag <- lubridate::wday(today(), week_start = 1)

if (wtag==5) { # friday/freitag: tabelle erscheint beim rki immer do
  url_rkihosp <- "https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Klinische_Aspekte.xlsx?__blob=publicationFile"
  destfile_rkihosp <- "../data/klinische_aspekte.xlsx"
  curl::curl_download(url_rkihosp, destfile_rkihosp)
  rki_hosp <- read_excel(destfile_rkihosp)
  skip = which(rki_hosp[1] == "Meldejahr" & rki_hosp[2] == "MW") # often changes
  rki_hosp <- read_excel(destfile_rkihosp,
                         # sheet = "Daten",
                         skip = skip) %>% 
    clean_names()
  rki_hosp_age <- read_excel(destfile_rkihosp,
                             sheet = 3)
  skip = which(rki_hosp_age[1] == "Meldejahr" & rki_hosp_age[2] == "Meldewoche") # often changes
  rki_hosp_age <- read_excel(destfile_rkihosp,
                             sheet = 3,
                             skip = skip) %>% 
    clean_names()
  DBI::dbWriteTable(conn, "rki_klinischeaspekte_sympt_hosp", rki_hosp, overwrite=TRUE)
  DBI::dbWriteTable(conn, "rki_klinischeaspekte_age_hosp", rki_hosp_age, overwrite=TRUE)
}

if (wtag==3) { # wednesday/mittwoch: tabelle erscheibt bei destatis immer di
  url_sterblk <- "https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Sterbefaelle-Lebenserwartung/Tabellen/sonderauswertung-sterbefaelle.xlsx?__blob=publicationFile"
  destfile_sterblk <- "../data/sonderauswertung_sterbefaelle.xlsx"
  curl::curl_download(url_sterblk, destfile_sterblk)
  sterbefaelle_kw <- bind_rows(read_excel(destfile_sterblk, 
                                          sheet = "D_2016_2022_KW_AG_Männlich", 
                                          skip = 8,
                                          na="X") %>% mutate(sex="maennlich"),
                               read_excel(destfile_sterblk, 
                                          sheet = "D_2016_2022_KW_AG_Weiblich", 
                                          skip = 8,
                                          na="X") %>% mutate(sex="weiblich")) %>%
    select(-"Nr.") %>% 
    rename("Jahr"="...2", "Alter"= "unter … Jahren" ) %>%
    relocate(Jahr,Alter,sex) %>% 
    pivot_longer(cols=-c("Jahr", "Alter", "sex"), names_to="KW", values_to="Tote")
  DBI::dbWriteTable(conn, "destatis_sterblichkeit", sterbefaelle_kw, overwrite=TRUE)
}

DBI::dbSendStatement(conn, "GRANT SELECT ON ALL TABLES IN SCHEMA public TO zireader;")

DBI::dbDisconnect(conn)
# DBI::dbListTables(conn)
# DBI::dbRemoveTable(conn, "brd_testungen")
