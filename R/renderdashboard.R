library(lubridate)
library(dplyr)
library(stringr)
library(readr)
conn <- DBI::dbConnect(RPostgres::Postgres(),
                       host   = Sys.getenv("DBHOST"),
                       dbname = Sys.getenv("DBNAME"),
                       user      =  Sys.getenv("DBUSER"),
                       password        = Sys.getenv("DBPASSWORD"),
                       port     = 5432,
                       sslmode = 'require')

RepoStand <- as_date((tbl(conn,"Stand") %>% collect())$Datum)
DashboardStand <- as_date((tbl(conn,"Dashboardstand") %>% collect())$Datum)

if (DashboardStand<RepoStand) {
  rmarkdown::render('../Start.Rmd')
  Dashboardstand <- tibble(
    "Datum"=as.character(date(now())),
    "Zeit"=paste0(str_pad(format(hour(now())),2, pad = "0"),
                  ":",
                  str_pad(format(minute(now())),2, pad = "0"),
                  " Uhr"))
  DBI::dbWriteTable(conn,"Dashboardstand",Dashboardstand,overwrite=TRUE)
  test_new_vacc <- tryCatch(
    {
      mytemp <- tempfile()
      test_vacc_link <- "https://raw.githubusercontent.com/ard-data/2020-rki-impf-archive/master/data/9_csv_v2/metric_dosen_astrazeneca_kumulativ.csv"
      download.file(test_vacc_link, mytemp, method = "curl")
      test_vacc_data <- read_csv(mytemp)
      write_csv(test_vacc_data, "../data/test_vacc_ard_new.csv")
      test_vacc_data
    },
    error=function(e) {
      # read old data
      test_vacc_data <- read_csv("../data/test_vacc_ard_new.csv")
      return(test_vacc_data)
    }
  )
  write_csv(test_new_vacc, "../data/test_vacc_ard_old.csv")
  

}

test_new_kbv_vacc <- tbl(conn,"kbvcovidvacczi") %>% 
  summarise(maxdate=max(vacc_date)) %>%
  collect() %>% 
  pull(maxdate)

test_kbv_aggr_vacc <- tbl(conn,"kbv_impfstoff_kreise") %>% 
  summarise(maxdate=max(vacc_date)) %>%
  collect() %>% 
  pull(maxdate)

if (test_new_kbv_vacc>test_kbv_aggr_vacc) {
  kbv_impfstoff <- tbl(conn,"kbvcovidvacczi") %>% 
    collect()
  kbv_age <- tbl(conn,"kbvcovidagegroup") %>%
    collect()
  
  kreise_plz <- read_csv2("../data/plz_krs_2016.csv") %>% 
    mutate(PLZ=as.integer(PLZ)) %>% 
    select(PLZ, Kreis2016, Kreis2016name) %>% 
    bind_rows(tibble(PLZ=0, Kreis2016=0, Kreis2016name="unbekannt"))
  
  kbv_plz_kv_liste <- read_csv("../data/kbv_plz_liste.csv") %>% 
    group_by(plz) %>% filter(row_number()==1) %>% 
    bind_rows(tibble(plz=0, kv="unbekannt"))
  
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
    select(vacc_date=Impfdatum, BundeslandId_Impfort,
           Bundesland,
           vacc_series=Impfserie,
           Impfstoff,
           anzahl_alleorte=Anzahl) # %>% 
    # group_by(vacc_date, LandkreisId, vacc_series, Altersgruppe) %>% 
    # summarise(anzahl_alleorte=sum(anzahl_alleorte), .groups = "drop")
  
  kbv_impfstoff_plz <- kbv_impfstoff %>% 
    mutate(anzahl=as.integer(anzahl),
           arzt_plz=as.integer(arzt_plz),
           vacc_series=as.integer(vacc_series)) %>% 
    select(vacc_date, PLZ=arzt_plz, 
           vacc_product,
           vacc_series, anzahl_praxen=anzahl) %>% 
    group_by(vacc_date, PLZ, vacc_series, vacc_product) %>% 
    summarise(anzahl_praxen=sum(anzahl_praxen, na.rm=TRUE),
              .groups="drop") %>% 
    mutate(PLZ=ifelse(PLZ%in%setdiff(unique(PLZ), unique(kreise_plz$PLZ)), 
                      0, 
                      PLZ))
  
  kbv_age_plz <- kbv_age %>% 
    mutate(anzahl=as.integer(anzahl),
           arzt_plz=as.integer(arzt_plz),
           vacc_series=as.integer(vacc_series)) %>% 
    select(vacc_date, PLZ=arzt_plz, 
           Altersgruppe=altersgruppe,
           vacc_series, anzahl_praxen=anzahl) %>% 
    group_by(vacc_date, PLZ, vacc_series, Altersgruppe) %>% 
    summarise(anzahl_praxen=sum(anzahl_praxen, na.rm=TRUE),
              .groups="drop") %>% 
    mutate(PLZ=ifelse(PLZ%in%setdiff(unique(PLZ), unique(kreise_plz$PLZ)), 
                      0, 
                      PLZ)) %>% 
    mutate(Altersgruppe=case_when(
      Altersgruppe=="0" ~ "18-59",
      Altersgruppe=="1" ~ "60+",
      Altersgruppe=="2" ~ "12-17",
      TRUE ~ "u"
    ))
  
  kbv_impfen_plz <- kbv_age_plz %>% 
    group_by(vacc_date, PLZ, vacc_series) %>% 
    summarise(anzahl_praxen=sum(anzahl_praxen),
              .groups="drop")
  
  rki_impfen_kreise <- rki_vacc_kreise %>% 
    group_by(vacc_date, LandkreisId, vacc_series) %>% 
    summarise(anzahl_alleorte=sum(anzahl_alleorte), .groups="drop")
  
  kbv_impfen_kreise_kv <- kbv_impfen_plz %>% 
    left_join(kreise_plz, by="PLZ") %>% 
    left_join(kbv_plz_kv_liste %>% 
                select(plz, kv), by=c("PLZ"="plz")) %>% 
    group_by(vacc_date, Kreis2016, vacc_series) %>% 
    summarise(anzahl_praxen=sum(anzahl_praxen, na.rm=TRUE),
              kv=kv[1],
              Kreis2016name=Kreis2016name[1],
              .groups="drop")
  
  kbv_age_kreise_kv <- kbv_age_plz %>% 
    left_join(kreise_plz, by="PLZ") %>% 
    left_join(kbv_plz_kv_liste %>% 
                select(plz, kv), by=c("PLZ"="plz")) %>% 
    group_by(vacc_date, Kreis2016, vacc_series, Altersgruppe) %>% 
    summarise(anzahl_praxen=sum(anzahl_praxen, na.rm=TRUE),
              kv=kv[1],
              Kreis2016name=Kreis2016name[1],
              .groups="drop")
  
  kbv_impfstoff_kreise_kv <- kbv_impfstoff_plz %>% 
    left_join(kreise_plz, by="PLZ") %>% 
    left_join(kbv_plz_kv_liste %>% 
                select(plz, kv), by=c("PLZ"="plz")) %>% 
    group_by(vacc_date, Kreis2016, vacc_series, vacc_product) %>% 
    summarise(anzahl_praxen=sum(anzahl_praxen, na.rm=TRUE),
              kv=kv[1],
              Kreis2016name=Kreis2016name[1],
              .groups="drop")
  kbv_impfstoff_laender <- kbv_impfstoff_kreise_kv %>% 
    mutate(Bundesland=case_when(
      kv=="Westfalen-Lippe" ~ "Nordrhein-Westfalen",
      kv=="Nordrhein" ~ "Nordrhein-Westfalen",
      kv=="Bayerns" ~ "Bayern",
      TRUE ~ kv
    ),
    Impfstoff=case_when(
      vacc_product=="BNT162b2" ~ "Comirnaty",
      vacc_product=="AZD1222" ~ "AstraZeneca",
      vacc_product=="mRNA-1273" ~ "Moderna",
      vacc_product=="Ad26.COV2.S" ~ "Janssen",
      TRUE ~ "ERROR"
    )) %>% 
    group_by(vacc_date, vacc_series, Impfstoff, Bundesland) %>% 
    summarise(anzahl_praxen=sum(anzahl_praxen), .groups = "drop")
  
  impfstoff_laender_kbv_rki <- full_join(kbv_impfstoff_laender,
                                         rki_vacc_laender,
                                         by=c("vacc_date",
                                              "vacc_series",
                                              "Impfstoff",
                                              "Bundesland")) %>% 
    mutate(KW=isoweek(vacc_date))
  
  age_kreise_kbv_rki <- full_join(kbv_age_kreise_kv,
                                     rki_vacc_kreise,
                                     by=c("vacc_date",
                                          "Kreis2016"="LandkreisId",
                                          "Altersgruppe",
                                          "vacc_series")) %>% 
    mutate(across(c(anzahl_praxen, anzahl_alleorte), ~replace(., is.na(.), 0))) %>% 
    group_by(Kreis2016) %>% 
    mutate(kv=max(kv, na.rm=TRUE),
           Kreis2016name=max(Kreis2016name, na.rm=TRUE)) %>% 
    ungroup() %>% 
    mutate(KW=isoweek(vacc_date))
  
  DBI::dbWriteTable(conn, "kbv_rki_impfstoffe_laender", impfstoff_laender_kbv_rki, overwrite=TRUE)
  DBI::dbWriteTable(conn, "kbv_impfstoff_kreise", kbv_impfstoff_kreise_kv %>% 
                      mutate(KW=isoweek(vacc_date)), overwrite=TRUE)
  DBI::dbWriteTable(conn, "kbv_rki_altersgruppen_kreise", age_kreise_kbv_rki, overwrite=TRUE)

}

test_new_vacc <- tryCatch(
  {
    mytemp <- tempfile()
    test_vacc_link <- "https://raw.githubusercontent.com/ard-data/2020-rki-impf-archive/master/data/9_csv_v3/metric_dosen_astrazeneca_kumulativ.csv"
    download.file(test_vacc_link, mytemp, method = "curl")
    test_vacc_data <- read_csv(mytemp)
    write_csv(test_vacc_data, "../data/test_vacc_ard_new.csv")
    test_vacc_data
  },
  error=function(e) {
    # read old data
    test_vacc_data <- read_csv("../data/test_vacc_ard_new.csv")
    return(test_vacc_data)
  }
)
test_old_vacc <- read_csv("../data/test_vacc_ard_old.csv")

if (max(test_new_vacc$date)>max(test_old_vacc$date)) {
  rmarkdown::render('../Start.Rmd')
  Dashboardstand <- tibble(
    "Datum"=as.character(date(now())),
    "Zeit"=paste0(str_pad(format(hour(now())),2, pad = "0"),
                  ":",
                  str_pad(format(minute(now())),2, pad = "0"),
                  " Uhr"))
  DBI::dbWriteTable(conn,"Dashboardstand",Dashboardstand,overwrite=TRUE)
  write_csv(test_new_vacc, "../data/test_vacc_ard_old.csv")
  vacc_zahlen <- read_csv("../data/vacc_zahlen_ard.csv")
  DBI::dbWriteTable(conn, "rki_excel_impfdaten", vacc_zahlen, overwrite=TRUE)
}

source("generatedata_impfindex.R")
lieferungen <- read_csv("../data/tabledata/impfstoff_lieferungen_bmg.csv")
DBI::dbWriteTable(conn, "bmg_impfstofflieferungen", lieferungen, overwrite=TRUE)

DBI::dbSendStatement(conn, "GRANT SELECT ON ALL TABLES IN SCHEMA public TO zireader;")

DBI::dbDisconnect(conn)
# DBI::dbListTables(conn)
# DBI::dbRemoveTable(conn, "brd_testungen")
