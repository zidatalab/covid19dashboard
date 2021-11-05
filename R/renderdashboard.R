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
  DBI::dbWriteTable(conn, "rki_impfdaten", vacc_zahlen, overwrite=TRUE)
}

DBI::dbDisconnect(conn)

source("generatedata_impfindex.R")
