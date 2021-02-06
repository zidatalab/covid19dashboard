library(lubridate)
library(dplyr)
library(stringr)
conn <- DBI::dbConnect(RPostgres::Postgres(),
                       host   = Sys.getenv("DBHOST"),
                       dbname = Sys.getenv("DBNAME"),
                       user      =  Sys.getenv("DBUSER"),
                       password        = Sys.getenv("DBPASSWORD"),
                       port     = 5432,
                       sslmode = 'require')

RepoStand <- as_date((tbl(conn,"Stand") %>% collect())$Datum)
DashboardStand <- as_date((tbl(conn,"Dashboardstand") %>% collect())$Datum)

if (Dashboardstand<RepoStand) {
  rmarkdown::render('../Start.Rmd')
  Dashboardstand <- tibble("Datum"=as.character(date(now())),
                         "Zeit"=paste0(str_pad(format(hour(now())),2, pad = "0"),
                                       ":",
                                       str_pad(format(minute(now())),2, pad = "0"),
                                       " Uhr"))
  DBI::dbWriteTable(conn,"Dashboardstand",Dashboardstand,overwrite=TRUE)
}

DBI::dbDisconnect(conn)
