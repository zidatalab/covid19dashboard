library(lubridate)
library(dplyr)
conn <- DBI::dbConnect(RPostgres::Postgres(),
                       host   = Sys.getenv("DBHOST"),
                       dbname = Sys.getenv("DBNAME"),
                       user      =  Sys.getenv("DBUSER"),
                       password        = Sys.getenv("DBPASSWORD"),
                       port     = 5432,
                       sslmode = 'require')

RepoStand <- as_date((tbl(conn,"Stand") %>% collect())$Datum)

if (RepoStand<today()) {
  rmarkdown::render('../Start.Rmd')
}

DBI::dbDisconnect(conn)
