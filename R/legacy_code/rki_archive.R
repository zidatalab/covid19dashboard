library(tidyverse)
library(lubridate)
library(DBI)

startdate <- as_date("2020-11-25")
enddate <- as_date("2020-12-10")

conn <- DBI::dbConnect(RPostgres::Postgres(),
                       host   = Sys.getenv("DBHOST"),
                       dbname = Sys.getenv("DBNAME"),
                       user      =  Sys.getenv("DBUSER"),
                       password        = Sys.getenv("DBPASSWORD"),
                       port     = 5432,
                       sslmode = 'require')

startrki <- tbl(conn,paste0("rki_", as_date(startdate))) %>% collect()
secondrki <- tbl(conn,paste0("rki_", as_date(startdate)+1)) %>% collect()

endrki <- tbl(conn, paste0("rki_", as_date(enddate))) %>% collect()
dbWriteTable(conn, "rki", endrki, overwrite=TRUE)

testarchive <- tbl(conn, "rki_archive") %>% collect()
