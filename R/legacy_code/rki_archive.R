library(tidyverse)
library(lubridate)
library(DBI)
library(rpostgis)

startdate <- as_date("2020-11-25")
enddate <- as_date("2020-12-11")

conn <- DBI::dbConnect(RPostgres::Postgres(),
                       host   = Sys.getenv("DBHOST"),
                       dbname = Sys.getenv("DBNAME"),
                       user      =  Sys.getenv("DBUSER"),
                       password        = Sys.getenv("DBPASSWORD"),
                       port     = 5432,
                       sslmode = 'require')

# add latest
thisrki <- tbl(conn, paste0("rki_", as_date(enddate))) %>% collect()
tt <- system.time({
  dbWriteTable(conn, "rki_archive", thisrki, append=TRUE)
})
cat(as_date(enddate), ": ", tt[3]/60, "minutes\n")

tt_query <- system.time({testarchive_teil <- tbl(conn, "rki_archive") %>% 
  filter(Datenstand==startdate) %>% 
  collect() })

# old tables
startrki <- tbl(conn,paste0("rki_", as_date(startdate))) %>% collect()
secondrki <- tbl(conn,paste0("rki_", as_date(startdate)+1)) %>% collect()

dbWriteTable(conn, "rki_archive", startrki, overwrite=TRUE)
dbWriteTable(conn, "rki_archive", secondrki, append=TRUE)

for (idx in seq(2, 15)) {
  thisrki <- tbl(conn,paste0("rki_", as_date(startdate)+idx)) %>% collect()
  tt <- system.time({
    dbWriteTable(conn, "rki_archive", thisrki, append=TRUE)
  })
  cat(as_date(startdate)+idx, ": ", tt[3]/60, "minutes\n")
}

tt_index <- system.time({indexcreated <- dbIndex(conn, "rki_archive", "Datenstand", unique = TRUE)})
# dbDrop(conn, "rki_archive_Datenstand_idx", "INDEX")

# testarchive <- tbl(conn, "rki_archive") %>% collect()
tt_query <- system.time({testarchive_teil <- tbl(conn, "rki_archive") %>% 
  filter(Datenstand=="2020-12-06") %>% 
  collect() })
