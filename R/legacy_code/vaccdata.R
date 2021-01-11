library(tidyverse)
library(lubridate)
library(zicolors)

conn <- DBI::dbConnect(RPostgres::Postgres(),
                       host   = Sys.getenv("DBHOST"),
                       dbname = Sys.getenv("DBNAME"),
                       user      =  Sys.getenv("DBUSER"),
                       password        = Sys.getenv("DBPASSWORD"),
                       port     = 5432,
                       sslmode = 'require')
dbListTables(conn)

vaccinations <- tbl(conn, "vaccinations") %>% collect()
strukturdaten <- tbl(conn, "strukturdaten") %>% collect()


DBI::dbDisconnect(conn)

pflegeheimbewohnende_2019_bundeslaender <- read_delim("data/pflegeheimbewohnende_2019_bundeslaender.csv", 
                                                      ";", escape_double = FALSE, trim_ws = TRUE)

kreise_regstat_alter <- read_delim("data/Bev2019_Kreis_AG_rki_geschlecht.txt", 
                                   ";",
                                   escape_double = FALSE,
                                   col_types = cols(X1 = col_skip()), 
                                   trim_ws = TRUE) %>%
  mutate(id=ifelse(Kreis==11000, 11, Kreis*1000)) %>%
  select(-Kreis, -m, -w) %>%
  pivot_wider(id_cols="id",
              names_from="AG_rki",
              names_prefix="ag_",
              values_from="ges")
kreise_regstat_alter <- bind_rows(tibble(id=0,
                                         ag_1=sum(kreise_regstat_alter$ag_1),
                                         ag_2=sum(kreise_regstat_alter$ag_2),
                                         ag_3=sum(kreise_regstat_alter$ag_3),
                                         ag_4=sum(kreise_regstat_alter$ag_4),
                                         ag_5=sum(kreise_regstat_alter$ag_5),
                                         ag_6=sum(kreise_regstat_alter$ag_6)),
                                  kreise_regstat_alter %>%
                                    mutate(blid=ifelse(id==11, 11, floor(id/1000000))) %>%
                                    group_by(blid) %>%
                                    summarise(ag_1=sum(ag_1, na.rm = TRUE),
                                              ag_2=sum(ag_2, na.rm = TRUE),
                                              ag_3=sum(ag_3, na.rm = TRUE),
                                              ag_4=sum(ag_4, na.rm = TRUE),
                                              ag_5=sum(ag_5, na.rm = TRUE),
                                              ag_6=sum(ag_6, na.rm = TRUE),
                                              .groups="drop") %>%
                                    mutate(id=blid) %>%
                                    filter(id!=11) %>%
                                    select(-blid),
                                  kreise_regstat_alter)

vaccinations <- vaccinations %>%
  mutate(datum=as_date(as_datetime(date)))
