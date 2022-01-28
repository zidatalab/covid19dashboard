conn <- DBI::dbConnect(RPostgres::Postgres(),
                       host   = Sys.getenv("DBHOST"),
                       dbname = Sys.getenv("DBNAME"),
                       user      =  Sys.getenv("DBUSER"),
                       password        = Sys.getenv("DBPASSWORD"),
                       port     = 5432,
                       sslmode = 'require')

rki <- tbl(conn,"rki") %>% collect()

nachkw <- rki %>% 
  group_by(Meldedatum) %>% 
  summarise(AnzahlFall=sum(AnzahlFall[NeuerFall>=0], na.rm=TRUE)) %>% 
  mutate(KW=isoweek(Meldedatum),
         Jahr=year(Meldedatum),
         isoJahr=isoyear(Meldedatum),
         Monat=month(Meldedatum),
         Jahr=case_when(
           KW>=52 & Monat==1 ~ Jahr-1L,
           TRUE ~ Jahr
         ),
         JahrKW=100*Jahr+KW) %>% 
  group_by(JahrKW) %>% 
  summarise(AnzahlFall=sum(AnzahlFall))
  