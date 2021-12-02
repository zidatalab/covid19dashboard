View(lieferungen %>% 
       filter(dosen>0) %>% 
       mutate(jahr=year(date),
              quartal=lubridate::quarter(date), 
              quartal=ifelse(jahr==2020, 1, quartal)) %>% 
       group_by(impfstoff, quartal) %>% 
       summarise(geliefertq=sum(dosen, na.rm=TRUE)))

View(lieferungen %>% 
       filter(dosen<0) %>% 
       mutate(jahr=year(date),
              quartal=lubridate::quarter(date), 
              quartal=ifelse(jahr==2020, 1, quartal)) %>% 
       group_by(impfstoff, quartal) %>% 
       summarise(abgezogen=sum(dosen, na.rm=TRUE)))

View(lieferungen %>% 
       filter(impfstoff=="moderna" | impfstoff=="comirnaty") %>% 
       group_by(impfstoff, KW) %>% 
       summarise(geliefert=sum(dosen)))

View(lieferungen %>% 
       filter(impfstoff=="moderna" | impfstoff=="comirnaty") %>% 
       group_by(impfstoff, KW, einrichtung) %>% 
       summarise(geliefert=sum(dosen)))

View(impfstoff_laender_kbv_rki %>% 
       group_by(KW, Impfstoff) %>% 
       summarise(verimpft=sum(anzahl_alleorte)))

View(impfstoff_laender_kbv_rki %>% 
       group_by(KW, Impfstoff, vacc_series) %>% 
       filter(KW<52) %>%
       summarise(verimpft_serie=sum(anzahl_alleorte, na.rm=TRUE)))

# lager bund bis november
lager_bund <- tibble(
  comirnaty_bund = 12.4e6+50.3e6+41.5e6+4.4e6,
  moderna_bund = 1.8e6+6.7e6+2*5.7e6 + 6.6e6,
  comirnaty_le = lieferungen %>% 
    filter(impfstoff=="comirnaty") %>% 
    summarise(ausgeliefert=sum(dosen, na.rm=TRUE)) %>% 
    pull(ausgeliefert) + 3.2e6 + 0.6e6,
  moderna_le = lieferungen %>% 
    filter(impfstoff=="moderna") %>% 
    summarise(ausgeliefert=sum(dosen, na.rm=TRUE)) %>% 
    pull(ausgeliefert) + 4.9e6 + 3.3e6,  
  comirnaty_lager = comirnaty_bund-comirnaty_le,
  moderna_lager = moderna_bund-moderna_le
)

lager_bund %>% select(contains("lager"))

lieferungen_dezember <- tibble(
  comirnaty_bund = 5.9e6+2.9e6,
  moderna_bund = 2*(4.1e6 + 4.6e6 + 3.2e6 + 3.1e6),
  comirnaty_le = 2.9e6 + 5e6 + 2.5e6 + 0.9e6,
  moderna_le = 10.1e6 + 5.5e6 + 8e6 + 8e6
)

unerklaert <- tibble(
  comirnaty = lager_bund$comirnaty_lager + lieferungen_dezember$comirnaty_bund - lieferungen_dezember$comirnaty_le,
  moderna = lager_bund$moderna_lager + lieferungen_dezember$moderna_bund - lieferungen_dezember$moderna_le
)

unerklaert
