geschlecht <- rki %>% 
  filter(Meldedatum>="2021-11-01") %>% 
  filter(NeuerFall>=0) %>% 
  group_by(Geschlecht) %>% 
  summarise(AnzahlFall=sum(AnzahlFall))

altersgruppe <- rki %>% 
  filter(Meldedatum>="2021-11-01") %>% 
  filter(NeuerFall>=0) %>% 
  group_by(Altersgruppe) %>% 
  summarise(AnzahlFall=sum(AnzahlFall))

alter_steps <- tibble(
  alter=c(0, 4, 5, 14, 15, 34, 35, 59, 60, 79, 80, 94,
          0, 1, 1, 4, 5, 9, 10, 19, 20, 29, 30, 39, 40, 49, 50, 59, 60, 69, 70, 79, 80, 94,
          0, 1, 1, 4, 5, 9, 10, 19, 20, 29, 30, 39, 40, 49, 50, 59, 60, 69, 70, 79, 80, 94),
  anteil=c(c(96832/5, 96832/5, 559596/10, 559596/10, 909209/20, 909209/20, 1124098/20, 1124098/20, 337263/20, 337263/20, 97311/15, 97311/15)/(sum(altersgruppe$AnzahlFall[altersgruppe$Altersgruppe!="unbekannt"])),
           c(rep(c(136/1, 677/4, 1166/5, 2388/10, 2243/10, 3127/10, 2863/10, 2177/10, 1389/10, 525/10, 147/15), each=2)/16838),
           c(rep(c(175/1, 1060/4, 2146/5, 6063/10, 10952/10, 11496/10, 8919/10, 6485/10, 3169/10, 1154/10, 258/15), each=2)/51877)),
  gruppe=rep(c("rki_brd",
           "kaiser_delta",
           "kaiser_omicron"), times=c(12, 22, 22))
)

ggplot(alter_steps, 
       aes(x=alter, y=anteil)) +
  geom_line(aes(col=gruppe))

1569348/(1569348+1527637)
