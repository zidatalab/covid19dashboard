divi_berlin <- divi_all %>% filter(id==0) %>%
  pivot_longer(cols=c(faelle_covid_aktuell, betten_belegt, ICU_Betten),
               names_to="Betten",
               values_to="Zahl")

berlinbettenplot <- ggplot(divi_berlin,
                           aes(x=daten_stand, y=Zahl, col=Betten)) +
  geom_line()
berlinbettenplot
