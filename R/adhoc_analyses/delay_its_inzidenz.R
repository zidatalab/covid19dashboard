map_bl <- tibble(
  BL_ID=0:16,
  geo=c("Gesamt",
        "Schleswig-Holstein",
        "Hamburg",
        "Niedersachsen",
        "Bremen",
        "Nordrhein-Westfalen",
        "Hessen",
        "Rheinland-Pfalz",
        "Baden-Württemberg",
        "Bayern",
        "Saarland",
        "Berlin",
        "Brandenburg",
        "Mecklenburg-Vorpommern",
        "Sachsen",
        "Sachsen-Anhalt",
        "Thüringen")
)

#### icurates nach altersgruppen
# delay für fälle-->icu:
rkidivi <- rki_alter_destatis %>%
  filter(id==0) %>%
  left_join(., divi_all %>% 
              filter(id==0), by=c("Meldedatum"="daten_stand")) %>% 
  drop_na() %>% 
  mutate(inf60steigerung=round(100*Infected60/lag(Infected60, 7)-100),
         itsteigerung=round(100*faelle_covid_aktuell/lag(faelle_covid_aktuell, 7)-100))
lengthrkidivi <- dim(rkidivi)[1]
autocorhorizont <- 30
obshorizont <- 100
autocors <- matrix(0, ncol=(autocorhorizont+1),
                   nrow=(lengthrkidivi-obshorizont+1-autocorhorizont))
for (lag in 0:autocorhorizont) {
  for (startidx in 1:(lengthrkidivi-obshorizont-autocorhorizont+1)) {
    autocors[startidx, lag+1] <- 
      cor(rkidivi$Infected60[startidx:(startidx+obshorizont-1)],
          rkidivi$faelle_covid_aktuell[(startidx+lag):(startidx+obshorizont-1+lag)])
    }
  }
# iculag <- which.max(autocors)-1
iculag_dyn <- apply(autocors, 1, which.max)
plot(iculag_dyn, ylim=c(0,autocorhorizont))
iculag_dyn_plot_data <- tibble(iculag_last100=iculag_dyn,
                               endtag=rkidivi$Meldedatum[obshorizont:(lengthrkidivi-autocorhorizont)])
ggplot(iculag_dyn_plot_data,
       aes(x=endtag, y=iculag_last100)) +
  geom_point()

ggplot(rkidivi %>% 
         pivot_longer(cols=c(Infected60, faelle_covid_aktuell)),
       aes(x=Meldedatum, y=value)) +
  geom_point() +
  facet_wrap(.~name, ncol=1, scales="free_y")

ggplot(rkidivi %>% 
         pivot_longer(cols=contains("steigerung")),
       aes(x=Meldedatum, y=value, col=name)) +
  geom_point() #+
  # facet_wrap(.~name, ncol=1, scales="free_y")


lmhorizont <- 30
itslm_data <- tibble(its=rkidivi %>%
                       filter(Meldedatum>=max(Meldedatum)-days(lmhorizont-1)) %>% 
                       pull(faelle_covid_aktuell),
                     infiziertue60=rkidivi %>% 
                       filter(Meldedatum>=max(Meldedatum)-days(lmhorizont+7) &
                                Meldedatum<max(Meldedatum)-days(7)) %>% 
                       pull(Infected60))

itslm <- lm(its ~ 1 + infiziertue60, itslm_data)
summary(itslm)
itslm$coefficients[1] + itslm$coefficients[2]*(rkidivi %>% 
  filter(Meldedatum>=max(Meldedatum)-days(7)) %>% 
  pull(Infected60))

itslm$coefficients[1] + itslm$coefficients[2]*c(60000, 70000)

# itsglm <- glm(its ~ 1 + infiziertue60, itslm_data, family=poisson)
# summary(itsglm)
# 
# exp(predict(itsglm, newdata = tibble(infiziertue60=rkidivi %>% 
#                                        filter(Meldedatum>=max(Meldedatum)-days(7)) %>% 
#                                        pull(Infected60))))

# its one-week steigerung
itssteigerungen <- divi_all %>% 
  filter(id<17) %>% 
  mutate(Meldedatum=as_date(daten_stand)) %>% 
  group_by(id) %>% 
  arrange(Meldedatum) %>% 
  mutate(itssteigerung_7tage=faelle_covid_aktuell/lag(faelle_covid_aktuell, 7)) %>% 
  ungroup() %>% 
  select(Meldedatum, id, itssteigerung_7tage, faelle_covid_aktuell, ICU_Betten, 
         betten_frei) %>% 
  filter(Meldedatum>max(Meldedatum)-days(7)) %>% 
  group_by(id) %>% 
  arrange(rev(Meldedatum)) %>% 
  summarise(itssteigerung_7tage=mean(itssteigerung_7tage),
            faelle_covid_aktuell=faelle_covid_aktuell[1],
            ICU_Betten=ICU_Betten[1],
            betten_frei=betten_frei[1],
            .groups="drop"
            ) %>% 
  left_join(map_bl, by=c("id"="BL_ID")) %>% 
  mutate(aktuelle_kapazitaet_covid=faelle_covid_aktuell+betten_frei,
         its_projektion_naechstewoche=round(itssteigerung_7tage*faelle_covid_aktuell),
         auslastung_projektion_naechstewoche=round(its_projektion_naechstewoche/aktuelle_kapazitaet_covid, 3),
         itssteigerung_7tage_prozent=round(itssteigerung_7tage-1, 3)) %>% 
  select(Bundesland=geo, 
         `durchschnittl. ITS-Steigerung pro Woche`=itssteigerung_7tage_prozent,
         `Aktuelle Belegung ITS mit COVID-19`=faelle_covid_aktuell,
         `Projektion Belegung ITS mit COVID-19 in 7 Tagen`=its_projektion_naechstewoche,
         `Aktuelle Kapazität für COVID-19-Behandlungen auf ITS`=aktuelle_kapazitaet_covid,
         `Projizierte Auslastung Kapazität in 7 Tagen`=auslastung_projektion_naechstewoche)
library(openxlsx)
write.xlsx(itssteigerungen, "data/its_projektion_7tage.xlsx")
