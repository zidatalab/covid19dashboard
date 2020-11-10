## delay für fälle-->icu:
rkidivi <- rki_cases_infected %>% left_join(., divi_all %>% filter(id==0), by=c("Meldedatum"="daten_stand")) %>% drop_na()
lengthrkidivi <- dim(rkidivi)[1]
autocorhorizont <- 30
autocors <- rep(0, lengthrkidivi-autocorhorizont+1)
for (lag in 0:autocorhorizont) { autocors[lag+1] <- cor(rkidivi$Infected80[1:(lengthrkidivi-autocorhorizont)], rkidivi$faelle_covid_aktuell[(1+lag):(lengthrkidivi-autocorhorizont+lag)]) }
iculag <- which.max(autocors)-1