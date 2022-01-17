aror1401 <- ausgangsdaten_ror %>% filter(ROR11==1401)
aror1401 <- aror1401[1, ]

ngesamt = c(aror1401$EW059, aror1401$EW60p)
cases = c(aror1401$cases059, aror1401$cases60p)
akutinfiziert = c(aror1401$Infected059,
                  aror1401$Infected60p)
icubelegt = round((aror1401$faelle_covid_aktuell*altersgruppen_beatmet/
                     sum(altersgruppen_beatmet)) %>%
                    as.numeric())
Kapazitaet_Betten = aror1401$Kapazitaet_Betten
Rt = 1.3
icurate_altersgruppen = c(aror1401$icurate_mitgeimpften_60minus, 
                          aror1401$icurate_mitgeimpften_60plus)

    gamma <- 1/infektperiode # contagious period
    delta <- 1/infekt2icudays # iculag # time till icu
    nu <- 1/icu_days # time in icu
    infected <- akutinfiziert-icubelegt
    recovered <- cases-infected-icubelegt
    mysir_AG <- vector("list", 2)
    for (i in 1:2) {
      mysir <- sihrmodel(ngesamt = ngesamt[i],
                         S = ngesamt[i] - infected[i] - icubelegt[i] - recovered[i],
                         I = infected[i],
                         H=icubelegt[i],
                         R = recovered[i],
                         R0 = Rt,
                         gamma = gamma,
                         qa = icurate_altersgruppen[i],
                         delta = delta,
                         nu = nu,
                         horizont = 180) %>% mutate(Neue_ICU_Faelle=H-lag(H))
      mysir_AG[[i]] <- mysir
    }
    myresult <- (mysir_AG[[1]]+mysir_AG[[2]]) %>% 
      mutate(Tage=row_number()-1) %>% filter(H>=Kapazitaet_Betten) %>%
      head(1) %>% pull(Tage)
    myresult <- ifelse(is_empty(myresult), NA, myresult)