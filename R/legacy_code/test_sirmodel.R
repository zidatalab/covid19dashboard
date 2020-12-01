i <- 1
ngesamt = c(vorwarnzeitergebnis$EW059[i], vorwarnzeitergebnis$EW6079[i], vorwarnzeitergebnis$EW80[i])
cases = c(vorwarnzeitergebnis$cases059[i], vorwarnzeitergebnis$cases6079[i], vorwarnzeitergebnis$cases80[i])
faelle = c(vorwarnzeitergebnis$Faelle_letzte_7_Tage_pro_Tag_059[i], vorwarnzeitergebnis$Faelle_letzte_7_Tage_pro_Tag_6079[i], vorwarnzeitergebnis$Faelle_letzte_7_Tage_pro_Tag_80[i])
Kapazitaet_Betten = vorwarnzeitergebnis$Kapazitaet_Betten[i]
Rt = 1.3
icurate_altersgruppen_busse <- icurate_altersgruppen
icurate_altersgruppen = icurate_altersgruppen_busse%>%slice(1)%>%as.numeric()


Tage <- vorwarnzeit_berechnen_AG(ngesamt = c(vorwarnzeitergebnis$EW059[i], vorwarnzeitergebnis$EW6079[i], vorwarnzeitergebnis$EW80[i]),
                                 cases = c(vorwarnzeitergebnis$cases059[i], vorwarnzeitergebnis$cases6079[i], vorwarnzeitergebnis$cases80[i]),
                                 faelle = c(vorwarnzeitergebnis$Faelle_letzte_7_Tage_pro_Tag_059[i], vorwarnzeitergebnis$Faelle_letzte_7_Tage_pro_Tag_6079[i], vorwarnzeitergebnis$Faelle_letzte_7_Tage_pro_Tag_80[i]),
                                 Kapazitaet_Betten = vorwarnzeitergebnis$Kapazitaet_Betten[i],
                                 Rt = 1.3,
                                 icurate_altersgruppen = icurate_altersgruppen)


gamma <- 1/10
infected <- faelle/gamma
recovered <- cases-infected
mysir_AG <- vector("list", 3)
for (idx in 1:3) {
  mysir <- sirmodel(ngesamt = ngesamt[idx],
                    S = ngesamt[idx] - infected[idx] - recovered[idx],
                    I = infected[idx],
                    R = recovered[idx],
                    R0 = Rt,
                    gamma = gamma,
                    horizont = 180) %>% mutate(Neue_Faelle_hq=icurate_altersgruppen[idx]*(I-lag(I)+R-lag(R)))
  mysir_AG[[idx]] <- mysir
}
totalmysir_AG <- (mysir_AG[[1]]+mysir_AG[[2]]+mysir_AG[[3]])
View(totalmysir_AG)
# totalmysir_AG$Neue_ICU_Faelle[1:3]
totalmysir_AG$S[1]-totalmysir_AG$S[2]
totalmysir_AG$S[2]-totalmysir_AG$S[3]
myresult <- (mysir_AG[[1]]+mysir_AG[[2]]+mysir_AG[[3]]) %>% mutate(Tage=row_number()-1) %>% 
  filter(Neue_Faelle_hq>=Kapazitaet_Betten) %>% head(1) %>% pull(Tage)

par(mfrow=c(1,3))
aa <- c((divi_all%>%filter(id==0)%>%arrange(desc(daten_stand))%>%pull(faelle_covid_aktuell))[50:1],
        totalmysir_AG$Neue_Faelle_hq[1:50]*icu_days)
plot(1:100, aa, main="Belegung ICU")

aa <- c((offline_timeseries%>%filter(id==0)%>%arrange(desc(date))%>%pull(cases))[50:1],
        totalmysir_AG$I[1:50]+totalmysir_AG$R[1:50])
plot(1:100, aa, main="Gesamtfallzahl")

aa <- c((offline_timeseries%>%filter(id==0)%>%arrange(desc(date))%>%pull(cases))[50:1]-(offline_timeseries%>%filter(id==0)%>%arrange(desc(date))%>%pull(cases))[51:2],
        totalmysir_AG$S[1:50]-totalmysir_AG$S[2:51])
plot(1:100, aa, main="Neue Fälle")
