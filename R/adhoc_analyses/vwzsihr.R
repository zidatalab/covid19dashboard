day1 <- ausgangsdaten %>% filter(date=="2020-10-01" & id==0)
ngesamt = c(day1$EW059, day1$EW6079, day1$EW80)
cases = c(day1$cases059, day1$cases6079, day1$cases80)
akutinfiziert = c(day1$Infected059, day1$Infected6079, day1$Infected80)
icubelegt = round(
  (day1$faelle_covid_aktuell*
     altersgruppen_beatmet/
     sum(altersgruppen_beatmet)) %>%
    as.numeric())
Kapazitaet_Betten = day1$Kapazitaet_Betten
Rt = 1.3
# icurate_altersgruppen = c(0.0079, 0.0769,0.0497)
icurate_altersgruppen = c(day1$`bis 60`, day1$`60-80`, day1$`ueber 80`)

gamma <- 1/infektperiode # contagious period
delta <- 1/infekt2icudays # iculag # time till icu
nu <- 1/icu_days # time in icu
infected <- akutinfiziert-icubelegt
recovered <- cases-infected-icubelegt

for (i in 1:3) {
  ngesamti = ngesamt[i]
  S = ngesamt[i] - infected[i] - icubelegt[i] - recovered[i]
  I = infected[i]
  H=icubelegt[i]
  R = recovered[i]
  # R0 = Rt
  # gamma = gamma
  qa = icurate_altersgruppen[i]
  # delta = delta
  # nu = nu
  
  init <- c("S"=S, "I"=I, "H"=H, "R"=R)
  
  out <- ode(y = init, times = times, 
             func = SIHR, parms = params, 
             ngesamt=ngesamti, 
             gamma=gamma, qa=qa, delta=delta, nu=nu)
  
}


