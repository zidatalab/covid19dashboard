SIR <- function(time, state, parameters, ngesamt, gamma) {
  par <- as.list(c(state, parameters))
  with(par, {
    dS <- -beta/ngesamt * I * S
    dI <- beta/ngesamt * I * S - gamma * I
    dR <- gamma * I
    list(c(dS, dI, dR))
  })
}

sirmodel<- function(ngesamt,  S,   I,   R,  R0,  gamma,  horizont=365) {
  # Set parameters
  ## Infection parameter beta; gamma: recovery parameter
  params <- c("beta" = R0*gamma)
  ## Timeframe
  times      <- seq(0, horizont, by = 1)
  ## Initial numbers
  init       <- c("S"=S, "I"=I, "R"=R)
  ## Time frame
  times      <- seq(0, horizont, by = 1)
  
  # Solve using ode (General Solver for Ordinary Differential Equations)
  out <- ode(y = init, times = times, func = SIR, parms = params, ngesamt=ngesamt, gamma=gamma)
  
  # change to data frame and reformat
  out <- as.data.frame(out) %>% select(-time) %>% rename(S=1,I=2,R=3) %>%
    mutate_at(c("S","I","R"),round)
  ## Show data
  return(as_tibble(out))
}

# Funktion zur Vorwarnzeit bei festem Rt
vorwarnzeit_berechnen_AG <- function(ngesamt,cases,faelle,Kapazitaet_Betten,Rt=1.3, icurate_altersgruppen){
  # achtung, hier sind ngesamt, cases und faelle jeweils vektoren der dim 3 (AG 0-59, 60-79, 80+)
  gamma <- 1/10
  infected <- faelle/gamma
  recovered <- cases-infected
  mysir_AG <- vector("list", 3)
  for (i in 1:3) {
    mysir <- sirmodel(ngesamt = ngesamt[i],
                      S = ngesamt[i] - infected[i] - recovered[i],
                      I = infected[i],
                      R = recovered[i],
                      R0 = Rt,
                      gamma = gamma,
                      horizont = 180) %>% mutate(Neue_Faelle_hq=icurate_altersgruppen[i]*(I-lag(I)+R-lag(R)))
    mysir_AG[[i]] <- mysir
  }
  myresult <- (mysir_AG[[1]]+mysir_AG[[2]]+mysir_AG[[3]]) %>% mutate(Tage=row_number()-1) %>% filter(Neue_Faelle_hq>=Kapazitaet_Betten) %>% head(1) %>% pull(Tage)
  return(myresult)
}