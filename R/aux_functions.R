SIR <- function(time, state, parameters, ngesamt, gamma) {
  par <- as.list(c(state, parameters))
  with(par, {
    dS <- -beta/ngesamt * I * S
    dI <- beta/ngesamt * I * S - gamma * I
    dR <- gamma * I
    list(c(dS, dI, dR))
  })
}

SIHR <- function(time, state, parameters, ngesamt, gamma, qa, delta, nu) {
  par <- as.list(c(state, parameters))
  with(par, {
    dS <- -beta/ngesamt * (I+H) * S
    dI <- beta/ngesamt * (I+H) * S - (1-qa) * gamma * I - qa * delta * I
    dH <- qa * delta * I - nu * H
    dR <- (1-qa) * gamma * I + nu * H
    list(c(dS, dI, dH, dR))
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
  out <- ode(y = init, times = times, func = SIR, parms = params, 
             ngesamt=ngesamt, gamma=gamma)
  
  # change to data frame and reformat
  out <- as.data.frame(out) %>% select(-time) %>% rename(S=1,I=2,R=3) %>%
    mutate_at(c("S","I","R"),round)
  ## Show data
  return(as_tibble(out))
}

sihrmodel<- function(ngesamt, S, I, H, R, R0, gamma, qa, delta, nu, horizont=365) {
  # Set parameters
  ## Infection parameter beta; gamma: recovery parameter
  params <- c("beta" = R0*((1-qa)*gamma+qa*delta))
  ## Timeframe
  times      <- seq(0, horizont, by = 1)
  ## Initial numbers
  init       <- c("S"=S, "I"=I, "H"=H, "R"=R)
  ## Time frame
  times      <- seq(0, horizont, by = 1)
  
  # Solve using ode (General Solver for Ordinary Differential Equations)
  out <- ode(y = init, times = times, func = SIHR, parms = params, ngesamt=ngesamt, gamma=gamma, qa=qa, delta=delta, nu=nu)
  
  # change to data frame and reformat
  out <- as.data.frame(out) %>% select(-time) %>% rename(S=1, I=2, H=3, R=4) %>%
    mutate_at(c("S", "I", "H", "R"), round)
  ## Show data
  return(as_tibble(out))
}

# Funktion zur Vorwarnzeit bei festem Rt
vorwarnzeit_berechnen_AG <- function(ngesamt, cases, akutinfiziert, icubelegt, 
                                     Kapazitaet_Betten, Rt=1.3, 
                                     icurate_altersgruppen){
  # achtung, hier sind ngesamt, cases und faelle jeweils vektoren der dim 3 (AG 0-59, 60-79, 80+)
  if (is.na(Kapazitaet_Betten)) { # no divi date for this date or kreis 
    myresult <- NA
  } else {
    gamma <- 1/infektperiode # contagious period
    delta <- 1/infekt2icudays # iculag # time till icu
    nu <- 1/icu_days # time in icu
    infected <- akutinfiziert-icubelegt
    recovered <- cases-infected-icubelegt
    mysir_AG <- vector("list", 3)
    for (i in 1:3) {
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
    myresult <- (mysir_AG[[1]]+mysir_AG[[2]]+mysir_AG[[3]]) %>% 
      mutate(Tage=row_number()-1) %>% filter(H>=Kapazitaet_Betten) %>%
      head(1) %>% pull(Tage)
    myresult <- ifelse(is_empty(myresult), NA, myresult)
  }
  return(myresult)
}

projektion_datum <- function(STI_aktuell, STI_Ziel=50, Rt=0.7, tage_infektioes=5) {
  if (STI_aktuell<=STI_Ziel) {
    result <- "\U2713"
  } else if (Rt>=1) {
    result <- "nie"
  } else {
    tage_bis_ziel <- round(tage_infektioes*log(STI_Ziel/STI_aktuell)/log(Rt))
    datum <- lubridate::now()-lubridate::days(1)+lubridate::days(tage_bis_ziel)
    result <- format(datum, format="%d.%m.%Y")
  }
  return(result)
}

projektion_dauer <- function(STI_aktuell, STI_Ziel=50, Rt=0.7, tage_infektioes=5) {
  if (STI_aktuell<=STI_Ziel) {
    result <- 0
  } else if (Rt>=1) {
    result <- NA
  } else {
    tage_bis_ziel <- round(tage_infektioes*log(STI_Ziel/STI_aktuell)/log(Rt))
    result <- tage_bis_ziel
  }
  return(result)
}

projektion_datum2 <- function(STI_aktuell, STI_Ziel=50, STI_alt) {
  if (STI_aktuell<=STI_Ziel) {
    result <- "\U2713"
  } else if (STI_alt<=STI_aktuell) {
    result <- "nie"
  } else {
    x <- 7/log(STI_aktuell/STI_alt)
    tage_bis_ziel <- round(x*log(STI_Ziel/STI_aktuell))
    datum <- lubridate::now()-lubridate::days(1)+lubridate::days(tage_bis_ziel)
    result <- format(datum, format="%d.%m.%Y")
  }
  return(result)
}
