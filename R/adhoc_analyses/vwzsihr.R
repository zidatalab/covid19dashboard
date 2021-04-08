library(minpack.lm)

horizont=60

SIHR <- function(time, state, parameters, ngesamt, qa, nu) {
  par <- as.list(c(state, parameters))
  with(par, {
    dS <- -beta/ngesamt * (I+H) * S
    dI <- beta/ngesamt * (I+H) * S - (1-qa) * gamma * I - qa * delta * I
    dH <- qa * delta * I - nu * H
    dR <- (1-qa) * gamma * I + nu * H
    list(c(dS, dI, dH, dR))
  })
}

ssq <- function(parms) {
  # inital concentration
  init <- c(S=S, I=I, H=H, R=R)
  # time
  times <- seq(0, horizont, by = 1)
  # solve ODE for a given set of parameters
  out <- ode(y=init, times=times, func=SIHR, parms=parms, 
             ngesamt=ngesamt, qa=qa, nu=nu)
  # Filter data that contains time points where data is available
  outdf <- data.frame(out)
  outdf <- outdf[outdf$time %in% df$time, ]
  # Evaluate predicted vs experimental residual
  preddf <- melt(outdf, id.var="time",
                 variable.name="compart", 
                 value.name="anzahl")
  expdf <- melt(df, id.var="time",
             variable.name="compart",
             value.name="anzahl")
  ssqres <- preddf$anzahl-expdf$anzahl
  # return predicted vs experimental residual
  return(ssqres)
}

df <- ausgangsdaten %>% filter(date>="2020-11-01" & date<="2021-03-31" & id==0) %>% 
  arrange(date) %>% 
  head(horizont+1)
day1 <- ausgangsdaten %>% filter(date=="2020-11-01" & id==0)
ngesamt = day1$EW059 + day1$EW6079 + day1$EW80
cases = day1$cases059 + day1$cases6079 + day1$cases80
akutinfiziert = day1$Infected059 + day1$Infected6079 + day1$Infected80
icubelegt = day1$faelle_covid_aktuell
Kapazitaet_Betten = day1$Kapazitaet_Betten
Rt = 1.3
icurate_altersgruppen = 0.0222

infected <- akutinfiziert-icubelegt
recovered <- cases-infected-icubelegt

S = ngesamt - infected - icubelegt - recovered
I = infected
H = icubelegt
R = recovered
qa = icurate_altersgruppen
nu <- 1/9
init <- c("S"=S, "I"=I, "H"=H, "R"=R)
df <- df %>% 
    mutate(S=ngesamt - (cases059 + cases6079 + cases80),
           I=Infected059 + Infected6079 + Infected80 - faelle_covid_aktuell,
           H=faelle_covid_aktuell,
           R=(cases059 + cases6079 + cases80)-I-H,
           time=0:horizont
           ) %>% 
  select(time, S, I, H, R)
  
# parameter fitting using levenberg marquart algorithm
# initial guess for parameters
parms <- c(beta=0.1, gamma=0.1, delta=0.1)
# fitting
fitval <- nls.lm(par=parms, fn=ssq, lower=c(0,0,0), upper=c(1,1,1))



