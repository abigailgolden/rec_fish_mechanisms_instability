# Function to calculate the threshold of Emax at which extirpation occurs across a range of values of a parameter of interest

rm(list = ls())
source("functions/model_funs.R")
source("functions/angler_effort_funs.R")

library(lubridate)
library(tidyverse)


# Model parameters ----------------------------------------------------

age <- 1:10   # number of ages
t <- 1000      # number of yearly time steps
t_return <- t-500  # point at which to start returning values
Linf <- 1     # von Bertalanffy L_infinity
K <- 0.6      # von Bertalanffy K
i_lw <- 1*10^(-5) # length-weight slope parameter
m <- 3        # length-weight shape parameter
sa <- 0.9    # survivorship
amat <- 2    # age at maturity
asel <- 2    # age entering fishery
f <- 0.2     # fishing mortality
R0 <- 1      # recruits per spawner at equilibrium
RecK <- 10   # compensation ratio
gamma <- 1   # gamma parameter of Deriso stock-recruit function
qfish <- 0.01  #catchability coefficient


# Parameters of interest --------------------------------------------------

d <- 0.001  ## when d approaches 0, no depensation; d = 0 is undefined
sdrec <- 0  ## when sdrec = 0, turns off stochasticity in recruitment
rho <- 0.43 ## when rho = 0, there is no autocorrelation in the recruitment residuals
## default to 0.43, because the "switch" we're interested in is stochasticity vs determinism, not the degree of autocorrelation
beta <- 1   ## when beta = 1, relationship between abundance and CPUE is linear


param_vec <- c(d, sdrec, rho, beta)


# Calculated parameters -------------------------------------------------

# length and weight at age
length_at_age <- Linf*(1-exp(-K*age))
weight_at_age <- i_lw*length_at_age^m

# starting amount of depensation
d0 <- 1 - exp(log(0.5)/d)  

# survivorship under fishing
lx <- c()
lx[1] <- 1
for (i in 2:10){
  lx[i] = lx[i-1]*sa
}

# selectivity and maturity at age
sel <- knife_edge(age, asel)
mat <- knife_edge(age, amat)

# fecundity incidence function and stock-recruit parameters
phiE <- sum(weight_at_age*lx*mat)*d0
alpha_rec <- RecK/phiE
beta_rec <- (RecK-1)/(R0*phiE)
carK <- (phiE*R0)/d0

### Parameters relevant to running simulations and assessing outcomes
Bmsy <- 0.00001432295 # based on angler effort for Whitehead et al. mackerel
msy <-   0.3998698  # based on angler effort for Whitehead et al. mackerel
nsims <- 100


# Model -------------------------------------------------------------------

ext_thresh <- function(params, Emaxes, utilfun, Bmsy, msy, utilname){
  
out <- data.frame(Emax = numeric(), Bt = numeric(), Ct = numeric())
Emax <- Emaxes

d <- params[1]
sdrec <- params[2]
rho <- params[3]
beta <- params[4]


for(e in 1:length(Emax)){
  
  N_at_age <- matrix(data = NA, nrow = t, ncol = length(age))
  Bt <- c()
  Dt <- c()
  AR1 <- c()
  Et <- c()
  f <- c()
  Ct <- c()
  CPUE <- c()
  Pf <- c()
  
  
  
  # start at unfished equilibrium
  N_at_age[1,1] <- R0
  
  for (i in 2:10){
    N_at_age[1,i] <- R0*lx[i]
  }
  
  # error at the first time step is uncorrelated
  AR1[1] <- ifelse(sdrec == 0, 0, rnorm(1, 0, sdrec))
  
  # fishing effort starts at zero
  
  Et[1] <- 0
  f[1] <- 0
  CPUE[1] <- 0
  Pf[1] <- 0
  Ct[1] <- 0
  
  for (y in 1:t){
    for(a in age){
      
      # starting in second time step, calculate dynamic fishing effort based on utility
      if(y >= 2){
        
        Pf[y] <- do.call(utilfun, args = list(x = CPUE[y-1]))
        Et[y] <- Emax[e]*Pf[y]
        f[y] <- qfish*Et[y]
        Ct[y] <- f[y]*sum(sel*N_at_age[y-1,]^beta)
        CPUE[y] <- Ct[y]/Emax[e]
      }
      
      # for age classes above 1 and time steps after the first one, 
      #calculate N at age as a function of fishing effort and survival from previous age and time step
      if( y >= 2 & a >= 2)
      {
        N_at_age[y,a] <- (N_at_age[y-1, a-1] - f[y]*sel[a]*N_at_age[y-1,a-1]^beta)*sa
        # make sure abundance doesn't go negative
        N_at_age[y,a] <- ifelse(N_at_age[y,a] < 0, 0, N_at_age[y,a])
      }
      
      # for first age class, calculate recruitment with depensation and autocorrelated error
      if(y >= 2 & a ==1)
      {
        AR1[y] <- ifelse(sdrec == 0, 0, rho*AR1[y-1] + sqrt(1-rho^2)*rnorm(1, 0, sdrec))
        N_at_age[y,a] <- ((alpha_rec*Bt[y-1]*Dt[y-1])/(1+beta_rec*Bt[y-1]*Dt[y-1]))*exp(AR1[y])
      }
      
      # calculate biomass and depensation term in year y
      Bt[y] <- sum(N_at_age[y,]*weight_at_age*mat)
      Dt[y] <- 1-exp((log(0.5)*Bt[y])/(d*carK))
      
    }
  }
  
  out[e,1] <- Emax[e]
  out[e,2] <- mean(Bt[t_return:t])
  out[e,3] <- mean(Ct[t_return:t])
  out[e,4] <- mean(CPUE[t_return:t])
}

e_extinction <- out %>% 
  filter(Bt == 0) %>% 
  slice(1) %>% 
  select(Emax)

return(e_extinction)

}

ext_thresh(params = c(0.3,0,0,1), Emaxes = c(0:1000),
           utilfun = "Pi_whi_sg", Bmsy = Bmsy, msy = msy, utilname = "snapper-grouper")
