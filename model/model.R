# this script sets up a simplified model of a social-ecological recreational fishery model

# model structure is explained in section 2.2 of the manuscript

# Setup -------------------------------------------------------------------

# clear workspace and set options

rm(list = ls())
options(scipen = 999)

# load packages and functions

library(here)
library(tidyverse)

library(RColorBrewer)

source("functions/model_funs.R")
source("functions/angler_effort_funs.R")
source("functions/vis_funs.R")

# create output path

outfig <- here::here("figures")


# Model parameters ----------------------------------------------------

age <- 1:10   # number of ages
t <- 200      # number of yearly time steps
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
c50 <- 0.1 # scaling parameter for catch


# Parameters of interest --------------------------------------------------

d <- 0.001  ## when d approaches 0, no depensation; d = 0 is undefined
sdrec <- 0  ## when sdrec = 0, turns off stochasticity in recruitment
rho <- 0.43 ## when rho = 0, there is no autocorrelation in the recruitment residuals
## default to 0.43, because the "switch" we're interested in is stochasticity vs determinism, not the degree of autocorrelation
beta <- 1   ## when beta = 1, relationship between abundance and CPUE is linear
int <- 0.42 # the y-intercept represents the probability of fishing when catch is zero; min intercept in our dataset is 0.01, max is 0.45
# Use values 0.02 and 0.42 to get as close as possible to empirical values used in the previous versions of the analysis
stp <- 3.5  # the steepness of the logistic curve indicates how rapidly the probability of fishing increases as catch rates increase; use 0.875 and 3.5 to replicate the steepnesses of the rag_prize (low intercept, low steepness) and kur_bottom (high intercept, high steepness) functions
# to replicate the extremes of the empirical dataset, range from steepness = 0.1 (replicates Raguragavan et al. butterfish) to steepness = 270 (replicates Whitehead et al. billfish)

param_vec <- c(d, sdrec, rho, beta, int, stp)


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


##### Function to run model ------------------------------------------

# function requires the following arguments:
# 1. params: a vector of parameter values to use. Vector must be length 6 and be ordered as follows: depensation parameter d, recruitment variability standard deviation, recruitment variability parameter rho, density-dependent catchability parameter beta, the intercept of the angler effort function, and the steepness of the angler effort function
# 2. nsims: number of simulations to iterate over
# 3. utilfun: the angler effort function to use
# 4. Emax: maximum effort that anglers can exert
# 5. Bmsy: biomass that produces MSY
# 6. msy: maximum sustained yield
# 7. ts: a true/false argument indicating either to return outcome variables (if FALSE) or turn time series of biomass, effort, etc. (if TRUE)

# params <- param_vec
# Emax <- 48
 
simulate <- function(params, nsims, Emax, Bmsy = Bmsy, msy = msy, ts = FALSE){
  
  d <- params[1]
  sdrec <- params[2]
  rho <- params[3]
  beta <- params[4]
  intercept <- params[5]
  steepness <- params[6]

  # create empty matrices to hold calculations of abundance, biomass, depensation, etc.

N_at_age <- array(data = NA, dim = c(t,length(age),nsims))
N_at_age[1,1,] <- 1

for (i in 2:length(age)){
  N_at_age[1,i,] <- R0*lx[i]
}

Bt <- emptymat(t, nsims)
Dt <- emptymat(t, nsims)
AR1 <- emptymat(t, nsims)
Et <- emptymat(t, nsims)
f <- emptymat(t, nsims)
Ct <- emptymat(t, nsims)
CPUE <- emptymat(t, nsims)
Pf <- emptymat(t, nsims)

# error at the first time step is uncorrelated
if (sdrec == 0){
  AR1[1,1:nsims] <- 0
  } else{
  AR1[1,1:nsims] <- rnorm(nsims, 0, sdrec)
}
  
# fishing effort starts at zero
  
Et[1,1:nsims] <- 0
f[1,1:nsims] <- 0
CPUE[1,1:nsims] <- 0
Pf[1,1:nsims] <- 0
Ct[1,1:nsims] <- 0


# Run dynamic model 
for (y in 1:t){
  for(a in age){
    
    # starting in second time step, calculate dynamic fishing effort based on utility
    if(y >= 2){
      
      
      Pf[y,] <- logistic(c = intercept, a = steepness, x = Ct[y-1,]/c50)
      Et[y,] <- Emax*Pf[y,]
      f[y,] <- qfish*Et[y,]
      Ct[y,] <- catch(n = N_at_age[y-1,,], Mf = f[y,])
      CPUE[y,] <- Ct[y,]/Et[y,]
    }
    
    # for age classes above 1 and time steps after the first one, 
    #calculate N at age as a function of fishing effort and survival from previous age and time step
    if( y >= 2 & a >= 2){
      N_at_age[y,a,] <- (N_at_age[y-1, a-1,] - f[y,]*sel[a]*N_at_age[y-1,a-1,]^beta)*sa
      # make sure abundance doesn't go negative
      N_at_age[y,a,] <- cutoff(N_at_age[y,a,])
    }
    
    # for first age class, calculate recruitment with depensation and autocorrelated error
    if(y >= 2 & a == 1){
     AR1[y,] <- recAR(prev_ar = AR1[y-1,], sdrec = sdrec)
     N_at_age[y,a,] <- recruit(ar = AR1[y,], b = Bt[y-1,], dt = Dt[y-1]) 
    }
    
    # calculate biomass and depensation term in year y
    Bt[y,] <- biomass(dat = N_at_age[y,,])
    Dt[y,] <- depensation(d, Bt[y,])
    
  }
}
  
# Analyze model outputs 

  if(ts == FALSE){
  outvars <- data.frame(matrix(nrow = nsims, ncol = 0))

## calculate coefficient of variation of effort
outvars$cv_effort <- cv_effort(effort = Et[101:200,])

## calculate coefficient of variation of biomass
outvars$cv_biomass <- cv_biomass(bt = Bt[101:200,])


## calculate cumulative catch and effort relative to Emax and catch in median scenario
outvars$cumulative_effort <- cumul_effort(et = Et[101:200,], Emax = Emax)
outvars$cumulative_catch <- cumul_catch(ct = Ct[101:200,])

## calculate proportion of simulations resulting in final recruitment = 0
outvars$prop_extirpated <- extirpation(final_rec = N_at_age[200,1,])

## calculate the number of years across all simulations (after y = 100) in which biomass < 0.5*Bmsy
  outvars$prop_overfished <- prop_overfished(start = 101, end = t, dat = Bt)

  return(outvars)
  }else{
    outdat <- list("Bt" = Bt, "Et" = Et, "Ct" = Ct, "CPUE" = CPUE, Nt = apply(N_at_age, c(1,3), sum), "Rt" = N_at_age[,1,])
    return(outdat)
  }
}


