# Functions used in setting up base model and calculating output variables


# create knife-edge selectivity or maturity

knife_edge <- function(age, cutoff){
  ifelse(age < cutoff, 0, 1)
}

# set up empty matrices to hold simulation outputs
emptymat <-function(nrows, ncols){
  matrix(data = NA, nrow = nrows, ncol = ncols)
}

# calculate biomass based on N at age

biomass<- function(dat){
  if(nsims == 1){
  bt <- sum(dat*weight_at_age*mat)
  } else {
  bt <- apply(dat, c(2), function(x) sum(x*weight_at_age*mat))
return(bt)
  }
}


# calculate depensation term based on biomass and depensation parameter d
depensation <- function(d, b){
  1 - exp((log(0.5)*b)/(d*carK))
}

# make sure N at age never goes below zero

cutoff <- function(x){
  ifelse(x < 0, 0, x)
}

##calculate recruitment error, with or without autocorrelation

recAR <- function(prev_ar, sdrec){
  if (sdrec == 0){
    AR <- 0
  } else{
    AR <- rho*prev_ar + sqrt(1-rho^2)*rnorm(nsims, 0, sdrec)
  }
  return(AR)
}

# calculate recruitment

recruit <- function(ar, b, dt){
    ((alpha*b*dt)/(1+beta*b*dt))*exp(ar)
}

## calculate catch based on fishing mortality, selectivity at age, N at age, and catchability beta

catch <- function(n, Mf){
  # find vulnerable pop. of fish across ages
  v <- apply(n, c(2), function(x) sum(sel*x^beta_c))
  # catch is a proportion of this
  Mf*v
}


# Functions to calculate outcome variables ---------------------------------

# calculate number of years in which the stock is overfished (Bt < 0.5*Bmsy)

overfished <- function(x){
  ofl <- 0.5*Bmsy
  length(which(x < ofl))
}

# calculate the proportion of years in each simulation in which the population is overfished

prop_overfished <- function(start = 101,end,dat){
  temp <- as.data.frame(dat) %>% 
    slice(start:end) %>% 
    summarize_all(overfished)
  vec <- t(temp/length(start:end))
  return(vec)
}

# calculate the proportion of simulations in which final recruitment = 0

extirpation <- function(final_rec){
  vec <- ifelse(final_rec == 0, 1, 0)
  return(vec)
}

# calculate coefficient of variation of effort

cv_effort <- function(effort){
  effort_sd <- apply(effort, c(2), sd)
  effort_mean <- apply(effort, c(2), mean)
  effort_cv <- effort_sd/effort_mean
  return(effort_cv)
}

# calculate coefficient of variation of biomass

cv_biomass <- function(bt){
  biomass_sd <- apply(bt, c(2), sd)
  biomass_mean <- apply(bt, c(2), mean)
  biomass_cv <- biomass_sd/biomass_mean
  return(biomass_cv)
}

# calculate the cumulative sum of catch as a fraction of MSY

cumul_catch <- function(ct){
  dat <- ct/msy
  vec <- apply(dat, c(2), sum)
  return(vec)
}

# calculate the cumulative effort relative to Emax

cumul_effort <- function(et, Emax){
  dat <- et/Emax
  vec <- apply(dat, c(2), sum)
  return(vec)
}

# calculate variance of effort

effort_var <- function(effort){
  vec <- apply(effort, c(2), var)
  return(vec)
}
