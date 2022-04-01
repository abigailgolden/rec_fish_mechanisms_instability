# Script to calculate the steepness of each angler effort functions
# Defined as the inverse of the difference between the CPUE that produces p(fishing) = 0.6 and the CPUE that produces p(fishing) = 0.5 (referred to in this script as c50 and c60)


# Setup -------------------------------------------------------------------


rm(list = ls())
options(scipen = 999)

source("functions/angler_effort_funs.R")
outdir<- here::here("sim_data")


# Calculate c50 and c60 ----------------------------------------------------

steepnesses <- data.frame(citation = rep(NA, nrow(df)), 
                          species = rep(NA, nrow(df)),
                          intercept = rep(NA, nrow(df)), 
                          c50 = rep(NA, nrow(df)), 
                          p50 = rep(NA, nrow(df)), 
                          c60 = rep(NA, nrow(df)), 
                          p60 = rep(NA, nrow(df)), 
                          funlab = rep(NA, nrow(df)))


for (i in 1:nrow(df)){
  # define a function to find the C that produces p50 for each utility function
  find_c50 <- function(x = Ct, fn = funlist[[i]], prob = 0.5){
    abs(fn(x)-prob)
  }
  
  # define a function to find the C that produces p60 for each utility function
  find_c60 <- function(x = Ct, fn = funlist[[i]], prob = 0.60){
    abs(fn(x)-prob)
  }
  
  # use optim to find c50
  rawdat50 <- optim(c(1), find_c50, method = "BFGS", control = list(maxit = 10000000))
  
  c50 <- rawdat50$par
  p50 <- do.call(funlist[[i]], args = list(x = rawdat50$par))
  
  # use optim to find c60
  rawdat60 <- optim(c(1), find_c60, method = "BFGS", control = list(maxit = 10000000))
  c60 <- rawdat60$par
  p60 <- do.call(funlist[[i]], args = list(x = rawdat60$par))
  
  steepnesses[i,] = list(df[i,1], 
                        df[i,2], 
                        df[i,3],
                        c50,
                        p50,
                        c60,
                        p60,
                        names(funlist)[[i]])
}


# add c50 and c60 for billfish and butter fish through manual approximation, since the optimization algorithm doesn't seem to be precise enough for them

steepnesses[5,4] <- 3489.7315
steepnesses[5,5] <- Pi_rag_butter(3489.7315)
steepnesses[5,6] <- 3848.99
steepnesses[5,7] <- Pi_rag_butter(3848.99)


steepnesses[12,4] <- 0.00143465
steepnesses[12,5] <- Pi_whi_billfish(0.00143465)
steepnesses[12,6] <- 0.00173725
steepnesses[12,7] <- Pi_whi_billfish(0.00173725)
 

# calculate the inverse of the difference between c60 and c50
# (we only take the inverse to ensure that increasing values of this metric; that is, smaller differences between c50 and c60; indicate increasingly steep curves)

steepnesses$lambda <- 1/(steepnesses$c60 - steepnesses$c50)

# output this data

lambdas <- steepnesses %>% 
  select(citation, species, lambda)

write.csv(lambdas,file = paste(outdir, "lambdas.csv", sep = "/"), row.names = FALSE)
