# Script to determine levels of the intercept and steepness of an abstract angler effort function that produce similar curves to the empirically derived angler effort functions in the previous version of the analysis


rm(list = ls())

source("model/model.R")
source("functions/sensitivity_analysis_funs.R")


param_vec <- c(0.001, 0, 0.430, 1.000, 0.01, 5)
param_names <- c("d", "sd", "rho", "beta", "int", "stp")

Ct <- seq(0,4,by = 0.001)


# figure out a steepness value that reproduces the shape of the high intercept, high steepness empirical scenario (Kuriyama et al. bottomfish)

prob_hi_hs <- logistic(0.42, 3.5, Ct)
effort_hi_hs <- prob_hi_hs*Emax

df <- data.frame(catch = Ct, prob = prob_hi_hs, effort = effort_hi_hs)

plot(Ct, effort_hi_hs, type = "l", #xaxt = "n", yaxt = "n", 
     xlab = "Past catch rates", ylab = "Current effort", ylim = c(0,48))



# figure out a steepness value that reproduces the shape of the low intercept, low steepness empirical scenario (Raguragavan et al. prize fish)

prob_li_ls <- logistic(0.02, 0.875, Ct)
effort_li_ls <- prob_li_ls*Emax

rag_prize_df <- data.frame(catch = Ct, prob = prob_li_ls, effort = effort_li_ls)

plot(Ct, effort_li_ls, type = "l", #xaxt = "n", yaxt = "n", 
     xlab = "Past catch rates", ylab = "Current effort", ylim = c(0,48))


# figure out a steepness value that replicates the shape of the empirical angler effort function with the highest steepness (Whitehead et al. billfish)

prob_highest_s <- logistic(0.128, 270, Ct)
effort_highest_s <- prob_highest_s*Emax

highest_s_df <- data.frame(catch = Ct, prob = prob_highest_s, effort = effort_highest_s)

plot(Ct, effort_highest_s, type = "l", #xaxt = "n", yaxt = "n", 
     xlab = "Past catch rates", ylab = "Current effort",
     xlim = c(0, 0.1), ylim = c(0,48))


# figure out a steepness value that replicates the shape of the empirical angler effort function with the lowest steepness (Raguragavan et al. butterfish)


Ct <- seq(0,100,by = 1)

prob_lowest_s <- logistic(0.019, 0.1, Ct)
effort_lowest_s <- prob_lowest_s*Emax

lowest_s_df <- data.frame(catch = Ct, prob = prob_lowest_s, effort = effort_lowest_s)

plot(Ct, effort_lowest_s, type = "l", #xaxt = "n", yaxt = "n", 
     xlab = "Past catch rates", ylab = "Current effort",
     xlim = c(0, 100), ylim = c(0,48))

