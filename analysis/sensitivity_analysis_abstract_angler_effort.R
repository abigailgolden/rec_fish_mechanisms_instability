# Determine sensitivity of the model to an abstract angler effort function with values of the y-intercept and steepness that range from the lowest to the highest values found in the empirical dataset

rm(list = ls())

source("functions/sensitivity_analysis_funs.R")

options(scipen = 999)

param_vec <- c(0.001, 0, 0, 1.000, 0.01, 0.1)
param_names <- c("d", "sd", "rho", "beta", "int", "stp")

outdir <- here::here("sim_data")
outfig <- here::here("figures")


# Set intercept to range from 0 to 0.5

ints <- seq(0.01, 0.5, by = 0.01)

# Set steepness to range from 0 to 300

stps <- seq(0, 300, by = 0.5)

# create a matrix with every combination of these intercept and steepness values

effort_mat <- expand_grid(intercept = ints, steepness = stps)


# Run the model for each combination of intercept and steepness

# use a single simulation per combo to reduce computation time and because this is all   deterministic
nsims <- 5

angler_effort_dat <- simulate_along_effort(effort_mat = effort_mat, 
                                params = param_vec,
                                Emax = 48,
                                Bmsy = Bmsy,
                                msy = msy)

write.csv(angler_effort_dat,file = paste(outdir, "abstract_angler_effort_sensitivity_analysis.csv", sep = "/"), row.names = FALSE)
