# Script to produce Figure 3

# Steps:

# 1. Run sensitivity analysis across the range of empirical values for four parameters of interest (d, sd, rho, beta) using a high intercept, high steepness angler effort function

# 2. Create heatmaps of sensitivity analysis results, using a common color scale across the 4 plots

# 3. Collect them into a single multipanel plot with patchwork


# Setup -------------------------------------------------------------------

# clear workspace and set options

rm(list = ls())
options(scipen = 999)

# set source

source("model/model.R")
source("functions/sensitivity_analysis_funs.R")

# set output folder

outfig <- here::here("figures")

# set default parameter values

param_vec <- c(0.001, 0, 0, 1.000)
param_names <- c("d", "sd", "rho", "beta")
Emax <- 48  # value of effort that produces extinction at equilibrium
Bmsy <- 0.00001437875   # Bmsy when effort is at equilibrium
msy <- 0.3998861        # MSY when effort is at equilibrium

# Depensation -------------------------------------------------------------

# set range of depensation values to loop over
dep_range <- seq(0.001, 0.31, by = 0.01)

# empirically observed values of d
emp_dep <- c(0.04, 0.06, 0.3)

# run sensitivity analysis
dep <- simulate_along(par_range = dep_range, params = param_vec, par_id = 1, utilfun = Pi_kur_bottom, Emax = Emax, Bmsy = Bmsy, msy = msy, utilname = "kur_bottom")

# scale results relative to the reference or null value for depensation
dep_scaled <- scale_output(dat = dep, ref_param_val = 0.001)



# SD of recruitment stochasticity -----------------------------------------

# set range of SD values to loop over
max_sd <- 0.79
sd_range <- seq(0, round(max_sd,1), by = 0.05)

# empirically observed values of sd
emp_sd <- c(0.67, 0.77, 0.74, 0.78, 0.64, 0.71, 0.74)

# run sensitivity analysis
sd <- simulate_along(par_range = sd_range, params = param_vec, par_id = 2, Emax = Emax, Bmsy = Bmsy, msy = msy, utilfun = Pi_kur_bottom, utilname = "kur_bottom")

# scale results relative to null
sd_scaled <- scale_output(dat = sd, ref_param_val = 0)


# autocorrelation coefficient rho -----------------------------------------

# set sd to equal the mean observed sd
param_vec <- c(0.001, 0.74, 0, 1.000)

max_rho <- 0.49
rho_range <- seq(0, round(max_rho,1), by = 0.02)

emp_rho <- c(0.45, 0.49, 0.42, 0.46, 0.38)

rho<- simulate_along(par_range = rho_range, params = param_vec, par_id = 3, Emax = Emax, Bmsy = Bmsy, msy = msy, utilfun = Pi_kur_bottom, utilname = "kur_bottom")

rho_scaled <- scale_output(dat = rho, ref_param_val = 0)


# Density-dependent catchability ------------------------------------------

param_vec <- c(0.001, 0, 0, 1.000)

# read in values of beta from the literature


betas <- data.frame(citation = c("Dassow et al. 2020", "Erisman et al. 2011", 
                                 "Giacomini et al. 2020", 
                                 "Hansen et al. 2005", "Hansen et al. 2005", 
                                 "Mrnak et al. 2018", "Mrnak et al. 2018",
                                 "Pierce and Tomcko 2003", 
                                 "Ward et al. 2013"), 
                    species = c("Micropterus salmoides", "Palabrax clathratus",
                                "Sander vitreus", "Sander vitreus", 
                                "Sander vitreus","Sander vitreus", "Sander vitreus",
                                "Esox lucius", "Oncorhynchus mykiss"), 
                    Family = c("Centrarchidae", "Serranidae", "Percidae", "Percidae", "Percidae", "Percidae", "Percidae", "Esocidae", "Salmonidae"),
                    Bc = c(0.47, 0.46, 1.017, 0.825, 0.659, 0.53, 0.41, 1.7, 0.4276),
                    y = 0,
                    stringsAsFactors = FALSE)

# set range of beta values to loop over
min_beta <- min(betas$Bc)
max_beta <- max(betas$Bc)
b_range <- seq(round(min_beta, 1), max_beta, by = 0.1)

# extract empirical values
emp_b <- betas$Bc

# run sensitivity analysis
hyp <- simulate_along(par_range = b_range, params = param_vec, par_id = 4, Emax = Emax, Bmsy = Bmsy, msy = msy, utilfun = Pi_kur_bottom, utilname = "kur_bottom")
 
hyp_scaled <- scale_output(dat = hyp, ref_param_val = 1)



# Set a common color scale for all panels ---------------------------------

dat_all <- rbind(dep_scaled, sd_scaled, rho_scaled, hyp_scaled)
dat_range <- dat_all %>% 
  group_by(type) %>% 
  summarize(min_val = min(val),
            max_val = max(val),
            min_change = min(pct_change),
            max_change = max(pct_change))


# Create heatmaps ---------------------------------------------------------

dep_plot <- outvar_heatmap(dat = dep_scaled, 
                                   ref = 0.001,
                                   title = "A) Depensation",
                                   dat_range = dat_range,
                                   emp_dat= emp_dep,
                                   xlab = "Depensation parameter d", 
                           ylabelling = TRUE)

sd_plot <- outvar_heatmap(dat = sd_scaled, 
                           ref = 0,
                           title = "C) SD of recruitment stochasticity",
                           dat_range = dat_range,
                           emp_dat= emp_sd,
                           xlab = "Standard deviation of recruitment stochasticity",
                          ylabelling = TRUE)

rho_plot <- outvar_heatmap(dat = rho_scaled,
                           ref = 0,
                           title = "D) Autocorrelation in recruitment stochasticity",
                           dat_range = dat_range,
                           emp_dat= emp_rho,
                           xlab ="Autocorrelation parameter \u03c1",
                           ylabelling = FALSE)

hyp_plot <- outvar_heatmap(dat = hyp_scaled,
                           title = "B) Density dependence in catchability",
                           ref = 1,
                           dat_range = dat_range,
                           emp_dat= emp_b,
                           xlab = "Density-dependent catchability parameter \u03b2",
                           ylabelling = FALSE)

figname <-  paste(todaysdate, "fig3.png", sep = "_")
png(paste(outfig, figname, sep = "/"), width = 12, height = 10, units = "in", res = 1000)

(dep_plot + hyp_plot) / (sd_plot + rho_plot) + plot_layout(guides = "collect")

dev.off()
graphics.off()

