# Determine sensitivity of the model to the value of the two parameters associated with recruitment stochasticity: standard deviation of normally distributed variability (sd) and autocorrelation parameter rho

rm(list = ls())

source("model/model.R")
source("functions/sensitivity_analysis_funs.R")


library(ggpubr)

param_vec <- c(0.001, 0, 0, 1.000, 0.02, 0.875)
param_names <- c("qRec", "sd", "rho", "beta", "int", "stp")


# Run analysis across a range of values of sd with no autocorrelation --------

max_sd <- 0.79
sd_range <- seq(0, round(max_sd,1), by = 0.05)
emp_sd <- c(0.67, 0.77, 0.74, 0.78, 0.64, 0.71, 0.74)


nsims <- 100

Bmsy <- 0.00001474532

sd_li_ls <- simulate_along(par_range = sd_range, params = c(0.001, 0, 0.430, 1.000, 0.02, 0.875), par_id = 2, Emax = 48, Bmsy = 0.00001437875, msy = 0.3998861)
sd_li_hs <- simulate_along(par_range = sd_range, params = c(0.001, 0, 0.430, 1.000, 0.02, 3.5), par_id = 2,  Emax = 48, Bmsy = 0.00001437875, msy = 0.3998861)
sd_hi_ls <- simulate_along(par_range = sd_range, params = c(0.001, 0, 0.430, 1.000, 0.42, 0.875), par_id = 2,  Emax = 48, Bmsy = 0.00001437875, msy = 0.3998861)
sd_hi_hs <- simulate_along(par_range = sd_range, params = c(0.001, 0, 0.430, 1.000, 0.42, 3.5),par_id = 2, Emax = 48, Bmsy = 0.00001437875, msy = 0.3998861)

sd_scaled_dat_li_ls <- scale_output(dat = sd_li_ls, ref_param_val = 0)
sd_scaled_dat_li_hs <- scale_output(dat = sd_li_hs, ref_param_val = 0)
sd_scaled_dat_hi_ls <- scale_output(dat = sd_hi_ls, ref_param_val = 0)
sd_scaled_dat_hi_hs <- scale_output(dat = sd_hi_hs, ref_param_val = 0)


# Run analysis across a range of values of rho with mean SD ---------------

max_rho <- 0.49
rho_range <- seq(0, round(max_rho,1), by = 0.02)
emp_rho <- c(0.45, 0.49, 0.42, 0.46, 0.38)

param_vec <- param_vec <- c(0.001, 0.74, 0, 1.000)

nsims <- 100

Bmsy <- 0.00001474532

rho_li_ls <- simulate_along(par_range = rho_range, params = c(0.001, 0, 0.430, 1.000, 0.02, 0.875), par_id = 3, Emax = 48, Bmsy = 0.00001437875, msy = 0.3998861)
rho_li_hs <- simulate_along(par_range = rho_range, params = c(0.001, 0, 0.430, 1.000, 0.02, 3.5), par_id = 3, Emax = 48, Bmsy = 0.00001437875, msy = 0.3998861)
rho_hi_hs <- simulate_along(par_range = rho_range, params = c(0.001, 0, 0.430, 1.000, 0.42, 0.875), par_id = 3, Emax = 48, Bmsy = 0.00001437875, msy = 0.3998861)
rho_hi_ls <- simulate_along(par_range = rho_range, params = c(0.001, 0, 0.430, 1.000, 0.42, 3.5), par_id = 3, Emax = 48, Bmsy = 0.00001437875, msy = 0.3998861)


rho_scaled_dat_li_ls <- scale_output(dat = rho_li_ls, ref_param_val = 0)
rho_scaled_dat_li_hs <- scale_output(dat = rho_li_hs, ref_param_val = 0)
rho_scaled_dat_hi_hs <- scale_output(dat = rho_hi_hs, ref_param_val = 0)
rho_scaled_dat_hi_ls <- scale_output(dat = rho_hi_ls, ref_param_val = 0)


# Plot the effects of sd and rho on model outputs for Figure S2 ------------------


dat_all <- rbind(sd_scaled_dat_li_ls, sd_scaled_dat_li_hs, 
                 sd_scaled_dat_hi_hs, sd_scaled_dat_hi_ls,
                 rho_scaled_dat_li_ls, rho_scaled_dat_li_hs, 
                 rho_scaled_dat_hi_hs, rho_scaled_dat_hi_ls)

dat_range <- dat_all %>% 
  group_by(type) %>% 
  summarize(min_val = min(val),
            max_val = max(val),
            min_change = min(pct_change),
            max_change = max(pct_change))

sd_li_ls_plot <- outvar_heatmap(dat = sd_scaled_dat_li_ls, 
                                  title = "A) Low intercept, low steepness",
                                  dat_range = dat_range,
                                  emp_dat= emp_sd,
                                  xlab = "Standard deviation of recruitment stochasticity", ylabelling = TRUE)


sd_li_hs_plot <- outvar_heatmap(dat = sd_scaled_dat_li_hs, 
                                     title = "B) Low intercept, high steepness",
                                     dat_range = dat_range,
                                     emp_dat= emp_sd,
                                     xlab = "Standard deviation of recruitment stochasticity",
                                     ylabelling = FALSE)

sd_hi_ls_plot <- outvar_heatmap(dat = sd_scaled_dat_hi_ls, 
                                    title = "C) High intercept, low steepness",
                                    dat_range = dat_range,
                                    emp_dat= emp_sd,
                                    xlab = "Standard deviation of recruitment stochasticity")

sd_hi_hs_plot <- outvar_heatmap(dat = sd_scaled_dat_hi_hs, 
                                 title = "D) High intercept, high steepness",
                                 dat_range = dat_range,
                                 emp_dat= emp_sd,
                                 xlab = "Standard deviation of recruitment stochasticity",
                                 ylabelling = FALSE)


rho_li_ls_plot <- outvar_heatmap(dat = rho_scaled_dat_li_ls, 
                                   title = "E) Low intercept, low steepness",
                                   dat_range = dat_range,
                                   emp_dat= emp_rho,
                                   xlab ="Autocorrelation parameter rho",
                                   ylabelling = TRUE)


rho_li_hs_plot <- outvar_heatmap(dat = rho_scaled_dat_li_hs, 
                                      title = "F) Low intercept, high steepness",
                                      dat_range = dat_range,
                                      emp_dat= emp_rho,
                                      xlab = "Autocorrelation parameter rho", 
                                      ylabelling = FALSE)

rho_hi_ls_plot <- outvar_heatmap(dat = rho_scaled_dat_hi_ls, 
                                     title = "G) High intercept, low steepness",
                                     dat_range = dat_range,
                                     emp_dat= emp_rho,
                                     xlab = "Autocorrelation parameter rho", ylabelling = TRUE)

rho_hi_hs_plot <- outvar_heatmap(dat = rho_scaled_dat_hi_hs, 
                                  title = "H) High intercept, high steepness",
                                  dat_range = dat_range,
                                  emp_dat= emp_rho,
                                  xlab = "Autocorrelation parameter rho",
                                  ylabelling = FALSE)



figname <-"figS2_stochasticity_sensitivity_analysis.png"
png(paste(outfig, figname, sep = "/"), width = 15, height = 20, units = "in", res = 1000)

(sd_hi_ls_plot | sd_hi_hs_plot) / 
  (sd_li_ls_plot | sd_li_hs_plot) /
  (rho_hi_ls_plot | rho_hi_hs_plot) /
  (rho_li_ls_plot | rho_li_hs_plot)+plot_layout(guides = "collect")


dev.off()
graphics.off()


