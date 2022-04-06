# Sensitivity analysis to evaluate the effect of Emax on model behavior
# Step 4: How does increasing Emax affect the behavior of recruitment stochasticity?

rm(list = ls())

source("model/model.R")
source("functions/sensitivity_analysis_funs.R")

outfig <- here::here("appendix_figs")


param_vec <- c(0.001, 0, 0.430, 1.000)
param_names <- c("d", "sd", "rho", "beta")

nsims <- 100
Bmsy <- 0.00001474532


# Emax = 75 ---------------------------------------------------------------

# Standard deviation

max_sd <- 0.79
sd_range <- seq(0, round(max_sd,1), by = 0.05)
emp_sd <- c(0.67, 0.77, 0.74, 0.78, 0.64, 0.71, 0.74)

sd_kur_coastal <- simulate_along(par_range = sd_range, params = param_vec, par_id = 2, utilfun = Pi_kur_coastal, Emax = 75, Bmsy = 0.00001437875, msy = 0.3998861, utilname = "kur_coastal")
sd_kur_bottom <- simulate_along(par_range = sd_range, params = param_vec, par_id = 2, utilfun = Pi_kur_bottom, Emax = 75, Bmsy = 0.00001437875, msy = 0.3998861, utilname = "kur_bottom")
sd_rag_prize <- simulate_along(par_range = sd_range, params = param_vec, par_id = 2, utilfun = Pi_rag_prize,  Emax = 75, Bmsy = 0.00001437875, msy = 0.3998861, utilname = "rag_prize")
sd_whi_sg <- simulate_along(par_range = sd_range, params = param_vec, par_id = 2, Emax = 75, Bmsy = 0.00001437875, msy = 0.3998861, utilfun = Pi_whi_sg, utilname = "whi_sg")

sd_scaled_dat_kur_coastal <- scale_output(dat = sd_kur_coastal, ref_param_val = 0)
sd_scaled_dat_kur_bottom <- scale_output(dat = sd_kur_bottom, ref_param_val = 0)
sd_scaled_dat_rag_prize <- scale_output(dat = sd_rag_prize, ref_param_val = 0)
sd_scaled_dat_whi_sg <- scale_output(dat = sd_whi_sg, ref_param_val = 0)


# rho ---------------

max_rho <- 0.49
rho_range <- seq(0, round(max_rho,1), by = 0.02)
emp_rho <- c(0.45, 0.49, 0.42, 0.46, 0.38)

param_vec <- param_vec <- c(0.001, 0.74, 0, 1.000)

rho_kur_coastal <- simulate_along(par_range = rho_range, params = param_vec, par_id = 3, utilfun = Pi_kur_coastal, Emax = 75, Bmsy = 0.00001437875, msy = 0.3998861, utilname = "kur_coastal")
rho_kur_bottom <- simulate_along(par_range = rho_range, params = param_vec, par_id = 3, Emax = 75, Bmsy = 0.00001437875, msy = 0.3998861, utilfun = Pi_kur_bottom, utilname = "kur_bottom")
rho_whi_sg <- simulate_along(par_range = rho_range, params = param_vec, par_id = 3, utilfun = Pi_whi_sg, Emax = 75, Bmsy = 0.00001437875, msy = 0.3998861, utilname = "whi_sg")
rho_rag_prize <- simulate_along(par_range = rho_range, params = param_vec, par_id = 3, utilfun = Pi_rag_prize, Emax = 75, Bmsy = 0.00001437875, msy = 0.3998861, utilname = "rag_prize")


rho_scaled_dat_kur_coastal <- scale_output(dat = rho_kur_coastal, ref_param_val = 0)
rho_scaled_dat_kur_bottom <- scale_output(dat = rho_kur_bottom, ref_param_val = 0)
rho_scaled_dat_whi_sg <- scale_output(dat = rho_whi_sg, ref_param_val = 0)
rho_scaled_dat_rag_prize <- scale_output(dat = rho_rag_prize, ref_param_val = 0)


dat_all <- rbind(sd_scaled_dat_kur_coastal, sd_scaled_dat_kur_bottom, 
                 sd_scaled_dat_whi_sg, sd_scaled_dat_rag_prize,
                 rho_scaled_dat_kur_coastal, rho_scaled_dat_kur_bottom, 
                 rho_scaled_dat_whi_sg, rho_scaled_dat_rag_prize)

dat_range <- dat_all %>% 
  group_by(type) %>% 
  summarize(min_val = min(val),
            max_val = max(val),
            min_change = min(pct_change),
            max_change = max(pct_change))

sd_kur_coastal_plot <- outvar_heatmap(dat = sd_scaled_dat_kur_coastal, 
                                      title = "A) High intercept, low steepness",
                                      dat_range = dat_range,
                                      emp_dat= emp_sd,
                                      xlab = "Standard deviation of recruitment stochasticity", ylabelling = TRUE)


sd_kur_bottom_plot <- outvar_heatmap(dat = sd_scaled_dat_kur_bottom, 
                                     title = "B) High intercept, high steepness",
                                     dat_range = dat_range,
                                     emp_dat= emp_sd,
                                     xlab = "Standard deviation of recruitment stochasticity",
                                     ylabelling = FALSE)

sd_rag_prize_plot <- outvar_heatmap(dat = sd_scaled_dat_rag_prize, 
                                    title = "C) Low intercept, low steepness",
                                    dat_range = dat_range,
                                    emp_dat= emp_sd,
                                    xlab = "Standard deviation of recruitment stochasticity")

sd_whi_sg_plot <- outvar_heatmap(dat = sd_scaled_dat_whi_sg, 
                                 title = "D) Low intercept, high steepness",
                                 dat_range = dat_range,
                                 emp_dat= emp_sd,
                                 xlab = "Standard deviation of recruitment stochasticity",
                                 ylabelling = FALSE)


rho_kur_coastal_plot <- outvar_heatmap(dat = rho_scaled_dat_kur_coastal, 
                                       title = "E) High intercept, low steepness",
                                       dat_range = dat_range,
                                       emp_dat= emp_rho,
                                       xlab ="Autocorrelation parameter rho",
                                       ylabelling = TRUE)


rho_kur_bottom_plot <- outvar_heatmap(dat = rho_scaled_dat_kur_bottom, 
                                      title = "F) High intercept, high steepness",
                                      dat_range = dat_range,
                                      emp_dat= emp_rho,
                                      xlab = "Autocorrelation parameter rho", 
                                      ylabelling = FALSE)

rho_rag_prize_plot <- outvar_heatmap(dat = rho_scaled_dat_rag_prize, 
                                     title = "G) Low intercept, low steepness",
                                     dat_range = dat_range,
                                     emp_dat= emp_rho,
                                     xlab = "Autocorrelation parameter rho", ylabelling = TRUE)

rho_whi_sg_plot <- outvar_heatmap(dat = rho_scaled_dat_whi_sg, 
                                  title = "H) Low intercept, high steepness",
                                  dat_range = dat_range,
                                  emp_dat= emp_rho,
                                  xlab = "Autocorrelation parameter rho",
                                  ylabelling = FALSE)

emax75 <- (sd_kur_coastal_plot | sd_kur_bottom_plot) / 
  (sd_rag_prize_plot | sd_whi_sg_plot) /
  (rho_kur_coastal_plot | rho_kur_bottom_plot) /
  (rho_rag_prize_plot | rho_whi_sg_plot)+plot_layout(guides = "collect")+plot_annotation(title = "Emax = 75")


# Emax = 100 ---------------------------------------------------------------

# Standard deviation

max_sd <- 0.79
sd_range <- seq(0, round(max_sd,1), by = 0.05)
emp_sd <- c(0.67, 0.77, 0.74, 0.78, 0.64, 0.71, 0.74)

sd_kur_coastal <- simulate_along(par_range = sd_range, params = param_vec, par_id = 2, utilfun = Pi_kur_coastal, Emax = 100, Bmsy = 0.00001437875, msy = 0.3998861, utilname = "kur_coastal")
sd_kur_bottom <- simulate_along(par_range = sd_range, params = param_vec, par_id = 2, utilfun = Pi_kur_bottom, Emax = 100, Bmsy = 0.00001437875, msy = 0.3998861, utilname = "kur_bottom")
sd_rag_prize <- simulate_along(par_range = sd_range, params = param_vec, par_id = 2, utilfun = Pi_rag_prize,  Emax = 100, Bmsy = 0.00001437875, msy = 0.3998861, utilname = "rag_prize")
sd_whi_sg <- simulate_along(par_range = sd_range, params = param_vec, par_id = 2, Emax = 100, Bmsy = 0.00001437875, msy = 0.3998861, utilfun = Pi_whi_sg, utilname = "whi_sg")

sd_scaled_dat_kur_coastal <- scale_output(dat = sd_kur_coastal, ref_param_val = 0)
sd_scaled_dat_kur_bottom <- scale_output(dat = sd_kur_bottom, ref_param_val = 0)
sd_scaled_dat_rag_prize <- scale_output(dat = sd_rag_prize, ref_param_val = 0)
sd_scaled_dat_whi_sg <- scale_output(dat = sd_whi_sg, ref_param_val = 0)


# rho ---------------

max_rho <- 0.49
rho_range <- seq(0, round(max_rho,1), by = 0.02)
emp_rho <- c(0.45, 0.49, 0.42, 0.46, 0.38)

param_vec <- param_vec <- c(0.001, 0.74, 0, 1.000)

rho_kur_coastal <- simulate_along(par_range = rho_range, params = param_vec, par_id = 3, utilfun = Pi_kur_coastal, Emax = 100, Bmsy = 0.00001437875, msy = 0.3998861, utilname = "kur_coastal")
rho_kur_bottom <- simulate_along(par_range = rho_range, params = param_vec, par_id = 3, Emax = 100, Bmsy = 0.00001437875, msy = 0.3998861, utilfun = Pi_kur_bottom, utilname = "kur_bottom")
rho_whi_sg <- simulate_along(par_range = rho_range, params = param_vec, par_id = 3, utilfun = Pi_whi_sg, Emax = 100, Bmsy = 0.00001437875, msy = 0.3998861, utilname = "whi_sg")
rho_rag_prize <- simulate_along(par_range = rho_range, params = param_vec, par_id = 3, utilfun = Pi_rag_prize, Emax = 100, Bmsy = 0.00001437875, msy = 0.3998861, utilname = "rag_prize")


rho_scaled_dat_kur_coastal <- scale_output(dat = rho_kur_coastal, ref_param_val = 0)
rho_scaled_dat_kur_bottom <- scale_output(dat = rho_kur_bottom, ref_param_val = 0)
rho_scaled_dat_whi_sg <- scale_output(dat = rho_whi_sg, ref_param_val = 0)
rho_scaled_dat_rag_prize <- scale_output(dat = rho_rag_prize, ref_param_val = 0)


dat_all <- rbind(sd_scaled_dat_kur_coastal, sd_scaled_dat_kur_bottom, 
                 sd_scaled_dat_whi_sg, sd_scaled_dat_rag_prize,
                 rho_scaled_dat_kur_coastal, rho_scaled_dat_kur_bottom, 
                 rho_scaled_dat_whi_sg, rho_scaled_dat_rag_prize)

dat_range <- dat_all %>% 
  group_by(type) %>% 
  summarize(min_val = min(val),
            max_val = max(val),
            min_change = min(pct_change, na.rm = TRUE),
            max_change = max(pct_change, na.rm = TRUE))

sd_kur_coastal_plot <- outvar_heatmap(dat = sd_scaled_dat_kur_coastal, 
                                      title = "A) High intercept, low steepness",
                                      dat_range = dat_range,
                                      emp_dat= emp_sd,
                                      xlab = "Standard deviation of recruitment stochasticity", ylabelling = TRUE)


sd_kur_bottom_plot <- outvar_heatmap(dat = sd_scaled_dat_kur_bottom, 
                                     title = "B) High intercept, high steepness",
                                     dat_range = dat_range,
                                     emp_dat= emp_sd,
                                     xlab = "Standard deviation of recruitment stochasticity",
                                     ylabelling = FALSE)

sd_rag_prize_plot <- outvar_heatmap(dat = sd_scaled_dat_rag_prize, 
                                    title = "C) Low intercept, low steepness",
                                    dat_range = dat_range,
                                    emp_dat= emp_sd,
                                    xlab = "Standard deviation of recruitment stochasticity")

sd_whi_sg_plot <- outvar_heatmap(dat = sd_scaled_dat_whi_sg, 
                                 title = "D) Low intercept, high steepness",
                                 dat_range = dat_range,
                                 emp_dat= emp_sd,
                                 xlab = "Standard deviation of recruitment stochasticity",
                                 ylabelling = FALSE)


rho_kur_coastal_plot <- outvar_heatmap(dat = rho_scaled_dat_kur_coastal, 
                                       title = "E) High intercept, low steepness",
                                       dat_range = dat_range,
                                       emp_dat= emp_rho,
                                       xlab ="Autocorrelation parameter rho",
                                       ylabelling = TRUE)


rho_kur_bottom_plot <- outvar_heatmap(dat = rho_scaled_dat_kur_bottom, 
                                      title = "F) High intercept, high steepness",
                                      dat_range = dat_range,
                                      emp_dat= emp_rho,
                                      xlab = "Autocorrelation parameter rho", 
                                      ylabelling = FALSE)

rho_rag_prize_plot <- outvar_heatmap(dat = rho_scaled_dat_rag_prize, 
                                     title = "G) Low intercept, low steepness",
                                     dat_range = dat_range,
                                     emp_dat= emp_rho,
                                     xlab = "Autocorrelation parameter rho", ylabelling = TRUE)

rho_whi_sg_plot <- outvar_heatmap(dat = rho_scaled_dat_whi_sg, 
                                  title = "H) Low intercept, high steepness",
                                  dat_range = dat_range,
                                  emp_dat= emp_rho,
                                  xlab = "Autocorrelation parameter rho",
                                  ylabelling = FALSE)

emax100 <- (sd_kur_coastal_plot | sd_kur_bottom_plot) / 
  (sd_rag_prize_plot | sd_whi_sg_plot) /
  (rho_kur_coastal_plot | rho_kur_bottom_plot) /
  (rho_rag_prize_plot | rho_whi_sg_plot)+plot_layout(guides = "collect")+plot_annotation(title = "Emax = 100")


# Emax = 125 ---------------------------------------------------------------

# Standard deviation

max_sd <- 0.79
sd_range <- seq(0, round(max_sd,1), by = 0.05)
emp_sd <- c(0.67, 0.77, 0.74, 0.78, 0.64, 0.71, 0.74)

sd_kur_coastal <- simulate_along(par_range = sd_range, params = param_vec, par_id = 2, utilfun = Pi_kur_coastal, Emax = 125, Bmsy = 0.00001437875, msy = 0.3998861, utilname = "kur_coastal")
sd_kur_bottom <- simulate_along(par_range = sd_range, params = param_vec, par_id = 2, utilfun = Pi_kur_bottom, Emax = 125, Bmsy = 0.00001437875, msy = 0.3998861, utilname = "kur_bottom")
sd_rag_prize <- simulate_along(par_range = sd_range, params = param_vec, par_id = 2, utilfun = Pi_rag_prize,  Emax = 125, Bmsy = 0.00001437875, msy = 0.3998861, utilname = "rag_prize")
sd_whi_sg <- simulate_along(par_range = sd_range, params = param_vec, par_id = 2, Emax = 125, Bmsy = 0.00001437875, msy = 0.3998861, utilfun = Pi_whi_sg, utilname = "whi_sg")

sd_scaled_dat_kur_coastal <- scale_output(dat = sd_kur_coastal, ref_param_val = 0)
sd_scaled_dat_kur_bottom <- scale_output(dat = sd_kur_bottom, ref_param_val = 0)
sd_scaled_dat_rag_prize <- scale_output(dat = sd_rag_prize, ref_param_val = 0)
sd_scaled_dat_whi_sg <- scale_output(dat = sd_whi_sg, ref_param_val = 0)


# rho ---------------

max_rho <- 0.49
rho_range <- seq(0, round(max_rho,1), by = 0.02)
emp_rho <- c(0.45, 0.49, 0.42, 0.46, 0.38)

param_vec <- param_vec <- c(0.001, 0.74, 0, 1.000)

rho_kur_coastal <- simulate_along(par_range = rho_range, params = param_vec, par_id = 3, utilfun = Pi_kur_coastal, Emax = 125, Bmsy = 0.00001437875, msy = 0.3998861, utilname = "kur_coastal")
rho_kur_bottom <- simulate_along(par_range = rho_range, params = param_vec, par_id = 3, Emax = 125, Bmsy = 0.00001437875, msy = 0.3998861, utilfun = Pi_kur_bottom, utilname = "kur_bottom")
rho_whi_sg <- simulate_along(par_range = rho_range, params = param_vec, par_id = 3, utilfun = Pi_whi_sg, Emax = 125, Bmsy = 0.00001437875, msy = 0.3998861, utilname = "whi_sg")
rho_rag_prize <- simulate_along(par_range = rho_range, params = param_vec, par_id = 3, utilfun = Pi_rag_prize, Emax = 125, Bmsy = 0.00001437875, msy = 0.3998861, utilname = "rag_prize")


rho_scaled_dat_kur_coastal <- scale_output(dat = rho_kur_coastal, ref_param_val = 0)
rho_scaled_dat_kur_bottom <- scale_output(dat = rho_kur_bottom, ref_param_val = 0)
rho_scaled_dat_whi_sg <- scale_output(dat = rho_whi_sg, ref_param_val = 0)
rho_scaled_dat_rag_prize <- scale_output(dat = rho_rag_prize, ref_param_val = 0)


dat_all <- rbind(sd_scaled_dat_kur_coastal, sd_scaled_dat_kur_bottom, 
                 sd_scaled_dat_whi_sg, sd_scaled_dat_rag_prize,
                 rho_scaled_dat_kur_coastal, rho_scaled_dat_kur_bottom, 
                 rho_scaled_dat_whi_sg, rho_scaled_dat_rag_prize)

dat_range <- dat_all %>% 
  group_by(type) %>% 
  summarize(min_val = min(val),
            max_val = max(val),
            min_change = min(pct_change, na.rm = TRUE),
            max_change = max(pct_change, na.rm = TRUE))

sd_kur_coastal_plot <- outvar_heatmap(dat = sd_scaled_dat_kur_coastal, 
                                      title = "A) High intercept, low steepness",
                                      dat_range = dat_range,
                                      emp_dat= emp_sd,
                                      xlab = "Standard deviation of recruitment stochasticity", ylabelling = TRUE)


sd_kur_bottom_plot <- outvar_heatmap(dat = sd_scaled_dat_kur_bottom, 
                                     title = "B) High intercept, high steepness",
                                     dat_range = dat_range,
                                     emp_dat= emp_sd,
                                     xlab = "Standard deviation of recruitment stochasticity",
                                     ylabelling = FALSE)

sd_rag_prize_plot <- outvar_heatmap(dat = sd_scaled_dat_rag_prize, 
                                    title = "C) Low intercept, low steepness",
                                    dat_range = dat_range,
                                    emp_dat= emp_sd,
                                    xlab = "Standard deviation of recruitment stochasticity")

sd_whi_sg_plot <- outvar_heatmap(dat = sd_scaled_dat_whi_sg, 
                                 title = "D) Low intercept, high steepness",
                                 dat_range = dat_range,
                                 emp_dat= emp_sd,
                                 xlab = "Standard deviation of recruitment stochasticity",
                                 ylabelling = FALSE)


rho_kur_coastal_plot <- outvar_heatmap(dat = rho_scaled_dat_kur_coastal, 
                                       title = "E) High intercept, low steepness",
                                       dat_range = dat_range,
                                       emp_dat= emp_rho,
                                       xlab ="Autocorrelation parameter rho",
                                       ylabelling = TRUE)


rho_kur_bottom_plot <- outvar_heatmap(dat = rho_scaled_dat_kur_bottom, 
                                      title = "F) High intercept, high steepness",
                                      dat_range = dat_range,
                                      emp_dat= emp_rho,
                                      xlab = "Autocorrelation parameter rho", 
                                      ylabelling = FALSE)

rho_rag_prize_plot <- outvar_heatmap(dat = rho_scaled_dat_rag_prize, 
                                     title = "G) Low intercept, low steepness",
                                     dat_range = dat_range,
                                     emp_dat= emp_rho,
                                     xlab = "Autocorrelation parameter rho", ylabelling = TRUE)

rho_whi_sg_plot <- outvar_heatmap(dat = rho_scaled_dat_whi_sg, 
                                  title = "H) Low intercept, high steepness",
                                  dat_range = dat_range,
                                  emp_dat= emp_rho,
                                  xlab = "Autocorrelation parameter rho",
                                  ylabelling = FALSE)

emax125 <- (sd_kur_coastal_plot | sd_kur_bottom_plot) / 
  (sd_rag_prize_plot | sd_whi_sg_plot) /
  (rho_kur_coastal_plot | rho_kur_bottom_plot) /
  (rho_rag_prize_plot | rho_whi_sg_plot)+plot_layout(guides = "collect")+plot_annotation(title = "Emax = 125")

figname <- paste(todaysdate, "appendix_fig4_stochasticity.pdf", sep = "-")

pdf(file = paste(outfig, figname, sep = "/"), width = 15, height = 16, onefile = TRUE, title = "Stochasticity sensitivity analysis with varying Emax")

emax75

emax100

emax125

dev.off()
graphics.off()
