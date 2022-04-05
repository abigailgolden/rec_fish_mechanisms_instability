# Sensitivity analysis to evaluate the effect of Emax on model behavior
# Step 3: How does increasing Emax affect the behavior of depensation?

rm(list = ls())

source("model/model.R")
source("functions/sensitivity_analysis_funs.R")

outfig <- here::here("appendix_figs")


param_vec <- c(0.001, 0, 0.430, 1.000)
param_names <- c("d", "sd", "rho", "beta")


dep_range <- seq(0.001, 0.301, by = 0.005)
emp_dep <- c(0.04, 0.06, 0.3)

Bmsy <- 0.00001474532

# With Emax = 75 ----------------------------------------------------------

dep_kur_coastal <- simulate_along(par_range = dep_range, params = param_vec, par_id = 1, utilfun = Pi_kur_coastal, Emax = 75, Bmsy = 0.00001437875, msy = 0.3998861, utilname = "kur_coastal")
dep_kur_bottom <- simulate_along(par_range = dep_range, params = param_vec, par_id = 1, utilfun = Pi_kur_bottom, Emax = 75, Bmsy = 0.00001407869, utilname = "kur_bottom")
dep_rag_prize <- simulate_along(par_range = dep_range, params = param_vec, par_id = 1, utilfun = Pi_rag_prize, Emax = 75, Bmsy = 0.00001436646, utilname = "rag_prize")
dep_whi_sg <- simulate_along(par_range = dep_range, params = param_vec, par_id = 1, utilfun = Pi_whi_sg, Emax = 75, utilname = "whi_sg")


scaled_dat_kur_coastal <- scale_output(dat = dep_kur_coastal, ref_param_val = 0.001)
scaled_dat_kur_bottom <- scale_output(dat = dep_kur_bottom, ref_param_val = 0.001)
scaled_dat_rag_prize <- scale_output(dat = dep_rag_prize, ref_param_val = 0.001)
scaled_dat_whi_sg <- scale_output(dat = dep_whi_sg, ref_param_val = 0.001)

dat_all <- rbind(
  scaled_dat_kur_coastal, 
  scaled_dat_kur_bottom, 
  scaled_dat_rag_prize, 
  scaled_dat_whi_sg)

dat_range <- dat_all %>% 
  group_by(type) %>% 
  summarize(min_val = min(val),
            max_val = max(val),
            min_change = min(pct_change),
            max_change = max(pct_change))

dep_kur_coastal_plot <- outvar_heatmap(dat = scaled_dat_kur_coastal, 
                                       ref = 0.001,
                                       title = "A) High intercept, low steepness",
                                       dat_range = dat_range,
                                       emp_dat= emp_dep,
                                       xlab = NULL)

dep_kur_bottom_plot <- outvar_heatmap(dat = scaled_dat_kur_bottom, 
                                      ref = 0.001,
                                      title = "B) High intercept, high steepness",
                                      dat_range = dat_range,
                                      emp_dat= emp_dep,
                                      xlab = NULL,
                                      ylabelling = FALSE)

dep_rag_prize_plot <- outvar_heatmap(dat = scaled_dat_rag_prize, 
                                     ref = 0.001,
                                     title = "C) Low intercept, low steepness",
                                     dat_range = dat_range,
                                     emp_dat= emp_dep,
                                     xlab = "Depensation parameter d",
                                     ylabelling = TRUE)


dep_whi_sg_plot <- outvar_heatmap(dat = scaled_dat_whi_sg, 
                                  ref = 0.001,
                                  title = "D) Low intercept, high steepness",
                                  dat_range = dat_range,
                                  emp_dat= emp_dep,
                                  xlab = "Depensation parameter d",
                                  ylabelling = FALSE)


emax75 <- (dep_kur_coastal_plot + dep_kur_bottom_plot) / (dep_rag_prize_plot + dep_whi_sg_plot) +plot_layout(guides = "collect")+plot_annotation(title = "Emax = 75")


# With Emax = 100 ----------------------------------------------------------

dep_kur_coastal <- simulate_along(par_range = dep_range, params = param_vec, par_id = 1, utilfun = Pi_kur_coastal, Emax = 125, Bmsy = 0.00001437875, msy = 0.3998861, utilname = "kur_coastal")
dep_kur_bottom <- simulate_along(par_range = dep_range, params = param_vec, par_id = 1, utilfun = Pi_kur_bottom, Emax = 125, Bmsy = 0.00001407869, utilname = "kur_bottom")
dep_rag_prize <- simulate_along(par_range = dep_range, params = param_vec, par_id = 1, utilfun = Pi_rag_prize, Emax = 125, Bmsy = 0.00001436646, utilname = "rag_prize")
dep_whi_sg <- simulate_along(par_range = dep_range, params = param_vec, par_id = 1, utilfun = Pi_whi_sg, Emax = 125, utilname = "whi_sg")


scaled_dat_kur_coastal <- scale_output(dat = dep_kur_coastal, ref_param_val = 0.001)
scaled_dat_kur_bottom <- scale_output(dat = dep_kur_bottom, ref_param_val = 0.001)
scaled_dat_rag_prize <- scale_output(dat = dep_rag_prize, ref_param_val = 0.001)
scaled_dat_whi_sg <- scale_output(dat = dep_whi_sg, ref_param_val = 0.001)

dat_all <- rbind(
  scaled_dat_kur_coastal, 
  scaled_dat_kur_bottom, 
  scaled_dat_rag_prize, 
  scaled_dat_whi_sg)

dat_range <- dat_all %>% 
  group_by(type) %>% 
  summarize(min_val = min(val),
            max_val = max(val),
            min_change = min(pct_change),
            max_change = max(pct_change))

dep_kur_coastal_plot <- outvar_heatmap(dat = scaled_dat_kur_coastal, 
                                       ref = 0.001,
                                       title = "A) High intercept, low steepness",
                                       dat_range = dat_range,
                                       emp_dat= emp_dep,
                                       xlab = NULL)

dep_kur_bottom_plot <- outvar_heatmap(dat = scaled_dat_kur_bottom, 
                                      ref = 0.001,
                                      title = "B) High intercept, high steepness",
                                      dat_range = dat_range,
                                      emp_dat= emp_dep,
                                      xlab = NULL,
                                      ylabelling = FALSE)

dep_rag_prize_plot <- outvar_heatmap(dat = scaled_dat_rag_prize, 
                                     ref = 0.001,
                                     title = "C) Low intercept, low steepness",
                                     dat_range = dat_range,
                                     emp_dat= emp_dep,
                                     xlab = "Depensation parameter d",
                                     ylabelling = TRUE)


dep_whi_sg_plot <- outvar_heatmap(dat = scaled_dat_whi_sg, 
                                  ref = 0.001,
                                  title = "D) Low intercept, high steepness",
                                  dat_range = dat_range,
                                  emp_dat= emp_dep,
                                  xlab = "Depensation parameter d",
                                  ylabelling = FALSE)


emax100 <- (dep_kur_coastal_plot + dep_kur_bottom_plot) / (dep_rag_prize_plot + dep_whi_sg_plot) +plot_layout(guides = "collect")+plot_annotation(title = "Emax = 125")


# With Emax = 125 ----------------------------------------------------------

dep_kur_coastal <- simulate_along(par_range = dep_range, params = param_vec, par_id = 1, utilfun = Pi_kur_coastal, Emax = 125, Bmsy = 0.00001437875, msy = 0.3998861, utilname = "kur_coastal")
dep_kur_bottom <- simulate_along(par_range = dep_range, params = param_vec, par_id = 1, utilfun = Pi_kur_bottom, Emax = 125, Bmsy = 0.00001407869, utilname = "kur_bottom")
dep_rag_prize <- simulate_along(par_range = dep_range, params = param_vec, par_id = 1, utilfun = Pi_rag_prize, Emax = 125, Bmsy = 0.00001436646, utilname = "rag_prize")
dep_whi_sg <- simulate_along(par_range = dep_range, params = param_vec, par_id = 1, utilfun = Pi_whi_sg, Emax = 125, utilname = "whi_sg")


scaled_dat_kur_coastal <- scale_output(dat = dep_kur_coastal, ref_param_val = 0.001)
scaled_dat_kur_bottom <- scale_output(dat = dep_kur_bottom, ref_param_val = 0.001)
scaled_dat_rag_prize <- scale_output(dat = dep_rag_prize, ref_param_val = 0.001)
scaled_dat_whi_sg <- scale_output(dat = dep_whi_sg, ref_param_val = 0.001)

dat_all <- rbind(
  scaled_dat_kur_coastal, 
  scaled_dat_kur_bottom, 
  scaled_dat_rag_prize, 
  scaled_dat_whi_sg)

dat_range <- dat_all %>% 
  group_by(type) %>% 
  summarize(min_val = min(val),
            max_val = max(val),
            min_change = min(pct_change, na.rm = TRUE),
            max_change = max(pct_change, na.rm = TRUE))

dep_kur_coastal_plot <- outvar_heatmap(dat = scaled_dat_kur_coastal, 
                                       ref = 0.001,
                                       title = "A) High intercept, low steepness",
                                       dat_range = dat_range,
                                       emp_dat= emp_dep,
                                       xlab = NULL)

dep_kur_bottom_plot <- outvar_heatmap(dat = scaled_dat_kur_bottom, 
                                      ref = 0.001,
                                      title = "B) High intercept, high steepness",
                                      dat_range = dat_range,
                                      emp_dat= emp_dep,
                                      xlab = NULL,
                                      ylabelling = FALSE)

dep_rag_prize_plot <- outvar_heatmap(dat = scaled_dat_rag_prize, 
                                     ref = 0.001,
                                     title = "C) Low intercept, low steepness",
                                     dat_range = dat_range,
                                     emp_dat= emp_dep,
                                     xlab = "Depensation parameter d",
                                     ylabelling = TRUE)


dep_whi_sg_plot <- outvar_heatmap(dat = scaled_dat_whi_sg, 
                                  ref = 0.001,
                                  title = "D) Low intercept, high steepness",
                                  dat_range = dat_range,
                                  emp_dat= emp_dep,
                                  xlab = "Depensation parameter d",
                                  ylabelling = FALSE)


emax125 <- (dep_kur_coastal_plot + dep_kur_bottom_plot) / (dep_rag_prize_plot + dep_whi_sg_plot) +plot_layout(guides = "collect")+plot_annotation(title = "Emax = 125")


# Print to a single pdf ---------------------------------------------------
figname <- paste(todaysdate, "appendix_fig3_depensation.pdf", sep = "-")
pdf(file = paste(outfig, figname, sep = "/"), width = 8, height = 6, onefile = TRUE, title = "Depensation sensitivity analysis with varying Emax")

emax75

emax100

emax125

dev.off()
graphics.off()


