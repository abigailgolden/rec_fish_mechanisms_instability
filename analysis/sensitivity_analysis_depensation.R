# Determine sensitivity of the model to the value of depensation parameter d

rm(list = ls())

source("model/model.R")
source("functions/sensitivity_analysis_funs.R")


param_vec <- c(0.001, 0, 0.430, 1.000)
param_names <- c("d", "sd", "rho", "beta")


dep_range <- seq(0.001, 0.301, by = 0.005)
emp_dep <- c(0.04, 0.06, 0.3)

Bmsy <- 0.00001474532

# Run sensitivity analysis along observed range of depensation parameter d --------------------------------------------

dep_kur_coastal <- simulate_along(par_range = dep_range, params = param_vec, par_id = 1, utilfun = Pi_kur_coastal, Emax = 48, Bmsy = 0.00001437875, msy = 0.3998861, utilname = "kur_coastal")
dep_kur_bottom <- simulate_along(par_range = dep_range, params = param_vec, par_id = 1, utilfun = Pi_kur_bottom, Emax = 48, Bmsy = 0.00001407869, utilname = "kur_bottom")
dep_rag_prize <- simulate_along(par_range = dep_range, params = param_vec, par_id = 1, utilfun = Pi_rag_prize, Emax = 48, Bmsy = 0.00001436646, utilname = "rag_prize")
dep_whi_sg <- simulate_along(par_range = dep_range, params = param_vec, par_id = 1, utilfun = Pi_whi_sg, Emax = 48, utilname = "whi_sg")


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



# Visualize output --------------------------------------------------------

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



figname <- paste(todaysdate, "figS1_depensation_sensitivity_analysis.png", sep = "-")
png(paste(outfig, figname, sep = "/"), width = 12, height = 10, units = "in", res = 1000)

(dep_kur_coastal_plot + dep_kur_bottom_plot) / (dep_rag_prize_plot + dep_whi_sg_plot) +plot_layout(guides = "collect")

dev.off()
graphics.off()


