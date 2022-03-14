# Determine sensitivity of the model to the value of depensation parameter d

rm(list = ls())

source("model/model.R")
source("functions/sensitivity_analysis_funs.R")


param_vec <- c(0.001, 0, 0.430, 1.000, 0.02, 0.875)
param_names <- c("d", "sd", "rho", "beta", "int", "stp")


dep_range <- seq(0.001, 0.301, by = 0.005)
emp_dep <- c(0.04, 0.06, 0.3)

Bmsy <- 0.00001474532

# Run sensitivity analysis along observed range of depensation parameter d --------------------------------------------

dep_li_ls <- simulate_along(par_range = dep_range, params = c(0.001, 0, 0.430, 1.000, 0.02, 0.875), par_id = 1, Emax = 48, Bmsy = 0.00001437875, msy = 0.3998861)
dep_li_hs <- simulate_along(par_range = dep_range, params = c(0.001, 0, 0.430, 1.000, 0.02, 3.5), par_id = 1, Emax = 48, Bmsy = 0.00001407869)
dep_hi_ls <- simulate_along(par_range = dep_range, params = c(0.001, 0, 0.430, 1.000, 0.42, 0.875), par_id = 1, Emax = 48, Bmsy = 0.00001436646)
dep_hi_hs <- simulate_along(par_range = dep_range, params = c(0.001, 0, 0.430, 1.000, 0.42, 3.5), par_id = 1, Emax = 48)


scaled_dat_li_ls <- scale_output(dat = dep_li_ls, ref_param_val = 0.001)
scaled_dat_li_hs <- scale_output(dat = dep_li_hs, ref_param_val = 0.001)
scaled_dat_hi_ls <- scale_output(dat = dep_hi_ls, ref_param_val = 0.001)
scaled_dat_hi_hs <- scale_output(dat = dep_hi_hs, ref_param_val = 0.001)

dat_all <- rbind(
  scaled_dat_li_ls, 
  scaled_dat_li_hs, 
  scaled_dat_hi_ls, 
  scaled_dat_hi_hs)
dat_range <- dat_all %>% 
  group_by(type) %>% 
  summarize(min_val = min(val),
            max_val = max(val),
            min_change = min(pct_change),
            max_change = max(pct_change))



# Visualize output --------------------------------------------------------

dep_li_ls_plot <- outvar_heatmap(dat = scaled_dat_li_ls, 
                                   ref = 0.001,
                                   title = "C) Low intercept, low steepness",
                                   dat_range = dat_range,
                                   emp_dat= emp_dep,
                                   xlab =NULL, ylabelling = TRUE)


dep_li_hs_plot <- outvar_heatmap(dat = scaled_dat_li_hs, 
                                      ref = 0.001,
                                      title = "D) Low intercept, high steepness",
                                      dat_range = dat_range,
                                      emp_dat= emp_dep,
                                      xlab = NULL,
                                      ylabelling = FALSE)

dep_hi_ls_plot <- outvar_heatmap(dat = scaled_dat_hi_ls, 
                                     ref = 0.001,
                                     title = "A) High intercept, low steepness",
                                     dat_range = dat_range,
                                     emp_dat= emp_dep,
                                     xlab = "Depensation parameter d")

dep_hi_hs_plot <- outvar_heatmap(dat = scaled_dat_hi_hs, 
                                  ref = 0.001,
                                  title = "B) High intercept, high steepness",
                                  dat_range = dat_range,
                                  emp_dat= emp_dep,
                                  xlab = "Depensation parameter d",
                                  ylabelling = FALSE)

figname <- "figS1_depensation_sensitivity_analysis.png"
png(paste(outfig, figname, sep = "/"), width = 12, height = 10, units = "in", res = 1000)

(dep_hi_ls_plot + dep_hi_hs_plot) / (dep_li_ls_plot + dep_li_hs_plot) +plot_layout(guides = "collect")

dev.off()
graphics.off()


