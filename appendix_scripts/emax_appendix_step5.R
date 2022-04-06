# Sensitivity analysis to evaluate the effect of Emax on model behavior
# Step 4: How does increasing Emax affect the behavior of density-dependent catchability?

rm(list = ls())

options(scipen = 0)

source("model/model.R")
source("functions/sensitivity_analysis_funs.R")

outfig <- here::here("appendix_figs")


param_vec <- c(0.001, 0, 0.430, 1.000)
param_names <- c("d", "sd", "rho", "beta")

nsims <- 100
Bmsy <- 0.00001474532

# hyperstability values

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

min_beta <- min(betas$Bc)
max_beta <- max(betas$Bc)

b_range <- seq(round(min_beta, 1), max_beta, by = 0.05)
emp_b <- betas$Bc


# Emax = 75 ---------------------------------------------------------------

b_kur_coastal <- simulate_along(par_range = b_range, params = param_vec, par_id = 4, utilfun = Pi_kur_coastal, Emax = 75, Bmsy = 0.00001437875, msy = 0.3998861, utilname = "kur_coastal")
b_kur_bottom <- simulate_along(par_range = b_range, params = param_vec, par_id = 4, utilfun = Pi_kur_bottom,Emax = 75, Bmsy = 0.00001437875, msy = 0.3998861, utilname = "kur_bottom")
b_rag_prize <- simulate_along(par_range = b_range, params = param_vec, par_id = 4, Emax = 75, Bmsy = 0.00001437875, msy = 0.3998861, utilfun = Pi_rag_prize, utilname = "rag_prize")
b_whi_sg <- simulate_along(par_range = b_range, params = param_vec, par_id = 4, utilfun = Pi_whi_sg, Emax = 75, Bmsy = 0.00001437875, msy = 0.3998861, utilname = "whi_sg")

# scale their outputs relative to the reference value

scaled_dat_kur_coastal <- scale_output(dat = b_kur_coastal, ref_param_val = 1)
scaled_dat_kur_bottom <- scale_output(dat = b_kur_bottom, ref_param_val = 1)
scaled_dat_rag_prize <- scale_output(dat = b_rag_prize, ref_param_val = 1)
scaled_dat_whi_sg <- scale_output(dat = b_whi_sg, ref_param_val = 1)

# collect all outputs in one dataframe

dat_all <- rbind(scaled_dat_kur_coastal, scaled_dat_kur_bottom, scaled_dat_rag_prize, scaled_dat_whi_sg)
dat_range <- dat_all %>% 
  group_by(type) %>% 
  summarize(min_val = min(val),
            max_val = max(val),
            min_change = min(pct_change),
            max_change = max(pct_change))

# plot sensitivity analysis outputs for each angler effort function

b_kur_coastal_plot <- outvar_heatmap(dat = scaled_dat_kur_coastal, 
                                     title = "A) High intercept, low steepness",
                                     dat_range = dat_range,
                                     emp_dat= emp_b,
                                     xlab = NULL)


b_kur_bottom_plot <- outvar_heatmap(dat = scaled_dat_kur_bottom, 
                                    title = "B) High intercept, high steepness",
                                    dat_range = dat_range,
                                    emp_dat= emp_b,
                                    xlab = NULL, ylabelling = FALSE)

b_rag_prize_plot <- outvar_heatmap(dat = scaled_dat_rag_prize, 
                                   title = "C) Low intercept, low steepness",
                                   dat_range = dat_range,
                                   emp_dat= emp_b,
                                   xlab = "Density-dependent catchability parameter beta")

b_whi_sg_plot <- outvar_heatmap(dat = scaled_dat_whi_sg,
                                title = "D) Low intercept, high steepness",
                                dat_range = dat_range,
                                emp_dat= emp_b,
                                xlab = "Density-dependent catchability parameter beta",
                                ylabelling = FALSE)

emax75 <- (b_kur_coastal_plot + b_kur_bottom_plot) / (b_rag_prize_plot + b_whi_sg_plot) +plot_layout(guides = "collect")+plot_annotation(title = "Emax = 75")


# Emax = 125 ---------------------------------------------------------------

b_kur_coastal <- simulate_along(par_range = b_range, params = param_vec, par_id = 4, utilfun = Pi_kur_coastal, Emax = 125, Bmsy = 0.00001437875, msy = 0.3998861, utilname = "kur_hms")
b_kur_bottom <- simulate_along(par_range = b_range, params = param_vec, par_id = 4, utilfun = Pi_kur_bottom,Emax = 125, Bmsy = 0.00001437875, msy = 0.3998861, utilname = "kur_bottom")
b_rag_prize <- simulate_along(par_range = b_range, params = param_vec, par_id = 4, Emax = 125, Bmsy = 0.00001437875, msy = 0.3998861, utilfun = Pi_rag_prize, utilname = "rag_prize")
b_whi_sg <- simulate_along(par_range = b_range, params = param_vec, par_id = 4, utilfun = Pi_whi_sg, Emax = 125, Bmsy = 0.00001437875, msy = 0.3998861, utilname = "whi_sg")

# scale their outputs relative to the reference value

scaled_dat_kur_coastal <- scale_output(dat = b_kur_coastal, ref_param_val = 1)
scaled_dat_kur_bottom <- scale_output(dat = b_kur_bottom, ref_param_val = 1)
scaled_dat_rag_prize <- scale_output(dat = b_rag_prize, ref_param_val = 1)
scaled_dat_whi_sg <- scale_output(dat = b_whi_sg, ref_param_val = 1)

# collect all outputs in one dataframe

dat_all <- rbind(scaled_dat_kur_coastal, scaled_dat_kur_bottom, scaled_dat_rag_prize, scaled_dat_whi_sg)
dat_range <- dat_all %>% 
  group_by(type) %>% 
  summarize(min_val = min(val),
            max_val = max(val),
            min_change = min(pct_change),
            max_change = max(pct_change))

# plot sensitivity analysis outputs for each angler effort function

b_kur_coastal_plot <- outvar_heatmap(dat = scaled_dat_kur_coastal, 
                                     title = "A) High intercept, low steepness",
                                     dat_range = dat_range,
                                     emp_dat= emp_b,
                                     xlab = NULL)


b_kur_bottom_plot <- outvar_heatmap(dat = scaled_dat_kur_bottom, 
                                    title = "B) High intercept, high steepness",
                                    dat_range = dat_range,
                                    emp_dat= emp_b,
                                    xlab = NULL, ylabelling = FALSE)

b_rag_prize_plot <- outvar_heatmap(dat = scaled_dat_rag_prize, 
                                   title = "C) Low intercept, low steepness",
                                   dat_range = dat_range,
                                   emp_dat= emp_b,
                                   xlab = "Density-dependent catchability parameter beta")

b_whi_sg_plot <- outvar_heatmap(dat = scaled_dat_whi_sg,
                                title = "D) Low intercept, high steepness",
                                dat_range = dat_range,
                                emp_dat= emp_b,
                                xlab = "Density-dependent catchability parameter beta",
                                ylabelling = FALSE)

emax100 <- (b_kur_coastal_plot + b_kur_bottom_plot) / (b_rag_prize_plot + b_whi_sg_plot) +plot_layout(guides = "collect")+plot_annotation(title = "Emax = 125")



# Emax = 125 ---------------------------------------------------------------

b_kur_coastal <- simulate_along(par_range = b_range, params = param_vec, par_id = 4, utilfun = Pi_kur_coastal, Emax = 125, Bmsy = 0.00001437875, msy = 0.3998861, utilname = "kur_hms")
b_kur_bottom <- simulate_along(par_range = b_range, params = param_vec, par_id = 4, utilfun = Pi_kur_bottom,Emax = 125, Bmsy = 0.00001437875, msy = 0.3998861, utilname = "kur_bottom")
b_rag_prize <- simulate_along(par_range = b_range, params = param_vec, par_id = 4, Emax = 125, Bmsy = 0.00001437875, msy = 0.3998861, utilfun = Pi_rag_prize, utilname = "rag_prize")
b_whi_sg <- simulate_along(par_range = b_range, params = param_vec, par_id = 4, utilfun = Pi_whi_sg, Emax = 125, Bmsy = 0.00001437875, msy = 0.3998861, utilname = "whi_sg")

# scale their outputs relative to the reference value

scaled_dat_kur_coastal <- scale_output(dat = b_kur_coastal, ref_param_val = 1)
scaled_dat_kur_bottom <- scale_output(dat = b_kur_bottom, ref_param_val = 1)
scaled_dat_rag_prize <- scale_output(dat = b_rag_prize, ref_param_val = 1)
scaled_dat_whi_sg <- scale_output(dat = b_whi_sg, ref_param_val = 1)

# collect all outputs in one dataframe

dat_all <- rbind(scaled_dat_kur_coastal, scaled_dat_kur_bottom, scaled_dat_rag_prize, scaled_dat_whi_sg)
dat_range <- dat_all %>% 
  group_by(type) %>% 
  summarize(min_val = min(val),
            max_val = max(val),
            min_change = min(pct_change, na.rm = TRUE),
            max_change = max(pct_change, na.rm = TRUE))

# plot sensitivity analysis outputs for each angler effort function

b_kur_coastal_plot <- outvar_heatmap(dat = scaled_dat_kur_coastal, 
                                     title = "A) High intercept, low steepness",
                                     dat_range = dat_range,
                                     emp_dat= emp_b,
                                     xlab = NULL)


b_kur_bottom_plot <- outvar_heatmap(dat = scaled_dat_kur_bottom, 
                                    title = "B) High intercept, high steepness",
                                    dat_range = dat_range,
                                    emp_dat= emp_b,
                                    xlab = NULL, ylabelling = FALSE)

b_rag_prize_plot <- outvar_heatmap(dat = scaled_dat_rag_prize, 
                                   title = "C) Low intercept, low steepness",
                                   dat_range = dat_range,
                                   emp_dat= emp_b,
                                   xlab = "Density-dependent catchability parameter beta")

b_whi_sg_plot <- outvar_heatmap(dat = scaled_dat_whi_sg,
                                title = "D) Low intercept, high steepness",
                                dat_range = dat_range,
                                emp_dat= emp_b,
                                xlab = "Density-dependent catchability parameter beta",
                                ylabelling = FALSE)

emax125 <- (b_kur_coastal_plot + b_kur_bottom_plot) / (b_rag_prize_plot + b_whi_sg_plot) +plot_layout(guides = "collect")+plot_annotation(title = "Emax = 125")


# Emax = 200 ---------------------------------------------------------------

b_kur_coastal <- simulate_along(par_range = b_range, params = param_vec, par_id = 4, utilfun = Pi_kur_coastal, Emax = 200, Bmsy = 0.00001437875, msy = 0.3998861, utilname = "kur_hms")
b_kur_bottom <- simulate_along(par_range = b_range, params = param_vec, par_id = 4, utilfun = Pi_kur_bottom,Emax = 200, Bmsy = 0.00001437875, msy = 0.3998861, utilname = "kur_bottom")
b_rag_prize <- simulate_along(par_range = b_range, params = param_vec, par_id = 4, Emax = 200, Bmsy = 0.00001437875, msy = 0.3998861, utilfun = Pi_rag_prize, utilname = "rag_prize")
b_whi_sg <- simulate_along(par_range = b_range, params = param_vec, par_id = 4, utilfun = Pi_whi_sg, Emax = 200, Bmsy = 0.00001437875, msy = 0.3998861, utilname = "whi_sg")

# scale their outputs relative to the reference value

scaled_dat_kur_coastal <- scale_output(dat = b_kur_coastal, ref_param_val = 1)
scaled_dat_kur_bottom <- scale_output(dat = b_kur_bottom, ref_param_val = 1)
scaled_dat_rag_prize <- scale_output(dat = b_rag_prize, ref_param_val = 1)
scaled_dat_whi_sg <- scale_output(dat = b_whi_sg, ref_param_val = 1)

# collect all outputs in one dataframe

dat_all <- rbind(scaled_dat_kur_coastal, scaled_dat_kur_bottom, scaled_dat_rag_prize, scaled_dat_whi_sg)
dat_range <- dat_all %>% 
  group_by(type) %>% 
  summarize(min_val = min(val),
            max_val = max(val),
            min_change = min(pct_change, na.rm = TRUE),
            max_change = max(pct_change, na.rm = TRUE))

# plot sensitivity analysis outputs for each angler effort function

b_kur_coastal_plot <- outvar_heatmap(dat = scaled_dat_kur_coastal, 
                                     title = "A) High intercept, low steepness",
                                     dat_range = dat_range,
                                     emp_dat= emp_b,
                                     xlab = NULL)


b_kur_bottom_plot <- outvar_heatmap(dat = scaled_dat_kur_bottom, 
                                    title = "B) High intercept, high steepness",
                                    dat_range = dat_range,
                                    emp_dat= emp_b,
                                    xlab = NULL, ylabelling = FALSE)

b_rag_prize_plot <- outvar_heatmap(dat = scaled_dat_rag_prize, 
                                   title = "C) Low intercept, low steepness",
                                   dat_range = dat_range,
                                   emp_dat= emp_b,
                                   xlab = "Density-dependent catchability parameter beta")

b_whi_sg_plot <- outvar_heatmap(dat = scaled_dat_whi_sg,
                                title = "D) Low intercept, high steepness",
                                dat_range = dat_range,
                                emp_dat= emp_b,
                                xlab = "Density-dependent catchability parameter beta",
                                ylabelling = FALSE)

emax200 <- (b_kur_coastal_plot + b_kur_bottom_plot) / (b_rag_prize_plot + b_whi_sg_plot) +plot_layout(guides = "collect")+plot_annotation(title = "Emax = 200")



figname <- paste(todaysdate, "appendix_fig5_hyperstability.pdf", sep = "-")

pdf(file = paste(outfig, figname, sep = "/"), width = 12, height = 8, onefile = TRUE, title = "Hyperstability sensitivity analysis with varying Emax")

emax75

emax100

emax125

emax200

dev.off()
graphics.off()
