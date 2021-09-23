# Run model across the range of observed values for the 2 values associated with autocorrelated recruitment stochasticity: sd and rho



source("scripts/base_model_vec_multisite.R")
source("functions/sensitivity_analysis_funs.R")

library(ggpubr)

param_vec <- c(0.001, 0, 0, 1.000)
param_names <- c("qRec", "sd", "rho", "beta")


# Run analysis across a range of values of sd with no autocorrelation --------

# mean value of sigma^2 for an unobserved order and for all observed orders is 0.74
# lowest observed value is 0.64 for Pleuronectiformes, 16 stocks
# highest observed value is 0.79 for Perciformes and Scorpaeniformes (19 stocks total)


max_sd <- 0.79
sd_range <- seq(0, round(max_sd,1), by = 0.05)
emp_sd <- c(0.67, 0.77, 0.74, 0.78, 0.64, 0.71, 0.74)

# 4-panel pez plot --------------------------------------------------------

nsims <- 100

Bmsy <- 0.00001474532

sd_kur_hms <- simulate_along(par_range = sd_range, params = param_vec, par_id = 2, utilfun = Pi_kur_hms, Emax = 48, Bmsy = 0.00001437875, msy = 0.3998861, utilname = "kur_hms")
sd_kur_bottom <- simulate_along(par_range = sd_range, params = param_vec, par_id = 2, utilfun = Pi_kur_bottom, Emax = 48, Bmsy = 0.00001437875, msy = 0.3998861, utilname = "kur_bottom")
sd_rag_prize <- simulate_along(par_range = sd_range, params = param_vec, par_id = 2, utilfun = Pi_rag_prize,  Emax = 48, Bmsy = 0.00001437875, msy = 0.3998861, utilname = "rag_prize")
sd_whi_sg <- simulate_along(par_range = sd_range, params = param_vec, par_id = 2, Emax = 48, Bmsy = 0.00001437875, msy = 0.3998861, utilfun = Pi_whi_sg, utilname = "whi_sg")
# sd_whi_other <- simulate_along(par_range = sd_range, params = param_vec, par_id = 2, Emax = 48, Bmsy = Bmsy, utilfun = Pi_whi_other, utilname = "whi_other")
# sd_whi_mackerel <- simulate_along(par_range = sd_range, params = param_vec, par_id = 2, Emax = 48, Bmsy = Bmsy, utilfun = Pi_whi_mackerel, utilname = "whi_mackerel")

sd_scaled_dat_kur_hms <- scale_output(dat = sd_kur_hms, ref_param_val = 0)
sd_scaled_dat_kur_bottom <- scale_output(dat = sd_kur_bottom, ref_param_val = 0)
sd_scaled_dat_rag_prize <- scale_output(dat = sd_rag_prize, ref_param_val = 0)
sd_scaled_dat_whi_sg <- scale_output(dat = sd_whi_sg, ref_param_val = 0)
# scaled_dat_whi_other <- scale_output(dat = sd_whi_other, ref_param_val = 0)
# scaled_dat_whi_mackerel <- scale_output(dat = sd_whi_mackerel, ref_param_val = 0)

sd_dat_all <- rbind(sd_scaled_dat_kur_hms, sd_scaled_dat_kur_bottom, sd_scaled_dat_rag_prize, sd_scaled_dat_whi_sg)
dat_range <- dat_all %>% 
  group_by(type) %>% 
  summarize(min_val = min(val),
            max_val = max(val),
            min_change = min(pct_change),
            max_change = max(pct_change))

sd_kur_hms_plot <- outvar_heatmap(dat = sd_scaled_dat_kur_hms, 
                                 title = "High intercept, low lambda",
                                 dat_range = dat_range,
                                 emp_dat= emp_sd,
                                 xlab = "Standard deviation of recruitment stochasticity")


sd_kur_bottom_plot <- outvar_heatmap(dat = sd_scaled_dat_kur_bottom, 
                                    title = "High intercept, high lambda",
                                    dat_range = dat_range,
                                    emp_dat= emp_sd,
                                    xlab = "Standard deviation of recruitment stochasticity")

sd_rag_prize_plot <- outvar_heatmap(dat = sd_scaled_dat_rag_prize, 
                              title = "Low intercept, low lambda",
                              dat_range = dat_range,
                              emp_dat= emp_sd,
                              xlab = "Standard deviation of recruitment stochasticity")

sd_whi_sg_plot <- outvar_heatmap(dat = sd_scaled_dat_whi_sg, 
                                title = "Low intercept, high lambda",
                                dat_range = dat_range,
                                emp_dat= emp_sd,
                                xlab = "Standard deviation of recruitment stochasticity")




figname <- paste(todaysdate, "sd_plot_Emax_48.png", sep = "-")
png(paste(outfig, figname, sep = "/"), width = 12, height =10, units = "in", res = 1000)

(sd_kur_hms_plot + sd_kur_bottom_plot) / (sd_rag_prize_plot + sd_whi_sg_plot) +plot_layout(guides = "collect") + 
  plot_annotation("Emax = 48")
dev.off()
graphics.off()


# Run analysis across a range of values of rho with mean SD ---------------

# mean rho is 0.45 across all stocks and 0.43 for an unobserved stock
# lowest observation is 0.38 for Salmoniformes (41 stocks)
# highest observation is 0.49  for Aulopiformes and Perciformes (13 stocks total)

max_rho <- 0.49
rho_range <- seq(0, round(max_rho,1), by = 0.02)
emp_rho <- c(0.45, 0.49, 0.42, 0.46, 0.38)

param_vec <- param_vec <- c(0.001, 0.74, 0, 1.000)



# 4-panel pez plot --------------------------------------------------------

nsims <- 100

Bmsy <- 0.00001474532

rho_kur_hms <- simulate_along(par_range = rho_range, params = param_vec, par_id = 3, utilfun = Pi_kur_hms, Emax = 48, Bmsy = 0.00001437875, msy = 0.3998861, utilname = "kur_hms")
rho_kur_bottom <- simulate_along(par_range = rho_range, params = param_vec, par_id = 3, Emax = 48, Bmsy = 0.00001437875, msy = 0.3998861, utilfun = Pi_kur_bottom, utilname = "kur_bottom")
# rho_gent <- simulate_along(par_range = rho_range, params = param_vec, par_id = 3, Emax = 48, Bmsy = Bmsy, utilfun = Pi_gent, utilname = "gent")
rho_whi_sg <- simulate_along(par_range = rho_range, params = param_vec, par_id = 3, utilfun = Pi_whi_sg, Emax = 48, Bmsy = 0.00001437875, msy = 0.3998861, utilname = "whi_sg")
# rho_whi_other <- simulate_along(par_range = rho_range, params = param_vec, par_id = 3, utilfun = Pi_whi_other, Emax = 48, Bmsy = Bmsy, utilname = "whi_other")
# rho_whi_mackerel <- simulate_along(par_range = rho_range, params = param_vec, par_id = 3, utilfun = Pi_whi_mackerel, Emax = 48, Bmsy = Bmsy, utilname = "whi_mackerel")
rho_rag_prize <- simulate_along(par_range = rho_range, params = param_vec, par_id = 3, utilfun = Pi_rag_prize, Emax = 48, Bmsy = 0.00001437875, msy = 0.3998861, utilname = "rag_prize")


rho_scaled_dat_kur_hms <- scale_output(dat = rho_kur_hms, ref_param_val = 0)
rho_scaled_dat_kur_bottom <- scale_output(dat = rho_kur_bottom, ref_param_val = 0)
rho_scaled_dat_whi_sg <- scale_output(dat = rho_whi_sg, ref_param_val = 0)
rho_scaled_dat_rag_prize <- scale_output(dat = rho_rag_prize, ref_param_val = 0)

rho_dat_all <- rbind(rho_scaled_dat_kur_hms, rho_scaled_dat_kur_bottom, 
                 rho_scaled_dat_whi_sg, rho_scaled_dat_rag_prize)
                 
dat_range <- dat_all %>% 
  group_by(type) %>% 
  summarize(min_val = min(val),
            max_val = max(val),
            min_change = min(pct_change),
            max_change = max(pct_change))

rho_kur_hms_plot <- outvar_heatmap(dat = scaled_dat_kur_hms, 
                                   title = "High intercept, low steepness",
                                   dat_range = dat_range,
                                   emp_dat= emp_rho,
                                   xlab ="Autocorrelation parameter rho")


rho_kur_bottom_plot <- outvar_heatmap(dat = scaled_dat_kur_bottom, 
                                      title = "High intercept, high steepness",
                                      dat_range = dat_range,
                                      emp_dat= emp_rho,
                                      xlab = "Autocorrelation parameter rho")

rho_gent_plot <- outvar_heatmap(dat = scaled_dat_gent, 
                                title = "Low intercept, low lambda",
                                dat_range = dat_range,
                                emp_dat= emp_rho,
                                xlab = "Autocorrelation parameter rho")

rho_whi_sg_plot <- outvar_heatmap(dat = scaled_dat_whi_sg, 
                                  title = "Low intercept, high steepness",
                                  dat_range = dat_range,
                                  emp_dat= emp_rho,
                                  xlab = "Autocorrelation parameter rho")


rho_whi_other_plot <- outvar_heatmap(dat = scaled_dat_whi_other, 
                                     title = "Mean intercept, low lambda",
                                     dat_range = dat_range,
                                     emp_dat= emp_rho,
                                     xlab = "Autocorrelation parameter rho")

rho_whi_mackerel_plot <- outvar_heatmap(dat = scaled_dat_whi_mackerel, 
                                        title = "Mean intercept, high lambda",
                                        dat_range = dat_range,
                                        emp_dat= emp_rho,
                                        xlab = "Autocorrelation parameter rho")

rho_rag_prize_plot <- outvar_heatmap(dat = scaled_dat_rag_prize, 
                                        title = "Low intercept, low lambda",
                                        dat_range = dat_range,
                                        emp_dat= emp_rho,
                                        xlab = "Autocorrelation parameter rho")

figname <- paste(todaysdate, "rho_plot_Emax_48.png", sep = "-")
png(paste(outfig, figname, sep = "/"), width = 12, height =10, units = "in", res = 1000)

(rho_kur_hms_plot + rho_kur_bottom_plot) / (rho_rag_prize_plot + rho_whi_sg_plot) + plot_layout(guides = "collect") +  plot_annotation("Emax = 48")

dev.off()
graphics.off()


# Plot sd and rho with common color scales for Figure S2 ------------------


dat_all <- rbind(sd_scaled_dat_kur_hms, sd_scaled_dat_kur_bottom, 
                 sd_scaled_dat_whi_sg, sd_scaled_dat_rag_prize,
                 rho_scaled_dat_kur_hms, rho_scaled_dat_kur_bottom, 
                 rho_scaled_dat_whi_sg, rho_scaled_dat_rag_prize)

dat_range <- dat_all %>% 
  group_by(type) %>% 
  summarize(min_val = min(val),
            max_val = max(val),
            min_change = min(pct_change),
            max_change = max(pct_change))

sd_kur_hms_plot <- outvar_heatmap(dat = sd_scaled_dat_kur_hms, 
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


rho_kur_hms_plot <- outvar_heatmap(dat = rho_scaled_dat_kur_hms, 
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


outfig <- here::here("figures")

figname <- paste(todaysdate, "figS2_stoch_sensitivity_analysis.png", sep = "-")
png(paste(outfig, figname, sep = "/"), width = 15, height = 20, units = "in", res = 1000)

(sd_kur_hms_plot | sd_kur_bottom_plot) / 
  (sd_rag_prize_plot | sd_whi_sg_plot) /
  (rho_kur_hms_plot | rho_kur_bottom_plot) /
  (rho_rag_prize_plot | rho_whi_sg_plot)+plot_layout(guides = "collect")


dev.off()
graphics.off()



ggarrange(sd_kur_hms_plot, sd_kur_bottom_plot,
          sd_rag_prize_plot, sd_whi_sg_plot,
          rho_kur_hms_plot, rho_kur_bottom_plot,
          rho_rag_prize_plot, rho_whi_sg_plot,
          nrow = )

sd_plots <- (sd_kur_hms_plot | sd_kur_bottom_plot) / 
  (sd_rag_prize_plot | sd_whi_sg_plot)+
  plot_layout(guides = "collect")+
  plot_annotation(title = "Normally distributed recruitment stochasticity")

rho_plots <- (rho_kur_hms_plot | rho_kur_bottom_plot) /
  (rho_rag_prize_plot | rho_whi_sg_plot)+plot_layout(guides = "collect")+
  plot_annotation(title = "Autocorrelated recruitment stochasticity")

sd_plots / rho_plots+plot_layout(guides = "collect")


# Create simplified plot for presentations with only biological outcomes --------
  
sd_bio_dat <- sd_dat_all %>% 
  filter(varname == "prop_overfished" | varname == "prop_extirpated") %>% 
  mutate(varname = ordered(varname, levels = c("prop_overfished", "prop_extirpated")),
         val_dodged = ifelse(varname == "prop_extirpated", val + 0.01,
                             val),
         util_scenario = ifelse(cr_fun == "rag_prize", 
                                "Low \u03b1, low \u03bb", 
                                ifelse(cr_fun == "whi_sg", 
                                       "Low \u03b1, high \u03bb",
                                       ifelse(cr_fun == "kur_hms",
                                              "High \u03b1, low \u03bb", 
                                              "High \u03b1, high \u03bb"))),
         util_scenario = ordered(util_scenario, 
                                 levels = c("High \u03b1, low \u03bb", 
                                            "High \u03b1, high \u03bb",
                                            "Low \u03b1, low \u03bb", 
                                            "Low \u03b1, high \u03bb")))

cols <- c("orange", "#ca0020")


outfig <- here::here("figures")


figname <- paste(todaysdate, "figS2_rec_sd_sensitivity_analysis_for_ppt.png", sep = "-")
png(paste(outfig, figname, sep = "/"), width = 8, height = 5, units = "in", res = 1000)

ggplot(sd_bio_dat, aes(x = param_val, y = val_dodged))+
  geom_line(aes(color = varname)
            , size = 1)+
  scale_color_manual(values = cols, name = "",
                     labels = c("Proportion overfished", 
                                "Proportion extirpated"))+
  geom_vline(xintercept = median(emp_sd), linetype = 2, size = 0.5)+
  facet_wrap(util_scenario~., scales = "free")+
  scale_x_continuous(limits = c(0, 0.8))+
  scale_y_continuous(limits = c(0,1.01))+
  labs(x = "Standard deviation of recruitment stochasticity",
       y = "Proportion")+
  theme_classic()+
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 10))

dev.off()
graphics.off()



rho_bio_dat <- rho_dat_all %>% 
  filter(varname == "prop_overfished" | varname == "prop_extirpated") %>% 
  mutate(varname = ordered(varname, levels = c("prop_overfished", "prop_extirpated")),
         val_dodged = ifelse(varname == "prop_extirpated", val + 0.01,
                             val),
         util_scenario = ifelse(cr_fun == "rag_prize", 
                                "Low \u03b1, low \u03bb", 
                                ifelse(cr_fun == "whi_sg", 
                                       "Low \u03b1, high \u03bb",
                                       ifelse(cr_fun == "kur_hms",
                                              "High \u03b1, low \u03bb", 
                                              "High \u03b1, high \u03bb"))),
         util_scenario = ordered(util_scenario, 
                                 levels = c("High \u03b1, low \u03bb", 
                                            "High \u03b1, high \u03bb",
                                            "Low \u03b1, low \u03bb", 
                                            "Low \u03b1, high \u03bb")))

figname <- paste(todaysdate, "figS2_rec_rho_sensitivity_analysis_for_ppt.png", sep = "-")
png(paste(outfig, figname, sep = "/"), width = 8, height = 5, units = "in", res = 1000)

ggplot(rho_bio_dat, aes(x = param_val, y = val_dodged))+
  geom_line(aes(color = varname)
            , size = 1)+
  scale_color_manual(values = cols, name = "",
                     labels = c("Proportion overfished", 
                                "Proportion extirpated"))+
  geom_vline(xintercept = median(emp_rho), linetype = 2, size = 0.5)+
  facet_wrap(util_scenario~., scales = "free")+
  scale_x_continuous(limits = c(0, 0.5))+
  scale_y_continuous(limits = c(0,1.01))+
  labs(x = "Autocorrelation parameter \u03c1",
       y = "Proportion")+
  theme_classic()+
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 10))

dev.off()
graphics.off()
