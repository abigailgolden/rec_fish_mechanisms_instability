# Determine sensitivity of the model to the value of density-dependent catchability beta

rm(list = ls())

source("model/model.R")
source("functions/sensitivity_analysis_funs.R")


param_vec <- c(0.001, 0, 0, 1.000, 0.02, 0.875)
param_names <- c("d", "sd", "rho", "beta", "int", "stp")

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
median(betas$Bc)

## test how sensitive the min, max and median of beta are to the presence of walleye
betas_test <- filter(betas, species != "Sander vitreus")
min(betas_test$Bc)
max(betas_test$Bc)
median(betas_test$Bc)

t.test(betas$Bc, betas_test$Bc)
wilcox.test(betas$Bc, betas_test$Bc)
######

b_range <- seq(round(min_beta, 1), max_beta, by = 0.05)
emp_b <- betas$Bc

# Run hyperstability sensitivity analysis for 4 representative angler effort functions-----

Bmsy <- 0.00001474532


b_li_ls <- simulate_along(par_range = b_range, params = c(0.001, 0, 0.430, 1.000, 0.02, 0.875), par_id = 4, Emax = 48, Bmsy = 0.00001437875, msy = 0.3998861)
b_li_hs <- simulate_along(par_range = b_range, params =  c(0.001, 0, 0.430, 1.000, 0.02, 3.5), par_id = 4, Emax = 48, Bmsy = 0.00001437875, msy = 0.3998861)
b_hi_ls <- simulate_along(par_range = b_range, params = c(0.001, 0, 0.430, 1.000, 0.42, 0.875), par_id = 4, Emax = 48, Bmsy = 0.00001437875, msy = 0.3998861)
b_hi_hs <- simulate_along(par_range = b_range, params = c(0.001, 0, 0.430, 1.000, 0.42, 3.5), par_id = 4, Emax = 48, Bmsy = 0.00001437875, msy = 0.3998861)

# scale their outputs relative to the reference value

scaled_dat_li_ls <- scale_output(dat = b_li_ls, ref_param_val = 1)
scaled_dat_li_hs <- scale_output(dat = b_li_hs, ref_param_val = 1)
scaled_dat_hi_ls <- scale_output(dat = b_hi_ls, ref_param_val = 1)
scaled_dat_hi_hs <- scale_output(dat = b_hi_hs, ref_param_val = 1)

# collect all outputs in one dataframe

dat_all <- rbind(scaled_dat_li_ls, scaled_dat_li_hs, scaled_dat_hi_ls, scaled_dat_hi_hs)
dat_range <- dat_all %>% 
  group_by(type) %>% 
  summarize(min_val = min(val),
            max_val = max(val),
            min_change = min(pct_change),
            max_change = max(pct_change))

# plot sensitivity analysis outputs for each angler effort function

b_li_ls_plot <- outvar_heatmap(dat = scaled_dat_li_ls, 
                                     title = "A) Low intercept, low steepness",
                                     dat_range = dat_range,
                                     emp_dat= emp_b,
                                     xlab = NULL)


b_li_hs_plot <- outvar_heatmap(dat = scaled_dat_li_hs, 
                                 title = "B) Low intercept, high steepness",
                                 dat_range = dat_range,
                                 emp_dat= emp_b,
                                 xlab = NULL, ylabelling = FALSE)

b_hi_ls_plot <- outvar_heatmap(dat = scaled_dat_hi_ls, 
                                 title = "C) High intercept, low steepness",
                                 dat_range = dat_range,
                                 emp_dat= emp_b,
                                 xlab = "Density-dependent catchability parameter beta")

b_hi_hs_plot <- outvar_heatmap(dat = scaled_dat_hi_hs,
                                 title = "D) High intercept, high steepness",
                                 dat_range = dat_range,
                                 emp_dat= emp_b,
                                 xlab = "Density-dependent catchability parameter beta",
                                ylabelling = FALSE)


figname <-  "figS3_hyp_sensitivity_analysis.png"
png(paste(outfig, figname, sep = "/"), width = 12, height = 10, units = "in", res = 1000)

(b_hi_ls_plot + b_hi_hs_plot) / (b_li_ls_plot + b_li_hs_plot) +plot_layout(guides = "collect")

dev.off()
graphics.off()

## Why does B > 1 produce more overfishing than B < 1 when a low intercept, high steepness effort function is used?

# To answer this, simulate time series using the Whitehead et al. snapper-grouper function and hyperstable, density independent, and hyperdeplete values of beta

test1<- simulate(params = c(0.001, 0, 0, 0.8, 0.02, 3.5), nsims = 100, Emax = 48, Bmsy = 0.00001437875, msy = 0.3998861, ts = TRUE)

test2 <- simulate(params = c(0.001, 0, 0, 1, 0.02, 3.5), nsims = 100, Emax = 48, Bmsy = 0.00001437875, msy = 0.3998861, ts = TRUE)

test3 <- simulate(params = c(0.001, 0, 0, 1.2, 0.02, 3.5), nsims = 100, Emax = 48, Bmsy = 0.00001437875, msy = 0.3998861, ts = TRUE)


# plot the relationships between biomass + effort and biomass + catch for each of these sets of simulations

figname <- "figS4_stable_limit_cycle.png"
png(paste(outfig, figname, sep = "/"), width = 6, height = 4, units = "in", res = 1000)

par(mfrow = c(2,3))

plot(test1[["Bt"]][2:200,1], test1[["Et"]][2:200,1], type = "l", xlim = c(0,0.000045), ylim = c(0,50),  xlab = "", ylab = "")
abline(v = 0.5*Bmsy, lty= 2)
points(test1[["Bt"]][2,1], test1[["Et"]][2,1], pch = 16, cex = 1.2)
points(test1[["Bt"]][200,1], test1[["Et"]][200,1], pch = 17, cex = 1.2)
title(main = "\u03b2 = 0.8", xlab = "Biomass", ylab = "Effort")

plot(test2[["Bt"]][2:200,1], test2[["Et"]][2:200,1], type = "l", xlim = c(0,0.000045), ylim = c(0,50), xlab = "", ylab = "")
abline(v = 0.5*Bmsy, lty= 2)
points(test2[["Bt"]][2,1], test2[["Et"]][2,1], pch = 16, cex = 1.2)
points(test2[["Bt"]][200,1], test2[["Et"]][200,1], pch = 17, cex = 1.2)
title(main = "\u03b2 = 1", xlab = "Biomass", ylab = "")

plot(test3[["Bt"]][2:200,1], test3[["Et"]][2:200,1], type = "l",  xlim = c(0,0.000045),ylim = c(0,50), xlab = "", ylab = "")
abline(v = 0.5*Bmsy, lty= 2)
points(test3[["Bt"]][2,1], test3[["Et"]][2,1], pch = 16, cex = 1.2)
points(test3[["Bt"]][200,1], test3[["Et"]][200,1], pch = 17, cex = 1.2)
title(main = "\u03b2 = 1.2", xlab = "Biomass", ylab = "")

## row 2

plot(test1[["Bt"]][2:200,1], test1[["Ct"]][2:200,1], type = "l", xlim = c(0,0.00004), ylim = c(0,2.5), xlab = "Biomass", ylab = "Catch")
abline(v = 0.5*Bmsy, lty= 2)
points(test1[["Bt"]][2,1], test1[["Ct"]][2,1], pch = 16, cex = 1.2)
points(test1[["Bt"]][200,1], test1[["Ct"]][200,1], pch = 17, cex = 1.2)

plot(test2[["Bt"]][2:200,1], test2[["Ct"]][2:200,1], type = "l", xlim = c(0,0.00004), ylim = c(0,2.5), xlab = "Biomass", ylab = "")
abline(v = 0.5*Bmsy, lty= 2)
points(test2[["Bt"]][2,1], test2[["Ct"]][2,1], pch = 16, cex = 1.2)
points(test2[["Bt"]][200,1], test2[["Ct"]][200,1], pch = 17, cex = 1.2)

plot(test3[["Bt"]][2:200,1], test3[["Ct"]][2:200,1], type = "l", xlim = c(0,0.00004), ylim = c(0,2.5), xlab = "Biomass", ylab = "")
abline(v = 0.5*Bmsy, lty= 2)
points(test3[["Bt"]][2,1], test3[["Ct"]][2,1], pch = 16, cex = 1.2)
points(test3[["Bt"]][200,1], test3[["Ct"]][200,1], pch = 17, cex = 1.2)


dev.off()
graphics.off()
