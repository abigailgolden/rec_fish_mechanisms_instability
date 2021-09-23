# Determine sensitivity of the model to the value of density-dependent catchability beta

rm(list = ls())

source("model/model.R")
source("functions/sensitivity_analysis_funs.R")


param_vec <- c(0.001, 0, 0, 1.000)
param_names <- c("d", "sd", "rho", "beta")

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


b_kur_hms <- simulate_along(par_range = b_range, params = param_vec, par_id = 4, utilfun = Pi_kur_hms, Emax = 48, Bmsy = 0.00001437875, msy = 0.3998861, utilname = "kur_hms")
b_kur_bottom <- simulate_along(par_range = b_range, params = param_vec, par_id = 4, utilfun = Pi_kur_bottom,Emax = 48, Bmsy = 0.00001437875, msy = 0.3998861, utilname = "kur_bottom")
b_rag_prize <- simulate_along(par_range = b_range, params = param_vec, par_id = 4, Emax = 48, Bmsy = 0.00001437875, msy = 0.3998861, utilfun = Pi_rag_prize, utilname = "rag_prize")
b_whi_sg <- simulate_along(par_range = b_range, params = param_vec, par_id = 4, utilfun = Pi_whi_sg, Emax = 48, Bmsy = 0.00001437875, msy = 0.3998861, utilname = "whi_sg")

# scale their outputs relative to the reference value

scaled_dat_kur_hms <- scale_output(dat = b_kur_hms, ref_param_val = 1)
scaled_dat_kur_bottom <- scale_output(dat = b_kur_bottom, ref_param_val = 1)
scaled_dat_rag_prize <- scale_output(dat = b_rag_prize, ref_param_val = 1)
scaled_dat_whi_sg <- scale_output(dat = b_whi_sg, ref_param_val = 1)

# collect all outputs in one dataframe

dat_all <- rbind(scaled_dat_kur_hms, scaled_dat_kur_bottom, scaled_dat_rag_prize, scaled_dat_whi_sg)
dat_range <- dat_all %>% 
  group_by(type) %>% 
  summarize(min_val = min(val),
            max_val = max(val),
            min_change = min(pct_change),
            max_change = max(pct_change))

# plot sensitivity analysis outputs for each angler effort function

b_kur_hms_plot <- outvar_heatmap(dat = scaled_dat_kur_hms, 
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


figname <-  "figS3_hyp_sensitivity_analysis.png"
png(paste(outfig, figname, sep = "/"), width = 12, height = 10, units = "in", res = 1000)

(b_kur_hms_plot + b_kur_bottom_plot) / (b_rag_prize_plot + b_whi_sg_plot) +plot_layout(guides = "collect")

dev.off()
graphics.off()

## Why does B > 1 produce higher proportion overfished in whi_sg?

par(mfrow = c(1,3))

test3 <- sim_multisite(params = c(0.001, 0, 0, 1.2), nsims = 100, utilfun = Pi_whi_sg, Emax = 48, Bmsy = 0.00001437875, msy = 0.3998861, ts = TRUE)

plot(1:t, test3[["Bt"]][,1])
abline(h = 0.00001437875)
par(new = T)
plot(1:t, test3[["CPUE"]][,1], type = "l", axes = F, xlab = NA, ylab = NA, col = "coral3")
title(main = "beta = 1.2")

test2 <- sim_multisite(params = c(0.001, 0, 0, 1), nsims = 100, utilfun = Pi_whi_sg, Emax = 48, Bmsy = 0.00001437875, msy = 0.3998861, ts = TRUE)

plot(1:t, test2[["Bt"]][,1])
abline(h = 0.00001437875)
par(new = T)
plot(1:t, test2[["CPUE"]][,1], type = "l", axes = F, xlab = NA, ylab = NA, col = "coral3")
title(main = "beta = 1")

test1<- sim_multisite(params = c(0.001, 0, 0, 0.8), nsims = 100, utilfun = Pi_whi_sg, Emax = 48, Bmsy = 0.00001437875, msy = 0.3998861, ts = TRUE)

plot(1:t, test1[["Bt"]][,1])
abline(h = 0.00001437875)
par(new = T)
plot(1:t, test1[["CPUE"]][,1], type = "l", axes = F, xlab = NA, ylab = NA, col = "coral3")
title(main = "beta = 0.8")

# plot the stable limit cycle produced by whi_sg

outfig <- here::here("figures")

figname <- paste(todaysdate, "figS4_whi_sg_stable_limit_cycle.png", sep = "-")
png(paste(outfig, figname, sep = "/"), width = 6, height = 4, units = "in", res = 1000)

par(mfrow = c(2,3))

plot(test1[["Bt"]][2:200,1], test1[["Et"]][2:200,1], type = "l", xlim = c(0,0.00004), ylim = c(0,48),  xlab = "", ylab = "")
abline(v = 0.5*Bmsy, lty= 2)
points(test1[["Bt"]][2,1], test1[["Et"]][2,1], pch = 16, cex = 1.2)
points(test1[["Bt"]][200,1], test1[["Et"]][200,1], pch = 17, cex = 1.2)
title(main = "\u03b2 = 0.8", xlab = "Biomass", ylab = "Effort")

plot(test2[["Bt"]][2:200,1], test2[["Et"]][2:200,1], type = "l", xlim = c(0,0.00004), ylim = c(0,48), xlab = "", ylab = "")
abline(v = 0.5*Bmsy, lty= 2)
points(test2[["Bt"]][2,1], test2[["Et"]][2,1], pch = 16, cex = 1.2)
points(test2[["Bt"]][200,1], test2[["Et"]][200,1], pch = 17, cex = 1.2)
title(main = "\u03b2 = 1", xlab = "Biomass", ylab = "Effort")

plot(test3[["Bt"]][2:200,1], test3[["Et"]][2:200,1], type = "l",  xlim = c(0,0.00004),ylim = c(0,48), xlab = "", ylab = "")
abline(v = 0.5*Bmsy, lty= 2)
points(test3[["Bt"]][2,1], test3[["Et"]][2,1], pch = 16, cex = 1.2)
points(test3[["Bt"]][200,1], test3[["Et"]][200,1], pch = 17, cex = 1.2)
title(main = "\u03b2 = 1.2", xlab = "Biomass", ylab = "Effort")

## row 2

plot(test1[["Bt"]][2:200,1], test1[["Ct"]][2:200,1], type = "l", xlim = c(0,0.00004), ylim = c(0,2), xlab = "Biomass", ylab = "Catch")
abline(v = 0.5*Bmsy, lty= 2)
points(test1[["Bt"]][2,1], test1[["Ct"]][2,1], pch = 16, cex = 1.2)
points(test1[["Bt"]][200,1], test1[["Ct"]][200,1], pch = 17, cex = 1.2)

plot(test2[["Bt"]][2:200,1], test2[["Ct"]][2:200,1], type = "l", xlim = c(0,0.00004), ylim = c(0,2), xlab = "Biomass", ylab = "")
abline(v = 0.5*Bmsy, lty= 2)
points(test2[["Bt"]][2,1], test2[["Ct"]][2,1], pch = 16, cex = 1.2)
points(test2[["Bt"]][200,1], test2[["Ct"]][200,1], pch = 17, cex = 1.2)

plot(test3[["Bt"]][2:200,1], test3[["Ct"]][2:200,1], type = "l", xlim = c(0,0.00004), ylim = c(0,2), xlab = "Biomass", ylab = "")
abline(v = 0.5*Bmsy, lty= 2)
points(test3[["Bt"]][2,1], test3[["Ct"]][2,1], pch = 16, cex = 1.2)
points(test3[["Bt"]][200,1], test3[["Ct"]][200,1], pch = 17, cex = 1.2)


dev.off()
graphics.off()

# Answer: because whi_sg has cyclic dynamics that a higher value of B disrupts


### Why does B > 1 produce greater cumulative benefits in kur_bottom?

par(mfrow = c(1,3))

test1 <- sim_multisite(params = c(0.001, 0, 0, 1.5), nsims = 100, utilfun = Pi_kur_bottom, Emax = 48, Bmsy = 0.00001437875, msy = 0.3998861, ts = TRUE)

plot(1:t, test1[["Bt"]][,1], type = "l", ylim = c(0,0.00005))
abline(h = 0.00001437875)
text(x = 150, y = 0.00002, labels = "Ct", col = "coral3")
text(x = 150, y = 0.000035, labels = "Et", col = "darkgreen")
par(new = T)
plot(1:t, test1[["Ct"]][,1], type = "l", axes = F, xlab = NA, ylab = NA, col = "coral3")
par(new = T)
plot(1:t, test1[["Et"]][,1], type = "l", axes = F, xlab = NA, ylab = NA, col = "darkgreen")
par(new = T)
plot(1:t, test1[["CPUE"]][,1], type = "l", axes = F, xlab = NA, ylab = NA, col = "purple")
title(main = "kur_bottom\nbeta = 1.5")

test2 <- sim_multisite(params = c(0.001, 0, 0, 1), nsims = 100, utilfun = Pi_kur_bottom, Emax = 48, Bmsy = 0.00001437875, msy = 0.3998861, ts = TRUE)

plot(1:t, test2[["Bt"]][,1], type = "l", ylim = c(0,0.00005))
abline(h = 0.00001437875)
par(new = T)
plot(1:t, test2[["Ct"]][,1], type = "l", axes = F, xlab = NA, ylab = NA, col = "coral3")
par(new = T)
plot(1:t, test2[["Et"]][,1], type = "l", axes = F, xlab = NA, ylab = NA, col = "darkgreen")
par(new = T)
plot(1:t, test2[["CPUE"]][,1], type = "l", axes = F, xlab = NA, ylab = NA, col = "purple")
title(main = "kur_bottom\nbeta = 1")


test3 <- sim_multisite(params = c(0.001, 0, 0, 0.5), nsims = 100, utilfun = Pi_kur_bottom, Emax = 48, Bmsy = 0.00001437875, msy = 0.3998861, ts = TRUE)

plot(1:t, test3[["Bt"]][,1], type = "l", ylim = c(0,0.00005))
abline(h = 0.00001437875)
par(new = T)
plot(1:t, test3[["Ct"]][,1], type = "l", axes = F, xlab = NA, ylab = NA, col = "coral3")
par(new = T)
plot(1:t, test3[["Et"]][,1], type = "l", axes = F, xlab = NA, ylab = NA, col = "darkgreen")
par(new = T)
plot(1:t, test3[["CPUE"]][,1], type = "l", axes = F, xlab = NA, ylab = NA, col = "purple")
title(main = "kur_bottom\nbeta = 0.5")


par(mfrow = c(1,3))

plot(test3[["CPUE"]][49:199, 1], test3[["Et"]][50:200, 1], 
     #xlim = c(0,0.1),
     ylim = c(0, 48))
title(main = "kur_bottom\nbeta = 0.5")
plot(test2[["CPUE"]][49:199, 1], test2[["Et"]][50:200, 1], 
     #xlim = c(0,0.1),
     ylim = c(0, 48))
title(main = "kur_bottom\nbeta = 1")
plot(test1[["CPUE"]][49:199, 1], test1[["Et"]][50:200, 1], 
     #xlim = c(0,0.1),
     ylim = c(0, 48))
title(main = "kur_bottom\nbeta = 1.5")


#, xlim = c(0,0.00005), ylim = c(0, 0.02)

par(mfrow = c(2,3))

plot(test3[["Bt"]][2:200,1], test3[["Et"]][2:200,1], type = "l", xlim = c(0,0.00004), ylim = c(0,48))
title(main = "kur_bottom\nbeta = 0.5")
plot(test2[["Bt"]][2:200,1], test2[["Et"]][2:200,1], type = "l", xlim = c(0,0.00004), ylim = c(0,48))
title(main = "kur_bottom\nbeta = 1")
plot(test1[["Bt"]][2:200,1], test1[["Et"]][2:200,1], type = "l",  xlim = c(0,0.00004),ylim = c(0,48))
title(main = "kur_bottom\nbeta = 1.5")

plot(test3[["Bt"]][2:200,1], test3[["Ct"]][2:200,1], type = "l", xlim = c(0,0.00004), ylim = c(0,2))
plot(test2[["Bt"]][2:200,1], test2[["Ct"]][2:200,1], type = "l", xlim = c(0,0.00004), ylim = c(0,2))
plot(test1[["Bt"]][2:200,1], test1[["Ct"]][2:200,1], type = "l", xlim = c(0,0.00004), ylim = c(0,2))


par(mfrow = c(2,3))

plot(test3[["Bt"]][2:200,1], test3[["Et"]][2:200,1], type = "l", xlim = c(0,0.00004), ylim = c(0,48),  xlab = "", ylab = "")
abline(v = 0.5*Bmsy)
title(main = "beta = 0.8", xlab = "Biomass", ylab = "Effort")
plot(test2[["Bt"]][2:200,1], test2[["Et"]][2:200,1], type = "l", xlim = c(0,0.00004), ylim = c(0,48), xlab = "", ylab = "")
abline(v = 0.5*Bmsy)
title(main = "beta = 1", xlab = "Biomass", ylab = "Effort")
plot(test1[["Bt"]][2:200,1], test1[["Et"]][2:200,1], type = "l",  xlim = c(0,0.00004),ylim = c(0,48), xlab = "", ylab = "")
abline(v = 0.5*Bmsy)
title(main = "beta = 1.2", xlab = "Biomass", ylab = "Effort")

plot(test3[["Bt"]][2:200,1], test3[["Ct"]][2:200,1], type = "l", xlim = c(0,0.00004), ylim = c(0,2), xlab = "Biomass", ylab = "Catch")
abline(v = 0.5*Bmsy)
plot(test2[["Bt"]][2:200,1], test2[["Ct"]][2:200,1], type = "l", xlim = c(0,0.00004), ylim = c(0,2), xlab = "Biomass", ylab = "")
abline(v = 0.5*Bmsy)
plot(test1[["Bt"]][2:200,1], test1[["Ct"]][2:200,1], type = "l", xlim = c(0,0.00004), ylim = c(0,2), xlab = "Biomass", ylab = "")
abline(v = 0.5*Bmsy)


# Create simplified plot for presentations with only biological outcomes --------

bio_dat <- dat_all %>% 
  filter(varname == "prop_overfished" | varname == "prop_extirpated") %>% 
  mutate(varname = ordered(varname, levels = c("prop_overfished", "prop_extirpated")),
    val_dodged = ifelse(varname == "prop_extirpated", val + 0.01,
                             val),
    util_scenario = ifelse(cr_fun == "rag_prize", 
                                "low \u03b1, low \u03bb", 
                                ifelse(cr_fun == "whi_sg", 
                                        "low \u03b1, high \u03bb",
                                        ifelse(cr_fun == "kur_hms",
                                               "high \u03b1, low \u03bb", 
                                               "high \u03b1, high \u03bb"))),
    util_scenario = ordered(util_scenario, 
                            levels = c("high \u03b1, low \u03bb", 
                                       "high \u03b1, high \u03bb",
                                       "low \u03b1, low \u03bb",
                                       "low \u03b1, high \u03bb")))

cols <- c("orange", "#ca0020")
outfig <- here::here("figures")

figname <- paste(todaysdate, "figS3_hyp_sensitivity_analysis_for_ppt.png", sep = "-")
png(paste(outfig, figname, sep = "/"), width = 8, height = 5, units = "in", res = 1000)

ggplot(bio_dat, aes(x = param_val, y = val_dodged))+
  geom_line(aes(color = varname)
                #, linetype = varname
                , size = 1)+
  scale_color_manual(values = cols,
                     name = "",
                     labels = c("Proportion overfished", 
                                "Proportion extirpated"))+
  geom_vline(xintercept = median(emp_b), linetype = 2, size = 0.5)+
  facet_wrap(util_scenario~., scales = "free")+
  scale_x_continuous(limits = c(0.4, 1.7))+
  scale_y_continuous(limits = c(0,1.01))+
  labs(x = "Density-dependent catchability parameter \u03b2",
       y = "Proportion")+
  theme_classic()+
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 10))

dev.off()
graphics.off()
  
