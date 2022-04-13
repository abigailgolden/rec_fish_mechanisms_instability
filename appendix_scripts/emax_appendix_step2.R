# Sensitivity analysis to evaluate the effect of Emax on model behavior
# Step 2: see how the model responds to eacj angler effort function in the presence of different values of Emax

rm(list = ls())

source("model/model.R")
source("functions/sensitivity_analysis_funs.R")

options(scipen = 999)

param_vec <- c(0.001, 0, 0, 1.000)
param_names <- c("d", "sd", "rho", "beta")
outfig <- here::here("appendix_figs")
outdir<- here::here("sim_data")

library(cowplot)
library(gridGraphics)
library(RColorBrewer)
library(viridis)
library(patchwork)
library(gridExtra)
library(ggpubr)


# Assess model behavior at incrasing values of Emax -----------------------


Emaxes <- seq(50, 3000, by = 25)

dat <- data.frame(name = rep(NA, length(Emaxes)*length(funlist)),
                  lambda = rep(NA, length(Emaxes)*length(funlist)),
                  intercept = rep(NA, length(Emaxes)*length(funlist)),
                  emax = rep(NA, length(Emaxes)*length(funlist)),
                  cv_effort = rep(NA, length(Emaxes)*length(funlist)),
                  cv_biomass = rep(NA, length(Emaxes)*length(funlist)),
                  cumulative_effort = rep(NA, length(Emaxes)*length(funlist)),
                  cumulative_catch = rep(NA, length(Emaxes)*length(funlist)),
                  prop_extirpated = rep(NA, length(Emaxes)*length(funlist)),
                  prop_overfished = rep(NA, length(Emaxes)*length(funlist)))

 k <- 1
 
 for (j in 1:length(Emaxes)){
   for (i in 1:length(funlist)){
     rawdat <- simulate(params = param_vec, 
                          nsims = 100, Emax = Emaxes[j], 
                          Bmsy = 0.00001437875, 
                          msy = 0.3998861, 
                          utilfun = funlist[[i]])
     dat[k,1] <- names(funlist)[[i]]
     dat[k,2] <- df$lambda[i]
     dat[k,3] <- df$intercept[i]
     dat[k,4] <- Emaxes[j]
     dat[k,5:10] <- colMeans(rawdat)
     k <- k+1
   }
 }

# save this output because the loop takes forever to run
 
write.csv(dat, file = paste(outdir, "emax_sensitivity_analysis.csv", sep = "/"), row.names = FALSE)

dat2 <- pivot_longer(dat, cols = 5:10, 
                      names_to = "varname", 
                      values_to = "val") %>% 
   mutate(type = ifelse(varname == "cv_effort" | varname == "cv_biomass", 
                        "variability",
                        ifelse(varname == "prop_extirpated" | varname == "prop_overfished",
                               "biological", 
                               "social")))
 
 
# Plot biological outcome variables across values of Emax--------------

fun_nam <- names(funlist)
biodat <- dat2 %>% 
   filter(type == "biological")
 
for (i in 1:length(funlist)){
  dat_subset <- biodat %>% filter(name == names(funlist)[[i]]) %>% 
    mutate(val_dodged = ifelse(varname == "prop_extirpated", val + 0.01,
                               val),
           cols = ifelse(varname == "prop_extirpated", "#ca0020", "orange"),
           labs = df[i,6])
  p <- ggplot(data = dat_subset,
              aes(x = emax, y = val_dodged))+
    geom_line(aes(color = varname), size = 1.5)+
    scale_color_manual(values = dat_subset$cols)+
    theme_classic()+
    theme(axis.line=element_line(),
          strip.background = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 9.5),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          legend.position = "none")+
    scale_y_continuous(limits = c(0,1.01))+
    labs(title = dat_subset$labs,
         x = "Emax",
         y = "Proportion")
  
  assign(fun_nam[i], p)
  
}

facet_p <- ggplot(data = biodat,
                  aes(x = emax, y = val))+
  geom_line(aes(color = varname), size = 1.5)+
  scale_color_manual(values = c("#ca0020", "orange"), name = "Biological outcomes", labels = c("Prop. extirpated", "Prop. overfished"))+
  theme_classic()+
  theme(axis.line=element_line(),
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 10)
  )+
  labs(title = names(funlist)[[i]],
       x = "Emax",
       y = "Proportion")

leg <- get_legend(facet_p)
gg_leg <- as_ggplot(leg)


bio_emax <- ggarrange(NULL, NULL, kur_inshore, kur_hms, kur_coastal, kur_bottom, NULL, NULL,
                  NULL, NULL, whi_other, NULL, NULL, whi_mackerel, NULL, whi_billfish,
                  NULL, NULL, NULL, NULL, whi_cmp, whi_sg, NULL, NULL,
                  rag_butter, rag_reef, rag_table, rag_key, rag_prize, mkwara, gent, NULL,
                  nrow = 4, ncol = 8
)+
  annotation_custom(leg, xmin = 0.01, ymin = 0.55, xmax = 0.25)

# Plot social outcome variables across values of Emax--------------

socdat <- dat2 %>% filter(type == "social")

for (i in 1:length(funlist)){
  dat_subset <- socdat %>% filter(name == names(funlist)[[i]])
  p <- ggplot(data = dat_subset,
              aes(x = emax, y = val))+
    geom_line(aes(color = varname), size = 1.5)+
    scale_color_manual(values = c("#c51b7d", "#4d9221"))+
    scale_y_continuous(limits = c(0, max(dat$cumulative_catch)))+
    theme_classic()+
    theme(axis.line=element_line(),
          strip.background = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 10),
          legend.position = "none")+
    labs(title = names(funlist)[[i]],
         x = "Emax",
         y = "Cumulative benefits")
  
  assign(fun_nam[i], p)
  
}


facet_p <- ggplot(data = socdat,
                  aes(x = emax, y = val))+
  geom_line(aes(color = varname), size = 1.5)+
  scale_color_manual(values = c("#c51b7d", "#4d9221"))+
  theme_classic()+
  theme(axis.line=element_line(),
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 10),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  labs(title = names(funlist)[[i]],
       x = "Emax",
       y = "Cumulative benefits")

leg <- get_legend(facet_p)
gg_leg <- as_ggplot(leg)


soc_emax <- ggarrange(NULL, NULL, kur_inshore, kur_hms, kur_coastal, kur_bottom, NULL, NULL,
                      NULL, NULL, whi_other, NULL, NULL, whi_mackerel, NULL, whi_billfish,
                      NULL, NULL, NULL, NULL, whi_cmp, whi_sg, NULL, NULL,
                      rag_butter, rag_reef, rag_table, rag_key, rag_prize, mkwara, gent, NULL,
                      nrow = 4, ncol = 8
)+
  annotation_custom(leg, xmin = 0.01, ymin = 0.55, xmax = 0.25)

# Plot variability outcome variables across values of Emax--------------

vardat <- dat2 %>% filter(type == "variability")

for (i in 1:length(funlist)){
  dat_subset <- vardat %>% filter(name == names(funlist)[[i]])
  p <- ggplot(data = dat_subset,
              aes(x = emax, y = val))+
    geom_line(aes(color = varname), size = 1.5)+
    scale_color_manual(values = c("#8c96c6", "#88419d"))+
    scale_y_continuous(limits = c(0, 1))+
    theme_classic()+
    theme(axis.line=element_line(),
          strip.background = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 10),
          legend.position = "none")+
    labs(title = names(funlist)[[i]],
         x = "Emax",
         y = "CV")
  
  assign(fun_nam[i], p)
  
}


facet_p <- ggplot(data = vardat,
                  aes(x = emax, y = val))+
  geom_line(aes(color = varname), size = 1.5)+
  scale_color_manual(values = c("#8c96c6", "#88419d"))+
  theme_classic()+
  theme(axis.line=element_line(),
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 10)
  )+
  labs(title = names(funlist)[[i]],
       x = "Emax",
       y = "CV")

leg <- get_legend(facet_p)
gg_leg <- as_ggplot(leg)

var_emax <- ggarrange(NULL, NULL, kur_inshore, kur_hms, kur_coastal, kur_bottom, NULL, NULL,
                      NULL, NULL, whi_other, NULL, NULL, whi_mackerel, NULL, whi_billfish,
                      NULL, NULL, NULL, NULL, whi_cmp, whi_sg, NULL, NULL,
                      rag_butter, rag_reef, rag_table, rag_key, rag_prize, mkwara, gent, NULL,
                      nrow = 4, ncol = 8
)+
  annotation_custom(leg, xmin = 0.01, ymin = 0.55, xmax = 0.25)


# Output these figures ----------------------------------------------------

figname <- paste(todaysdate, "appendix_fig2.pdf", sep = "-")
pdf(file = paste(outfig, figname, sep = "/"), width = 12, height = 8, onefile = TRUE)

bio_emax
soc_emax
var_emax

dev.off()
graphics.off()



# Find the value of E that produces MSY and extinction for each function --------

# first, collect the brute-force approximations from section 1 above to give optim() a place to start

Emsys <- dat %>% 
  group_by(name) %>% 
  filter(cumulative_catch == max(cumulative_catch))

Eextincts <- dat %>% 
  group_by(name) %>% 
  filter(prop_extirpated == 1) %>% 
  summarize(E_extinction = min(emax))

Eextincts[15,] <- list("gent", 3000)
Eextincts[16,] <- list("mkwara", 3000)


# create an empty dataframe to hold the more precise estimates

Es <- data.frame(citation = rep(NA, nrow(df)), 
          species = rep(NA, nrow(df)),
          Emsy = rep(NA, nrow(df)),
          Eextinct = rep(NA, nrow(df)),
          funlab = rep(NA, nrow(df)))

# estimate Emsy and Eextinction for each function using optim()

for (i in 1:nrow(df)){
  
  # define a function to find the effort that maximizes cumulative catch
  find_emsy <- function(e, fn = funlist[[i]]){
    test <- simulate(params = param_vec, 
                     nsims = 2, Emax = e, 
                     Bmsy = 0.00001437875, 
                     msy = 0.3998861, 
                     utilfun = fn)
    return(mean(test$cumulative_catch))
  }
 
  # define a function to find the smallest effort that extirpates the population
  find_eextinct <- function(e, fn = funlist[[i]]){
    test <- simulate(params = param_vec, 
                     nsims = 2, Emax = e, 
                     Bmsy = 0.00001437875, 
                     msy = 0.3998861, 
                     utilfun = fn)
    test$emax <- e
    if(mean(test$prop_extirpated) == 0){
      return(1000000)
    }else{
      return(mean(test$emax))
    }
  }
   
  # use optim to find these 2 values
  
  emsy <- Emsys %>% filter(name == names(funlist)[[i]]) %>% select(emax)
  eextinct <- Eextincts %>% filter(name == names(funlist)[[i]]) %>% select(E_extinction)
  
  raw_emsy <- optim(c(emsy[1,2]), find_emsy, method = "BFGS", control = list(fnscale = -1))
  raw_eextinct <- optim(c(eextinct), find_eextinct, method = "BFGS", control = list(maxit = 10000000))

  Es[i,] <- list(df[i,1],
                 df[i,2],
                 raw_emsy$par,
                 raw_eextinct$par,
                 names(funlist)[[i]])
}


# add in trout and striped bass Eextinctions manually, because optim() needs a more precise starting point for them

Es[10,4] <- 3445.426
Es[11,4] <- 4411.88


# export this dataframe for appendix table 1


Emaxes <- arrange(Es, Emsy)

write.csv(Emaxes, file = paste(outdir, "emsys=_eextinction.csv", sep = "/"), row.names = FALSE)


# run simulations with Emax set to Emsy for each species

outlist <- list()

for (i in 1:length(funlist)){
  spp_specific_dat <- simulate(params = param_vec, 
                       nsims = 2, Emax = Es[i,3], 
                       Bmsy = 0.00001437875, 
                       msy = 0.3998861, 
                       utilfun = funlist[[i]])
  spp_specific_dat$name <- names(funlist)[[i]]
  spp_specific_dat$lambda <- df$lambda[i]
  spp_specific_dat$intercept <- df$intercept[i]
  spp_specific_dat$emax <- Es[i,3]
  outlist[[i]] <- spp_specific_dat
  
}

outdat <- do.call(rbind, outlist)

mean_dat <- outdat %>% 
  group_by(emax, name, lambda, intercept) %>% 
  summarize(across(1:6, mean)) %>% 
  mutate(log_lambda = round(log(lambda),1),
         intercept = round(intercept, 2))


po <- ggplot(data = mean_dat, aes(x = lambda, y = intercept))+
  geom_point(aes(color = prop_overfished), size = 4)+
  geom_point(shape= 1, size = 4, color = "black")+
  scale_color_distiller(type = "seq", palette = "Reds", direction = 1,
                        name = "Proportion",
                        limits = c(0,1))+
  scale_x_log10()+
  labs(title = "A) Mean proportion of years\nin which pop. is overfished", y = "Probability of fishing\nwhen catch rate is zero", x = NULL)+
  geom_hline(yintercept = median(mean_dat$intercept), linetype = 2)+
  geom_vline(xintercept = median(mean_dat$lambda), linetype = 2)+
  theme_classic()+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        plot.title = element_text(size = 17),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15))


pe <- ggplot(data = mean_dat, aes(x = lambda, y = intercept))+
  geom_point(aes(color = prop_extirpated), size = 4)+
  geom_point(shape= 1, size = 4, color = "black")+
  scale_color_distiller(type = "seq", palette = "Reds", direction = 1,
                        name = "Proportion",
                        limits = c(0,1))+
  scale_x_log10()+ 
  geom_hline(yintercept = median(mean_dat$intercept), linetype = 2)+
  geom_vline(xintercept = median(mean_dat$lambda), linetype = 2)+
  labs(title = "B) Proportion of simulations\nin which pop. is extirpated", x = NULL, y = NULL)+
  theme_classic()+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        plot.title = element_text(size = 17),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15))

ce <- ggplot(data = mean_dat, aes(x = lambda, y = intercept))+
  geom_point(aes(color = cumulative_effort), size = 4)+
  geom_point(shape= 1, size = 4, color = "black")+
  scale_color_distiller(type = "seq", palette = "Greens", direction = 1,
                        name = "Cumulative\nbenefits",
                        limits = c(0, max(mean_dat[,7:8], na.rm = TRUE)))+
  scale_x_log10()+
  geom_hline(yintercept = median(mean_dat$intercept), linetype = 2)+
  geom_vline(xintercept = median(mean_dat$lambda), linetype = 2)+
  labs(title = "C) Cumulative fishing effort", x = NULL, y = "Probability of fishing\nwhen catch rate is zero")+
  theme_classic()+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        plot.title = element_text(size = 17),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15))

cc <- ggplot(data = mean_dat, aes(x = lambda, y = intercept))+
  geom_point(aes(color = cumulative_catch), size = 4)+
  geom_point(shape= 1, size = 4, color = "black")+
  scale_color_distiller(type = "seq", palette = "Greens", direction = 1,
                        name = "Cumulative\nbenefits",
                        limits = c(0, max(mean_dat[,7:8], na.rm = TRUE)))+
  scale_x_log10()+
  geom_hline(yintercept = median(mean_dat$intercept), linetype = 2)+
  geom_vline(xintercept = median(mean_dat$lambda), linetype = 2)+
  labs(title = "D) Cumulative catch", x = NULL, y = NULL)+
  theme_classic()+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        plot.title = element_text(size = 17),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15))


cve <- ggplot(data = mean_dat, aes(x = lambda, y = intercept))+
  geom_point(aes(color = cv_effort), size = 4)+
  geom_point(shape= 1, size = 4, color = "black")+
  scale_color_distiller(type = "seq", palette = "BuPu", direction = 1,
                        name = "Coefficient\nof variation",
                        limits = c(0, 1))+
  scale_x_log10()+
  geom_hline(yintercept = median(mean_dat$intercept), linetype = 2)+
  geom_vline(xintercept = median(mean_dat$lambda), linetype = 2)+
  labs(title = "E) Coefficient of variation of effort", x = "Steepness of angler response to catch rates (\u03bb)", y = "Probability of fishing\nwhen catch rate is zero")+
  theme_classic()+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        plot.title = element_text(size = 17),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15))

cvb <- ggplot(data = mean_dat, aes(x = lambda, y = intercept))+
  geom_point(aes(color = cv_biomass), size = 4)+
  geom_point(shape= 1, size = 4, color = "black")+
  scale_color_distiller(type = "seq", palette = "BuPu", direction = 1,
                        name = "Coefficient\nof variation",
                        limits = c(0, 1))+
  geom_hline(yintercept = median(mean_dat$intercept), linetype = 2)+
  geom_vline(xintercept = median(mean_dat$lambda), linetype = 2)+
  labs(title = "F) Coefficient of variation of biomass", x = "Steepness of angler response to catch rates (\u03bb)", y = NULL)+
  scale_x_log10()+
  theme_classic()+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        plot.title = element_text(size = 17),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15))

bio <- po + pe + 
  plot_layout(guides = "collect")

soc <- ce + cc +
  plot_layout(guides = "collect")

vari <- cve + cvb +
  plot_layout(guides = "collect")


figname <- paste(todaysdate, "appendix_fig_2a_spp_specific_Emax.png", sep = "-")
png(paste(outfig, figname, sep = "/"), width = 12, height = 12, units = "in", res = 500)

bio / soc / vari

dev.off()
graphics.off()

      