# Sensitivity analysis to evaluate the effect of Emax on model behavior
# Step 2: see how the model responds to eacj angler effort function in the presence of different values of Emax

rm(list = ls())

source("model/model.R")
source("functions/sensitivity_analysis_funs.R")

options(scipen = 999)

param_vec <- c(0.001, 0, 0, 1.000)
param_names <- c("d", "sd", "rho", "beta")
outfig <- here::here("appendix_figs")

library(cowplot)
library(gridGraphics)
library(RColorBrewer)
library(viridis)
library(patchwork)
library(gridExtra)
library(ggpubr)


# Assess model behavior at incrasing values of Emax -----------------------


Emaxes <- seq(50, 1000, by = 50)

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
