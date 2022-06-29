# Determine sensitivity of the model to the angler effort function
rm(list = ls())

source("model/model.R")
source("functions/sensitivity_analysis_funs.R")

options(scipen = 999)

param_vec <- c(0.001, 0, 0, 1.000)
param_names <- c("d", "sd", "rho", "beta")
outfig <- here::here("figures")
outdir<- here::here("sim_data")



# Save model runs using each angler effort function ----------------------------------

Emax <- 48

outlist <- list()

for (i in 1:length(funlist)){
  dat <- simulate(params = param_vec, 
                                      nsims = 100, Emax = Emax, 
                                      Bmsy = Bmsy <- 0.00001437875, 
                                      msy = 0.3998861, 
                                      utilfun = funlist[[i]])
  dat$name <- names(funlist)[[i]]
  dat$citation <- df$citation[i]
  dat$species <- df$species[i]
  dat$lambda <- df$lambda[i]
  dat$intercept <- df$intercept[i]
  outlist[[i]] <- dat
}

outdat <- do.call(rbind, outlist)

mean_dat <- outdat %>% 
  group_by(name, citation, species, lambda, intercept) %>% 
  summarize(across(1:6, mean)) %>% 
  mutate(intercept = round(intercept, 2),
         lambda = round(lambda, 3),
         cv_effort = round(cv_effort, 2),
         cv_biomass = round(cv_biomass, 2),
         cumulative_effort = round(cumulative_effort, 0),
         cumulative_catch = round(cumulative_catch, 0))
  

# Export this dataframe as results table 6

table_dat <- mean_dat %>% 
  ungroup() %>% 
  select(citation, species, lambda, intercept, prop_overfished, prop_extirpated,
         cumulative_catch, cumulative_effort, cv_biomass, cv_effort)

colnames(table_dat) <- c("Citation", "Species", "\u03bb",
                         "Intercept", "Prop. years overfished",
                         "Prop. extirpated", "Cumul. catch",
                         "Cumul. effort", "CV of biomass", "CV of effort")

write.csv(table_dat, file = paste(outdir, "Table 6.csv", sep = "/"), row.names = FALSE)


# Put together plots ------------------------------------------------------

# proportion overfished

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

# proportion extirpated

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

# cumulative effort

ce <- ggplot(data = mean_dat, aes(x = lambda, y = intercept))+
  geom_point(aes(color = cumulative_effort), size = 4)+
  geom_point(shape= 1, size = 4, color = "black")+
  scale_color_distiller(type = "seq", palette = "Greens", direction = 1,
                        name = "Cumulative\nbenefits",
                        limits = c(0, max(mean_dat[,6:7], na.rm = TRUE)))+
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

# cumulative catch

cc <- ggplot(data = mean_dat, aes(x = lambda, y = intercept))+
  geom_point(aes(color = cumulative_catch), size = 4)+
  geom_point(shape= 1, size = 4, color = "black")+
  scale_color_distiller(type = "seq", palette = "Greens", direction = 1,
                       name = "Cumulative\nbenefits",
                       limits = c(0, max(mean_dat[,6:7], na.rm = TRUE)))+
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

# CV of effort
            

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

# CV of biomass

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



figname <- paste(todaysdate, "fig4.png", sep = "-")
png(paste(outfig, figname, sep = "/"), width = 10, height = 11, units = "in", res = 500)

bio / soc / vari

dev.off()
graphics.off()

