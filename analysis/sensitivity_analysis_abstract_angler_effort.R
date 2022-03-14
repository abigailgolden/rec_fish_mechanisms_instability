# Determine sensitivity of the model to an abstract angler effort function with values of the y-intercept and steepness that range from the lowest to the highest values found in the empirical dataset

rm(list = ls())

source("functions/sensitivity_analysis_funs.R")

options(scipen = 999)

param_vec <- c(0.001, 0, 0, 1.000, 0.01, 0.1)
param_names <- c("d", "sd", "rho", "beta", "int", "stp")

outdir <- here::here("sim_data")
outfig <- here::here("figures")


# Set intercept to range from 0 to 0.5

ints <- seq(0, 1, by = 0.01)

# Set steepness to range from 0 to 300

stps <- seq(0, 300, by = 0.1)

# create a matrix with every combination of these intercept and steepness values

effort_mat <- expand_grid(intercept = ints, steepness = stps)


# Run the model for each combination of intercept and steepness

# use a couple simulations per combo to reduce computation time and because this is all   deterministic
nsims <- 2

angler_effort_dat <- simulate_along_effort(effort_mat = effort_mat, 
                                params = param_vec,
                                Emax = 48,
                                Bmsy = Bmsy,
                                msy = msy)

write.csv(angler_effort_dat,file = paste(outdir, "abstract_angler_effort_sensitivity_analysis.csv", sep = "/"), row.names = FALSE)


# make a heatmap for each response variable with steepness on the x axis and intercept on the y-axis

# separate out each type of response variable to set color scales that allow comparison across variables
ce_cc_dat <- angler_effort_dat %>% 
  filter(varname == "cumulative_effort" | varname == "cumulative_catch")

cv_dat <- angler_effort_dat %>% 
  filter(varname == "cv_effort" | varname == "cv_biomass")

po <- ggplot()+
  geom_tile(data = filter(angler_effort_dat, varname == "prop_overfished" & stp <= 10
                          ),
            aes(x = stp, y = int, fill = val))+
  scale_fill_distiller(type = "seq",
                       palette = "Reds",
                       direction = 1,
                       name = "Proportion overfished",
                       limits = c(0,1),
                       guide = guide_legend(order = 1),
                       na.value = "white")+
  labs(title = "A) Mean proportion of years\nin which pop. is overfished",
       x = NULL, y = "Intercept")+
  theme_classic()



pe <- ggplot()+
  geom_tile(data = filter(angler_effort_dat, varname == "prop_extirpated" # & stp <= 10
                          ),
            aes(x = stp, y = int, fill = val))+
  scale_fill_distiller(type = "seq",
                       palette = "Reds",
                       direction = 1,
                       name = "Proportion overfished",
                       limits = c(0,1),
                       guide = guide_legend(order = 1),
                       na.value = "white")+
  labs(title = "B) Proportion of simulations\nin which pop. is extirpated", x = NULL, y = NULL)+
  theme_classic()


ce <- ggplot()+
  geom_tile(data = filter(angler_effort_dat, varname == "cumulative_effort" & stp <= 10),
            aes(x = stp, y = int, fill = val))+
  scale_fill_distiller(type = "seq", palette = "Greens",
                       direction = 1,
                       limits = c(0, max(ce_cc_dat$val, na.rm = TRUE)),
                       name = "Cumulative benefits",
                       na.value = "white")+
  labs(title = "C) Cumulative fishing effort", x = NULL, y = "Intercept")+
  theme_classic()


cc <- ggplot()+
  geom_tile(data = filter(angler_effort_dat, varname == "cumulative_catch" 
                          & stp <= 10
                          ),
            aes(x = stp, y = int, fill = val))+
  scale_fill_distiller(type = "seq", palette = "Greens",
                       direction = 1,
                       limits = c(0, max(ce_cc_dat$val, na.rm = TRUE)),
                       name = "Cumulative benefits",
                       na.value = "white")+
  labs(title = "D) Cumulative catch", x = NULL, y = NULL)+
  theme_classic()


cve <- ggplot()+
  geom_tile(data = filter(angler_effort_dat, varname == "cv_effort" 
                          & stp <= 10
  ),
  aes(x = stp, y = int, fill = val))+
  scale_fill_distiller(type = "seq", palette = "BuPu",
                       direction = 1,
                       limits = c(0, max(cv_dat$val, na.rm = TRUE)),
                       name = "Coefficient of variation",
                       na.value = "white")+
  labs(title = "E) Coefficient of variation of effort", x = "Steepness", y = "Intercept")+
  theme_classic()


cvb <- ggplot()+
  geom_tile(data = filter(angler_effort_dat, varname == "cv_biomass" 
                          & stp <= 10
  ),
  aes(x = stp, y = int, fill = val))+
  scale_fill_distiller(type = "seq", palette = "BuPu",
                       direction = 1,
                       limits = c(0, max(cv_dat$val, na.rm = TRUE)),
                       name = "Coefficient of variation",
                       na.value = "white")+
  labs(title = "F) Coefficient of variation of biomass", x = "Steepness", y = NULL)+
  theme_classic()


bio <- po + pe + 
  plot_layout(guides = "collect")

soc <- ce + cc +
  plot_layout(guides = "collect")

vari <- cve + cvb +
  plot_layout(guides = "collect")


figname <- paste("fig4_abstract_effort.png", sep = "-")
png(paste(outfig, figname, sep = "/"), width = 12, height = 11, units = "in", res = 1000)

bio / soc / vari


dev.off()
graphics.off()
