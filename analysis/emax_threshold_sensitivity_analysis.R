# Script to implement John Post's idea to rerun sensitivity analysis by calculating the value of Emax at which each parameter value extirpates the population

rm(list = ls())
source("functions/emax_threshold_functions.R")



# Depensation -------------------------------------------------------------

dep_emax_kur_coastal <- emax_thresh(par_range = seq(0.001, 0.301, by = 0.01), 
                                    params = param_vec, par_id = 1, utilfun = Pi_kur_hms, 
                                    Emaxes = seq(1, 1501, by = 5), Bmsy = 0.00001436646,
                                    utilname = "High \u03b1, low \u03bb")
dep_emax_kur_bottom <- emax_thresh(par_range = seq(0.001, 0.301, by = 0.01), 
                                    params = param_vec, par_id = 1, utilfun = Pi_kur_bottom, 
                                    Emaxes = seq(1, 1501, by = 5), Bmsy = 0.00001436646,
                                    utilname = "High \u03b1, high \u03bb")
dep_emax_rag_prize <- emax_thresh(par_range = seq(0.001, 0.301, by = 0.01), 
                                    params = param_vec, par_id = 1, utilfun = Pi_rag_prize, 
                                    Emaxes = seq(1, 1501, by = 5), Bmsy = 0.00001436646,
                                    utilname = "Low \u03b1, low \u03bb")
dep_emax_whi_sg <- emax_thresh(par_range = seq(0.001, 0.301, by = 0.01), 
                        params = param_vec, par_id = 1, utilfun = Pi_whi_sg, 
                        Emaxes = seq(1, 1501, by = 5), Bmsy = 0.00001436646,
                        utilname = "Low \u03b1, high \u03bb")


# fix this
ggplot(data = dep_emax, aes(x = param_val, y = Emax))+
  geom_line()+
  theme_classic()


# hyperstability ----------------------------------------------------------

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
