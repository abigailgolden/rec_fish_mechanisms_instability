# Script to implement John Post's idea to rerun sensitivity analysis by calculating the value of Emax at which each parameter value extirpates the population

rm(list = ls())
source("functions/emax_threshold_funs.R")
outdir<- here::here("sim_data")
outfig<- here::here("figures")

library(patchwork)


# Depensation -------------------------------------------------------------

param_vec <- c(0.001, 0, 0, 1.000)

emp_dep <- c(0.04, 0.06, 0.3)

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
                                    Emaxes = seq(1, 3001, by = 5), Bmsy = 0.00001436646,
                                    utilname = "Low \u03b1, low \u03bb")
dep_emax_whi_sg <- emax_thresh(par_range = seq(0.001, 0.301, by = 0.01), 
                        params = param_vec, par_id = 1, utilfun = Pi_whi_sg, 
                        Emaxes = seq(1, 1501, by = 5), Bmsy = 0.00001436646,
                        utilname = "Low \u03b1, high \u03bb")


all_dep_data <- rbind(dep_emax_kur_bottom, dep_emax_kur_coastal, dep_emax_rag_prize, dep_emax_whi_sg)


write.csv(all_dep_data, file = paste(outdir, "emax_threshold_dep.csv", sep = "/"), row.names = FALSE)

cols <- c("#D7191C", "#FDAE61","#91BFDB", "#2C7BB6")

dep_plot <- ggplot(data = all_dep_data, aes(x = param_val, y = Emax))+
  geom_line(aes(color = utilfun), size = 1)+
  scale_color_manual(values = cols, name = "Angler effort\nfunction")+
  labs( title = "A) Depensation",
    x = "Depensation parameter d", y = "Effort that produces extirpation")+
  geom_vline(xintercept = median(emp_dep), linetype = 2)+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10000))+
  theme_classic()+
  theme(legend.position = c(0.75, 0.75))

dep_plot


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


hyp_emax_kur_coastal <- emax_thresh(par_range = b_range, 
                                    params = param_vec, par_id = 4, utilfun = Pi_kur_hms, 
                                    Emaxes = seq(1, 1501, by = 5), Bmsy = 0.00001436646,
                                    utilname = "High \u03b1, low \u03bb")
hyp_emax_kur_bottom <- emax_thresh(par_range = b_range, 
                                   params = param_vec, par_id = 4, utilfun = Pi_kur_bottom, 
                                   Emaxes = seq(1, 1501, by = 5), Bmsy = 0.00001436646,
                                   utilname = "High \u03b1, high \u03bb")
hyp_emax_rag_prize <- emax_thresh(par_range = b_range, 
                                  params = param_vec, par_id = 4, utilfun = Pi_rag_prize, 
                                  Emaxes = seq(1, 10001, by = 5), Bmsy = 0.00001436646,
                                  utilname = "Low \u03b1, low \u03bb")
hyp_emax_whi_sg <- emax_thresh(par_range = b_range, 
                               params = param_vec, par_id = 4, utilfun = Pi_whi_sg, 
                               Emaxes = seq(1, 5001, by = 5), Bmsy = 0.00001436646,
                               utilname = "Low \u03b1, high \u03bb")



all_hyp_data <- rbind(hyp_emax_kur_bottom, hyp_emax_kur_coastal, hyp_emax_rag_prize, hyp_emax_whi_sg)

write.csv(all_hyp_data, file = paste(outdir, "emax_threshold_hyp.csv", sep = "/"), row.names = FALSE)


hyp_plot <- ggplot(data = all_hyp_data, aes(x = param_val, y = Emax))+
  geom_line(aes(color = utilfun), size = 1)+
  scale_color_manual(values = cols, name = "Angler effort\nfunction")+
  labs(title = "D) Density dependence in catchability",
       x = "Density-dependent catchability parameter \u03b2", 
       y = NULL)+
  geom_vline(xintercept = median(emp_b), linetype = 2)+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10000))+
  theme_classic()+
  theme(legend.position = "none")

hyp_plot

# SD of recruitment stochasticity -----------------------------------------

# set range of SD values to loop over
max_sd <- 0.79
sd_range <- seq(0, round(max_sd,1), by = 0.05)

# empirically observed values of sd
emp_sd <- c(0.67, 0.77, 0.74, 0.78, 0.64, 0.71, 0.74)

rec_sd_emax_kur_coastal <- emax_thresh(par_range = sd_range, 
                                    params = param_vec, par_id = 2, utilfun = Pi_kur_hms, 
                                    Emaxes = seq(1, 1501, by = 5), Bmsy = 0.00001436646,
                                    utilname = "High \u03b1, low \u03bb")
rec_sd_emax_kur_bottom <- emax_thresh(par_range = sd_range, 
                                   params = param_vec, par_id = 2, utilfun = Pi_kur_bottom, 
                                   Emaxes = seq(1, 1501, by = 5), Bmsy = 0.00001436646,
                                   utilname = "High \u03b1, high \u03bb")
rec_sd_emax_rag_prize <- emax_thresh(par_range = sd_range, 
                                  params = param_vec, par_id = 2, utilfun = Pi_rag_prize, 
                                  Emaxes = seq(1, 10001, by = 5), Bmsy = 0.00001436646,
                                  utilname = "Low \u03b1, low \u03bb")
rec_sd_emax_whi_sg <- emax_thresh(par_range = sd_range, 
                               params = param_vec, par_id = 2, utilfun = Pi_whi_sg, 
                               Emaxes = seq(1, 5001, by = 5), Bmsy = 0.00001436646,
                               utilname = "Low \u03b1, high \u03bb")


all_rec_sd_data <- rbind(rec_sd_emax_kur_bottom, rec_sd_emax_kur_coastal, rec_sd_emax_rag_prize, rec_sd_emax_whi_sg)

write.csv(all_rec_sd_data, file = paste(outdir, "emax_threshold_rec_sd.csv", sep = "/"), row.names = FALSE)


all_rec_sd_data <- read.csv(paste(outdir, "emax_threshold_rec_sd.csv", sep = "/"), as.is = T)


rec_sd_plot <- ggplot(data = all_rec_sd_data, aes(x = param_val, y = Emax))+
  geom_line(aes(color = utilfun), size = 1)+
  scale_color_manual(values = cols, name = "Angler effort\nfunction")+
  labs(title = "B) SD of recruitment stochasticity",
       x = "Standard deviation of recruitment stochasticity", 
       y = NULL)+
  geom_vline(xintercept = median(emp_sd), linetype = 2)+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10000))+
  theme_classic()+
  theme(legend.position = "none")

rec_sd_plot

# autocorrelation coefficient rho -----------------------------------------

# set sd to equal the mean observed sd
param_vec <- c(0.001, 0.74, 0, 1.000)

max_rho <- 0.49
rho_range <- seq(0, round(max_rho,1), by = 0.02)

emp_rho <- c(0.45, 0.49, 0.42, 0.46, 0.38)


rec_rho_emax_kur_coastal <- emax_thresh(par_range = rho_range, 
                                       params = param_vec, par_id = 3, utilfun = Pi_kur_hms, 
                                       Emaxes = seq(1, 1501, by = 5), Bmsy = 0.00001436646,
                                       utilname = "High \u03b1, low \u03bb")
rec_rho_emax_kur_bottom <- emax_thresh(par_range = rho_range, 
                                      params = param_vec, par_id = 3, utilfun = Pi_kur_bottom, 
                                      Emaxes = seq(1, 1501, by = 5), Bmsy = 0.00001436646,
                                      utilname = "High \u03b1, high \u03bb")
rec_rho_emax_rag_prize <- emax_thresh(par_range = rho_range, 
                                     params = param_vec, par_id = 3, utilfun = Pi_rag_prize, 
                                     Emaxes = seq(1, 10001, by = 5), Bmsy = 0.00001436646,
                                     utilname = "Low \u03b1, low \u03bb")
rec_rho_emax_whi_sg <- emax_thresh(par_range = rho_range, 
                                  params = param_vec, par_id = 3, utilfun = Pi_whi_sg, 
                                  Emaxes = seq(1, 5001, by = 5), Bmsy = 0.00001436646,
                                  utilname = "Low \u03b1, high \u03bb")


all_rec_rho_data <- rbind(rec_rho_emax_kur_bottom, rec_rho_emax_kur_coastal, rec_rho_emax_rag_prize, rec_rho_emax_whi_sg)

write.csv(all_rec_rho_data, file = paste(outdir, "emax_threshold_rec_rho.csv", sep = "/"), row.names = FALSE)


all_rec_rho_data <- read.csv(paste(outdir, "emax_threshold_rec_rho.csv", sep = "/"), as.is = T)

rec_rho_plot <- ggplot(data = all_rec_rho_data, aes(x = param_val, y = Emax))+
  geom_line(aes(color = utilfun), size = 1)+
  scale_color_manual(values = cols, name = "Angler effort\nfunction")+
  labs(title = "C) Autocorrelation in recruitment stochasticity",
       x = "Autocorrelation parameter \u03c1", 
       y = "Effort that produces extirpation")+
  geom_vline(xintercept = median(emp_rho), linetype = 2)+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10000))+
  theme_classic()+
  theme(legend.position = "none")


rec_rho_plot


# Stitch plots together ---------------------------------------------------

figname <- "FigX_emax_threshold.png"
png(paste(outfig, figname, sep = "/"), width = 9, height = 6, units = "in", res = 750)


(dep_plot + rec_sd_plot) / (rec_rho_plot + hyp_plot)

dev.off()
graphics.off()
