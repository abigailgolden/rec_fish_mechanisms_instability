# make plot of Eextinction by angler effort function
# copy original Fig4 plot layout


rm(list = ls())
options(scipen = 999)

# load packages and functions

library(here)
library(tidyverse)
source("functions/angler_effort_funs.R")
dir<- here::here("sim_data")

# set output directory

outfig <- here::here("ppt_figs")


# read in data

dat_raw <- read.csv(paste(dir, "emsys_eextinction.csv", sep = "/"), as.is = T)

dat <- left_join(dat_raw, df, by = c("citation", "species"))

figname <- "angler_effort_functions.png"
png(paste(outfig, figname, sep = "/"), width = , height = 4, units = "in", res = 750)


ggplot(data = dat, aes(x = lambda, y = intercept))+
  #geom_point(aes(color = prop_overfished), size = 4)+
  geom_point(shape= 19, size = 3, color = "black")+
  # scale_color_distiller(type = "seq", palette = "Reds", direction = 1,
  #                       name = "Proportion",
  #                       limits = c(0,1))+
  scale_x_log10()+
  labs(#title = "A) Mean proportion of years\nin which pop. is overfished", 
    y = "Giving-up point (\u03b1)", 
    x = "Reactivity of anglers to increasing catch rates (\u03bb)")+
  geom_hline(yintercept = median(dat$intercept), linetype = 2)+
  geom_vline(xintercept = median(dat$lambda), linetype = 2)+
  theme_classic()+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        plot.title = element_text(size = 17),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15))

dev.off()
graphics.off()



figname <- "angler_effort_functions_extinction.png"
png(paste(outfig, figname, sep = "/"), width = 8, height = 4, units = "in", res = 750)


ggplot(data = dat, aes(x = lambda, y = intercept))+
  geom_point(aes(color = Eextinct), size = 3)+
  geom_point(shape= 1, size = 3, color = "black")+
  scale_color_distiller(type = "seq", palette = "Reds", direction = -1,
                        name = "Fishing effort\nneeded to\ncollapse\npopulation",
                        #limits = c(0,1)
                        )+
  scale_x_log10()+
  labs(#title = "A) Mean proportion of years\nin which pop. is overfished", 
    y = "Giving-up point (\u03b1)", 
    x = "Reactivity of anglers to increasing catch rates (\u03bb)")+
  geom_hline(yintercept = median(dat$intercept), linetype = 2)+
  geom_vline(xintercept = median(dat$lambda), linetype = 2)+
  theme_classic()+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        plot.title = element_text(size = 17),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15))#+
 #theme(legend.position = "none")

dev.off()
graphics.off()

