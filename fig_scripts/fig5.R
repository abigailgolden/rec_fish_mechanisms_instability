# Script to produce Figure 5 of manuscript

# Steps:

# 1. Run model with all parameters set at their null expectation and the angler effort function for North Carolina mackerel. Save output

# 2. Run a set of simulations where each parameter is at the mean with the others turned off. For these purposes, say that the mean value of recruitment stochasticity is mean observed SD and mean observed rho for all taxonomic orders

# 3. Run an additional set of simulations where parameters are interacted with each other, with the third parameter turned off

# 4. Repeat steps 1-3 for the other three the representative angler effort functions used in the sensitivity analyses (Kuriyama et al. highly migratory species, Kuriyama et al. bottomfish, Raguragavan et al. prize fish)

# 5. Plot catch-effort relationship for each of these angler effort functions

# 6. Arrange all outcome plots 


# Setup -------------------------------------------------------------------

# clear workspace and set options

rm(list = ls())
options(scipen = 999)

# set source

source("model/model.R")
source("functions/sensitivity_analysis_funs.R")

library(GGally)

# set output folder

outfig <- here::here("figures")

# set default parameter values

param_vec <- c(0.001, 0, 0, 1.000)
Emax <- 48  # value of effort that produces extinction at equilibrium
Bmsy <- 0.00001437875   # Bmsy when effort is at equilibrium
msy <- 0.3998861        # MSY when effort is at equilibrium


# Null with whi_mackerel ------------------------------------------------------

whi_null <- simulate(params = param_vec, nsims = nsims, Emax = Emax, Bmsy = Bmsy, msy = msy, utilfun = Pi_whi_mackerel)

# define reference levels of cumulative catch and effort as the cumulative catch and effort when all parameters are at the null
whi_ref_effort <- whi_null$cumulative_effort[1]
whi_ref_catch <- whi_null$cumulative_catch[1]


# Single params with whi_mackerel -----------------------------------------------------------

whi_dep <- simulate(params = c(0.06, 0, 0, 1.000), nsims = nsims, Emax = Emax, Bmsy = Bmsy, msy = msy, utilfun = Pi_whi_mackerel)

whi_rec <- simulate(params = c(0.001, 0.74, 0.45, 1.000), nsims = nsims, Emax = Emax, Bmsy = Bmsy, msy = msy, utilfun = Pi_whi_mackerel)

whi_hyp <- simulate(params = c(0.001, 0, 0, 0.53), nsims = nsims, Emax = Emax, Bmsy = Bmsy, msy = msy, utilfun = Pi_whi_mackerel)


# Interactions with whi_mackerel------------------------------------------------------------

whi_dep_rec <- simulate(params = c(0.06, 0.74, 0.45, 1.000), nsims = nsims, Emax = Emax, Bmsy = Bmsy, msy = msy, utilfun = Pi_whi_mackerel)

whi_dep_hyp <- simulate(params = c(0.06, 0, 0, 0.53), nsims = nsims, Emax = Emax, Bmsy = Bmsy, msy = msy, utilfun = Pi_whi_mackerel)

whi_rec_hyp <- simulate(params = c(0.001, 0.74, 0.45, 0.53), nsims = nsims, Emax = Emax, Bmsy = Bmsy, msy = msy, utilfun = Pi_whi_mackerel)


# Reshape outcome dataframes relative to the null using the Whitehead et al. mackerel angler effort function-----------------------------------------------------------------

dfs <- c("whi_null", "whi_dep", "whi_rec", "whi_hyp", "whi_dep_rec", "whi_dep_hyp", "whi_rec_hyp")
for (i in dfs){
  assign(i, transform(get(i), 
                      cumulative_catch = ((cumulative_catch - whi_ref_catch)/whi_ref_catch)*100,
                      cumulative_effort = ((cumulative_effort - whi_ref_effort)/whi_ref_effort)*100))
}


# Null with kur_bottom ------------------------------------------------------

kur_null <- simulate(params = param_vec, nsims = nsims, Emax = Emax, Bmsy = Bmsy, msy = msy, utilfun = Pi_kur_bottom)

# define reference levels of cumulative catch and effort as the cumulative catch and effort when all parameters are at the null
kur_ref_effort <- kur_null$cumulative_effort[1]
kur_ref_catch <- kur_null$cumulative_catch[1]


# Single params with kur_bottom -----------------------------------------------------------

kur_dep <- simulate(params = c(0.06, 0, 0, 1.000), nsims = nsims, Emax = Emax, Bmsy = Bmsy, msy = msy, utilfun = Pi_kur_bottom)

kur_rec <- simulate(params = c(0.001, 0.74, 0.45, 1.000), nsims = nsims, Emax = Emax, Bmsy = Bmsy, msy = msy, utilfun = Pi_kur_bottom)

kur_hyp <- simulate(params = c(0.001, 0, 0, 0.53), nsims = nsims, Emax = Emax, Bmsy = Bmsy, msy = msy, utilfun = Pi_kur_bottom)


# Interactions with kur_bottom------------------------------------------------------------

kur_dep_rec <- simulate(params = c(0.06, 0.74, 0.45, 1.000), nsims = nsims, Emax = Emax, Bmsy = Bmsy, msy = msy, utilfun = Pi_kur_bottom)

kur_dep_hyp <- simulate(params = c(0.06, 0, 0, 0.53), nsims = nsims, Emax = Emax, Bmsy = Bmsy, msy = msy, utilfun = Pi_kur_bottom)

kur_rec_hyp <- simulate(params = c(0.001, 0.74, 0.45, 0.53), nsims = nsims, Emax = Emax, Bmsy = Bmsy, msy = msy, utilfun = Pi_kur_bottom)


# Reshape with kur_bottom-----------------------------------------------------------------

dfs <- c("kur_null", "kur_dep", "kur_rec", "kur_hyp", "kur_dep_rec", "kur_dep_hyp", "kur_rec_hyp")
for (i in dfs){
  assign(i, transform(get(i), 
                      cumulative_catch = ((cumulative_catch - kur_ref_catch)/kur_ref_catch)*100,
                      cumulative_effort = ((cumulative_effort - kur_ref_effort)/kur_ref_effort)*100))
}


# Repeat this with rag_prize --------------------------------------------------


rag_null <- simulate(params = param_vec, nsims = nsims, Emax = Emax, Bmsy = Bmsy, msy = msy, utilfun = Pi_rag_prize)

# define reference levels of cumulative catch and effort as the cumulative catch and effort when all parameters are at the null
rag_ref_effort <- rag_null$cumulative_effort[1]
rag_ref_catch <- rag_null$cumulative_catch[1]


# Single params with rag_prize -----------------------------------------------------------

rag_dep <- simulate(params = c(0.06, 0, 0, 1.000), nsims = nsims, Emax = Emax, Bmsy = Bmsy, msy = msy, utilfun = Pi_rag_prize)

rag_rec <- simulate(params = c(0.001, 0.74, 0.45, 1.000), nsims = nsims, Emax = Emax, Bmsy = Bmsy, msy = msy, utilfun = Pi_rag_prize)

rag_hyp <- simulate(params = c(0.001, 0, 0, 0.53), nsims = nsims, Emax = Emax, Bmsy = Bmsy, msy = msy, utilfun = Pi_rag_prize)


# Interactions with rag_prize------------------------------------------------------------

rag_dep_rec <- simulate(params = c(0.06, 0.74, 0.45, 1.000), nsims = nsims, Emax = Emax, Bmsy = Bmsy, msy = msy, utilfun = Pi_rag_prize)

rag_dep_hyp <- simulate(params = c(0.06, 0, 0, 0.53), nsims = nsims, Emax = Emax, Bmsy = Bmsy, msy = msy, utilfun = Pi_rag_prize)

rag_rec_hyp <- simulate(params = c(0.001, 0.74, 0.45, 0.53), nsims = nsims, Emax = Emax, Bmsy = Bmsy, msy = msy, utilfun = Pi_rag_prize)


# Reshape with rag_prize-----------------------------------------------------------------

dfs <- c("rag_null", "rag_dep", "rag_rec", "rag_hyp", "rag_dep_rec", "rag_dep_hyp", "rag_rec_hyp")
for (i in dfs){
  assign(i, transform(get(i), 
                      cumulative_catch = ((cumulative_catch - rag_ref_catch)/rag_ref_catch)*100,
                      cumulative_effort = ((cumulative_effort - rag_ref_effort)/rag_ref_effort)*100))
}

# Repeat this with kur_hms --------------------------------------------------

kur_hms_null <- simulate(params = param_vec, nsims = nsims, Emax = Emax, Bmsy = Bmsy, msy = msy, utilfun = Pi_kur_hms)

# define reference levels of cumulative catch and effort as the cumulative catch and effort when all parameters are at the null
kur_hms_ref_effort <- kur_hms_null$cumulative_effort[1]
kur_hms_ref_catch <- kur_hms_null$cumulative_catch[1]


# Single params with kur_hms -----------------------------------------------------------

kur_hms_dep <- simulate(params = c(0.06, 0, 0, 1.000), nsims = nsims, Emax = Emax, Bmsy = Bmsy, msy = msy, utilfun = Pi_kur_hms)

kur_hms_rec <- simulate(params = c(0.001, 0.74, 0.45, 1.000), nsims = nsims, Emax = Emax, Bmsy = Bmsy, msy = msy, utilfun = Pi_kur_hms)

kur_hms_hyp <- simulate(params = c(0.001, 0, 0, 0.53), nsims = nsims, Emax = Emax, Bmsy = Bmsy, msy = msy, utilfun = Pi_kur_hms)


# Interactions with kur_hms------------------------------------------------------------

kur_hms_dep_rec <- simulate(params = c(0.06, 0.74, 0.45, 1.000), nsims = nsims, Emax = Emax, Bmsy = Bmsy, msy = msy, utilfun = Pi_kur_hms)

kur_hms_dep_hyp <- simulate(params = c(0.06, 0, 0, 0.53), nsims = nsims, Emax = Emax, Bmsy = Bmsy, msy = msy, utilfun = Pi_kur_hms)

kur_hms_rec_hyp <- simulate(params = c(0.001, 0.74, 0.45, 0.53), nsims = nsims, Emax = Emax, Bmsy = Bmsy, msy = msy, utilfun = Pi_kur_hms)


# Reshape with kur_hms-----------------------------------------------------------------

dfs <- c("kur_hms_null", "kur_hms_dep", "kur_hms_rec", "kur_hms_hyp", "kur_hms_dep_rec", "kur_hms_dep_hyp", "kur_hms_rec_hyp")
for (i in dfs){
  assign(i, transform(get(i), 
                      cumulative_catch = ((cumulative_catch - kur_hms_ref_catch)/kur_hms_ref_catch)*100,
                      cumulative_effort = ((cumulative_effort - kur_hms_ref_effort)/kur_hms_ref_effort)*100))
}


# Repeat this with whi_sg --------------------------------------------------

whi_sg_null <- simulate(params = param_vec, nsims = nsims, Emax = Emax, Bmsy = Bmsy, msy = msy, utilfun = Pi_whi_sg)

# define reference levels of cumulative catch and effort as the cumulative catch and effort when all parameters are at the null
whi_sg_ref_effort <- whi_sg_null$cumulative_effort[1]
whi_sg_ref_catch <- whi_sg_null$cumulative_catch[1]


# Single params with whi_sg -----------------------------------------------------------

whi_sg_dep <- simulate(params = c(0.06, 0, 0, 1.000), nsims = nsims, Emax = Emax, Bmsy = Bmsy, msy = msy, utilfun = Pi_whi_sg)

whi_sg_rec <- simulate(params = c(0.001, 0.74, 0.45, 1.000), nsims = nsims, Emax = Emax, Bmsy = Bmsy, msy = msy, utilfun = Pi_whi_sg)

whi_sg_hyp <- simulate(params = c(0.001, 0, 0, 0.53), nsims = nsims, Emax = Emax, Bmsy = Bmsy, msy = msy, utilfun = Pi_whi_sg)


# Interactions with whi_sg------------------------------------------------------------

whi_sg_dep_rec <- simulate(params = c(0.06, 0.74, 0.45, 1.000), nsims = nsims, Emax = Emax, Bmsy = Bmsy, msy = msy, utilfun = Pi_whi_sg)

whi_sg_dep_hyp <- simulate(params = c(0.06, 0, 0, 0.53), nsims = nsims, Emax = Emax, Bmsy = Bmsy, msy = msy, utilfun = Pi_whi_sg)

whi_sg_rec_hyp <- simulate(params = c(0.001, 0.74, 0.45, 0.53), nsims = nsims, Emax = Emax, Bmsy = Bmsy, msy = msy, utilfun = Pi_whi_sg)


# Reshape with whi_sg-----------------------------------------------------------------

dfs <- c("whi_sg_null", "whi_sg_dep", "whi_sg_rec", "whi_sg_hyp", "whi_sg_dep_rec", "whi_sg_dep_hyp", "whi_sg_rec_hyp")
for (i in dfs){
  assign(i, transform(get(i), 
                      cumulative_catch = ((cumulative_catch - whi_sg_ref_catch)/whi_sg_ref_catch)*100,
                      cumulative_effort = ((cumulative_effort - whi_sg_ref_effort)/whi_sg_ref_effort)*100))
}

# Plot the four representative catch-effort functions ---------------------------------------------

Ct <- seq(0,1, by = 0.01)

whi_prob <- Pi_whi_mackerel(x = Ct, c50 = c50)
whi_effort <- whi_prob*Emax

plot(Ct, whi_effort, type = "l", #xaxt = "n", yaxt = "n", 
     xlab = "", ylab = "", ylim = c(0,48))
title(xlab = "Past catch rates", ylab = "Current effort", cex.lab = 3)


kur_prob <- Pi_kur_bottom(x = Ct, c50 = c50)
kur_effort <- kur_prob*Emax

plot(Ct, kur_effort, type = "l", #xaxt = "n", yaxt = "n", 
     xlab = "Past catch rates", ylab = "Current effort", ylim = c(0,48))


rag_prob <- Pi_rag_prize(x = Ct, c50 = c50)
rag_effort <- rag_prob*Emax

plot(Ct, rag_effort, type = "l", #xaxt = "n", yaxt = "n", 
     xlab = "Past catch rates", ylab = "Current effort", ylim = c(0,48))

kur_hms_prob <- Pi_kur_hms(x = Ct, c50 = c50)
kur_hms_effort <- kur_hms_prob*Emax

plot(Ct, kur_hms_effort, type = "l", #xaxt = "n", yaxt = "n", 
     xlab = "Past catch rates", ylab = "Current effort", ylim = c(0,48))

whi_sg_prob <- Pi_whi_sg(x = Ct, c50 = c50)
whi_sg_effort <- whi_sg_prob*Emax

plot(Ct, whi_sg_effort, type = "l", #xaxt = "n", yaxt = "n", 
     xlab = "Past catch rates", ylab = "Current effort", ylim = c(0,48))


# Plot interactions between depensation, hyperstability, and stochasticity at their median levels in the presence of four representative angler effort functions ------------------


figname <- "fig5.png"
png(paste(outfig, figname, sep = "/"), width = 17, height = 15, units = "in", res = 1000)

par(mar = c(1, 2, 4, 1))

par(mfrow = c(4,4))

# row 1
plot_radar_interaction(scenario1 = kur_hms_dep, 
                       scenario2 = kur_hms_rec,
                       int = kur_hms_dep_rec,
                       names = c("Median depensation",
                                 "Median stochasticity",
                                 "Median depensation and stochasticity"),
                       cols = col_key[1:2,1],
                       title = NULL,
                       legend = FALSE
)
mtext("A) High intercept, low steepness", side= 3, line = 2, font = 2, cex = 1.6)
mtext("Recruitment stochasticity\nSD = 0.74, \u03c1 = 0.45", side = 2,font = 2, line = -2, cex = 1.2, col = col_key[2,1])


par(mar = c(3, 4, 2, 5))

plot(Ct, kur_hms_effort, type = "l", xaxt = "n", yaxt = "n", 
     xlab = "", ylab = "", ylim = c(0,48))
title(xlab = "Past catch rates", ylab = "Current effort", line = 1, cex.lab = 2)

par(mar = c(1, 2, 4, 1))

plot_radar_interaction(scenario1 = kur_dep, 
                       scenario2 = kur_rec,
                       int = kur_dep_rec,
                       names = c("Median depensation",
                                 "Median stochasticity",
                                 "Median depensation and stochasticity"),
                       cols = col_key[1:2,1],
                       title = NULL,
                       legend = FALSE
)
mtext("B) High intercept, high steepness", side= 3, line = 2, font = 2, cex = 1.6)
mtext("Recruitment stochasticity\nSD = 0.74, \u03c1 = 0.45",font = 2, side = 2, line = -2, cex = 1.2, col = col_key[2,1])

par(mar = c(3, 4, 2, 5))

plot(Ct, kur_effort, type = "l", xaxt = "n", yaxt = "n", 
     xlab = "", ylab = "", ylim = c(0,48))
title(xlab = "Past catch rates", ylab = "Current effort", line = 1, cex.lab = 2)


# row 2

par(mar = c(4, 2, 2, 1))

plot_radar_interaction(scenario1 = kur_hms_dep, 
                       scenario2 = kur_hms_hyp,
                       int = kur_hms_dep_hyp,
                       names = c("Median depensation",
                                 "Median hyperstability",
                                 "Median depensation and hyperstability"),
                       cols = col_key[c(1,3),1],
                       title = NULL,
                       legend = FALSE
)
mtext("Hyperstability\n\u03b2 = 0.53", side = 2, font = 2, line = -2, cex = 1.2, col = col_key[3,1])
mtext("Depensation\nd = 0.06", side = 1, line =2, font = 2, cex = 1.2, col = col_key[1,1])

plot_radar_interaction(scenario1 = kur_hms_rec, 
                       scenario2 = kur_hms_hyp,
                       int = kur_hms_rec_hyp,
                       names = c("Median stochasticity",
                                 "Median hyperstability",
                                 "Median stochasticity and hyperstability"),
                       cols = col_key[2:3,1],
                       title = NULL,
                       legend = FALSE
)
mtext("Recruitment stochasticity\nSD = 0.74, \u03c1 = 0.45", side = 1,font = 2, line = 2, cex = 1.2, col = col_key[2,1])

plot_radar_interaction(scenario1 = kur_dep, 
                       scenario2 = kur_hyp,
                       int = kur_dep_hyp,
                       names = c("Median depensation",
                                 "Median hyperstability",
                                 "Median depensation and hyperstability"),
                       cols = col_key[c(1,3),1],
                       title = NULL,
                       legend = FALSE
)
mtext("Hyperstability\n\u03b2 = 0.53", side = 2, font = 2, line = -2, cex = 1.2, col = col_key[3,1])
mtext("Depensation\nd = 0.06", side = 1, line = 2,font = 2, cex = 1.2, col = col_key[1,1])

plot_radar_interaction(scenario1 = kur_rec, 
                       scenario2 = kur_hyp,
                       int = kur_rec_hyp,
                       names = c("Median stochasticity",
                                 "Median hyperstability",
                                 "Median stochasticity and hyperstability"),
                       cols = col_key[2:3,1],
                       title = NULL,
                       legend = FALSE
)
mtext("Recruitment stochasticity\nSD = 0.74, \u03c1 = 0.45", side = 1,font = 2, line = 2, cex = 1.2, col = col_key[2,1])

# row 3

par(mar = c(1, 2, 4, 1))

plot_radar_interaction(scenario1 = rag_dep, 
                       scenario2 = rag_rec,
                       int = rag_dep_rec,
                       names = c("Median depensation",
                                 "Median stochasticity",
                                 "Median depensation and stochasticity"),
                       cols = col_key[1:2,1],
                       title = NULL,
                       legend = FALSE
)
mtext("C) Low intercept, low steepness", side= 3, line = 2, font = 2, cex = 1.6)

mtext("Recruitment stochasticity\nSD = 0.74, \u03c1 = 0.45", side = 2, line = -2, cex = 1.2, col = col_key[2,1],font = 2)

par(mar = c(3, 4, 2, 5))

plot(Ct, rag_effort, type = "l", xaxt = "n", yaxt = "n", 
     xlab = "", ylab = "", ylim = c(0,48))
title(xlab = "Past catch rates", ylab = "Current effort", line = 1, cex.lab = 2)

par(mar = c(1, 2, 4, 1))


plot_radar_interaction(scenario1 = whi_sg_dep, 
                       scenario2 = whi_sg_rec,
                       int = whi_sg_dep_rec,
                       names = c("Median depensation",
                                 "Median stochasticity",
                                 "Median depensation and stochasticity"),
                       cols = col_key[1:2,1],
                       title = NULL,
                       legend = FALSE
)
mtext("D) Low intercept, high steepness", side= 3, line = 2, font = 2, cex = 1.6)
mtext("Recruitment stochasticity\nSD = 0.74, \u03c1 = 0.45", side = 2, line = -2, cex = 1.2, col = col_key[2,1],font = 2)

par(mar = c(3, 4, 2, 5))

plot(Ct, whi_sg_effort, type = "l", xaxt = "n", yaxt = "n", 
     xlab = "", ylab = "", ylim = c(0,48))
title(xlab = "Past catch rates", ylab = "Current effort", line = 1, cex.lab = 2)



# row 4

par(mar = c(4, 2, 2, 1))

plot_radar_interaction(scenario1 = rag_dep, 
                       scenario2 = rag_hyp,
                       int = rag_dep_hyp,
                       names = c("Median depensation",
                                 "Median hyperstability",
                                 "Median depensation and hyperstability"),
                       cols = col_key[c(1,3),1],
                       title = NULL,
                       legend = FALSE
)
mtext("Hyperstability\n\u03b2 = 0.53", side = 2, font = 2, line = -2, cex = 1.2, col = col_key[3,1])
mtext("Depensation\nd = 0.06", side = 1, line = 2, font = 2, cex = 1.2, col = col_key[1,1])

plot_radar_interaction(scenario1 = rag_rec, 
                       scenario2 = rag_hyp,
                       int = rag_rec_hyp,
                       names = c("Median stochasticity",
                                 "Median hyperstability",
                                 "Median stochasticity and hyperstability"),
                       cols = col_key[2:3,1],
                       title = NULL,
                       legend = FALSE
)
mtext("Recruitment stochasticity\nSD = 0.74, \u03c1 = 0.45", side = 1,font = 2, line = 2, cex = 1.2, col = col_key[2,1])

plot_radar_interaction(scenario1 = whi_sg_dep, 
                       scenario2 = whi_sg_hyp,
                       int = whi_sg_dep_hyp,
                       names = c("Median depensation",
                                 "Median hyperstability",
                                 "Median depensation and hyperstability"),
                       cols = col_key[c(1,3),1],
                       title = NULL,
                       legend = FALSE
)
mtext("Depensation\nd = 0.06", side = 1, line = 2,font = 2, cex = 1.2, col = col_key[1,1])
mtext("Hyperstability\n\u03b2 = 0.53", side = 2, font = 2, line = -2, cex = 1.2, col = col_key[3,1])

plot_radar_interaction(scenario1 = whi_sg_rec, 
                       scenario2 = whi_sg_hyp,
                       int = whi_sg_rec_hyp,
                       names = c("Median stochasticity",
                                 "Median hyperstability",
                                 "Median stochasticity and hyperstability"),
                       cols = col_key[2:3,1],
                       title = NULL,
                       legend = FALSE
)
mtext("Recruitment stochasticity\nSD = 0.74, \u03c1 = 0.45",font = 2, side = 1, line = 2, cex = 1.2, col = col_key[2,1])

dev.off()
graphics.off()
