## Create figure for powerpoints
## To illustrate varying steepnesses and intercepts, following Post et al. 2013


rm(list = ls())
options(scipen = 999)

# load packages and functions

library(here)
library(tidyverse)

# set output directory

outfig <- here::here("ppt_figs")


# simulate data

rational <- function(a, b, c, d, x){
  (a*x + c)/(b*x + d)
}

x <- seq(0,5, by = 0.1)


rat1 <- rational(a = 1.2, b = 1, c = 0, d= 1, x = x)
rat2 <- rational(a = 1.2, b = 1, c = 0.4, d= 1, x = x)
rat3 <- rational(a = 1.2, b = 1, c = -0.5, d= 1, x = x)

plot(x, rat1, xlim = c(0,5), ylim = c(0,1.1), type = "l", xlab = "", ylab = "", xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", bty = "l", lwd = 2)
lines(x, rat2, xlim = c(0,5), ylim = c(0,1.1), type = "l", xlab = "", ylab = "", xaxt = "n", yaxt = "n", col = "#D7191C", lwd = 2)
lines(x, rat3, xlim = c(0,5), ylim = c(0,1.1), type = "l", xlab = "", ylab = "", xaxt = "n", yaxt = "n", col = "#2C7BB6", lwd = 2)


rat1 <- rational(a = 1.2, b = 1, c = 0, d= 1, x = x)
rat4 <- rational(a = 1.75, b = 1, c = 0, d= 1, x = x)
rat5 <- rational(a = 0.8, b = 1, c = -0, d= 1, x = x)

plot(x, rat1, xlim = c(0,5), ylim = c(0,1.5), type = "l", xlab = "", ylab = "", xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", bty = "l", lwd = 2)
lines(x, rat4, xlim = c(0,5), ylim = c(0,1.5), type = "l", xlab = "", ylab = "", xaxt = "n", yaxt = "n", col = "#D7191C", lwd = 2)
lines(x, rat5, xlim = c(0,5), ylim = c(0,1.5), type = "l", xlab = "", ylab = "", xaxt = "n", yaxt = "n", col = "#2C7BB6", lwd = 2)

figname <- "Post_2013_for_ppt.png"
png(paste(outfig, figname, sep = "/"), width = 9, height = 5, units = "in", res = 1000)


par(mfrow = c(2,4))
par(mar = c(2, 2, 2, 2))

# varying intercept

plot(x, rat1, xlim = c(0,5), ylim = c(0,1.5), type = "l", xlab = "", ylab = "", xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", bty = "l", lwd = 2)
title(xlab = "Catch rates", ylab = "Fishing effort", line = 0.5)

plot(x, rat1, xlim = c(0,5), ylim = c(0,1.5), type = "l", xlab = "", ylab = "", xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", bty = "l", lwd = 2)
lines(x, rat3, xlim = c(0,5), ylim = c(0,1.5), type = "l", xlab = "", ylab = "", xaxt = "n", yaxt = "n", col = "#2C7BB6", lwd = 2)
title(xlab = "Catch rates", ylab = "Fishing effort", line = 0.5)


plot(x, rat1, xlim = c(0,5), ylim = c(0,1.5), type = "l", xlab = "", ylab = "", xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", bty = "l", lwd = 2)
lines(x, rat2, xlim = c(0,5), ylim = c(0,1.5), type = "l", xlab = "", ylab = "", xaxt = "n", yaxt = "n", col = "#D7191C", lwd = 2)
title(xlab = "Catch rates", ylab = "Fishing effort", line = 0.5)


plot(x, rat1, xlim = c(0,5), ylim = c(0,1.5), type = "l", xlab = "", ylab = "", xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", bty = "l", lwd = 2)
lines(x, rat2, xlim = c(0,5), ylim = c(0,1.5), type = "l", xlab = "", ylab = "", xaxt = "n", yaxt = "n", col = "#D7191C", lwd = 2)
lines(x, rat3, xlim = c(0,5), ylim = c(0,1.5), type = "l", xlab = "", ylab = "", xaxt = "n", yaxt = "n", col = "#2C7BB6", lwd = 2)
title(xlab = "Catch rates", ylab = "Fishing effort", line = 0.5)

# varying steepness

plot(x, rat1, xlim = c(0,5), ylim = c(0,1.5), type = "l", xlab = "", ylab = "", xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", bty = "l", lwd = 2)
title(xlab = "Catch rates", ylab = "Fishing effort", line = 0.5)

plot(x, rat1, xlim = c(0,5), ylim = c(0,1.5), type = "l", xlab = "", ylab = "", xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", bty = "l", col = "#2C7BB6", lwd = 2)
lines(x, rat5, xlim = c(0,5), ylim = c(0,1.5), type = "l", xlab = "", ylab = "", xaxt = "n", yaxt = "n", col = "#D7191C", lwd = 2)
title(xlab = "Catch rates", ylab = "Fishing effort", line = 0.5)


plot(x, rat1, xlim = c(0,5), ylim = c(0,1.5), type = "l", xlab = "", ylab = "", xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", bty = "l",col = "#2C7BB6",  lwd = 2)
lines(x, rat4, xlim = c(0,5), ylim = c(0,1.5), type = "l", xlab = "", ylab = "", xaxt = "n", yaxt = "n", col = "#D7191C", lwd = 2)
title(xlab = "Catch rates", ylab = "Fishing effort", line = 0.5)


plot(x, rat1, xlim = c(0,5), ylim = c(0,1.5), type = "l", xlab = "", ylab = "", xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", bty = "l",col = "#2C7BB6", lwd = 2)
lines(x, rat5, xlim = c(0,5), ylim = c(0,1.5), type = "l", xlab = "", ylab = "", xaxt = "n", yaxt = "n", col = "#D7191C", lwd = 2)
lines(x, rat4, xlim = c(0,5), ylim = c(0,1.5), type = "l", xlab = "", ylab = "", xaxt = "n", yaxt = "n", col = "#D7191C", lwd = 2)
title(xlab = "Catch rates", ylab = "Fishing effort", line = 0.5)


dev.off()
graphics.off()