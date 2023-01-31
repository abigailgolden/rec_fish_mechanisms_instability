## Script to produce multipanel conceptual figure 1

# Setup ---------------------------------------------------------

source("model/model.R")
outfig <- here::here("figures")

library(cowplot)
library(gridGraphics)
library(viridis)
library(patchwork)

set.seed(303)


# Stochasticity -------------------------------------------------------

mod1 <- simulate(params = c(0.001, 0, 0, 1), nsims = 100, utilfun = Pi_kur_hms, Emax = 38, Bmsy = 0.00001439714, msy = 0.3998857, ts = TRUE)

mod2 <- simulate(params = c(0.001, 0.75, 0, 1), nsims = 100, utilfun = Pi_kur_hms, Emax = 38, Bmsy = 0.00001439714, msy = 0.3998857, ts = TRUE)

mod3 <- simulate(params = c(0.001, 0.75, 1, 1), nsims = 100, utilfun = Pi_kur_hms, Emax = 38, Bmsy = 0.00001439714, msy = 0.3998857, ts = TRUE)

stoch_dat <- data.frame(t = 1:60, no_stoch = mod1[["Bt"]][1:60,1], uncor_stoch = mod2[["Bt"]][1:60,8],cor_stoch = mod3[["Bt"]][1:60,9],
                        no_stoch_rt = mod1[["Rt"]][1:60,1],
                        uncor_stoch_rt = mod2[["Rt"]][1:60, 8],
                        cor_stoch_rt = mod3[["Rt"]][1:60, 9])

stoch_dat_gg <- pivot_longer(stoch_dat, cols = 2:4, names_to = "type", values_to = "bt") %>% 
  mutate(type = factor(type, levels = c("no_stoch", "uncor_stoch", "cor_stoch")))

stoch_cols <- c("black", "orange", "#ca0020")

stoch <- ggplot(data = stoch_dat_gg, aes(x = t, y = bt))+
  geom_line(aes(color = type), size = 1)+
  scale_color_manual(values = stoch_cols)+
  xlab("Time")+
  ylab("Biomass")+
  annotate("text", x = 40, y = 0.00004, label = "delta", size = 5, col = stoch_cols[2], parse = TRUE, hjust = 0)+
  annotate("text", x = 40, y = 0.000035, label = "delta + rho", size = 5, col = stoch_cols[3], parse = TRUE,hjust = 0)+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.text=element_blank(),
        axis.ticks=element_blank())

print(stoch)


# Depensation ---------------------------------------------------

carK <- 0.00004216221
alpha <- 237179.2
beta <- 213461.3
sdrec <- 0.74
rho <- 0.43
nsims <- 1

qRec1 <- 0.001
qRec2 <- 0.2

ssb <- seq(0,0.00004216221, length.out = 200)

r1 <- rep(NA, times = length(ssb))
r2 <- rep(NA, times = length(ssb))

for(i in 1:length(ssb)){
  d <- depensation(qRec1, ssb[i])
  autocor <- recAR(prev_ar = 0, sdrec = 0)
  r1[i] <- recruit(ar = autocor, b = ssb[i], d = d)
}

for(i in 1:length(ssb)){
  d <- depensation(qRec2, ssb[i])
  autocor <- recAR(prev_ar = 0, sdrec = 0)
  r2[i] <- recruit(ar = autocor, b = ssb[i], d = d)
}


dep_dat <- data.frame(ssb = ssb, r1 = r1, r2 = r2) %>% 
  pivot_longer(cols = 2:3, names_to = "type", 
               values_to = "recruitment")


dep <- ggplot(data = dep_dat, aes(x = ssb, y = recruitment))+
  geom_line(aes(color = type, linetype = type), size = 1)+
  scale_color_manual(values = c("black", "#ca0020"))+
  xlab("Spawning Stock Biomass")+
  ylab("Recruitment")+
  annotate("text", x = 0.000005, y = 0.8, label = "d \u2248 0", col = "black", size = 5)+
  annotate("text", x = 0.000015, y = 0.5, label = "d = 0.2", col = "#ca0020", size = 5)+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.text=element_blank(),
        axis.ticks=element_blank())
print(dep)


# Hyperstability -------------------------------------------------------

n <- seq(0,1,by = 0.01)
q <- 0.01
E <- 1
b <- c(0.25, 0.5, 1,2,4)

hyp_dat <- data.frame(n = rep(NA, times = length(n)*length(b)),
                      b = rep(NA, times = length(n)*length(b)),
                      cpue = rep(NA, times = length(n)*length(b)))

tick <- 1

for (h in 1:length(b)){
  for (i in 1:length(n)){
    C = q*E*n[i]^b[h]
    hyp_dat[tick,1] = n[i]
    hyp_dat[tick,2] = b[h]
    hyp_dat[tick,3] = C/E
    tick <- tick + 1
  }
}

hyp_dat_clean <- hyp_dat %>% 
  mutate(b = as.factor(hyp_dat$b))

cols <- brewer.pal(7, "RdYlBu")
cols[3] <- "black"

labs <- paste("beta == ", b, sep = "")

hyp <- ggplot(data = hyp_dat_clean, aes(x = n, y = cpue))+
  geom_path(aes(color = b), size = 1)+
  scale_color_manual(values = cols)+
  labs(x = "Abundance", y = "CPUE", color = expression(beta))+
  geom_text(aes(label = labs), data = hyp_dat_clean %>% filter(n == 0.5), parse = T, nudge_y = 0.0005, nudge_x = -0.01, size = 4)+
  theme_classic()+
  theme(legend.position = "none"
  )+
  theme(axis.text=element_blank(),
        axis.ticks=element_blank())

hyp

# Simplified hyperstability plot for ppt

hyp_filt <- hyp_dat_clean %>% 
  filter(b == 0.25 | b == 1 | b == 4)

cols_filt <- c(cols[1], cols[3], cols[7])


hyp_simple <- ggplot(data = hyp_filt, aes(x = n, y = cpue))+
  geom_path(aes(color = b), size = 1)+
  scale_color_manual(values = cols_filt)+
  labs(x = "Abundance", y = "CPUE", color = expression(beta))+
  #geom_text(aes(label = labs), data = hyp_dat_clean %>% filter(n == 0.5), parse = T, nudge_y = 0.0005, nudge_x = -0.01, size = 4)+
  theme_classic()+
  theme(legend.position = "none"
  )+
  theme(axis.text=element_blank(),
        axis.ticks=element_blank())

outfig <- here::here("ppt_figs")


figname <- "hyperstab_simple.png"
png(paste(outfig, figname, sep = "/"), width = 5, height = 4, units = "in", res = 750)


hyp_simple

dev.off()
graphics.off()

# Catch responsiveness ------------------------------------------------




# Catch responsiveness, modified from Post et al. 2013 --------------------

logistic_null <- function(a, x){
  exp(a*x)/(exp(a*x) + 1)
}

logistic <- function(c, a, x){
  (c*exp(a*x))/(c*exp(a*x) + (1-c))
}

xnull <- seq(0,5, by = 0.01)
x <- seq(0,5, by = 0.01)

ynull <- logistic_null(1.75, xnull-2.5)
y1 <- (logistic(0.01, 0.5, x)) # low int, low lambda
y2 <- (logistic(0.03, 5, x))  # low int, high lambda
y3 <- (logistic(0.45, 0.35, x))  # high int, low lambda
y4 <- (logistic(0.5, 2.8, x))     # high int, high lambda

cols <- c("#2C7BB6","#91BFDB", "#FDAE61","#D7191C" )


cr <- ggplot()+
  geom_line(aes(x = xnull, y = ynull), col = "black", lty = "solid", size = 1)+
  geom_line(aes(x = x, y = y1), col = cols[1], size = 1)+
  geom_line(aes(x = x, y = y2), col = cols[2], size = 1)+
  geom_line(aes(x = x, y = y3), col = cols[3], size = 1)+
  geom_line(aes(x = x, y = y4), col = cols[4], size = 1)+
  annotate("text", x = 4.5, y = 0.2, label = "low intercept,\nlow \u03bb", color = cols[1], size = 3)+
  annotate("text", x = 1.8, y = 0.8, label = "low intercept,\nhigh \u03bb", color = cols[1], size = 3)+
  annotate("text", x = 4.5, y = 0.68, label = "high intercept,\nlow \u03bb", color = cols[3], size = 3)+
  annotate("text", x = 0.7, y = 1, label = "high intercept,\nhigh \u03bb", color = cols[4], size = 3)+
  labs(x = "Catch rates", y = "Angler effort response")+
  theme_classic()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 5))+ 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.1))

cr

# Assemble figure -----------------------------------------------------


# output lower resolution figure to manuscript folder


figname <- "Fig1.png"
png(paste(outfig, figname, sep = "/"), width = 8, height = 6, units = "in", res = 750)

(dep + stoch) / (hyp + cr) + plot_annotation(tag_levels = 'A')

dev.off()
graphics.off()