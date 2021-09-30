# Script to produce Figure 2: Empirical angler effort functions

# Setup ---------------------------------------------------------

# clear workspace and set options

rm(list = ls())
options(scipen = 999)

source("functions/angler_effort_funs.R")


library(cowplot)
library(gridGraphics)
library(RColorBrewer)
library(viridis)
library(patchwork)
library(gridExtra)
library(ggpubr)

outfig <- here::here("figures")


Ct <- seq(0,1, by = 0.01)
c50 <- 0.1
out <- data.frame(citation = NA, species = NA, lambda = NA, intercept = NA, Ct = NA, prob = NA, funlab = NA)


for (j in 1:length(Ct)){
  for (i in 1:nrow(df)){
    pr = do.call(funlist[[i]], args = list(x = Ct[j], c50 = c50))
    out[nrow(out) + 1,] = list(df[i,1], df[i,2], df[i,3],
                               df[i,4], Ct[j], pr, names(funlist)[[i]])
  }
}



out2 <- out %>% 
  slice(-1) %>% 
  filter(lambda >= 0) %>% 
  left_join(locations, by = "citation") %>% 
  mutate(lambda = signif(as.numeric(lambda), 1),
         intercept = round(as.numeric(intercept), 2),
         labs = paste(species, "\n\u03bb = ", lambda, "\nintercept = ", intercept, sep = ""),
         loc_labs = paste(loc, " (", citation, ")", sep = "")) %>% 
  arrange(lambda) %>% 
  mutate(labs_fac = ordered(labs, unique(labs)))

facet_p <- ggplot(out2, aes(x = Ct, y = prob))+
  geom_line(aes(color = loc_labs), size = 1.5)+
  scale_color_brewer(type = "qual", palette = "Set1", name = "Location")+
  theme_classic()+
  theme(axis.line=element_line(),
        strip.background = element_blank())+
  scale_x_continuous(limits=c(0,1)) + scale_y_continuous(limits=c(0,1))+
  labs(x = "Catch rate",
       y = "Proportion of max fishing effort")+
  facet_wrap(.~labs_fac, scales = "free")

leg <- get_legend(facet_p)
gg_leg <- as_ggplot(leg)

cols <- brewer.pal(5,"Set1")
col_key <- data.frame(cols = cols, loc = unique(out2$loc))
out_manual <- left_join(out2, col_key, by = "loc")

fun_nam <- names(funlist)


for (i in 1:length(funlist)){
  dat <- out_manual %>% filter(funlab == names(funlist)[[i]])
  p <- ggplot(data = dat,
         aes(x = Ct, y = prob))+
    geom_line(color = dat$cols, size = 1.5)+
    theme_classic()+
    theme(axis.line=element_line(),
          strip.background = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 10),
          axis.text=element_blank(),
          axis.ticks=element_blank())+
    scale_x_continuous(limits=c(0,1)) + scale_y_continuous(limits=c(0,1))+
    labs(title = dat$labs,
         x = NULL,
         y = NULL)
  
  assign(fun_nam[i], p)
    
}


fig2 <- ggarrange(NULL, NULL, kur_inshore, kur_hms, kur_coastal, kur_bottom, NULL,
          NULL, NULL, whi_other, NULL,  NULL, whi_mackerel, NULL,
          rag_reef, rag_key, NULL, NULL, whi_cmp, whi_sg, whi_billfish,
          rag_butter, rag_table, rag_prize, gent, mkwara, NULL, NULL,
          nrow = 4, ncol = 7
          )+
  annotation_custom(leg, xmin = 0.01, ymin = 0.55, xmax = 0.25)


figname <-"fig2.png"
png(paste(outfig, figname, sep = "/"), width = 12, height = 8, units = "in", res = 1000)

annotate_figure(fig2,
                bottom = text_grob(expression(atop("Catch rate", bolditalic("Increasing \u03bb")))),
                left = text_grob(expression(atop(bolditalic("Increasing intercept"), "Proportion of maximum fishing effort")), 
                                 rot = 90))


grid.lines(x = unit(c(0.1,0.95), 'npc'),
           y = unit(0.03, 'npc'),
           gp = gpar(col = "black"),
           arrow = arrow(angle = 30, length = unit(0.25, "inches"),
                         ends = "last", type = "open"))
grid.lines(x = 0.02,
           y = c(0.1,0.95),
           gp = gpar(col = "black"),
           arrow = arrow(angle = 30, length = unit(0.25, "inches"),
                         ends = "last", type = "open"))

dev.off()
graphics.off()
