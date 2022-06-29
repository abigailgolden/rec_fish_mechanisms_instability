# Functions used to visualize model outputs

library(fmsb)
library(fishualize)
library(dichromat)
library(scales)

# color key for mechanisms of interest
cols <- fish(5, option = "Serranus_scriba")
cols <- cols[c(1,2,4)]
col_key <- data.frame(cols = cols, 
                      dat = c("dep", "rec", "hyp"),
                      stringsAsFactors = FALSE)

# create radar plot of a single scenario

plot_radar_basic <- function(dat,  title, legend = FALSE){
  df <- dat
  radardat <- data.frame(coefvar_effort = c(1,0),
                         coefvar_biomass = c(1,0),
                         cumulative_effort = c(50,-50),
                         cumulative_catch = c(50,-50),
                         prop_overfished = c(1,0),
                         prop_bel_msy = c(1,0))
  radardat[3,] <- apply(df, c(2), mean)
  rownames(radardat) <- c("max", "min", "data")
  
  radarchart(radardat, 
             axistype = 2,
             cglcol="grey", cglty=1, axislabcol="grey",
             cglwd=0.8, vlabels = c("CV effort",
                                    "CV biomass",
                                    "Cumul. effort",
                                    "Cumul. catch",
                                    "Prop. overfished",
                                    "Prop. below MSY"),
             vlcex = 0.8,
             title = title)
  if (legend == TRUE){
    legend(x=1.2, y = 1.2, legend = rownames(radardat[-c(1,2),]),
           bty = "n", pch=20,
           col=coul,
           text.col = "black",
           cex=0.9, pt.cex=3)
  }
}

# create a radar plot of a scenario compared to some null scenario 
plot_radar <- function(null, scenario, nullname, scenario_name, title, cols, legend = FALSE){
  null_output <- null
  scenario_output <- scenario
  radardat <- data.frame(coefvar_effort = c(5,0),
                         coefvar_biomass = c(5,0),
                         cumulative_effort = c(-100,50),
                         cumulative_catch = c(-100,50),
                         prop_extirpated = c(1,0),
                         prop_overfished = c(1,0))
  radardat[3,] <- apply(null_output, c(2), mean)
  radardat[4,] <- apply(scenario_output, c(2), mean)
  rownames(radardat) <- c("max", "min", nullname, scenario_name)
  cols <- c("gray40", cols)
  radarchart(radardat, 
             axistype = 2,
             cglcol="grey", cglty=1, axislabcol="grey",
             cglwd=1, vlabels = c("CV effort",
                                    "CV biomass",
                                    "Cumul. effort",
                                    "Cumul. catch",
                                    "Prop. extirpated",
                                    "Prop. overfished"),
             pcol=cols,
             plty = "solid",
             plwd = c(1,3),
             vlcex = 1,
             title = title)
  if (legend == TRUE){
  legend(x=1.2, y = 1.2, legend = rownames(radardat[-c(1,2),]),
         bty = "n", pch=20,
         col=coul,
         text.col = "black",
         cex=0.9, pt.cex=3)
  }
}

# create a radar plot with the option to have 2 null scenarios (for instance, all parameters turned off, then all parameters but one turned off)
# useful for exploring interactions

plot_radar_interaction <- function(scenario1, scenario2, int, names, title, cols, legend = FALSE){
  s1_output <- scenario1
  s2_output <- scenario2
  int_output <- int
  radardat <- data.frame(coefvar_effort = c(5,0),
                         coefvar_biomass = c(5,0),
                         cumulative_effort = c(-100,50),
                         cumulative_catch = c(-100,50),
                         prop_extirpated = c(1,0),
                         prop_overfished = c(1,0))
  radardat[3,] <- apply(s1_output, c(2), mean)
  radardat[4,] <- apply(s2_output, c(2), mean)
  radardat[5,] <- apply(int_output, c(2), mean)
  rownames(radardat) <- c("max", "min", names)
  cols <- c(cols, "black")
  
  radarchart(radardat, 
             axistype = 2,
             cglcol="seashell3", cglty=1, axislabcol="grey",
             cglwd=1, vlabels = c("CVE",
                                    "CVB",
                                    "CC",
                                    "CE",
                                    "PE",
                                    "PO"),
             pcol=cols,
             plty = c("solid", "solid", "dotted"),
             plwd = c(2,2,3),
             vlcex = 2,
             title = title)
  if (legend == TRUE){
    legend(x=1, y = 1.5, legend = rownames(radardat[-c(1,2),]),
           bty = "n", pch=20,
           col=coul,
           text.col = "black",
           cex=0.9, pt.cex=3)
    

  }
}
