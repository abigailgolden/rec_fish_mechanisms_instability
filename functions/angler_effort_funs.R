# Study-specific functions for calculating anglers' probability of fishing
# Full citations are included at the bottom of this script

library(tidyverse)

# set up dataframe to output the parameters lambda (anglers' responsiveness to catch; equation 9 in manuscript) and y-intercept (probability of fishing when catch is zero; equation 13 in manuscript) for each study and species

effort_params <- data.frame(citation = NA,
                            species =  NA,
                            lambda =  NA,
                            intercept = NA)

c50 <- 0.1  # scaling parameter for catch

# Raguragavan et al. 2013 --------------------------------------------

# prize fish

Pi_rag_prize <- function(x, c50){
  S <- 48
  prize <- 1.28
  reef <- 1.47
  key <- 1.39
  table <- 1.97
  butter <- 8.86
  Vbar <- 0.003*1104.33 + -0.001*141.81 + 0.09*prize + 0.01*reef + 0.05*key + 0.03*table + 0.01*butter + 0.001*reef*0.24
  Vi <- 0.003*1104.33 + -0.001*141.81 + 0.09*(x/c50) + 0.01*reef + 0.05*key + 0.03*table + 0.01*butter + 0.001*reef*0.24
  Pi <- exp(Vi)/(exp(Vi) + (S-1)*exp(Vbar))
  return(Pi)
}

effort_params[1,] <- c("Raguragavan et al. 2013", 
                       "prize fish",
                       0.09*Pi_rag_prize(x = 0.001, c50 = c50)*(1-Pi_rag_prize(x = 0.001, c50 = c50)),
                       Pi_rag_prize(x = 0, c50 = c50))

# reef fish

Pi_rag_reef <- function(x, c50){
  S <- 48
  prize <- 1.28
  reef <- 1.47
  key <- 1.39
  table <- 1.97
  butter <- 8.86
  Vbar <- 0.003*1104.33 + -0.001*141.81 + 0.09*prize + 0.01*reef + 0.05*key + 0.03*table + 0.01*butter + 0.001*reef*0.24
  Vi <- 0.003*1104.33 + -0.001*141.81 + 0.09*prize + 0.01*(x/c50) + 0.05*key + 0.03*table + 0.01*butter + 0.001*(x/c50)*0.24
  Pi <- exp(Vi)/(exp(Vi) + (S-1)*exp(Vbar))
  return(Pi)
}


effort_params[2,] <- c("Raguragavan et al. 2013", 
                       "reef fish", 
                       0.01*Pi_rag_reef(x = 0.001, c50 = c50)*(1-Pi_rag_reef(x = 0.001, c50 = c50)), 
                       Pi_rag_reef(x = 0, c50 = c50))

# key sports fish

Pi_rag_key <- function(x, c50){
  S <- 48
  prize <- 1.28
  reef <- 1.47
  key <- 1.39
  table <- 1.97
  butter <- 8.86
  Vbar <- 0.003*1104.33 + -0.001*141.81 + 0.09*prize + 0.01*reef + 0.05*key + 0.03*table + 0.01*butter + 0.001*reef*0.24
  Vi <- 0.003*1104.33 + -0.001*141.81 + 0.09*prize + 0.01*reef + 0.05*(x/c50) + 0.03*table + 0.01*butter + 0.001*reef*0.24
  Pi <- exp(Vi)/(exp(Vi) + (S-1)*exp(Vbar))
  return(Pi)
}

effort_params[3,] <- c("Raguragavan et al. 2013", 
                       "key sports fish", 
                       0.05*Pi_rag_key(x = 0.001, c50 = c50)*(1-Pi_rag_key(x = 0.001, c50 = c50)), 
                       Pi_rag_key(x = 0, c50 = c50))

# table fish

Pi_rag_table <- function(x, c50){
  S <- 48
  prize <- 1.28
  reef <- 1.47
  key <- 1.39
  table <- 1.97
  butter <- 8.86
  Vbar <- 0.003*1104.33 + -0.001*141.81 + 0.09*prize + 0.01*reef + 0.05*key + 0.03*table + 0.01*butter + 0.001*reef*0.24
  Vi <- 0.003*1104.33 + -0.001*141.81 + 0.09*prize + 0.01*reef + 0.05*key + 0.03*(x/c50) + 0.01*butter + 0.001*reef*0.24
  Pi <- exp(Vi)/(exp(Vi) + (S-1)*exp(Vbar))
  return(Pi)
}

effort_params[4,] <- c("Raguragavan et al. 2013", 
                       "table fish", 0.03*Pi_rag_table(x = 0.001, c50 = c50)*(1-Pi_rag_table(x = 0.001, c50 = c50)), 
                       Pi_rag_table(x = 0, c50 = c50))

# butter fish

Pi_rag_butter <- function(x, c50){
  S <- 48
  prize <- 1.28
  reef <- 1.47
  key <- 1.39
  table <- 1.97
  butter <- 8.86
  Vbar <- 0.003*1104.33 + -0.001*141.81 + 0.09*prize + 0.01*reef + 0.05*key + 0.03*table + 0.01*butter + 0.001*reef*0.24
  Vi <- 0.003*1104.33 + -0.001*141.81 + 0.09*prize + 0.01*reef + 0.05*key + 0.03*table + 0.01*(x/c50) + 0.001*reef*0.24
  Pi <- exp(Vi)/(exp(Vi) + (S-1)*exp(Vbar))
  return(Pi)
}

effort_params[5,] <- c("Raguragavan et al. 2013", 
                       "butter fish",  
                       0.01*Pi_rag_butter(x = 0.001, c50 = c50)*(1-Pi_rag_butter(x = 0.001, c50 = c50)), 
                       Pi_rag_butter(x = 0, c50 = c50))

# Kuriyama et al. 2013 ------------------------------------------------

Pi_kur_bottom <- function(x, c50){
  S <- 37
  bottomfish <- 0.3931
  coastal <- 1.9405
  hms <- 1.7687
  inshore <- 1.5734
  notarget <- 0.2108
  Vbar <- -2.4853*0.6851 + 0.6436*0.4453 + -0.3706*0.4574 + 0.3532*bottomfish + 0.0986*coastal + 0.0506*hms + 0.0231*inshore + -1.632*notarget 
  Vi <- 2.4853*0.6851 + 0.6436*0.4453 + -0.3706*0.4574 + 0.3532*(x/c50) + 0.0986*coastal + 0.0506*hms + 0.0231*inshore + -1.632*notarget 
  Pi <- exp(Vi)/(exp(Vi) + (S-1)*exp(Vbar))
  return(Pi)
}


effort_params[6,] <- c("Kuriyama et al. 2013", 
                       "bottomfish",  
                       0.3532*Pi_kur_bottom(x = 0.001, c50 = c50)*(1-Pi_kur_bottom(x = 0.001, c50 = c50)), 
                       Pi_kur_bottom(x = 0, c50 = c50))

# coastal migratory species

Pi_kur_coastal <- function(x, c50){
  S <- 37
  bottomfish <- 0.3931
  coastal <- 1.9405
  hms <- 1.7687
  inshore <- 1.5734
  notarget <- 0.2108
  Vbar <- -2.4853*0.6851 + 0.6436*0.4453 + -0.3706*0.4574 + 0.3532*bottomfish + 0.0986*coastal + 0.0506*hms + 0.0231*inshore + -1.632*notarget 
  Vi <- 2.4853*0.6851 + 0.6436*0.4453 + -0.3706*0.4574 + 0.3532*bottomfish + 0.0986*(x/c50) + 0.0506*hms + 0.0231*inshore + -1.632*notarget 
  Pi <- exp(Vi)/(exp(Vi) + (S-1)*exp(Vbar))
  return(Pi)
}

effort_params[7,] <- c("Kuriyama et al. 2013", 
                       "coastal migratory species",  
                       0.0986*Pi_kur_coastal(x = 0.001, c50 = c50)*(1-Pi_kur_coastal(x = 0.001, c50 = c50)), 
                       Pi_kur_coastal(x = 0, c50 = c50))


# highly migratory species

Pi_kur_hms <- function(x, c50){
  S <- 37
  bottomfish <- 0.3931
  coastal <- 1.9405
  hms <- 1.7687
  inshore <- 1.5734
  notarget <- 0.2108
  Vbar <- -2.4853*0.6851 + 0.6436*0.4453 + -0.3706*0.4574 + 0.3532*bottomfish + 0.0986*coastal + 0.0506*hms + 0.0231*inshore + -1.632*notarget 
  Vi <- 2.4853*0.6851 + 0.6436*0.4453 + -0.3706*0.4574 + 0.3532*bottomfish + 0.0986*coastal + 0.0506*(x/c50) + 0.0231*inshore + -1.632*notarget 
  Pi <- exp(Vi)/(exp(Vi) + (S-1)*exp(Vbar))
  return(Pi)
}


effort_params[8,] <- c("Kuriyama et al. 2013", 
                       "highly migratory species",
                       0.0506*Pi_kur_hms(x = 0.001, c50 = c50)*(1-Pi_kur_hms(x = 0.001, c50 = c50)), 
                       Pi_kur_hms(x = 0, c50 = c50))

# inshore

Pi_kur_inshore <- function(x, c50){
  S <- 37
  bottomfish <- 0.3931
  coastal <- 1.9405
  hms <- 1.7687
  inshore <- 1.5734
  notarget <- 0.2108
  Vbar <- -2.4853*0.6851 + 0.6436*0.4453 + -0.3706*0.4574 + 0.3532*bottomfish + 0.0986*coastal + 0.0506*hms + 0.0231*inshore + -1.632*notarget 
  Vi <- 2.4853*0.6851 + 0.6436*0.4453 + -0.3706*0.4574 + 0.3532*bottomfish + 0.0986*coastal + 0.0506*hms + 0.0231*(x/c50) + -1.632*notarget 
  Pi <- exp(Vi)/(exp(Vi) + (S-1)*exp(Vbar))
  return(Pi)
}


effort_params[9,] <- c("Kuriyama et al. 2013", 
                       "inshore species", 
                       0.0231*Pi_kur_inshore(x = 0.001, c50 = c50)*(1-Pi_kur_inshore(x = 0.001, c50 = c50)), 
                       Pi_kur_inshore(x = 0, c50 = c50))



# Gentner et al. 2006 ---------------------------------------------------
# use the full model, which has the highest R^2 value

Pi_gent <- function(x, c50){
  S <- 63
  distance <- 41.54
  Vi <- 0.05242*(0.33*distance*2) + 0.67653*(x/c50) + -0.23226*((distance*2)/40) + 0.68125*log(63)
  Vbar <- 0.05242*(0.33*distance*2) + 0.67653*0.19 + -0.23226*((distance*2)/40) + 0.68125*log(63)
  Pi <- exp(Vi)/(exp(Vi) + (S-1)*exp(Vbar))
  return(Pi)
}

effort_params[10,] <- c("Gentner 2006", 
                        "striped bass",  
                        0.67653*Pi_gent(x = 0.001, c50 = c50)*(1-Pi_gent(x = 0.001, c50 = c50)), 
                        Pi_gent(x = 0, c50 = c50))


# Mkwara et al. 2015 ------------------------------------------------------
# use Rotoiti as the modeled site, because it has the greatest number of fishing days in the sample
# use model 1 (no seasonal variability) because the paper doesn't report all bimonthly sample means and the 2 models have equivalent log likelihoods
# uses average weight of fish at each site as a proxy for fishing quality; can I use this as my catch attribute?

Pi_mkwara <- function(x, c50){
  cost <- 58.7
  fweight <- 1.6
  sd <- 6.4
  lksize <- 18.7
  fdv <- 2.4
  depth <- 29.3
  urban <- 0.014
  forest <- 0.568
  hwarning <- 0.45
  
  S <- 11
  Vbar <- -0.072*cost + 0.19*sd + 1.376*fweight + 3.407*log(lksize) + 0.356*fdv + -0.352*urban + 0.015*forest + -0.056*depth + -0.606*hwarning
  Vi <- -0.072*cost + 0.19*sd + 1.376*(x/c50) + 3.407*log(lksize) + 0.356*fdv + -0.352*urban + 0.015*forest + -0.056*depth + -0.606*hwarning
  Pi <- exp(Vi)/(exp(Vi) + (S-1)*exp(Vbar))
  return(Pi)
}


effort_params[11,] <- c("Mkwara et al. 2015", 
                        "trout",  
                        1.376*Pi_mkwara(x = 0.001, c50 = c50)*(1-Pi_mkwara(x = 0.001, c50 = c50)), 
                        Pi_mkwara(x = 0, c50 = c50))

# Whitehead et al. 2013 -----------------------------------------------

# use the Hanover County site as the focal site because the highest percentage of trips originate there (31%)
# mode-specific intercepts are not provided, so assume that the mode-specific utilities for each site are all the same
# Use model of primary purpose anglers

Pi_whi_billfish <- function(x, c50){
  Pi <- c()
  for(i in 1:length(x)){
  if((x[i]/c50) > 22){
    Pi[i] <- 1
  }else{
    billfish <- 0.02
    cmp <- 2
    mackerel <- 1
    sg <- 1
    other <- 4
    iv <- 0.84
    vi_han <- -0.012*630 +26.798*(x[i]/c50) + 0.662*cmp +0.47*mackerel + 1.117*sg + 0.048*other + 2.06*1
    vbar_han <- -0.012*630 +26.798*billfish + 0.662*cmp +0.47*mackerel + 1.117*sg + 0.048*other + 2.06*1
    v_roanoke <- -0.012*630 +26.798*billfish + 0.662*cmp +0.47*mackerel + 1.117*sg + 0.048*other
    v_central <- -0.012*630 +26.798*billfish + 0.662*cmp +0.47*mackerel + 1.117*sg + 0.048*other + 1.501*1
    v_outerbanks <- -0.012*630 +26.798*billfish + 0.662*cmp +0.47*mackerel + 1.117*sg + 0.048*other + 1.177
    v_brunswick <- -0.012*630 +26.798*billfish + 0.662*cmp +0.47*mackerel + 1.117*sg + 0.048*other + 0.016
    Pi[i] <- (exp(vi_han/iv)*((3*exp(vi_han/iv))^(iv-1))) / ((exp(vi_han/iv)*((3*exp(vi_han/iv))^(iv-1))) + 4*(exp(vbar_han/iv)*((3*exp(vbar_han/iv))^(iv-1))))
  }
  }
  return(Pi)

}

effort_params[12,] <- c("Whitehead et al. 2013", 
                        "billfish",  
                        26.798*Pi_whi_billfish(x = 0.001, c50 = c50)*(1-Pi_whi_billfish(x = 0.001, c50 = c50)), 
                        Pi_whi_billfish(x = 0, c50 = c50))

Pi_whi_cmp <- function(x, c50){
  billfish <- 0.02
  cmp <- 2
  mackerel <- 1
  sg <- 1
  other <- 4
  iv <- 0.84
  vi_han <- -0.012*630 +26.798*billfish + 0.662*(x/c50) +0.47*mackerel + 1.117*sg + 0.048*other + 2.06*1
  vbar_han <- -0.012*630 +26.798*billfish + 0.662*cmp +0.47*mackerel + 1.117*sg + 0.048*other + 2.06*1
  v_roanoke <- -0.012*630 +26.798*billfish + 0.662*cmp +0.47*mackerel + 1.117*sg + 0.048*other
  v_central <- -0.012*630 +26.798*billfish + 0.662*cmp +0.47*mackerel + 1.117*sg + 0.048*other + 1.501*1
  v_outerbanks <- -0.012*630 +26.798*billfish + 0.662*cmp +0.47*mackerel + 1.117*sg + 0.048*other + 1.177
  v_brunswick <- -0.012*630 +26.798*billfish + 0.662*cmp +0.47*mackerel + 1.117*sg + 0.048*other + 0.016
  Pi <- (exp(vi_han/iv)*((3*exp(vi_han/iv))^(iv-1))) / ((exp(vi_han/iv)*((3*exp(vi_han/iv))^(iv-1))) + 4*(exp(vbar_han/iv)*((3*exp(vbar_han/iv))^(iv-1))))
  return(Pi)
}

effort_params[13,] <- c("Whitehead et al. 2013", 
                        "coastal migratory pelagics",  
                        0.662*Pi_whi_cmp(x = 0.001, c50 = c50)*(1-Pi_whi_cmp(x = 0.001, c50 = c50)), 
                        Pi_whi_cmp(x = 0, c50 = c50))

Pi_whi_mackerel <- function(x, c50){
  billfish <- 0.02
  cmp <- 2
  mackerel <- 1
  sg <- 1
  other <- 4
  iv <- 0.84
  vi_han <- -0.012*630 +26.798*billfish + 0.662*cmp +0.47*(x/c50) + 1.117*sg + 0.048*other + 2.06*1
  vbar_han <- -0.012*630 +26.798*billfish + 0.662*cmp +0.47*mackerel + 1.117*sg + 0.048*other + 2.06*1
  v_roanoke <- -0.012*630 +26.798*billfish + 0.662*cmp +0.47*mackerel + 1.117*sg + 0.048*other
  v_central <- -0.012*630 +26.798*billfish + 0.662*cmp +0.47*mackerel + 1.117*sg + 0.048*other + 1.501*1
  v_outerbanks <- -0.012*630 +26.798*billfish + 0.662*cmp +0.47*mackerel + 1.117*sg + 0.048*other + 1.177
  v_brunswick <- -0.012*630 +26.798*billfish + 0.662*cmp +0.47*mackerel + 1.117*sg + 0.048*other + 0.016
  Pi <- (exp(vi_han/iv)*((3*exp(vi_han/iv))^(iv-1))) / ((exp(vi_han/iv)*((3*exp(vi_han/iv))^(iv-1))) + 4*(exp(vbar_han/iv)*((3*exp(vbar_han/iv))^(iv-1))))
  return(Pi)
}


effort_params[14,] <- c("Whitehead et al. 2013", 
                        "mackerel",  
                        0.47*Pi_whi_mackerel(x = 0.001, c50 = c50)*(1-Pi_whi_mackerel(x = 0.001, c50 = c50)), 
                        Pi_whi_mackerel(x = 0, c50 = c50))


Pi_whi_sg <- function(x, c50){
  billfish <- 0.02
  cmp <- 2
  mackerel <- 1
  sg <- 1
  other <- 4
  iv <- 0.84
  vi_han <- -0.012*630 +26.798*billfish + 0.662*cmp +0.47*mackerel + 1.117*(x/c50) + 0.048*other + 2.06*1
  vbar_han <- -0.012*630 +26.798*billfish + 0.662*cmp +0.47*mackerel + 1.117*sg + 0.048*other + 2.06*1
  v_roanoke <- -0.012*630 +26.798*billfish + 0.662*cmp +0.47*mackerel + 1.117*sg + 0.048*other
  v_central <- -0.012*630 +26.798*billfish + 0.662*cmp +0.47*mackerel + 1.117*sg + 0.048*other + 1.501*1
  v_outerbanks <- -0.012*630 +26.798*billfish + 0.662*cmp +0.47*mackerel + 1.117*sg + 0.048*other + 1.177
  v_brunswick <- -0.012*630 +26.798*billfish + 0.662*cmp +0.47*mackerel + 1.117*sg + 0.048*other + 0.016
  Pi <- (exp(vi_han/iv)*((3*exp(vi_han/iv))^(iv-1))) / ((exp(vi_han/iv)*((3*exp(vi_han/iv))^(iv-1))) + 4*(exp(vbar_han/iv)*((3*exp(vbar_han/iv))^(iv-1))))
  return(Pi)
}

effort_params[15,] <- c("Whitehead et al. 2013", 
                        "snapper-grouper",  
                        1.117*Pi_whi_sg(x = 0.001, c50 = c50)*(1-Pi_whi_sg(x = 0.001, c50 = c50)), 
                        Pi_whi_sg(x = 0, c50 = c50))

Pi_whi_other <- function(x, c50){
  billfish <- 0.02
  cmp <- 2
  mackerel <- 1
  sg <- 1
  other <- 4
  iv <- 0.84
  vi_han <- -0.012*630 + 26.798*billfish + 0.662*cmp +0.47*mackerel + 1.117*sg + 0.048*(x/c50) + 2.06*1
  vbar_han <- -0.012*630 +26.798*billfish + 0.662*cmp +0.47*mackerel + 1.117*sg + 0.048*other + 2.06*1
  v_roanoke <- -0.012*630 +26.798*billfish + 0.662*cmp +0.47*mackerel + 1.117*sg + 0.048*other
  v_central <- -0.012*630 +26.798*billfish + 0.662*cmp +0.47*mackerel + 1.117*sg + 0.048*other + 1.501*1
  v_outerbanks <- -0.012*630 +26.798*billfish + 0.662*cmp +0.47*mackerel + 1.117*sg + 0.048*other + 1.177
  v_brunswick <- -0.012*630 +26.798*billfish + 0.662*cmp +0.47*mackerel + 1.117*sg + 0.048*other + 0.016
  Pi <- (exp(vi_han/iv)*((3*exp(vi_han/iv))^(iv-1))) / ((exp(vi_han/iv)*((3*exp(vi_han/iv))^(iv-1))) + 4*(exp(vbar_han/iv)*((3*exp(vbar_han/iv))^(iv-1))))
  return(Pi)
}

effort_params[16,] <- c("Whitehead et al. 2013", 
                        "other fish", 
                        0.048*Pi_whi_other(x = 0.001, c50 = c50)*(1-Pi_whi_other(x = 0.001, c50 = c50)), 
                        Pi_whi_other(x = 0, c50 = c50))

# Function list -------------------------------------------------------

# save a list of the angler effort functions

funlist <- list(rag_prize = Pi_rag_prize, 
                rag_reef = Pi_rag_reef, 
                rag_key = Pi_rag_key, 
                rag_table = Pi_rag_table, 
                rag_butter = Pi_rag_butter,
                kur_bottom = Pi_kur_bottom, 
                kur_coastal = Pi_kur_coastal, 
                kur_hms = Pi_kur_hms, 
                kur_inshore = Pi_kur_inshore, 
                gent = Pi_gent,
                mkwara = Pi_mkwara,
                whi_billfish = Pi_whi_billfish, 
                whi_cmp = Pi_whi_cmp, 
                whi_mackerel = Pi_whi_mackerel, 
                whi_sg = Pi_whi_sg,
                whi_other = Pi_whi_other)

# add study location to the dataframe of summary parameters

locations <- data.frame(citation = unique(effort_params$citation),
                        loc = c("Australia", 
                                "California", 
                                "Northeast U.S.", 
                                "New Zealand",
                                "North Carolina"))

df <- effort_params %>%
  left_join(locations, by = "citation") %>%
  mutate(y = 0,
         lambda = as.numeric(lambda),
         log_lambda = log(lambda),
         intercept = as.numeric(intercept),
         labs = paste(loc, species, sep = "\n"))

# References

# Gentner, B. (2006). Sensitivity of angler benefit estimates from a model of recreational demand to the definition of the substitute sites considered by the angler. Fishery Bulletin, 105(2), 161–167.

# Kuriyama, K., Hilger, J., & Hanemann, M. (2013). A random parameter model with onsite sampling for recreation site choice: An application to Southern California shoreline sportfishing. Environmental and Resource Economics, 56(4), 481–497. https://doi.org/10.1007/s10640-013-9640-4

# Mkwara, L., Marsh, D., & Scarpa, R. (2015). The effect of within-season variability on estimates of recreational value for trout anglers in New Zealand. Ecological Economics, 119, 338–345. https://doi.org/10.1016/j.ecolecon.2015.09.012

# Raguragavan, J., Hailu, A., & Burton, M. (2013). Economic valuation of recreational fishing in Western Australia: Statewide random utility modelling of fishing site choice behaviour. Australian Journal of Agricultural and Resource Economics, 57(4), 539–558. https://doi.org/10.1111/1467-8489.12009

# Whitehead, J. C., Dumas, C. F., Landry, C. E., & Herstine, J. (2013). A recreation demand model of the North Carolina for-hire fishery: A comparison of primary and secondary purpose anglers. Applied Economics Letters, 20(16), 1481–1484. https://doi.org/10.1080/13504851.2013.826864




