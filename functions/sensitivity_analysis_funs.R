# Functions for running and visualizing a sensitivity analysis across the range of values observed for each parameter

source("model/model.R")

library(ggnewscale)
library(patchwork)

# function to simulate along a range of parameter values

simulate_along <- function(par_range, params, par_id, utilfun, Emax, Bmsy, msy, utilname){
  range <- par_range
  params <- param_vec
  outlist <- list()
  progress_bar = txtProgressBar(min=0, max=length(range), style = 1, char="=")
  for (i in 1:length(range)){
    params[par_id] <- range[i]
    dat <- sim_multisite(params = params, nsims = nsims, Emax = Emax, Bmsy = Bmsy, utilfun = utilfun)
    dat2 <- summarise_all(dat, mean) %>% 
      pivot_longer(cols = 1:6, names_to = "varname", 
                   values_to = "val") %>% 
      mutate(param_name = param_names[par_id],
             param_val = range[i],
             cr_fun = utilname)
    outlist[[i]] <- dat2
    setTxtProgressBar(progress_bar, value = i)
  }
  close(progress_bar)
  outdat <- do.call(rbind, outlist)
}

# function to scale the output relative to the value of a given outcome variable at the reference value

scale_output <- function(dat, ref_param_val) {
  ref <- dat %>% 
    filter(param_val == ref_param_val) %>% 
    select(varname, val)
  colnames(ref)[2] <- "ref_val"
  
  scaled_dat <- dat %>%
    left_join(ref, by = c("varname")) %>% 
    mutate(pct_change = ifelse(ref_val != 0, ((val - ref_val)/ref_val*100), NA),
           varname = fct_relevel(varname, "cv_effort",
                                 "cv_biomass",
                                 "cumulative_effort",
                                 "cumulative_catch",
                                 "prop_extirpated",
                                 "prop_overfished"),
           type = ifelse(varname == "cv_effort" | varname == "cv_biomass", "variability",
                         ifelse(varname == "prop_extirpated" | varname == "prop_overfished",
                                "biological", "social")))
  return(scaled_dat)
  
}

# function to make heatmap of outcome variables

outvar_heatmap <- function(dat, title = dat$cr_fun, dat_range, emp_dat, xlab, ref, ylabelling = TRUE){
  if(ylabelling == TRUE){
    labs <- c("CV of effort", 
              "CV of biomass",
              "Cumulative effort",
              "Cumulative catch",
              "Proportion extirpated",
               "Proportion of\nyears overfished")
  }else{
    labs <- NULL
    }
  
  ggplot()+
    scale_y_discrete(limits = levels(dat$varname),
                     labels = labs)+
    geom_tile(data = filter(dat, 
                            varname == "prop_overfished" |
                              varname == "prop_extirpated"),
              aes(x = param_val, 
                  y = varname, 
                  fill = val))+
    scale_fill_distiller(type = "seq", palette = "Reds", direction = 1,
                         name = "Proportion",
                         limits = c(0,1),
                         guide = guide_legend(order = 1),
                         na.value = "white")+
    new_scale_fill()+
    geom_tile(data = filter(dat, 
                            varname == "cumulative_catch" |
                              varname == "cumulative_effort"),
              aes(x = param_val, 
                  y = varname, 
                  fill = pct_change))+
    scale_fill_distiller(type = "div", palette = "PiYG", direction = 1,
                         name = "% change in\ncumulative\nsocial benefits",
                         limits = c(-1*max(abs(dat_range[2,4:5])),
                                           max(abs(dat_range[2,4:5]))),
                         guide = guide_legend(order = 2),
                         na.value = "white")+
    new_scale_fill()+
    geom_tile(data = filter(dat, 
                            varname == "cv_effort" |
                              varname == "cv_biomass"),
              aes(x = param_val, y = varname, 
                  fill = val))+
    scale_fill_distiller(type = "seq", palette = "BuPu", direction = 1,
                         name = "Coefficient\nof variation",
                         limits = c(0, ifelse(dat_range$max_val[3] < 2, 2, dat_range$max_val[3])),
                         guide = guide_legend(order = 3),
                         na.value = "white")+
    geom_vline(xintercept = emp_dat, linetype = 2)+
    geom_vline(xintercept = ref, linetype = 1, size = 1.2)+
    labs(title = title, 
         x = xlab,
         y = NULL)+
    theme_classic()
}

