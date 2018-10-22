# load libraries
library(knitr); library(pander); library(magrittr); library(forcats); 
library(cowplot); library(rstanarm); library(xtable);
library(png); library(grid); library(ggthemes); library(lsmeans)
library(lme4); library(feather); library(tidybayes); library(knitcitations)
library(papaja)

options(kableExtra.latex.load_packages = TRUE)
library(kableExtra)

library(tidyverse)

# set ggplot theme
theme_set(
  ggthemes::theme_few() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
)

## some functions for the speed-acc-novel chapter

aggregate_ss_looking <- function(df, ss_grouping_cols, ms_grouping_cols, aoi_column, return_ss_df = FALSE, use_bootstrap = FALSE, na_rm = FALSE, n_boot = 1000) {
  # create group by to get the total number of looks in each time slice
  total_looks_group_by <- rlang::syms(ss_grouping_cols)
  
  # add aoi looking column to group by to get the total number of looks to an AOI in a timeslice
  aoi_looks_group_by <- rlang::syms(c(ss_grouping_cols, aoi_column))
  
  ss_total <- df %>% 
    count(!!! total_looks_group_by) %>% 
    complete(!!! total_looks_group_by, fill = list(n = 0)) %>% 
    rename(n_total_looks = n)
  
  ss_aois <- df %>%
    count(!!! aoi_looks_group_by) %>%
    complete(!!! aoi_looks_group_by, fill = list(n = 0)) %>%
    rename(n_aoi_looks = n)
  
  ss_final <- left_join(ss_aois, ss_total) %>%
    mutate(prop_looking = n_aoi_looks / n_total_looks) %>% 
    filter(!is.nan(prop_looking))
  
  ## Aggreate if we want the subject level data 
  if (return_ss_df) {
    ss_final
  } else {
    # this code removes Sub.Num from grouping cols
    ms_groupings_no_subid <- stringr::str_remove(ms_grouping_cols, "subid") %>% .[. != ""]   
    
    if(use_bootstrap) {
      ss_final %>% 
        group_by(!!! rlang::syms(c(ms_grouping_cols, aoi_column))) %>% 
        summarise(m = mean(prop_looking)) %>% 
        group_by(!!! rlang::syms(c(ms_groupings_no_subid, aoi_column))) %>% 
        tidyboot::tidyboot_mean(column = m, nboot = n_boot, na.rm = na_rm)
    } else {
      ss_final %>% 
        group_by(!!! rlang::syms(c(ms_grouping_cols, aoi_column))) %>% 
        summarise(m = mean(prop_looking)) %>% 
        group_by(!!! rlang::syms(c(ms_groupings_no_subid, aoi_column))) %>% 
        summarise(m = mean(m), 
                  n = n()) 
    }
  }
  
}

# create time bins at the trial level
create_time_bin_trial <- function(trial, t_ms_diff = 33) {
  n_bins <- nrow(trial)
  max_time <- trial$t_rel_noun %>% max() * 1000
  min_time_bin <- trial$t_rel_noun %>% min() * 1000
  time_ms <- seq.int(min_time_bin, max_time, by = t_ms_diff %>% round()) %>% .[1:n_bins]
  
  trial %>% 
    mutate(time_ms_normalized = time_ms ,
           time_bin = seq.int(1, n_bins))
}

# create time bins at the subject level
create_time_bins_ss <- function(ss_df, t_ms_diff = 33) {
  ss_df %>% 
    split(.$trial_num_exp) %>% 
    purrr::map_dfr(create_time_bin_trial, t_ms_diff)
}


# Logit function for passing chance performance offsets to glms
logit <- function(x) {log(x/(1-x))}

# Fits logistic regressions to estimate coefficients and significance values 
# for individual factors
test.chance <- function(data,groups,formula,row=1) {
  chance.tests <- data %>%
    group_by_(.dots = groups) %>%
    do(chance.lm = summary(glm(formula, offset=logit(1/numPicN),
                               family="binomial", data = .)))
  
  chance.tests$betas<- sapply(chance.tests$chance.lm,
                              function(x) {x$coefficients[row,1]})
  chance.tests$zs <- sapply(chance.tests$chance.lm,
                            function(x) {x$coefficients[row,3]})
  chance.tests$ps <- sapply(chance.tests$chance.lm,
                            function(x) {x$coefficients[row,4]})
  
  return(select(chance.tests, -chance.lm))
}

## get stars for significance testing
getstars <- function(x) {
  if (x > .1) {return("")}
  if (x < .001) {return("***")}
  if (x < .01) {return("**")}
  if (x < .05) {return("*")}
  return(".")}

# make function to convert logit back to probability 
logit_to_prob <- function(logit) {
  odds <- exp(logit)
  odds / (1 + odds)
}
