# load libraries
library(knitr); library(pander); library(magrittr); library(forcats); 
library(cowplot); library(rstanarm); library(xtable);
library(png); library(grid); library(ggthemes); 

library(tidyverse)

# set ggplot theme
theme_set(
  ggthemes::theme_few() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
)
