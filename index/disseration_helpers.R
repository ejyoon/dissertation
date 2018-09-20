# load libraries
library(knitr); library(pander); library(magrittr); library(forcats); 
library(cowplot); library(rstanarm); library(xtable);
library(png); library(grid); library(ggthemes); library(lsmeans)
library(lme4)

library(tidyverse)

# set ggplot theme
theme_set(
  ggthemes::theme_few() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
)


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