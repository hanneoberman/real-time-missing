# setup simulation study
# run this once

# packages
library("dplyr") #for wrangling
library("purrr") #for vectorization
library("mvtnorm") #for DGM
library("mice") #for DGM
library("rms") #for prediction
library("party") #for prediction
library("ranger") #for prediction

# functions
source("./R/DGM.R")
source("./R/fit.R")
source("./R/imp.R")
source("./R/pred.R")

# parameters
set.seed(11)
n_sim <- 2 #TODO: make this 1000
n_devset <- 100 #TODO: make this 10000
n_valset <- 200 #TODO: make this 20000
m <- 11 #TODO: make this 51
p <- 10 #p_missing is hard coded as 4, 6 or 8
DGM <- define_DGM(p = 10)

# TODO: create wrapper function to repeat simulation

# fit models in dev set
mod <- generate_data(
  sample_size = n_devset,
  covariance_matrix = DGM$varcov,
  linear_bs = DGM$betas[1:p],
  non_linear_bs = DGM$betas[(p + 1):(2 * p)],
  interaction = TRUE) %>% 
    fit_mod(n_predictors = p)

# predict outcome in val set
pred <- generate_data(
  sample_size = round(1.01 * n_valset, 0),
  DGM$varcov,
  DGM$betas[1:p],
  DGM$betas[(p + 1):(2 * p)],
  interaction = TRUE) %>% 
    create_miss(missingness_pat = DGM$miss_pat, missingness_type = DGM$miss_type) %>% 
  # TODO: add prediction function here!
  pred_Y(fitted_mod = mod, n_imp = m)

