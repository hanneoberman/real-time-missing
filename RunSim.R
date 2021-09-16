# Simulation script SIG

setup_env <- function(){
# set-up environment
library("dplyr") #for the pipe operator
# library("ggplot2") #for plotting
# library("ggcorrplot") #for easy visualization of correlations
library("mvtnorm") #for multivariate normal distributions
library("purrr") #for vectorized 'for loops'
# library("pROC") #for easy calculation of the auc
library("mice") #for missing data stuffs
library("ranger") #for fast random forests
library("missRanger") #for fast random forests with imputation
library("party") #for random forests with surrogate splits
source("./R/DGM.R") #data generating mechanism
source("./R/fit.R") #fit models on complete data
source("./R/imp.R") #impute missing data
source("./R/pred.R") #predict outcome variable from incomplete data
source("./R/perf.R") #compute performance measures for each method
set.seed(1)
# define DGM and missingness parameters once
varcov <- define_varcov(p = 10)
linear_betas <- runif(nrow(varcov), -1, 1)
non_linear_betas <- runif(nrow(varcov), -0.1, 0.1)
miss_par <- define_miss(p = nrow(varcov))
}

########## start simulation ###########
sim_once <- function(){
# create devset and fit models
devset <-
  generate_sample(
    sample_size = 10000,
    covariance_matrix = varcov,
    linear_bs = linear_betas,
    non_linear_bs = non_linear_betas,
    interaction = TRUE
  )
mod_log <- fit_log(devset[,-1])
mod_sub_6 <- fit_sub(devset[,-1], Y ~ X1 + X2 + X3 + X4 + X5 + X6)
mod_sub_4 <- fit_sub(devset[,-1], Y ~ X1 + X2 + X3 + X4)
mod_sub_2 <- fit_sub(devset[,-1], Y ~ X1 + X2)
mod_rf_imp <- fit_rf_imp(devset[,-1])
# TODO: uncomment this for real sim 
# mod_rf_sur <- fit_rf_sur(devset) #too slow!
# TODO: make one list of fitted models!!

# create incomplete valset
valset <-
  generate_sample(
    sample_size = 20000,
    varcov,
    linear_betas,
    non_linear_betas,
    interaction = TRUE
  ) %>% create_miss(., miss_par)

# impute valset and predict the outcome
Y_pred_sub <- pred_sub(valset[,-c(2:3)], mod_sub_2, mod_sub_4, mod_sub_6)
# TODO: change number of imp to 51 for real sim
imp_all <- purrr::map(c(4,6,8), ~comp_cond(devset[,-c(1:2)], valset[,-c(2:3)], m = 11, p = .x))
Y_pred_mean <- suppressWarnings(pred_mean(imp_all, mod_log))
Y_pred_draw <- suppressWarnings(pred_draw(imp_all, mod_log))
# TODO: actually predict with surrogate splits for real sim
Y_pred_rf_sur <- rep(0, nrow(valset)) #too slow! 

# test performance function on list of predictions
pred_list <- list(Y_prob = valset$Y_prob,
                  Y_obs = valset$Y,
                  Y_sub = Y_pred_sub,
                  Y_rf_sur = Y_pred_rf_sur,
                  Y_imp_mean = Y_pred_mean,
                  Y_imp_sing = Y_pred_draw$sing,
                  Y_imp_mult = Y_pred_draw$mult)

}
######### end simulation ##########

# collect results