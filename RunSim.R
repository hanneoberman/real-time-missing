#####################
# setup environment #
#####################

# packages
library("dplyr") #for wrangling
library("purrr") #for vectorization
library("mvtnorm") #for DGM
library("mice") #for DGM
library("rms") #for prediction
library("party") #for prediction
library("ranger") #for prediction

# functions 
source("./R/setup.R")

# parameters
set.seed(11)
n_sim <- 2 #TODO: make this 1000
n_devset <- 100 #TODO: make this 10000
n_valset <- 200 #TODO: make this 20000
m <- 11 #TODO: make this 51
p <- 10 #p_missing is hard coded as 4, 6 or 8
DGM <- define_DGM(p)
# TODO: check error variance glm in simulation 

#####################
# run simulation ####
#####################

# repeat the simulation pipeline n_sim times
results <- replicate(n_sim, simulate_once(DGM, n_devset, n_valset, m, p), simplify = FALSE)

# export results
# TODO: figure out how to save things from the hpc
export_list <- list(DGM = DGM, results = results)
