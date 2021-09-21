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

# parameters
set.seed(11)
n_sim <- 2 #TODO: make this 1000
n_devset <- 10000
n_valset <- 20000
p <- 10
DGM <- define_DGM(p = 10)

# 