# setup environment
source("./R/setup.R") 

# run simulation
results <- replicate(n_sim, sim_once(n_devset, n_valset, m, p), simplify = FALSE)

# export results
# TODO: figure out how to save things from the hpc