# load hpc results

results_sur <- purrr::map(1:1000, ~{readRDS(paste0("./Results/hpc/output_sur_it_", .x, ".RDS"))})

# 81, 239, 397, 409, 410, 434, 913, 993
# 56, 789