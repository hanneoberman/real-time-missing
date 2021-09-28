# functions for imputing the missing values in an incomplete set

# helper function to compute conditional mean and var for imputation
impute_cond <- function(vals, dev_means, dev_cov, m) {
  imp_list <- purrr::map(c(4,6,8), function(p){
  # process inputs
  miss <- (nrow(dev_cov) - p + 1):nrow(dev_cov)
  obs <-  1:(nrow(dev_cov) - p)
  x_obs <- vals[vals$p_miss == p, -c(1,2)]
  # compute
  B <- dev_cov[miss, miss]
  C <- dev_cov[miss, obs, drop = FALSE]
  D <- dev_cov[obs, obs]
  CDinv <- C %*% solve(D)
  # conditionals
  c_var <- B - CDinv %*% t(C)
  c_mu <-
    purrr::map_dfr(1:nrow(x_obs), function(i) {
      c(dev_means[miss] + CDinv %*% (as.numeric(x_obs[i, obs]) - dev_means[obs])) %>%
        c(as.numeric(rownames(x_obs[i, obs])), p) %>%
        setNames(c(paste0("X", miss), "id", "p_miss"))
    })
  # impute mean
  imp_mean <- cbind(x_obs[, obs], c_mu, draw = NA)
  # impute draw
  imp_draw <-
    purrr::map_dfr(1:nrow(c_mu), function(i) {
      MASS::mvrnorm(m, mu = as.numeric(c_mu[i, c(paste0("X", miss))]), Sigma = c_var) %>%
        cbind(x_obs[i, obs], ., c_mu[i, c("id", "p_miss")], draw = 1:nrow(.), row.names = NULL) 
    })
  list(mean = imp_mean, draw = imp_draw)}) %>% setNames(c("miss_4", "miss_6", "miss_8"))
  # outputs
  imp_means <- map_dfr(imp_list, "mean") %>% dplyr::arrange(id)
  imp_draws <- map_dfr(imp_list, "draw") %>% dplyr::arrange(id)
  singles <- imp_draws$draw == m
  return(list(
    mean = imp_means, 
    sing = imp_draws[singles,], 
    mult = imp_draws[!singles,]))
}

# test
# imp_all <- purrr::map(c(4,6,8), ~comp_cond(devset, valset, p = .x))
# imp_4 <- comp_cond(devset, valset, p = 4)
# imp_6 <- comp_cond(devset, valset, p = 6)
# imp_8 <- comp_cond(devset, valset, p = 8)

# ## helper function to combine conditional mean imputations
# # first collect conditionals for all md patterns
# a <- comp_cond(devs = devset, vals = valset, p = 4)
# b <- comp_cond(devs = devset, vals = valset, p = 6)
# c <- comp_cond(devs = devset, vals = valset, p = 8)
#
# # imputation method 1: Missing values are imputed by their conditional mean
# imp_cond <- rbind(a$cond_means, b$cond_means, c$cond_means) %>% dplyr::arrange(as.numeric(id))
#
# ## helper function to draw from conditional as imputation
# cov_cond <- list("4" = a$cond_vars, "6" = b$cond_vars, "8" = c$cond_vars)
#
# # imputation method 2: Missing values are imputed by a random draw from their conditional multivariate distribution.
# ## make this a function to apply to all md patterns and repeat for method 3 with n = 50
# a2 <- purrr::map_dfr(1:nrow(a$cond_mean), function(i){mvtnorm::rmvnorm(n = 1, mean = as.numeric(a$cond_means[i, 9:12]), sigma = as.matrix(a$cond_vars)) %>%
#     setNames(names(a$cond_means)[9:12]) %>%
#     data.frame()})
# a3 <- cbind(a$cond_means[, 1:7], a2)
