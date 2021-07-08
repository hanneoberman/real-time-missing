# functions for imputing the missing values in an incomplete set

# helper function to compute conditional mean and var for imputation
comp_cond <- function(devs, vals, m, p) {
  # process inputs
  means <- colMeans(devs)
  vcov <- cov(devs)
  miss <- (nrow(vcov) - p + 1):nrow(vcov)
  obs <-  1:(nrow(vcov) - p)
  x_obs <- vals[vals$p_miss == p, -1]
  # compute
  B <- vcov[miss, miss]
  C <- vcov[miss, obs, drop = FALSE]
  D <- vcov[obs, obs]
  CDinv <- C %*% solve(D)
  # conditionals
  c_var <- B - CDinv %*% t(C)
  c_mu <-
    purrr::map_dfr(1:nrow(x_obs), function(i) {
      c(means[miss] + CDinv %*% (as.numeric(x_obs[i, obs]) - means[obs])) %>%
        c(as.numeric(rownames(x_obs[i, obs])), p) %>%
        setNames(c(paste0("X", miss), "id", "p_miss"))
    })
  # impute mean
  imp_mean <- cbind(x_obs[, obs], c_mu)
  # impute draw
  imp_draw <-
    purrr::map(1:nrow(c_mu), function(i) {
      MASS::mvrnorm(m, mu = as.numeric(c_mu[i, c(paste0("X", miss))]), Sigma = c_var) %>%
        cbind(x_obs[i, obs], ., c_mu[i, c("id", "p_miss")], row.names = NULL)
    })
  # outputs
  return(list(imp_mean = imp_mean, imp_draw = imp_draw))
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
