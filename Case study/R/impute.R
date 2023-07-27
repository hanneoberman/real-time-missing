impute <- function(data, means, varcov, m) {
    # process inputs
    x_obs <- data[, names(means)]
    miss <- apply(is.na(x_obs), 2, any)
    obs <-  !miss
    # compute
    B <- varcov[miss, miss]
    C <- varcov[miss, obs, drop = FALSE]
    D <- varcov[obs, obs]
    CDinv <- C %*% solve(D)
    # conditionals
    c_var <- B - CDinv %*% t(C)
    c_mu <-
      purrr::map_dbl(1:nrow(x_obs), function(i) {
        c(means[miss] + CDinv %*% (as.numeric(x_obs[i, obs]) - means[obs])) 
      })
    # impute mean
    imp_mean <- cbind(x_obs[, obs], c_mu, draw = NA)
    # impute draw
    imp_draw <-
      purrr::map_dfr(1:length(c_mu), function(i) {
        MASS::mvrnorm(m, mu = as.numeric(c_mu[i]), Sigma = c_var) %>%
          cbind(x_obs[i, obs],
                .,
                c_mu = c_mu[i],
                draw = 1:nrow(.),
                row.names = NULL)
      })
 
  # output
  imp_means <- map_dfr(imp_list, "mean") %>% dplyr::arrange(id)
  imp_draws <- map_dfr(imp_list, "draw") %>% dplyr::arrange(id)
  singles <- imp_draws$draw == m
  return(list(
    mean = imp_means,
    sing = imp_draws[singles, ],
    mult = imp_draws[!singles, ]
  ))
}