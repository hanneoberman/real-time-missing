impute <- function(data, mu, sigma, m) {
  imp_list <- purrr::map(c(4, 6, 8), function(p) {
    # process inputs
    miss <- (nrow(sigma) - p + 1):nrow(sigma)
    obs <-  1:(nrow(sigma) - p)
    x_obs <- data[data$p_miss == p,-c(1, 2)]
    # compute
    B <- sigma[miss, miss]
    C <- sigma[miss, obs, drop = FALSE]
    D <- sigma[obs, obs]
    CDinv <- C %*% solve(D)
    # conditionals
    c_var <- B - CDinv %*% t(C)
    c_mu <-
      purrr::map_dfr(1:nrow(x_obs), function(i) {
        c(mu[miss] + CDinv %*% (as.numeric(x_obs[i, obs]) - mu[obs])) %>%
          c(as.numeric(rownames(x_obs[i, obs])), p) %>%
          setNames(c(paste0("X", miss), "id", "p_miss"))
      })
    # impute mean
    imp_mean <- cbind(x_obs[, obs], c_mu, draw = NA)
    # impute draw
    imp_draw <-
      purrr::map_dfr(1:nrow(c_mu), function(i) {
        MASS::mvrnorm(m, mu = as.numeric(c_mu[i, c(paste0("X", miss))]), Sigma = c_var) %>%
          cbind(x_obs[i, obs],
                .,
                c_mu[i, c("id", "p_miss")],
                draw = 1:nrow(.),
                row.names = NULL)
      })
    list(mean = imp_mean, draw = imp_draw)
  }) %>% setNames(c("miss_4", "miss_6", "miss_8"))
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