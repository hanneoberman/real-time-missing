impute <- function(data,
                   means,
                   varcov,
                   method = NULL,
                   m = 5) {
  # process inputs
  if (!any(is.na(data))) {
    return(data)
  }
  if (is.null(method)) {
    method <- "norm"
  }
  x_obs <- data[, names(means)]
  miss <- apply(is.na(x_obs), 2, any)
  obs <-  !miss
  if (method == "norm" | method == "draw") {
    out <- x_obs
  }
  if (method == "mult") {
    out <- rep(list(x_obs), m)
  }
  
  # compute
  B <- varcov[miss, miss]
  C <- varcov[miss, obs, drop = FALSE]
  D <- varcov[obs, obs]
  CDinv <- C %*% solve(D)
  
  # conditionals
  c_var <- B - CDinv %*% t(C)
  c_mu <-
    purrr::map_dfr(1:nrow(x_obs), function(i) {
      as.data.frame(t(means[miss] + CDinv %*% (as.numeric(x_obs[i, obs]) - means[obs])))
    })
  
  # impute mean
  if (method == "norm") {
    out[, miss] <- c_mu
  }
  
  # impute draw
  if (method == "draw") {
    imp <- purrr::map_dfr(1:dim(c_mu)[1], function(i) {
      MASS::mvrnorm(1, mu = as.numeric(c_mu[i,]), Sigma = c_var)
    })
    out[, miss] <- imp
  }
  
  # impute multiply
  if (method == "mult") {
    imp <- purrr::map_dfr(1:dim(c_mu)[1], function(i) {
      cbind(.m = 1:m,
            as.data.frame(MASS::mvrnorm(
              m, mu = as.numeric(c_mu[i,]), Sigma = c_var
            )))
    }) %>% split(~ .m)
    out <- purrr::map(1:m, ~ {
      out[[.x]][, miss] <- imp[[.x]][, -1]
      out[[.x]]
    })
  }
  
  # output
  return(out)
}
