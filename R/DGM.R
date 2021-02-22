# function to generate data for 4 predictors
sample_predictors <- function(size) {
  # variance-covariance matrix
  S <- matrix(
    c(1, 0, 0.1, 0.1,
      0, 1, 0.3, 0.2,
      0.1, 0.3, 1, 0.5,
      0.1, 0.2, 0.5, 1),
    nrow = 4,
    byrow = TRUE
  )
  # generate observations
  dat <- rmvnorm(n = size, sigma = S) %>% data.frame()
  # convert some predictors to binary (see function below)
  dat <- dat %>% mutate(X1 = get_binary(X1),
                        X2 = get_binary(X2))
  return(dat)
}

# compute outcome from predictors
compute_outcome <- function(preds) {
  # compute observations for outcome based on linear function of predictors and error term 
  xb <- -2 + 5 * preds[["X1"]] + 2 * preds[["X2"]] - 0.5 * preds[["X3"]] + 0.1 * preds[["X4"]] + rnorm(n=nrow(preds),sd = 10)
  # convert to binary
  y <- get_binary(xb)
  return(y)
}

# UTILS: transform continuous into binary data
get_binary <- function(vec) {
  # convert to probabilities
  p <- 1 / (1 + exp(-vec))
  # convert to binary
  y <- rbinom(n = length(p),
              size = 1,
              prob = p)
  return(y)
}
