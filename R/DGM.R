# functions to simulate multiple datasets according to the same data generating mechanism

# function to generate the variance-covariance matrix of the predictors (used once)
define_varcov <- function(p = 10, seed = 123) {
  set.seed(seed)
  betas <- rnorm(p * (p - 1) / 2, 0, 0.1)
  sigma <- diag(p)
  sigma[upper.tri(sigma)] <- betas
  sigma[lower.tri(sigma)]  <- t(sigma)[lower.tri(sigma)]
  Sigma <- t(sigma) %*% sigma
  # output
  # if (!isSymmetric(Sigma)) {
  #   stop("The variance-covariance matrix should be symmetic.")
  # }
  return(Sigma)
}

# function to generate data for 10 continuous predictors and 1 binary outcome (used in every simulation)
generate_sample <-
  function(sample_size, covariance_matrix, linear_bs, non_linear_bs, interaction = FALSE) {
    # generate observations for the predictors
    dat <-
      mvtnorm::rmvnorm(n = sample_size, mean = rep(0, nrow(covariance_matrix)), sigma = covariance_matrix, checkSymmetry = FALSE) 
    # compute the outcome based on linear function of predictors and error term
    betas <- linear_bs #runif(nrow(covariance_matrix), -10, 10)
    lin_pred <- -3 + 
      betas[2]*log(abs(dat[,2])) + 
      dat[,c(1,3:nrow(covariance_matrix))] %*% betas[c(1,3:nrow(covariance_matrix))] + 
      rnorm(sample_size, sd = 2) 
    if (interaction) {
      lin_pred <- lin_pred + dat %*% non_linear_bs * dat[, 1]
    }
    # convert to probabilities
    p <- 1 / (1 + exp(-lin_pred))
    # convert to binary outcome
    Y <- rbinom(n = length(p),
                size = 1,
                prob = p)
    dat <- dat %>% data.frame() %>% cbind(Y = Y, Y_prob = p, .)
    return(dat)
  }

# function to define the missingness, setting the parameters for mice::ampute()
define_miss <- function(p = 10){
  # define 3x4 types of MAR missngness to get a mixture of mechanisms
  MAR_types <- rep(c("LEFT", "MID", "TAIL", "RIGHT"), 3)
  # define missing data pattern for the predictor variables (p)
  pat <- matrix(1, 12, p) %>% 
    data.frame() 
  # four var missing
  pat[1:4,7:p] <- 0 
  # six var missing
  pat[5:8,5:p] <- 0 
  # eight var missing
  pat[9:12,3:p] <- 0
  # output
  return(list(miss_pat = cbind(Y=1, pat), miss_type = MAR_types))
}

create_miss <- function(dataset, param){
  dataset %>% 
    mice::ampute(mech = "MAR", prop = 0.999, patterns = param$miss_pat, type = param$miss_type) %>% 
    .$amp %>% 
    cbind(p_miss = rowSums(is.na(.)), .) %>% 
    filter(p_miss != 0)
}
