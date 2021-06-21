# function to generate data for 10 continuous predictors and 1 binary outcome
generate_sample <-
  function(sample_size,
           covariance_matrix,
           linear_bs,
           non_linear_bs,
           interaction = FALSE) {
    # generate observations for the predictors
    dat <-
      mvtnorm::rmvnorm(n = sample_size, sigma = covariance_matrix) 
    # compute the outcome based on linear function of predictors and error term
    betas <- linear_bs #runif(nrow(covariance_matrix), -10, 10)
    lin_pred <- -4.5 + 
      betas[2]*log(abs(dat[,2])) + 
      dat[,c(1,3:10)] %*% betas[c(1,3:10)] + 
      rnorm(sample_size, sd = 2) 
    if (interaction) {
      more_betas <- non_linear_bs
      lin_pred <- lin_pred + dat %*% more_betas * dat[, 1]
    }
    # convert to probabilities
    p <- 1 / (1 + exp(-lin_pred))
    # convert to binary outcome
    Y <- rbinom(n = length(p),
                size = 1,
                prob = p)
    dat <- dat %>% data.frame() %>% cbind(Y, .)
    return(dat)
  }

