# function to generate data for 10 continuous predictors and 1 binary outcome
generate_sample <-
  function(sample_size,
           covariance_matrix,
           interaction = FALSE) {
    # generate observations for the predictors
    dat <-
      mvtnorm::rmvnorm(n = sample_size, sigma = covariance_matrix)
    # compute the outcome based on linear function of predictors and error term
    betas <- runif(nrow(covariance_matrix), -10, 10)
    lin_pred <- dat %*% betas + rnorm(sample_size, sd = 35)
    if (interaction) {
      more_betas <- runif(10, -1, 1)
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

# function to check the prevalence of the outcome variable and the auc of the prediction model
check_characteristics <- function(dataset) {
  # prevalence of the outcome
  prevalence <- mean(dataset$Y)
  # fit prediction model
  mod <- glm(Y ~ ., family = "binomial", data = dataset)
  # c index/auc
  auc <-
    pROC::roc(Y ~ prob,
              data = cbind(dat, prob = predict(mod)),
              quiet = TRUE)$auc
  # output
  return(data.frame(
    prevalence = round(prevalence, 2),
    auc = round(auc, 2)
  ))
}
