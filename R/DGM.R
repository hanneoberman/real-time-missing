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
    lin_pred <- 0 + 
      betas[2]*log(abs(dat[,2])) + 
      dat[,c(1,3:10)] %*% betas[c(1,3:10)] + 
      rnorm(sample_size, sd = 33)
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

# function to fit a logistic model and calculate the auc 
fit_logistic <- function(dataset) {
  # # prevalence of the outcome
  # prevalence <- mean(dataset$Y)
  # fit prediction model
  mod <- glm(Y ~ ., family = "binomial", data = dataset)
  # c index/auc
  auc <-
    pROC::roc(Y ~ prob,
              data = cbind(dataset, prob = predict(mod)),
              quiet = TRUE)$auc
  # output
  return(list(
    mod = mod,
    auc = round(auc, 2)
  ))
}


# function to fit a random forest and calculate the auc 
fit_rf <- function(dataset) {
  # # prevalence of the outcome
  # prevalence <- mean(dataset$Y)
  # fit prediction model
  mod <- party::cforest(Y~., data = dataset)
  # c index/auc
  pred_dat <- predict(mod) %>%  as.vector() %>% cbind(dataset, prob = .) #setNames(c(names(.)[-ncol(.)], "prob"))
  auc <-
    pROC::roc(Y ~ prob,
              data = pred_dat,
              quiet = TRUE)$auc
  # output
  return(list(
    mod = mod,
    auc = round(auc, 2)
  ))
}
