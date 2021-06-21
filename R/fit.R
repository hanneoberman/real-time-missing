# functions to fit different models on the complete data

# function to fit a logistic model and calculate the auc 
fit_true <- function(dataset) {
  # spline knots
  knots <- quantile(dataset$X10, p = c(0.25, 0.5, 0.75))
  # fit prediction model
  mod <- glm(Y ~ splines::bs(X10)+., family = "binomial", data = dataset)
  # # c index/auc
  # auc <-
  #   pROC::roc(Y ~ prob,
  #             data = cbind(dataset, prob = predict(mod)),
  #             quiet = TRUE)$auc
  # output
  return(list(
    mod = mod #,
    # auc = round(auc, 2)
  ))
}

# function to fit a box of submodels and calculate the auc 
fit_sub <- function(dataset, form) {
  # fit prediction models
  mod <- glm(form, family = "binomial", data = dataset)
  #mod_sub4 <- glm(Y ~ X1 + X2 + X3 + X4, family = "binomial", data = dat)
  #mod_sub2 <- glm(Y ~ X1 + X2, family = "binomial", data = dat)
  # c index/auc
  # auc <-
  #   pROC::roc(Y ~ prob,
  #             data = cbind(dataset, prob = predict(mod)),
  #             quiet = TRUE)$auc
  # output
  return(list(
    mod = mod #,
    # auc = round(auc, 2)
  ))
}

# function to fit a random forest and calculate the auc 
fit_rf_imp <- function(dataset) {
  # fit prediction model
  mod <- ranger::ranger(Y~., data = dataset, num.trees = 100, min.node.size = 10)
  # c index/auc
  # pred_dat <- predict(mod, data = dataset) %>% .$predictions %>% cbind(dataset, prob = .) 
  # auc <-
  #   pROC::roc(Y ~ prob,
  #             data = cbind(dataset, prob = predict(mod, data = dataset)[["predictions"]]),
  #             quiet = TRUE)$auc
  # output
  return(list(
    mod = mod #,
    # auc = round(auc, 2)
  ))
}

# function to fit a random forest and calculate the auc 
fit_rf_sub <- function(dataset) {
  # fit prediction model
  mod <- party::cforest(Y~., data=dataset)
  # c index/auc
  # pred_dat <- predict(mod, data = dataset) %>% .$predictions %>% cbind(dataset, prob = .) 
  # auc <-
  #   pROC::roc(Y ~ prob,
  #             data = cbind(dataset, prob = predict(mod, newdata = dataset)),
  #             quiet = TRUE)$auc
  # output
  return(list(
    mod = mod #,
    # auc = round(auc, 2)
  ))
}