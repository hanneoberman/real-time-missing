# functions to fit different models on the complete data

# function to fit a logistic model 
fit_true <- function(dataset) {
  # spline knots
  knots <- quantile(dataset$X10, p = c(0.25, 0.5, 0.75))
  # fit prediction model
  mod <- suppressWarnings(glm(Y ~ splines::bs(X10)+., family = "binomial", data = dataset))
  # output
  return(list(
    mod = mod 
  ))
}

# function to fit a box of submodels 
fit_sub <- function(dataset, form) {
  # fit prediction models
  mod <- glm(form, family = "binomial", data = dataset)
  # output
  return(list(
    mod = mod
  ))
}

# function to fit a random forest  
fit_rf_imp <- function(dataset) {
  # fit prediction model
  mod <- ranger::ranger(Y~., data = dataset, num.trees = 100, min.node.size = 10)
  # output
  return(list(
    mod = mod 
  ))
}

# function to fit a random forest  
fit_rf_sub <- function(dataset) {
  # fit prediction model
  mod <- party::cforest(Y~., data=dataset)
  # output
  return(list(
    mod = mod 
  ))
}