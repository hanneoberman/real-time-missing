# functions to fit different models on the complete data

# fit the prediction models on the devset
fit_mod <- function(development_set, n_predictors = 10){
  # save the means and var-cov matrix for imputation
  means <- colMeans(development_set[, -c(1,2)])
  varcov <- cov(development_set[, -c(1,2)])
  # calculate the number of observed predictors
  p_obs <- list(2:(n_predictors+2-4), 2:(n_predictors+2-6), 2:(n_predictors+2-8))
  # logistic model with splines
  log_mod <- glm(Y ~ rms::rcs(X10, 5) + ., family = "binomial", data = development_set[, -1])
  # random forest model
  rf_mod <- ranger::ranger(Y~., data = development_set[, -1], num.trees = 100, min.node.size = 10)
  # surrogate split model
  sur_mod <- party::cforest(Y~., data=development_set[, -1])
  # box of submodels
  sub_mod <- purrr::map(p_obs, ~{
    development_set[, .x] %>% 
      glm(Y ~ ., family = "binomial", data = .)}) %>% setNames(c("miss_4", "miss_6", "miss_8"))
  # output
  return(list(
    mu = means,
    sigma = varcov,
    log = log_mod,
    rf = rf_mod,
    sur = sur_mod,
    sub = sub_mod
  ))
}

# # function to fit a logistic model 
# fit_log <- function(dataset) {
#   # spline knots
#   knots <- quantile(dataset$X10, p = c(0.25, 0.5, 0.75))
#   # fit prediction model
#   mod <- suppressWarnings(glm(Y ~ splines::ns(X10)+., family = "binomial", data = dataset))
#   # output
#   return(list(
#     log = mod 
#   ))
# }
# 
# # function to fit a box of submodels 
# fit_sub <- function(dataset, form) {
#   # fit prediction models
#   mod <- glm(form, family = "binomial", data = dataset)
#   # output
#   return(list(
#     sub = mod
#   ))
# }
# 
# # function to fit a random forest  
# fit_rf_imp <- function(dataset) {
#   # fit prediction model
#   mod <- ranger::ranger(Y~., data = dataset, num.trees = 100, min.node.size = 10)
#   # output
#   return(list(
#     rf = mod 
#   ))
# }
# 
# # function to fit a random forest  
# fit_rf_sur <- function(dataset) {
#   # fit prediction model
#   mod <- party::cforest(Y~., data=dataset)
#   # mod <- moreparty::fastcforest(Y~., data=dat)
#   # output
#   return(list(
#     sur = mod 
#   ))
# }