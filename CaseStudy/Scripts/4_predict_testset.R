# fit models on test set

# setup env
set.seed(173)
library(dplyr)
library(purrr)
library(mice)
library(ggplot2)
library(ranger)
source("../CaseStudy/Scripts/0_imputation_function.R")

# load test set data
load("../CaseStudy/Data/test_filter.Rdata")
# split test data by pattern and remove completely missing columns
test_pat <- split(test_filter, ~ pat, drop = TRUE)
# extract true Y
Y <- do.call("rbind", test_pat)$Y
pats <- do.call("rbind", test_pat)$pat

#####################

# pattern submodels

# PS logistic model
load("../CaseStudy/Data/mod_log.Rdata")

# fit PS logistic
log_fit <- purrr::map2(log_mod, test_pat, ~{
  .y[, !apply(is.na(.y), 2, all)] %>% 
    .[, -c(1, 2), drop = FALSE] %>% 
    predict(.x, newdata = ., type = "response")
})

# PS random forest model
load("../CaseStudy/Data/mod_rf.Rdata")

# fit PS rf
rf_fit <- purrr::map2(rf_mod, test_pat, ~{
  .y[, !apply(is.na(.y), 2, all)] %>% 
    .[, -c(1, 2), drop = FALSE] %>% 
    predict(.x, data = ., type = "response") %>% 
    .$predictions
})

####################

# surrogate split method
load("../CaseStudy/Data/mod_sur.Rdata")

# fit surrogate split model
sur_fit <- predict(sur_mod, newdata = do.call("rbind", test_pat)[, -c(1:2)])
save(sur_fit, file = "../CaseStudy/Data/fit_sur.Rdata")

######################

## imputation methods

load("../CaseStudy/Data/mod_imp_means.Rdata")
load("../CaseStudy/Data/mod_imp_varcov.Rdata")

# for each md pattern impute with regression imputation
imp_mean <- purrr::map_dfr(test_pat, ~ {
  # impute 
  impute(.x, imp_mod_means, imp_mod_varcov, method = "norm")
})

# for each md pattern impute random draw
imp_draw <- purrr::map_dfr(test_pat, ~ {
  # impute 
  impute(.x, imp_mod_means, imp_mod_varcov, method = "draw")
})

# for each md pattern multiply impute
imp_mult <- purrr::map_dfr(test_pat, ~ {
  # impute 
  impute(.x, imp_mod_means, imp_mod_varcov, method = "mult")
}) %>% split( ~ .m)

# predict with logistic model
log_mean <- predict(log_mod[[1]], newdata = imp_mean, type = "response")
log_draw <- predict(log_mod[[1]], newdata = imp_draw, type = "response")
log_mult <- purrr::map_dfc(imp_mult, ~{
  data.frame(pred = predict(log_mod[[1]], newdata = .x[, -1], type = "response"))
}) %>% rowMeans()

# predict with rf
rf_mean <- predict(rf_mod[[1]], data = imp_mean, type = "response")$predictions
rf_draw <- predict(rf_mod[[1]], data = imp_draw, type = "response")$predictions
rf_mult <- purrr::map_dfc(imp_mult, ~{
  data.frame(pred = predict(rf_mod[[1]], data = .x, type = "response")$predictions)
}) %>% rowMeans()

############################

# combine results
predictions <- data.frame(
  pattern = pats,
  truth = Y,
  log_ps = stack(log_fit)$values,
  log_mean = c(log_mean),
  log_draw = c(log_draw),
  log_mult = c(log_mult),
  rf_ps = stack(rf_fit)$values,
  rf_mean = c(rf_mean),
  rf_draw = c(rf_draw),
  rf_mult = c(rf_mult),
  rf_sur = c(sur_fit)
)

save(predictions, file = "../CaseStudy/Results/predictions.Rdata")
