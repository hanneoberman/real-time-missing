# fit models on training set

# setup env
set.seed(172)
library(dplyr)
library(purrr)
library(mice)
library(ggplot2)

# load training set
load("../CaseStudy/Data/train_filter.Rdata")

# split training data by pattern and remove completely missing columns
train_pat <- split(train_filter, ~ pat) 

#####################

# pattern submodels

# PS logistic model
log_mod <- purrr::map(train_pat, function(.x) {
  .x[, !apply(is.na(.x), 2, all)] %>% 
    .[, -1] %>% 
    glm(Y ~ .,
        family = "binomial",
        data = .)
})
save(log_mod, file = "../CaseStudy/Data/mod_log.Rdata")

# PS random forest model
rf_mod <- purrr::map(train_pat, ~ {
  .x[, !apply(is.na(.x), 2, all)] %>% 
    .[, -1] %>% 
    ranger::ranger(
      Y ~ ., 
      data = .)
})
save(rf_mod, file = "../CaseStudy/Data/mod_rf.Rdata")

######################

# surrogate split method

sur_mod <- party::cforest(Y ~ ., data = train_filter[, -1])
save(sur_mod, file = "../CaseStudy/Data/mod_sur.Rdata")
# saveRDS(sur_mod, "../CaseStudy/Data/mod_sur.RDS")

######################

## imputation methods

# extract means and covariances
imp_mod_means <- colMeans(train_filter[,-c(1:2)], na.rm = TRUE)
imp_mod_varcov <- cov(train_filter[,-c(1:2)], use = "pairwise.complete.obs")

save(imp_mod_means, file = "../CaseStudy/Data/mod_imp_means.Rdata")
save(imp_mod_varcov, file = "../CaseStudy/Data/mod_imp_varcov.Rdata")


#####################