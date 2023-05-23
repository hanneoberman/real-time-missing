# case study

# setup env
set.seed(171)
library(dplyr)
library(purrr)

# load data
dat <-
  readRDS(
    "C:/Users/4216318/surfdrive/Documents/Student-assistentschap/MIMIC Data/mimic_cleaned.RDS"
  )
dat_clean <- cbind(Y = dat$hospital_expire_flag, dat[,-c(1, 11)])

# split train/test
ind <-
  sample(c(TRUE, FALSE),
         nrow(dat_clean),
         replace = TRUE,
         prob = c(0.7, 0.3))
train  <- dat_clean[ind,]
test   <- dat_clean[!ind,]

# run all methods except SS
means <- colMeans(train[,-1], na.rm = TRUE)
varcov <- cov(train[,-1], use = "pairwise.complete.obs")
# md <- mice::md.pattern(train)
# add md pattern to each row
R <- is.na(train)
nmis <- colSums(R)
R <- matrix(R[, order(nmis)], dim(train))
pat <- apply(R, 1, function(x)
  paste(as.numeric(x), collapse = ""))
train_pat <-
  split(train, ~ pat) %>% # %>% janitor::remove_empty(., which = "cols")
  purrr::map(., function(.x) {
    .x[,!apply(is.na(.x), 2, all)]
  })

# PS logistic model
log_mod <- purrr::map(train_pat, ~ {
  glm(Y ~ .,
      family = "binomial",
      data = .)
})

# PS random forest model
rf_mod <- purrr::map(train_pat, ~ {
  ranger::ranger(Y ~ ., data = .)
})

# TODO: think about number of patterns, model conv, runtime, sidenote patterns because of ps method, imp train set too?, test set patterns not in train set
# TODO: filter 20 most freq patterns

# predict(mod, newdata = test, type = "response")

## imputation methods
imputations <-
  impute_cond(
    vals = validation_set,
    dev_means = fitted_mod$mu,
    dev_cov = fitted_mod$sigma,
    m = n_imp
  )
# # conditional imputation with logistic model
# Y_pred_imp_log <- imputations %>% map( ~ {
#   select(.x,-p_miss,-draw) %>%
#     predict(fitted_mod$log$full, newdata = ., type = "response") %>%
#     matrix(ncol = nrow(validation_set)) %>% colMeans()
# })
# # conditional imputation with rf model
# Y_pred_imp_rf <- imputations %>% map( ~ {
#   select(.x,-p_miss,-draw) %>%
#     predict(fitted_mod$rf$full,
#             data = .,
#             type = "response") %>% .[["predictions"]] %>%
#     matrix(ncol = nrow(validation_set)) %>% colMeans()
# })
# # run surrogate split model
# sur_mod <- party::cforest(hospital_expire_flag ~ ., data = train)
