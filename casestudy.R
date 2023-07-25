# case study

# setup env
set.seed(171)
library(dplyr)
library(purrr)
library(mice)

# load data
dat <-
  readRDS(
    "C:/Users/4216318/surfdrive/Documents/Student-assistentschap/MIMIC Data/mimic_cleaned.RDS"
  )
dat_clean <- cbind(Y = dat$hospital_expire_flag, dat[,-c(1, 11)])

# # filter most common md patterns
# pat <- md.pattern(dat_clean)

# split train/test
ind <-
  sample(c(TRUE, FALSE),
         nrow(dat_clean),
         replace = TRUE,
         prob = c(0.7, 0.3))
train  <- dat_clean[ind,]
test   <- dat_clean[!ind,]

# save data
save(train, file = "./Case study/train_clean.Rdata")
save(test, file = "./Case study/test_clean.Rdata")

# filter out less frequent md patterns in training data
R <- is.na(train)
nmis <- colSums(R)
R <- matrix(R[, order(nmis)], dim(train))
pat <- apply(R, 1, function(x)
  paste(as.numeric(x), collapse = ""))
freq_pat <- names(sort(table(pat), decreasing = TRUE))[1:5]
train_filter <- cbind(pat, train) %>% .[.$pat %in% freq_pat, ]

# filter out less frequent md patterns in training data
R <- is.na(test)
nmis <- colSums(R)
R <- matrix(R[, order(nmis)], dim(test))
pat <- apply(R, 1, function(x)
  paste(as.numeric(x), collapse = ""))
test_filter <- cbind(pat, test) %>% .[.$pat %in% freq_pat, ]

# save data
save(train_filter, file = "./Case study/train_filter.Rdata")
save(test_filter, file = "./Case study/test_filter.Rdata")

# split training data by pattern and remove completely missing columns
train_pat <- split(train_filter, ~ pat) %>% 
  purrr::map(., function(.x) {
    .x[,!apply(is.na(.x), 2, all)]
  })

# PS logistic model
log_mod <- purrr::map(train_pat, ~ {
  glm(Y ~ .,
      family = "binomial",
      data = .x[, -1])
})
save(log_mod, file = "./Case study/mod_log.Rdata")

# PS random forest model
rf_mod <- purrr::map(train_pat, ~ {
  ranger::ranger(
    Y ~ ., 
    data = .x[, -1])
})
save(rf_mod, file = "./Case study/mod_rf.Rdata")

# TODO: think about number of patterns, model conv, runtime, sidenote patterns because of ps method, imp train set too?, test set patterns not in train set
# TODO: filter 20 most freq patterns
# note: the 10 most freqent patterns uncluded patterns with complete separation in the outcome (Y = 0 only), so just 5 patterns were selected

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
# 
# # run all methods except SS
# means <- colMeans(train[,-1], na.rm = TRUE)
# varcov <- cov(train[,-1], use = "pairwise.complete.obs")


# predict(mod, newdata = test, type = "response")

