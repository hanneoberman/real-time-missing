# split train and test set

###################

# setup env
set.seed(171)
library(dplyr)
library(purrr)
library(mice)
library(ggplot2)

# load data
dat <-
  readRDS(
    "C:/Users/4216318/surfdrive/Documents/_onderzoek/Student-assistentschap/MIMIC Data/mimic_cleaned.RDS"
  )
# dat <- readRDS("C:/Users/4216318/surfdrive - Oberman, H.I. (Hanne)@surfdrive.surf.nl/Documents/_onderzoek/Student-assistentschap/MIMIC Data/mimic_cleaned.RDS")
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
save(train, file = "../CaseStudy/Data/train_clean.Rdata")
save(test, file = "../CaseStudy/Data/test_clean.Rdata")

# filter out less frequent md patterns in training data
R <- is.na(train)
nmis <- colSums(R)
R <- matrix(R[, order(nmis)], dim(train))
pat <- apply(R, 1, function(x)
  paste(as.numeric(x), collapse = ""))
pat_freq <- names(sort(table(pat), decreasing = TRUE))[1:20]
train_freq <- cbind(pat, train) %>% .[.$pat %in% pat_freq, ]
pat_filt <- train_freq %>% 
  group_by(pat) %>% 
  summarise(Y_levels = sum(unique(Y))) %>% 
  filter(Y_levels > 0) %>% 
  .$pat
train_filter <- train_freq[train_freq$pat %in% pat_filt, ]

# filter out less frequent md patterns in training data
R <- is.na(test)
nmis <- colSums(R)
R <- matrix(R[, order(nmis)], dim(test))
pat <- apply(R, 1, function(x)
  paste(as.numeric(x), collapse = ""))
test_filter <- cbind(pat, test) %>% .[.$pat %in% pat_filt, ]

# save data
save(train_filter, file = "../CaseStudy/Data/train_filter.Rdata")
save(test_filter, file = "../CaseStudy/Data/test_filter.Rdata")

# visualize md patterns
ggmice::plot_pattern(train_filter)

# ########################
# 
# # pattern submodels
# 
# # PS logistic model
# log_mod <- purrr::map(train_pat, function(.x) {
#   .x[, !apply(is.na(.x), 2, all)] %>% 
#     .[, -1] %>% 
#   glm(Y ~ .,
#       family = "binomial",
#       data = .)
# })
# save(log_mod, file = "../CaseStudy/Data/mod_log.Rdata")
# 
# # fit PS logistic
# log_fit <- purrr::map2(log_mod, test_pat, ~{
#   .y[, !apply(is.na(.y), 2, all)] %>% 
#     .[, -c(1, 2)] %>% 
#   predict(.x, newdata = ., type = "response")
# })
# 
# # PS random forest model
# rf_mod <- purrr::map(train_pat, ~ {
#   .x[, !apply(is.na(.x), 2, all)] %>% 
#     .[, -1] %>% 
#     ranger::ranger(
#     Y ~ ., 
#     data = .)
# })
# save(rf_mod, file = "./Case study/mod_rf.Rdata")
# 
# # fit PS rf
# rf_fit <- purrr::map2(rf_mod, test_pat, ~{
#   .y[, !apply(is.na(.y), 2, all)] %>% 
#     .[, -c(1, 2)] %>% 
#   predict(.x, data = ., type = "response") %>% 
#     .$predictions
# })
# 
# ####################
# 
# # surrogate split method
# 
# sur_mod <- party::cforest(Y ~ ., data = train_filter[, -1])
# save(sur_mod, file = "./Case study/mod_sur.Rdata")
# saveRDS(sur_mod, "./Case study/mod_sur.RDS")
# 
# # fit surrogate split model
# sur_fit <- predict(sur_mod, newdata = do.call("rbind", test_pat)[, -c(1:2)])
# save(sur_fit, file = "./Case study/fit_sur.Rdata")
# 
# ######################
# 
# ## imputation methods
# 
# # extract means and covariances
# means <- colMeans(train_filter[,-c(1:2)], na.rm = TRUE)
# varcov <- cov(train_filter[,-c(1:2)], use = "pairwise.complete.obs")
# 
# # for each md pattern
# imp_mean <- purrr::map_dfr(test_pat, ~ {
#   # impute 
#   impute(.x, means, varcov, method = "norm")
# })
# 
# # for each md pattern
# imp_draw <- purrr::map_dfr(test_pat, ~ {
#   # impute 
#   impute(.x, means, varcov, method = "draw")
# })
# 
# # for each md pattern
# imp_mult <- purrr::map_dfr(test_pat, ~ {
#   # impute 
#   impute(.x, means, varcov, method = "mult")
# }) %>% split( ~ .m)
# 
# # predict with logistic model
# log_mean <- predict(log_mod[[1]], newdata = imp_mean, type = "response")
# log_draw <- predict(log_mod[[1]], newdata = imp_draw, type = "response")
# log_mult <- purrr::map_dfc(imp_mult, ~{
#   data.frame(pred = predict(log_mod[[1]], newdata = .x[, -1], type = "response"))
#   }) %>% rowMeans()
# 
# # predict with rf
# rf_mean <- predict(rf_mod[[1]], data = imp_mean, type = "response")$predictions
# rf_draw <- predict(rf_mod[[1]], data = imp_draw, type = "response")$predictions
# rf_mult <- purrr::map_dfc(imp_mult, ~{
#   data.frame(pred = predict(rf_mod[[1]], data = .x, type = "response")$predictions)
# }) %>% rowMeans()
# 
# predictions <- data.frame(
#   truth = Y,
#   log_ps = stack(log_fit)$values,
#   log_mean = c(log_mean),
#   log_draw = c(log_draw),
#   log_mult = c(log_mult),
#   rf_ps = stack(rf_fit)$values,
#   rf_mean = c(rf_mean),
#   rf_draw = c(rf_draw),
#   rf_mult = c(rf_mult),
#   rf_sur = c(sur_fit)
# )
# 
# save(predictions, file = "./Case study/predictions.Rdata")
# 
# ######
# load("./Case study/predictions.Rdata")
# long <- tidyr::pivot_longer(predictions, cols = names(predictions)[-1])
# ggplot(long, aes(value, truth, color = truth)) +
#   geom_jitter(width = 0, height = 0.05, alpha = 0.1) + 
#   geom_smooth(se = FALSE) +
#   facet_wrap(~ name)
# 
# meth <- names(predictions)[-1]
# rmse <- purrr::map_dbl(meth, function(.x){
#   sqrt(mean((predictions$truth - predictions[, .x])^2))
# }) %>% setNames(meth)
# 
# auc <- purrr::map_dbl(meth, function(.x){
#   pROC::roc(predictions$truth, predictions[, .x]) %>% 
#     .$auc %>% 
#     as.numeric()
# }) %>% setNames(meth)
# 
# brier <- purrr::map_dbl(meth, function(.x){
#  mean((predictions$truth - predictions[, .x])^2)
# }) %>% setNames(meth)
# 
# mae <- purrr::map_dbl(meth, function(.x){
#   mean(abs(predictions$truth - predictions[, .x]))
# }) %>% setNames(meth)
# 
# cali <- purrr::map_dfr(meth, function(.x){
#   lm(predictions[,.x] ~ predictions$truth)$coefficients %>% setNames(c("Intercept", "Slope"))
# }) %>% cbind(meth)
# 
# res <- cbind(rmse, brier, auc, cali) %>% 
#   as.data.frame()

# rmse <- purrr::map_dfr(results, function(.i){purrr::map_dfc(.i[,-c(1:2)], ~{sqrt(mean((.x - .i$Y_prob)^2))})}) %>% 
#   tidyr::pivot_longer(cols = everything(), names_to = "Method", values_to = "RMSE") #%>% 
# brier <- purrr::map_dfr(results, function(.i){purrr::map_dfc(.i[,-c(1:2)], ~{mean((.x - .i$Y_true)^2)})}) %>% 
#   tidyr::pivot_longer(cols = everything(), names_to = "Method", values_to = "Brier") # %>% 
# auc <- purrr::map_dfr(
#   results, function(.i){
#     purrr::map_dfc(
#       .i[,-c(1:2)], ~{
#         pROC::roc(.i$Y_true, .x) %>% .$auc %>% as.numeric()})}) %>%
#   tidyr::pivot_longer(cols = everything(), names_to = "Method", values_to = "AUC") #%>%
# mae <- purrr::map_dfr(results, function(.i){purrr::map_dfc(.i[,-c(1:2)], ~{abs(.x - .i$Y_prob) %>% mean()})}) %>% 
#   tidyr::pivot_longer(cols = everything(), names_to = "Method", values_to = "MAE")
#cali <- purrr::map_dfr(results, function(.i){
#purrr::map_dfr(meth_lab, ~{lm(.i$Y_prob ~ .i[ , .x])$coefficients %>% setNames(c("Intercept", "Slope"))}) %>% cbind(Method = meth_lab, .)
#}) 


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

######################

# TODO: think about number of patterns, model conv, runtime, sidenote patterns because of ps method, imp train set too?, test set patterns not in train set
# TODO: filter 20 most freq patterns
# note: the 10 most frequent patterns included patterns with complete separation in the outcome (Y = 0 only), so just 5 patterns were selected

#####

# md_plot + scale_fill_manual(values=c("#1269b0", "#7c1315"))

