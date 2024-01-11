# analyze the predictions

# setup env
set.seed(174)
library(dplyr)
library(purrr)
library(mice)
library(ggplot2)

#####################

load("../CaseStudy/Results/predictions.Rdata")
long <- tidyr::pivot_longer(predictions, cols = names(predictions)[-1])
ggplot(long, aes(value, truth, color = as.factor(truth))) +
  geom_jitter(width = 0, height = 0.05, alpha = 0.1) + 
  geom_smooth(se = FALSE, color = "black") +
  facet_wrap(~ name) +
  theme_classic()

meth <- names(predictions)[-1]
rmse <- purrr::map_dbl(meth, function(.x){
  sqrt(mean((predictions$truth - predictions[, .x])^2))
}) %>% setNames(meth)

auc <- purrr::map_dbl(meth, function(.x){
  pROC::roc(predictions$truth, predictions[, .x]) %>% 
    .$auc %>% 
    as.numeric()
}) %>% setNames(meth)

brier <- purrr::map_dbl(meth, function(.x){
  mean((predictions$truth - predictions[, .x])^2)
}) %>% setNames(meth)

mae <- purrr::map_dbl(meth, function(.x){
  mean(abs(predictions$truth - predictions[, .x]))
}) %>% setNames(meth)

cali <- purrr::map_dfr(meth, function(.x){
  lm(predictions[,.x] ~ predictions$truth)$coefficients %>% setNames(c("Intercept", "Slope"))
}) %>% cbind(meth)

res <- cbind(rmse, brier, auc, cali) %>% 
  as.data.frame()
