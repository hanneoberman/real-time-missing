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
  lm(predictions[,.x] ~ predictions$truth)$coefficients %>% setNames(c("intercept", "slope"))
}) %>% cbind(meth)

res <- cbind(
  Method = meth, 
  RMSE = rmse, 
  Brier = brier, 
  `C-index` = auc, 
  CITL = cali$intercept, 
  Slope = cali$slope) %>% 
  as.data.frame()

save(res, file = "../CaseStudy/Results/resultstable.Rdata")
write.csv(res, file = "../CaseStudy/Results/resultstable.csv", row.names = FALSE)

# with logistic model, the pattern submodel approach works best
# with rf, the pattern submodel and surrogate split methods both perform well
# imputation methods perform less good on the real-world data -> distributional assumptions/MNAR?
# within the imp methods, the random draw from the distribution clearly underperforms compared to regression imp and multiple imp
# these results do not show a clear superiority for multiple imputation (link to other research!!)
# overall, the method that performs best on real-world data is surrogate splitting
# this method also has advantages in explainability, and does not require the miss mech to be equal across datasets
# a small disadvantage is that the method is very slow to train, but with increasing computational power, that shouldn't be a problem

