# analyze the predictions

# setup env
set.seed(174)
library(dplyr)
library(purrr)
library(mice)
library(ggplot2)
source("../CaseStudy/Scripts/0_metrics_function.R")

#####################

# calibration plot
load("../CaseStudy/Results/predictions.Rdata")
long <-
  tidyr::pivot_longer(
    predictions,
    cols = names(predictions)[-c(1, 2)],
    names_to = "method",
    values_to = "prediction"
  ) |> 
  mutate(
       model = case_when(
         stringr::str_detect(method, "log")~"FLR", 
         TRUE~"RF"),
       strategy = case_when(
         stringr::str_detect(method, "mean")~"CMI",
         stringr::str_detect(method, "draw")~"SDI",
         stringr::str_detect(method, "mult")~"MDI",
         stringr::str_detect(method, "ps")~"BOS",
         TRUE~"SS"),
       strategy = factor(strategy, levels = c("CMI", "SDI", "MDI", "BOS", "SS"), ordered = TRUE)
       )
meth_col <- c("#1269b0", "#81c454") %>% setNames(c("FLR", "RF"))
ggplot(long, aes(prediction, truth, color = model)) +
  # geom_jitter(width = 0, height = 0.05, alpha = 0.01) + 
  geom_smooth(method = "gam", formula = y ~s(x, bs = "cs")) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
  facet_grid(strategy~model) +
  scale_color_manual(values = meth_col, guide = "none") +
  lims(x = c(0,1), y = c(0,1)) +
  theme_classic()

# performance measures
meth <- names(predictions)[-c(1,2)]
results <- purrr::map_dfr(meth, ~{
  calculate_metrics(predictions$truth, predictions[, .x]) 
}) %>% cbind(method = meth, .)

ggplot(results, aes(x =  method, y = CITL)) +
  geom_point() +
  theme_classic()
ggplot(results, aes(x =  method, y = slope)) +
  geom_point() +
  theme_classic()
ggplot(results, aes(x =  method, y = rmse)) +
  geom_point() +
  theme_classic()
ggplot(results, aes(x =  method, y = brier)) +
  geom_point() +
  theme_classic()
ggplot(results, aes(x =  method, y = auc)) +
  geom_point() +
  theme_classic()

# save(results, file = "../CaseStudy/Results/results.Rdata")
# write.csv(results, file = "../CaseStudy/Results/results.csv", row.names = FALSE)

# with logistic model, the pattern submodel approach works best
# with rf, the pattern submodel and surrogate split methods both perform well
# imputation methods perform less good on the real-world data -> distributional assumptions/MNAR?
# within the imp methods, the random draw from the distribution clearly underperforms compared to regression imp and multiple imp
# these results do not show a clear superiority for multiple imputation (link to other research!!)
# overall, the method that performs best on real-world data is surrogate splitting
# this method also has advantages in explainability, and does not require the miss mech to be equal across datasets
# a small disadvantage is that the method is very slow to train, but with increasing computational power, that shouldn't be a problem

# predicted values for imp methods should indeed be equal for complete cases
# performance for sur sp;lit method is better, also for complete cases???

# test complete cases only
cc <- predictions[predictions$pattern == "0000000000", ]
purrr::map_dfr(meth, ~{
  calculate_metrics(cc$truth, cc[, .x]) 
}) %>% cbind(method = meth, .)
## in the complete cases, sur method performs the worst of the rf models

# same but for each pattern separately
results_per_pattern <- purrr::map_dfr(split(predictions, ~pattern), function(.x){
  purrr::map_dfr(meth, function(.y){
  calculate_metrics(.x$truth, .x[, .y]) 
}) %>% cbind(method = meth, .)
}) %>% cbind(pattern = rep(unique(predictions$pattern), each = 9), .) 

# save(results_per_pattern, file = "../CaseStudy/Results/results_per_pattern.Rdata")
# write.csv(results_per_pattern, file = "../CaseStudy/Results/results_per_pattern.csv", row.names = FALSE)
# the more missingness, the better sur split works!

ggplot(results_per_pattern, aes(x =  method, y = CITL, color = pattern, group = pattern)) +
  geom_point() +
  geom_line(linetype = "dashed", alpha = 0.2) +
  theme_classic()
ggplot(results_per_pattern, aes(x =  method, y = slope)) +
  geom_point() +
  theme_classic()
ggplot(results_per_pattern, aes(x =  method, y = rmse)) +
  geom_point() +
  theme_classic()
ggplot(results_per_pattern, aes(x =  method, y = brier)) +
  geom_point() +
  theme_classic()
ggplot(results_per_pattern, aes(x =  method, y = auc)) +
  geom_point() +
  theme_classic()

# convert to missingness proportion
mis_prop <- results_per_pattern$pattern %>% 
  strsplit("") %>% 
  purrr::map_dbl(., ~{mean(as.numeric(.x))})
mod_type <- stringr::str_detect(results_per_pattern$method, "log")
results_per_pattern <- cbind(mis_prop, results_per_pattern)  
ggplot(results_per_pattern, aes(x = mis_prop, y = rmse, color = method, group = method)) +
  geom_point() +
  geom_line(linetype = "dashed", alpha = 0.5) +
  theme_classic()

             