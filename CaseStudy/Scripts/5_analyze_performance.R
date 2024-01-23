# analyze the predictions

# setup env
set.seed(174)
library(dplyr)
library(purrr)
library(mice)
library(ggplot2)
source("../CaseStudy/Scripts/0_metrics_function.R")

#####################

load("../CaseStudy/Results/predictions.Rdata")
long <- tidyr::pivot_longer(predictions, cols = names(predictions)[-c(1,2)])
ggplot(long, aes(value, truth, color = as.factor(truth))) +
  geom_jitter(width = 0, height = 0.05, alpha = 0.1) + 
  geom_smooth(se = FALSE, color = "black") +
  facet_wrap(~ name) +
  theme_classic()

meth <- names(predictions)[-c(1,2)]
results <- purrr::map_dfr(meth, ~{
  calculate_metrics(predictions$truth, predictions[, .x]) 
}) %>% cbind(method = meth, .)

save(results, file = "../CaseStudy/Results/results.Rdata")
write.csv(results, file = "../CaseStudy/Results/results.csv")

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