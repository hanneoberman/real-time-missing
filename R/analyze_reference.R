# reference performance

# packages
library(dplyr)

# functions
reference_performance <- function(devset){
  # flexible logistic regression
  perf_FLR <- devset %>%
    cbind(FLR = glm(Y ~ splines::ns(X1, df = 3) + . - X1,
                    family = "binomial",
                    data = .) %>% .$fitted.values)
  cali_FLR <- perf_FLR %>% lm(Y ~ FLR, data = .) %>% .$coefficients
  auc_FLR <- perf_FLR %>% pROC::roc(Y, FLR) %>% .$auc %>% as.numeric()
  # random forest
  perf_RF <- devset %>%
    cbind(RF = ranger::ranger(Y ~ ., data = .) %>% .$predictions)
  auc_RF <- perf_RF %>% pROC::roc(Y, RF) %>% .$auc %>% as.numeric()
  cali_RF <- perf_RF %>% lm(Y ~ RF, data = .) %>% .$coefficients
  out <- data.frame(
    method = c("FLR", "RF"),
    intercept = c(cali_FLR[1], cali_RF[1]),
    slope = c(cali_FLR[2], cali_RF[2]),
    auc = c(auc_FLR, auc_RF))
  return(out)
}

# reference performance
devsets <- readRDS("Data/datasets.RDS") %>% purrr::map("devset")
ref_perf <- devsets %>% purrr::map_dfr(~reference_performance(.x))
saveRDS(ref_perf, file = "Results/reference_performance.RDS")
rm(devsets)
gc()