# reference performance

# packages
library(dplyr)

# functions
reference_performance <- function(devset, valset){
  # flexible logistic regression
  mod_FLR <- devset %>% glm(Y ~ splines::ns(X1, df = 3) + . - X1,
                    family = "binomial",
                    data = .)
  perf <- valset %>% predict(mod_FLR, newdata = ., type = "response") %>% 
    cbind(valset[,1:2], FLR = .)
  cali_FLR <- perf %>% lm(Y_prob ~ FLR, data = .) %>% .$coefficients
  auc_FLR <- perf %>% pROC::roc(Y, FLR) %>% .$auc %>% as.numeric()
  rmse_FLR <- sqrt(mean((perf$FLR - perf$Y_prob)^2))
  brier_FLR <- mean((perf$FLR - perf$Y)^2) 
  mae_FLR <- mean(abs(perf$FLR - perf$Y_prob))
  # random forest
  mod_RF <- devset %>% ranger::ranger(Y ~ ., data = .)
  perf$RF <- valset %>% predict(mod_RF, data = ., type = "response") %>% .[["predictions"]] 
  cali_RF <- perf %>% lm(Y_prob ~ RF, data = .) %>% .$coefficients
  auc_RF <- perf %>% pROC::roc(Y, RF) %>% .$auc %>% as.numeric()
  rmse_RF <- sqrt(mean((perf$RF - perf$Y_prob)^2))
  brier_RF <- mean((perf$RF - perf$Y)^2) 
  mae_RF <- mean(abs(perf$RF - perf$Y_prob))
  # output
  out <- data.frame(
    method = c("FLR", "RF"),
    intercept = c(cali_FLR[1], cali_RF[1]),
    slope = c(cali_FLR[2], cali_RF[2]),
    auc = c(auc_FLR, auc_RF),
    rmse = c(rmse_FLR, rmse_RF),
    brier = c(brier_FLR, brier_RF),
    mae = c(mae_FLR, mae_RF)
    )
  return(out)
}

# reference performance
refsets <- readRDS("Data/datasets.RDS") %>% purrr::map("devset")
devsets <- refsets[1:100]
valsets <- refsets[101:300] %>% do.call(rbind.data.frame, .) %>% cbind(set = 1:100) %>% split(., .$set)
ref_perf <- purrr::map2_dfr(devsets, valsets, ~reference_performance(.x, .y))
saveRDS(ref_perf, file = "Results/reference_performance.RDS")
rm(devsets)
gc()