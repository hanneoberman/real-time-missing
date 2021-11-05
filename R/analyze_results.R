# preprocess results and compute performance

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

# labels
pred_lab <- paste0("X", 1:10)
pred_col <- c("#1269b0", "#7c1315") %>% setNames(c("Observed", "Missing"))
# meth <- c("Y_pred_mean_log", "Y_pred_sing_log", "Y_pred_mult_log", "Y_pred_mean_rf",  "Y_pred_sing_rf",  "Y_pred_mult_rf", "Y_pred_sub_log",  "Y_pred_sub_rf",   "Y_pred_sur" )
meth_lab <- c("CMI+FLR", "SDI+FLR", "MDI+FLR", "CMI+RF", "SDI+RF", "MDI+RF", "BOS+FLR", "BOS+RF", "SS+RF")
meth_ord <- c("CMI+FLR", "SDI+FLR", "MDI+FLR", "BOS+FLR", "CMI+RF", "SDI+RF", "MDI+RF", "BOS+RF", "SS+RF")
meth_col <- c("#1269b0", "#81c454") %>% setNames(c("FLR", "RF"))
miss_lab <- c("CMI", "SDI", "MDI", "BOS", "SS")

# dgm
dgm <- readRDS("Results/output.RDS") %>% .$DGM
# correlations
corr <- dgm$varcov %>% 
  cov2cor() %>% 
  cbind(pred_lab, .) %>% 
  as.data.frame() %>% 
  setNames(c("pred", pred_lab)) %>% 
  tidyr::pivot_longer(cols = everything()[-1]) %>% 
  mutate(value = as.numeric(value),
         text = round(value, 2))
corr[corr$pred==corr$name, "value"] <- NA
corr[c(11, 21, 22, 31:33, 41:44, 51:55, 61:66, 71:77, 81:88, 91:99), "value"] <- NA
corr[c(11, 21, 22, 31:33, 41:44, 51:55, 61:66, 71:77, 81:88, 91:99), "text"] <- NA
saveRDS(corr, "Data/correlations.RDS")

# coefficients
coef <- data.frame(
  value = dgm$betas, 
  pred = factor(pred_lab, levels = pred_lab), 
  type = c(rep("Y", 10), rep("Y*", 10))) %>% 
  mutate(value = value,
         text = round(value, 2))
saveRDS(coef, "Data/coefficients.RDS")

# missing data pattern
patt <- dgm$miss_pat[c(4,8,12), -c(1:2)] %>% 
  as.data.frame() %>% 
  cbind(row = 1:3, .) %>% 
  tidyr::pivot_longer(cols = everything()[-1]) %>% 
  mutate(value = factor(value, levels = c(0,1), labels = c("Missing", "Observed")))
saveRDS(patt, "Data/missing_data_pattern.RDS")

# reference performance
devsets <- readRDS("Data/datasets.RDS") %>% purrr::map("devset")
ref_perf <- devsets %>% purrr::map_dfr( ~reference_performance(.x))
saveRDS(ref_perf, file = "Results/reference_performance.RDS")
rm(devsets)
gc()

# simulation results
results <- readRDS("Results/output.RDS")$results
# add surrogate split results from hpc
results_sur <- purrr::map(1:1000, ~{readRDS(paste0("./Results/output_hpc/output_sur_it_", .x, ".RDS"))})
# combine and save
results <- purrr::map2(results, results_sur, ~{cbind(.x, Y_pred_sur = .y[[1]]$Y_pred_sur) %>% setNames(c("Y_true", "Y_prob", meth_lab))})
saveRDS(results, "Results/results.RDS")
rm(results_sur)
gc()

# calculate performance
readRDS("Results/results.RDS")
cali <- purrr::map_dfr(results, function(.i){
  purrr::map_dfr(meth_lab, ~{lm(.i$Y_prob ~ .i[ , .x])$coefficients %>% setNames(c("Intercept", "Slope"))}) %>% cbind(Method = meth_lab, .)
}) 
rmse <- purrr::map_dfr(results, function(.i){purrr::map_dfc(.i[,-c(1:2)], ~{sqrt(mean((.x - .i$Y_prob)^2))})}) %>% 
  tidyr::pivot_longer(cols = everything(), names_to = "Method", values_to = "RMSE") #%>% 
brier <- purrr::map_dfr(results, function(.i){purrr::map_dfc(.i[,-c(1:2)], ~{mean((.x - .i$Y_true)^2)})}) %>% 
  tidyr::pivot_longer(cols = everything(), names_to = "Method", values_to = "Brier") # %>% 
auc <- purrr::map_dfr(
  results, function(.i){
    purrr::map_dfc(
      .i[,-c(1:2)], ~{
        pROC::roc(.i$Y_true, .x) %>% .$auc %>% as.numeric()})}) %>%
  tidyr::pivot_longer(cols = everything(), names_to = "Method", values_to = "AUC") #%>%
mae <- purrr::map_dfr(results, function(.i){purrr::map_dfc(.i[,-c(1:2)], ~{(mean(abs(.x - .i$Y_prob)))})}) %>% 
  tidyr::pivot_longer(cols = everything(), names_to = "Method", values_to = "MAE")
  
# combine and save
performance <- cali %>% 
  cbind(AUC = auc$AUC) %>% 
  cbind(RMSE = rmse$RMSE) %>% 
  cbind(Brier = brier$Brier) %>% 
  cbind(MAE = mae$MAE) %>% 
  mutate(
    Method = factor(Method, levels = meth_ord, ordered = TRUE),
    Model = case_when(stringr::str_detect(Method, "FLR")~"FLR", TRUE~"RF"),
    Strategy = stringr::str_remove(Method, "[^+]*$"),
    Strategy = stringr::str_remove(Strategy, "[+].*"),
    Strategy = factor(Strategy, levels = miss_lab, ordered = TRUE)) # %>% 
  # rbind(data.frame(
  #   Method = "SS+FLR", 
  #   Intercept = NA, 
  #   Slope = NA, 
  #   AUC = NA, 
  #   RMSE = NA, 
  #   Brier = NA,
  #   Model = "FLR",
  #   Strategy = "SS")) 
saveRDS(performance, file = "./Results/performance.RDS")
