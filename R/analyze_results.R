# preprocess results and compute performance

# packages
library(dplyr)

# labels
meth_lab <- c("CMI+FLR", "SDI+FLR", "MDI+FLR", "CMI+RF", "SDI+RF", "MDI+RF", "BOS+FLR", "BOS+RF", "SS+RF")
meth_ord <- c("CMI+FLR", "SDI+FLR", "MDI+FLR", "BOS+FLR", "CMI+RF", "SDI+RF", "MDI+RF", "BOS+RF", "SS+RF")
meth_col <- c("#1269b0", "#81c454") %>% setNames(c("FLR", "RF"))
miss_lab <- c("CMI", "SDI", "MDI", "BOS", "SS")

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
results <- readRDS("Results/results.RDS")
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
mae <- purrr::map_dfr(results, function(.i){purrr::map_dfc(.i[,-c(1:2)], ~{abs(.x - .i$Y_prob) %>% mean()})}) %>% 
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
    Strategy = factor(Strategy, levels = miss_lab, ordered = TRUE)) 
saveRDS(performance, file = "./Results/performance.RDS")
