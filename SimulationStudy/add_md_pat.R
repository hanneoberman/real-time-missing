# add missing data pattern to simulation output
results <- readRDS("C:/Users/4216318/Desktop/SIG/SimulationStudy/Results/results.RDS")
datasets <- readRDS("C:/Users/4216318/Desktop/SIG/SimulationStudy/Data/datasets.RDS")

results_md_pat <- 
  purrr::map(1:1000, function(.x){
  cbind(
    pmis = datasets[[.x]][["valset"]][["p_miss"]],
    results[[.x]]
  )
})
save(results_md_pat, file = "./results_md_pat.Rdata")

#########

a = split(results_md_pat[[1]], ~pmis)

cali <- purrr::map(results_md_pat, function(.i){
    split(results_md_pat[[.i]], ~pmis) |>
    purrr::map(., function(.p){
    purrr::map_dfr(meth_lab, ~{lm(.i$Y_prob ~ .i[ , .x])$coefficients %>% setNames(c("Intercept", "Slope"))}) %>% cbind(Method = meth_lab, .)
}) 
rmse <- purrr::map_dfr(results_md_pat, function(.i){purrr::map_dfc(.i[,-c(1:2)], ~{sqrt(mean((.x - .i$Y_prob)^2))})}) %>% 
  tidyr::pivot_longer(cols = everything(), names_to = "Method", values_to = "RMSE") #%>% 
brier <- purrr::map_dfr(results_md_pat, function(.i){purrr::map_dfc(.i[,-c(1:2)], ~{mean((.x - .i$Y_true)^2)})}) %>% 
  tidyr::pivot_longer(cols = everything(), names_to = "Method", values_to = "Brier") # %>% 
auc <- purrr::map_dfr(
  results_md_pat, function(.i){
    purrr::map_dfc(
      .i[,-c(1:2)], ~{
        pROC::roc(.i$Y_true, .x) %>% .$auc %>% as.numeric()})}) %>%
  tidyr::pivot_longer(cols = everything(), names_to = "Method", values_to = "AUC") #%>%
mae <- purrr::map_dfr(results_md_pat, function(.i){purrr::map_dfc(.i[,-c(1:2)], ~{abs(.x - .i$Y_prob) %>% mean()})}) %>% 
  tidyr::pivot_longer(cols = everything(), names_to = "Method", values_to = "MAE")


# combine and save
performance_md_pat <- cali %>% 
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
saveRDS(performance, file = "./Results/performance_md_pat.RDS")
