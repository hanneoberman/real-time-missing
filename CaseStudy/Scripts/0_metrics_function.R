# utils function to compute performance measures
calculate_metrics <- function(Y_obs, Y_pred) {
  rmse <- sqrt(mean((Y_obs - Y_pred)^2))
  auc <- pROC::roc(Y_obs, Y_pred) %>% 
    .$auc %>% 
    as.numeric()
  brier <- mean((Y_obs - Y_pred)^2)
  mae <-   mean(abs(Y_obs - Y_pred))
  cali <- lm(Y_pred ~ Y_obs)$coefficients
  out <- data.frame(
    CITL = cali[1], 
    slope = cali[2], 
    rmse, 
    brier, 
    auc)
  row.names(out) <- NULL
  return(out)
}
  
# rmse <- purrr::map_dbl(meth, function(.x){
#   sqrt(mean((Y_obs - Y_pred)^2))
# }) %>% setNames(meth)
# 
# auc <- purrr::map_dbl(meth, function(.x){
#   pROC::roc(Y_obs, Y_pred) %>% 
#     .$auc %>% 
#     as.numeric()
# }) %>% setNames(meth)
# 
# brier <- purrr::map_dbl(meth, function(.x){
#   mean((Y_obs - Y_pred)^2)
# }) %>% setNames(meth)
# 
# mae <- purrr::map_dbl(meth, function(.x){
#   mean(abs(Y_obs - Y_pred))
# }) %>% setNames(meth)
# 
# cali <- purrr::map_dfr(meth, function(.x){
#   lm(Y_pred ~ Y_obs)$coefficients %>% setNames(c("intercept", "slope"))
# }) %>% cbind(meth, .)
# 
# res <- cbind(cali, rmse, brier, auc) %>% 
#   as.data.frame()
