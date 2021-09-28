# functions to predict the outcome variable from the incomplete data

pred_Y <- function(validation_set, fitted_mod, n_imp){
  # split off and save Y for revaluation
  Y_true <- validation_set$Y
  Y_prob <- validation_set$Y_prob
  validation_set <- validation_set[, -c(2:3)] %>% cbind(id = 1:nrow(.), .)
  # impute the data
  imputations <- impute_cond(vals = validation_set, dev_means = mod$mu, dev_cov = mod$sigma, m = n_imp)
  # conditional imputation with logistic model
  Y_pred_log <- imputations %>% map(~{
    select(.x, -p_miss, -draw) %>% 
    predict(fitted_mod$log, newdata = ., type = "response", terms = c("Y")) %>% 
      matrix(ncol = nrow(validation_set)) %>% colMeans()})
    #as_tibble() %>% dplyr::arrange(names(.)) %>% .$value})
  # conditional imputation with rf model
  Y_pred_rf <- imputations %>% map(~{
    select(.x, -p_miss, -draw) %>% 
      predict(mod$rf,
              data = .,
              type = "response",
              terms = c("Y", "id")) %>% .[["predictions"]]
    # TODO: add id so the mult method works!
  })
  # # pattern submodel method
  # validation_set %>% 
  #   filter(p_miss==4) %>% 
  #   predict(
  #     mod$sub[["miss_4"]],
  #     newdata = .[,-2],
  #     type = "response",
  #     terms = c("Y", "id")
  #   )
    
  # TODO: add submodel method
  # TODO: add surrogate split method
  # TODO: average the predictions from the mult method
  # TODO: combine predictions into list
  
  # output
  return(list(
    Y_true = Y_true,
    Y_prob = Y_prob,
    Y_pred_log_mean = Y_pred_log$mean,
    Y_pred_log_sing = Y_pred_log$sing,
    Y_pred_log_mult = Y_pred_log$mult,
    Y_pred_rf_mean = Y_pred_rf$mean,
    Y_pred_rf_sing = Y_pred_rf$sing,
    Y_pred_rf_mult = Y_pred_rf$mult,
    Y_pred_sub = NULL,
    Y_pred_sur = NULL
  ))
}

# # function for the box of submodels method
# pred_sub <- function(dataset,
#                      submodel_2,
#                      submodel_4,
#                      submodel_6) {
#   # add the id number to return in the correct order
#   dataset <- cbind(id = 1:nrow(dataset), dataset)
#   datasplit <- split(dataset, dataset$p_miss)
#   Y_pred <-
#     c(
#       predict(
#         submodel_2$mod,
#         newdata = datasplit$`8`[, -2],
#         type = "response",
#         terms = c("Y", "id")
#       ),
#       predict(
#         submodel_4$mod,
#         newdata = datasplit$`6`[, -2],
#         type = "response",
#         terms = c("Y", "id")
#       ),
#       predict(
#         submodel_6$mod,
#         newdata = datasplit$`4`[, -2],
#         type = "response",
#         terms = c("Y", "id")
#       )
#     ) %>%
#     data.frame(Y_pred = ., id = as.numeric(names(.))) %>% dplyr::arrange(id)
#   # output
#   return(Y_pred[, "Y_pred"])
# }

# # # function for rf with imputation (missranger)
# # pred_rf_imp <- function(dataset, rfmodel_imp){
# #   missRanger::missRanger(dataset) %>% predict(rfmodel_imp, data = .)
# # }
# 
# # function for strategy 1, method 1 (conditional mean imp)
# pred_mean <- function(imp_list, log_mod, rf_mod, p = 10) {
#   # predict Y for each md pattern using the logistic model
#   Y_pred_log <- map_dfr(imp_list, ~ {
#     predict(
#       log_mod$mod,
#       newdata = .x$imp_mean[,-c(p + 1, p + 2)],
#       type = "response",
#       terms = c("Y", "id")
#     ) %>%
#       data.frame(Y_pred = ., id = as.numeric(names(.)))
#   }) %>% dplyr::arrange(id)
#   
#   # predict Y for each md pattern using the rf model
#   Y_pred_rf <- map_dfr(imp_list, ~ {
#     predict(
#       rf_mod$mod, 
#       data = .x$imp_mean[,-c(p + 1, p + 2)], 
#       type = "response"
#       )[["predictions"]] %>% 
#       data.frame(Y_pred =., id = .x$imp_mean$id) 
#   }) %>% dplyr::arrange(id)
#  
#   # output
#   return(list(mean_log = Y_pred_log[, "Y_pred"], mean_rf = Y_pred_rf[, "Y_pred"]
#                 ))
# }
# 
# # function for strategy 1, method 2 and 3 (conditional draw imp)
# pred_draw <- function(imp_list, log_mod, rf_mod, p = 10) {
#   # predict Y for each md pattern using the logistic model
#   # for each md pattern
#   Y_pred_log <- map_dfr(1:3, function(md) {
#     # for each observation
#     map_dfr(imp_all[[md]]$imp_draw, function(i) {
#       # for each draw predict Y
#       predict(log_mod$mod,
#               newdata = i[,-c(p + 1, p + 2)],
#               type = "response") %>%
#         # split single and multiple draws
#         split_pred() %>%
#         c(i[1, p+1], .) %>%
#         setNames(c("id", "sing", "mult"))
#     })
#   }) %>% dplyr::arrange(id)
#   
#   # predict Y for each md pattern using the rf model
#   # for each md pattern
#   Y_pred_rf <- map_dfr(1:3, function(md) {
#     # for each observation
#     map_dfr(imp_all[[md]]$imp_draw, function(i) {
#       # for each draw predict Y
#       predict(rf_mod$mod,
#               data = i[,-c(p + 1, p + 2)],
#               type = "response")[["predictions"]] %>%
#         # split single and multiple draws
#         split_pred() %>%
#         c(i[1, p+1], .) %>%
#         setNames(c("id", "sing", "mult"))
#     })
#   }) %>% dplyr::arrange(id)
#   # output
#   return(list(sing_log = Y_pred_log$sing, mult_log = Y_pred_log$mult, sing_rf = Y_pred_rf$sing, mult_rf = Y_pred_rf$mult))
# }
# 
# # helper function for pred_draw() to split single and multiple imp methods
# # averages the draws from the distribution of predictions after multiple imp
# split_pred <-
#   function(preds) {
#     c(preds[1], mean(preds[2:length(preds)]))
#   }
