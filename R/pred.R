# functions to predict the outcome variable from the incomplete data

pred_Y <- function(validation_set, fitted_mod, n_imp){
  # split off and save Y for revaluation
  Y_true <- validation_set$Y
  Y_prob <- validation_set$Y_prob
  validation_set <- validation_set[, -c(2:3)] %>% cbind(id = 1:nrow(.), .)
  
  # non-imputation methods
  # pattern submodel method
  ids_miss <- map(c(4,6,8), ~{validation_set %>% filter(p_miss==.x) %>% .[,"id"]})
  Y_pred_sub <- map2_dfr(ids_miss, fitted_mod$sub, ~{validation_set[.x, -c(1,2)] %>%
      predict(
        .y,
        newdata = .,
        type = "response",
        terms = c("id", "Y")
      ) %>% data.frame(Y_pred = ., id = .x)}) %>% dplyr::arrange(id)
  # surrogate split method
  Y_pred_sur <- predict(fitted_mod$sur, newdata = validation_set)[,"Y"]
  
  # imputation methods 
  imputations <- impute_cond(vals = validation_set, dev_means = mod$mu, dev_cov = mod$sigma, m = n_imp)
  # conditional imputation with logistic model
  Y_pred_log <- imputations %>% map(~{
    select(.x, -p_miss, -draw) %>% 
    predict(fitted_mod$log, newdata = ., type = "response") %>% 
      matrix(ncol = nrow(validation_set)) %>% colMeans()
    })
  # conditional imputation with rf model
  Y_pred_rf <- imputations %>% map(~{
    select(.x, -p_miss, -draw) %>% 
      predict(mod$rf,
              data = .,
              type = "response") %>% .[["predictions"]] %>% 
      matrix(ncol = nrow(validation_set)) %>% colMeans()
  })

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
    Y_pred_sub = Y_pred_sub$Y_pred,
    Y_pred_sur = Y_pred_sur
  ))
}

