# functions to predict the outcome variable from the incomplete data

# function for the box of submodels method
pred_sub <- function(dataset,
                     submodel_2,
                     submodel_4,
                     submodel_6) {
  # add the id number to return in the correct order
  dataset <- cbind(id = 1:nrow(dataset), dataset)
  datasplit <- split(dataset, dataset$p_miss)
  Y_pred <-
    c(
      predict(
        submodel_2$mod,
        newdata = datasplit$`8`[, -2],
        type = "response",
        terms = c("Y", "id")
      ),
      predict(
        submodel_4$mod,
        newdata = datasplit$`6`[, -2],
        type = "response",
        terms = c("Y", "id")
      ),
      predict(
        submodel_6$mod,
        newdata = datasplit$`4`[, -2],
        type = "response",
        terms = c("Y", "id")
      )
    ) %>%
    data.frame(Y_pred = ., id = as.numeric(names(.))) %>% dplyr::arrange(id)
  # output
  return(Y_pred[, "Y_pred"])
}

# # function for rf with imputation (missranger)
# pred_rf_imp <- function(dataset, rfmodel_imp){
#   missRanger::missRanger(dataset) %>% predict(rfmodel_imp, data = .)
# }

# function for strategy 1, method 1 (conditional mean imp)
pred_mean <- function(imp_list) {
  # predict Y for each md pattern
  Y_pred <- map_dfr(imp_list, ~ {
    predict(
      mod_true$mod,
      newdata = .x$imp_mean[,-c(11:12)],
      type = "response",
      terms = c("Y", "id")
    ) %>%
      data.frame(Y_pred = ., id = as.numeric(names(.)))
  }) %>% dplyr::arrange(id)
  # output
  return(Y_pred[, "Y_pred"])
}

# function for strategy 1, method 2 and 3 (conditional draw imp)
# a <-imp_all[[1]]$imp_draw[[1]] 
# b <- a %>% 
#   predict(
#   mod_true$mod,
#   newdata = a[,-c(11:12)],
#   type = "response",
#   terms = c("Y", "id")
# )
