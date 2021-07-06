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
  }) %>% dplyr::arrange()
  # output
  return(Y_pred[, "Y_pred"])
}

# function for strategy 1, method 2 and 3 (conditional draw imp)

# helper function to average the draws from the distribution of predictions after imputation
split_pred <- function(preds) {c(preds[1], mean(preds[2:length(preds)]))} 

a <- imp_all[[1]]$imp_draw#[[2]]

b <- map_dfr(a, function(i){suppressWarnings(predict(mod_true$mod,
             newdata = i[,-c(11:12)],
             type = "response")) %>% 
  split_pred() %>% 
  c(i[1,11], .) %>% 
  setNames(c("id", "samp", "dist"))})

# a <- imp_all[[1]]$imp_draw[[2]]
# 
# b <- predict(mod_true$mod,
#              newdata = a[,-c(11:12)],
#              type = "response") %>% 
#   split_pred() %>% 
#   c(a[1,11]) %>% 
#   setNames(c("samp", "dist", "id"))
# pred_samp <- b[1,]
# pred_dist <- data.frame(Y_pred = mean(b[2:nrow(b), 1]), id = b[2,2])
