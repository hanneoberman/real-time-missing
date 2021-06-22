# functions to predict the outcome variable from the incomplete data

# function for the box of submodels method
pred_sub <- function(dataset, submodel_2, submodel_4, submodel_6) {
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
  return(Y_pred[,"Y_pred"])
}
