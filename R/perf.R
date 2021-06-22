# functions for the performance of the different methods

# function for the root mean square error
RMSE <- function(pred, obs){
  sqrt(mean((pred - obs)^2))
}

# function for the brier score (combine with RMSE function??)
brier <- function(pred, obs){
  mean((pred - obs)^2)
}