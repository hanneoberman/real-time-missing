########################

library(foreign)
library(mice) # version 3.6.0
library(condMVNorm) # version 2015.2-1
library(survival)
library(MASS)

########################

# Single patient joint imputation
#' @param data single patient data
#' @param imp_means prior estimated column means of training data
#' @param imp_cov prior estimated covariance matrix of training data
#' @param n.imp number of imputations to make; default==1, which means it uses expected values
#'
#' @return Returns single patient data with imputed values
impJoint <- function(data = data, imp_means = imp_means, imp_cov = imp_cov, n.imp = 1, ...) {
  if (class(data) != "data.frame") {
    stop("Data object should be a data frame")
  }
  if (nrow(data) > 1) {
    stop("Data should contain a single patient")
  }
  
  dep <- which(is.na(data)) # missing variables
  given <- which(!is.na(data)) # observed variables
  depnames <- colnames(data[dep])
  givennames <- colnames(data[given])
  data <- as.matrix(data)
  
  # if no variables are observed, then we just impute the colmeans
  if (length(given) == 0) {
    data_imp <- t(as.data.frame(imp_means))
  } else if (length(given) > 0) {
    # Extract conditional Mean and conditional var
    cond <- condMVNorm::condMVN(mean = imp_means, sigma = imp_cov, dep = dep, given = given, X = data[given])
    
    if (n.imp == 1) {
      # Just impute the expected value if we only need 1 imputation
      x.imp <- matrix(cond$condMean, nrow = 1)
    } else if (n.imp < 1000) {
      # We should not use multiple imputation if the number of imputed values is very low (i.e. <1000).
      # The empirical covariance of imputed values is very unreliable in such circumstances
      stop("A minimum of 1000 imputations should be generated when drawing random samples, instead of using the conditional mean.")
    } else {
      # Draw from a multivariate normal if multiple imputations required
      x.imp <- mvrnorm(n = n.imp, mu = cond$condMean, Sigma = cond$condVar)
    }
    
    x.obs <- matrix(data[given], nrow = n.imp, ncol = length(given), byrow = TRUE)
    data_imp <- as.data.frame(cbind(x.obs, x.imp))
    colnames(data_imp) <- c(givennames, depnames)
  }
  data.frame(data_imp)
}

set.seed(1)

varcov <- define_varcov(p = 10)
linear_betas <- runif(nrow(varcov),-1, 1)
non_linear_betas <- runif(nrow(varcov),-0.1, 0.1)

# let's generate some data
devset <-
  generate_sample(sample_size = 10000,
                  varcov,
                  linear_betas,
                  non_linear_betas,
                  interaction = TRUE)

glimpse(devset)

# extract means and covariance for imputation function
means <- colMeans(devset)
covs <- cov(devset)

# generate complete validation set
valset <-
  generate_sample(
    sample_size = 20000,
    varcov,
    linear_betas,
    non_linear_betas,
    interaction = TRUE
  )

valset_rec <- valset

# define the missingness parameters
miss_par <- define_miss(p = nrow(varcov))

# make the validation set incomplete according to the missing data pattern and MAR types
valset <- create_miss(valset, miss_par)

glimpse(valset)

# remove # of missings and outcome
valset <- valset[, -c(1,2)]

res <- as.data.frame(array(NA, dim = c(nrow(valset), ncol(valset))))
for(i in 1:nrow(valset)) res[i, ] <- impJoint(valset[i, ], means, covs)
Y_pred <- res %>% setNames(names(valset)) %>% predict(mod_true$mod, newdata = ., type = "response")
calc_RMSE(pred = Y_pred, obs = valset_rec$Y)

plot(Y_pred, valset_rec$Y)
max(Y_pred)
