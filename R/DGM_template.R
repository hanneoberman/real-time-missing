test <- sample_one_cluster(50000,a=0.6,b_bin=15,b_norm=c(0.5,0.2))
fit <- glm(Y~X1+X2+X3,data=test,family="binomial")



summary(fit)




#' Sample independent binary-outcome data
#'
#' @param n sample size per cluster
#' @param a intercept
#' @param b_bin coefficients for binary predictors
#' @param b_norm coeficients for standard normal predictors
#' @param covariance covariance for predictor values (note that binary variables
#' are dichotimized afterwards, reducing the covariance)
#' @param col_names colnames for returned data.frame
#'
#' @return independent sample with binary outcome and binary and normal predictors
sample_one_cluster <- function(n = 50,
                               
                               a = NULL,
                               b_bin = log(1),
                               b_norm = log(1),
                               
                               p_x_bin = .5,
                               covariance = .25,
                               
                               col_names = NULL,
                               ...) {??
    library(MASS)
  # Coefficients
  b <- c(b_bin, b_norm)
  J <- length(b)
  if (is.null(col_names)) col_names <- c("Y", paste0("X", 1:J))
  coefs <- c(a, b)
  
  # Observed x
  sigma <- matrix(covariance, ncol = J, nrow = J)
  diag(sigma) <- 1 # sd for normal is fixed at 1
  xx <- mvrnorm(n = n, mu = rep(0, J), Sigma = sigma)
  
  if (length(p_x_bin) == 1){??
      p_x_bin <- rep(p_x_bin, length(b_bin))
  }??
    
    x_bin_list <- list()
  if (length(b_bin)) {??
      for (i in seq_along(b_bin)) {??
          x_bin_temp <- xx[ , i]
          threshold <- quantile(x_bin_temp, probs = 1 - p_x_bin[i])
          positive <- x_bin_temp > threshold
          x_bin_temp[!positive] <- 0
          x_bin_temp[positive ] <- 1
          x_bin_list[[length(x_bin_list) + 1]] <- x_bin_temp
      }??
  }??
    
    x_bin <- Reduce(cbind, x_bin_list)
  x <- cbind(1, x_bin, xx[ , (length(b_bin)+1):ncol(xx)])
  
  # Predicted -> observed y
  lp <- coefs %*% t(x)
  p <- metamisc:::inv.logit(lp)
  y <- stats::rbinom(length(lp), size = 1, prob = p)
  
  # Return all except intercept
  out <- data.frame(cbind(y,x[ , -1]))
  colnames(out) <- col_names
  out
}??
  
  
  
  
  
  
  
  #' Sample clustered binary-outcome data
  #'
  #' @param n sample size(s) per cluster
  #' @param k number of clusters
#' @param a_mean mean of incercepts across clusters
#' @param a_sd standard deviation of intercepts across clusters
#' @param b_bin_mean means of coefficients for binary predictors across clusters
#' @param b_norm_mean means of coefficients for normal predictors across clusters
#' @param b_bin_sd sd of coefficients for binary predictors across clusters
#' @param b_norm_sd sd of coefficients for normal predictors across clusters
#' @param covariance covariance for predictor values (note that binary variables
#' are dichotimized afterwards, reducing the covariance)
#'
#' @return Clustered sample with binary outcome and binary, normal and exponential predictors
sample_clusters <- function(n = 50,
                            k = 10,
                            
                            a_mean = 0,
                            a_sd = 0,
                            
                            b_bin_mean = rep(log(1)),
                            b_norm_mean = rep(log(1)),
                            
                            b_bin_sd = rep(0, length(b_bin_mean)),
                            b_norm_sd = rep(0, length(b_norm_mean)),
                            
                            p_x_bin = matrix(0.5, ncol = k, nrow = length(b_bin_mean)),
                            
                            covariance = .20,
                            
                            ...) {??
    
    # Draw cluster-specific betas and intercept
    b_bin <- matrix(rnorm(k * length(b_bin_mean), b_bin_mean, b_bin_sd) , ncol = k, nrow = length(b_bin_mean))
    b_norm <- matrix(rnorm(k * length(b_norm_mean), b_norm_mean, b_norm_sd), ncol = k, nrow = length(b_norm_mean))
    
    a <- rnorm(k, a_mean, a_sd)
    
    # Draw cluster-specific probability / prevalence of x
    # If a single value is supplied, this is used each time.
    if (is.null(dim(p_x_bin)) || dim(p_x_bin) == c(1,1)) {??
        p_x_bin <- matrix(p_x_bin, ncol = k, nrow = length(b_bin_mean))
    }?? else {??
        p_x_bin <- t(apply(p_x_bin, 1, function(x) sample(x, replace = T)))
    }??
      
      # Draw cluster-specific covariance
      # If a single value is supplied, this is used each time.
      if (length(covariance) == 1) {??
          covariance <- rep(covariance, k)
      }?? else {??
          covariance <- sample(covariance, replace = T)
      }??
      
      # Replicate n if only one n is supplied
      if (length(n) == 1)
        n <- rep(n, k)
    
    # Sample per cluster
    dd <- list()
    for (i in seq_len(k)) {??
        dd[[length(dd) + 1]] <- cbind(sample_one_cluster(n = n[i],
                                                         
                                                         a = a[i],
                                                         
                                                         b_bin = b_bin[ , i],
                                                         b_norm = b_norm[ , i],
                                                         
                                                         p_x_bin = p_x_bin[ , i],
                                                         covariance = covariance[i],
                                                         
                                                         ...),
                                      cluster = i)
    }??
      
      # Return as data.frame
      do.call(rbind, dd)
}??
  
  
  
  load("params/sim_parameters_2021_01.RData")



set.seed(2021)
est_cor <- function(correction) {??
    d <- (sample_clusters(n = 10000,
                          k = length(sim_parameters$n),
                          a_mean = sim_parameters$meta_coefs["(Intercept)"],
                          a_sd = sim_parameters$meta_tau["(Intercept)"],
                          b_bin_mean = sim_parameters$meta_coefs[c("sex", "malign", "surg", "notraum", "vein", "calfdif3", "ddimdich")],
                          b_norm_mean = sim_parameters$meta_coefs[c("age", "durat")],
                          b_bin_sd = sim_parameters$meta_tau[c("sex", "malign", "surg", "notraum", "vein", "calfdif3", "ddimdich")],
                          b_norm_sd = sim_parameters$meta_tau[c("age", "durat")],
                          covariance = sim_parameters$cor * correction,
                          p_x_bin = sim_parameters$props_studyspecific))
    
    
    (mean(cor(d[ , 2:10])) - mean(sim_parameters$cor))^2
}??
  
  
  
  o <- optim(.9, est_cor, method = "Brent", lower = 0, upper = 2)
sim_parameters$optim_cor <- o$par * sim_parameters$cor


## example script from https://data.library.virginia.edu/simulating-a-logistic-regression-model/

# parameters
n <- 100

# predictors
gender <- sample(c(0,1), size = n, replace = TRUE)
age <- round(runif(n, 18, 80))

# predict the outcome
xb <- -9 + 3.5*gender + 0.2*age
p <- 1/(1 + exp(-xb))
y <- rbinom(n = n, size = 1, prob = p)

# test
mod <- glm(y ~ gender + age, family = "binomial")
