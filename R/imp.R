# functions for imputing the missing values in an incomplete set

# helper function to compute conditional mean and var for imputation
comp_cond <- function(devs, vals, p){

# inputs
means <- colMeans(devs[,-1])
vcov <- cov(devs[,-1])

# p <- 6
# dat <- valset[1,-c(1:2)]

miss <- (11-p):10 #which(is.na(dat))
obs <-  1:(10-p) #which(!is.na(dat))
# compute
B <- vcov[miss, miss]
C <- vcov[miss, obs, drop=FALSE]
D <- vcov[obs, obs]
CDinv <- C %*% solve(D)
# output
c_var <- B - CDinv %*% t(C)

# this is the part where you need individual observations, not before!
# cMu <- c(means[miss] + CDinv %*% (as.numeric(dat[,obs]) - means[obs]))

##########
x_obs <- vals[vals$p_miss == p,-c(1:2)]

# dat2 <- valset[3,-c(1:2)]
# 
# x_obs2 <- as.numeric(dat[,obs]) %>% list(as.numeric(dat2[,obs]))
# 
# cMu2 <- purrr::map(1:2, function(i){c(means[miss] + CDinv %*% (x_obs2[[i]] - means[obs])) %>% c(x_obs2[[i]], .)})
# cMu3 <- purrr::map(x_obs2, function(i){c(means[miss] + CDinv %*% (i - means[obs])) %>% c(i, .)})
# 
# rownames(dat2)

c_mu <- purrr::map_dfr(1:nrow(x_obs), function(i){c(means[miss] + CDinv %*% (as.numeric(x_obs[i, obs]) - means[obs])) %>% 
    c(rownames(x_obs[i, obs]), x_obs[i, obs], .) %>% 
    setNames(., c("id", names(x_obs)))})

return(list(cond_means = c_mu, cond_vars = c_var))
}

# collect all md patterns
a <- comp_cond(devs = devset, vals = valset, p = 4)
b <- comp_cond(devs = devset, vals = valset, p = 6)
c <- comp_cond(devs = devset, vals = valset, p = 8)

# imputation method 1: Missing values are imputed by their conditional mean
imp_cond <- rbind(a$cond_means, b$cond_means, c$cond_means) %>% dplyr::arrange(as.numeric(id))

# imputation method 2: Missing values are imputed by a random draw from their conditional multivariate distribution.
## make this a function to apply to all md patterns and repeat for method 3 with n = 50
a2 <- purrr::map_dfr(1:nrow(a$cond_mean), function(i){mvtnorm::rmvnorm(n = 1, mean = as.numeric(a$cond_means[i, 8:11]), sigma = as.matrix(a$cond_vars)) %>% 
    setNames(names(a$cond_means)[8:11]) %>% 
    data.frame()})
a3 <- cbind(a$cond_means[, 1:7], a2)
