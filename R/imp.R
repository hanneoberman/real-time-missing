# functions for imputing the missing values in an incomplete set

# inputs
means <- colMeans(devset[,-1])
vcov <- cov(devset[,-1])

p <- 6
# dat <- valset[1,-c(1:2)]

miss <- (11-p):10 #which(is.na(dat))
obs <-  1:(10-p) #which(!is.na(dat))
# compute
B <- vcov[miss, miss]
C <- vcov[miss, obs, drop=FALSE]
D <- vcov[obs, obs]
CDinv <- C %*% solve(D)
# output
cVar <- B - CDinv %*% t(C)

# this is the part where you need individual observations, not before!
# cMu <- c(means[miss] + CDinv %*% (as.numeric(dat[,obs]) - means[obs]))

##########
x_obs <- valset[valset$p_miss == p,-c(1:2)]

# dat2 <- valset[3,-c(1:2)]
# 
# x_obs2 <- as.numeric(dat[,obs]) %>% list(as.numeric(dat2[,obs]))
# 
# cMu2 <- purrr::map(1:2, function(i){c(means[miss] + CDinv %*% (x_obs2[[i]] - means[obs])) %>% c(x_obs2[[i]], .)})
# cMu3 <- purrr::map(x_obs2, function(i){c(means[miss] + CDinv %*% (i - means[obs])) %>% c(i, .)})
# 
# rownames(dat2)

cMu <- purrr::map_dfr(1:2, function(i){c(means[miss] + CDinv %*% (as.numeric(x_obs[i, obs]) - means[obs])) %>% 
    c(rownames(x_obs[i, obs]), x_obs[i, obs], .) %>% 
    setNames(., c("id", names(x_obs)))})
