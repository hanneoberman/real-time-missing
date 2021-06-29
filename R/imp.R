# functions for imputing the missing values in an incomplete set

# inputs
means <- colMeans(devset[,-1])
vcov <- cov(devset[,-1])
dat <- valset[1,-c(1:2)]
miss <- which(is.na(dat))
obs <- which(!is.na(dat))
# compute
B <- vcov[miss, miss]
C <- vcov[miss, obs, drop=FALSE]
D <- vcov[obs, obs]
CDinv <- C %*% solve(D)
# output
cVar <- B - CDinv %*% t(C)

# this is the part where you need individual observations, not before!
cMu <- c(means[miss] + CDinv %*% (as.numeric(dat[,obs]) - means[obs]))

# dat2 <- valset[3,-c(1:2)]
# 
# x_obs <- as.numeric(dat[,obs]) %>% list(as.numeric(dat2[,obs]))
# 
# cMu <- c(means[miss] + CDinv %*% (x_obs[[1]] - means[obs]))