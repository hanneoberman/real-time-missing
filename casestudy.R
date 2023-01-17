# case study

# setup env
set.seed(171)

# load data
dat <- readRDS("C:/Users/4216318/surfdrive/Documents/Student-assistentschap/MIMIC Data/mimic_cleaned.RDS")

# split train/test
ind <- sample(c(TRUE, FALSE), nrow(dat), replace = TRUE, prob = c(0.7,0.3))
train  <- dat[ind, -1]
test   <- dat[!ind, -1]

# run surrogate split model
sur_mod <- party::cforest(hospital_expire_flag ~ ., data = train)
