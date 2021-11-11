#######################
# simulation script hpc
#######################

###################
# setup environment 
###################
# packages
library("dplyr") #for wrangling
library("purrr") #for vectorization
library("mvtnorm") #for DGM
library("mice") #for DGM
library("party") #for prediction
library("ranger") #for prediction

# parameters
seed <- 13 #random seed: 11 for MAR, 13 for MNAR
n_sim <- 1000 #number of repetitions #TODO: make this 1000
n_devset <- 10000 #population size #TODO: make this 10000
n_valset <- 20000 #sample size #TODO: make this 20000
p <- 10 #number of predictors 
# p_missing <- c(4, 6, 8) #missing predictors (hard coded)
m <- 51 #number of imputations #TODO: make this 51
miss_mech <- "MNAR" #missingness mechanism

##################
# define functions
##################
# set a single data generating mechanism before simulating
# this defines the population and is used in each repetition
define_DGM <- function(p = 10, seed = 123) {
  # inputs: number of predictor variables, and random seed
  # output: list of parameters, i.e. variance-covariance matrix of the predictors,
  # regression coefficients for the outcome, missingness types and patterns.
  set.seed(seed)
  # variance-covariance matrix
  betas <- rnorm(p * (p - 1) / 2, 0, 0.1)
  sigma <- diag(p)
  sigma[upper.tri(sigma)] <- betas
  sigma[lower.tri(sigma)]  <- t(sigma)[lower.tri(sigma)]
  Sigma <- t(sigma) %*% sigma
  # regression coefficients
  linear_betas <- runif(p,-1, 1)
  non_linear_betas <- runif(p,-0.1, 0.1)
  # missingness types
  MAR_types <- rep(c("LEFT", "MID", "TAIL", "RIGHT"), 3)
  # missing data patterns
  pat <- matrix(1, 12, p) %>%
    data.frame() #for each missingness type and each predictor
  pat[1:4, 7:p] <- 0 #four predictors missing
  pat[5:8, 5:p] <- 0 #six predictors missing
  pat[9:12, 3:p] <- 0 #eight predictors missing
  pat <- cbind(Y_prob = 1, Y = 1, pat) #outcome never missing
  # output
  return(list(
    varcov = Sigma,
    betas = c(linear_betas, non_linear_betas),
    miss_type = MAR_types,
    miss_pat = pat
  ))
}

# wrap pipeline to run in each simulation repetition 
# included functions defined below (in order of appearance)
simulate_once <- function(DGM, n_devset, n_valset, m, p, i) {
  # create devset and fit models 
  mod <- generate_data(
    sample_size = n_devset,
    covariance_matrix = DGM$varcov,
    linear_bs = DGM$betas[1:p],
    non_linear_bs = DGM$betas[(p + 1):(2 * p)],
    interaction = TRUE
  ) %>%
    fit_mod(n_predictors = p) 
  # create valset, make incomplete, and predict outcome
  pred <- generate_data(
    sample_size = round(1.01 * n_valset, 0),
    DGM$varcov,
    DGM$betas[1:p],
    DGM$betas[(p + 1):(2 * p)],
    interaction = TRUE
  ) %>%
    create_miss(missingness_pat = DGM$miss_pat,
                missingness_type = DGM$miss_type
                ) %>%
    pred_outcome(fitted_mod = mod, n_imp = m) 
  # output
  return(pred)
}


# function to generate a development set or validation set
# with p continuous predictors and 1 binary outcome
generate_data <- function(sample_size,
                          covariance_matrix,
                          linear_bs,
                          non_linear_bs,
                          interaction = FALSE) {
  # generate observations for the predictors
  dat <-
    mvtnorm::rmvnorm(
      n = sample_size,
      mean = rep(0, nrow(covariance_matrix)),
      sigma = covariance_matrix,
      checkSymmetry = FALSE
    )
  # compute the outcome based on linear function of predictors and error term
  lin_pred <- -3 +
    linear_bs[2] * log(abs(dat[, 2])) +
    dat[, c(1, 3:nrow(covariance_matrix))] %*% linear_bs[c(1, 3:nrow(covariance_matrix))] +
    rnorm(sample_size, sd = 2)
  if (interaction) {
    lin_pred <- lin_pred + dat %*% non_linear_bs * dat[, 1]
  }
  # convert to probabilities
  p <- 1 / (1 + exp(-lin_pred))
  # convert to binary outcome
  Y <- rbinom(n = length(p),
              size = 1,
              prob = p)
  # output
  dat <- dat %>% data.frame() %>% cbind(Y_prob = p, Y = Y, .)
  return(dat)
}

# function to fit the prediction models on each development set
# and save the fitted models plus devset characteristics (means&covariance)
fit_mod <-
  function(development_set,
           n_predictors = 10,
           just_sur = FALSE) {
    if (just_sur) {
      # surrogate split model
      sur_mod <- party::cforest(Y ~ ., data = development_set[, -1])
      # output
      out <- list(sur = sur_mod)
    }
    if (!just_sur) {
      # save the means and var-cov matrix for imputation
      means <- colMeans(development_set[, -c(1, 2)])
      varcov <- cov(development_set[, -c(1, 2)])
      # set number of predictors for submodels
      p_obs <-
        list(
          2:(n_predictors + 2),
          2:(n_predictors + 2 - 4),
          2:(n_predictors + 2 - 6),
          2:(n_predictors + 2 - 8)
        )
      # logistic model with splines
      log_mod <- purrr::map(p_obs, ~ {
        development_set[, .x] %>%
          glm(Y ~ splines::ns(X1, df = 3) + . - X1,
              family = "binomial",
              data = .)
      }) %>% setNames(c("full", "miss_4", "miss_6", "miss_8"))
      # random forest model
      rf_mod <- purrr::map(p_obs, ~ {
        development_set[, .x] %>%
          ranger::ranger(Y ~ ., data = .)
      }) %>% setNames(c("full", "miss_4", "miss_6", "miss_8"))
      # output
      out <- list(
        mu = means,
        sigma = varcov,
        log = log_mod,
        rf = rf_mod
        )
    }
    return(out)
  }

# function to create missingness in each validation set
# according to the missingness parameters from define_DGM()
create_miss <- function(validation_set,
                        missingness_pat,
                        missingness_type,
                        missingness_mech = "MAR") {
  # ampute the complete valset
  incomplete <- validation_set %>%
    mice::ampute(
      mech = missingness_mech,
      prop = 0.999,
      patterns = missingness_pat,
      type = missingness_type
    ) %>%
    .$amp %>%
    # remove complete observations
    cbind(p_miss = rowSums(is.na(.)), .) %>%
    filter(p_miss != 0)
  # output
  return(incomplete)
}

# function to predict the outcome from the incomplete validation set
# using two non-imputation methods and 3x2 imputation methods
pred_outcome <- function(validation_set, fitted_mod, n_imp, just_sur = FALSE) {
  # split off and save Y for revaluation
  Y_true <- validation_set$Y
  Y_prob <- validation_set$Y_prob
  validation_set <-
    validation_set[,-c(2:3)] %>% cbind(id = 1:nrow(.), .)
  if(just_sur){
  # surrogate split method
  Y_pred_sur <-
    predict(fitted_mod$sur, newdata = validation_set)[, "Y"]
  out <- data.frame(
    Y_true = Y_true,
    Y_prob = Y_prob,
    Y_pred_sur = Y_pred_sur  )
  }
  if(!just_sur){
  ## imputation methods
  imputations <-
    impute_cond(
      vals = validation_set,
      dev_means = fitted_mod$mu,
      dev_cov = fitted_mod$sigma,
      m = n_imp
    )
  # conditional imputation with logistic model
  Y_pred_imp_log <- imputations %>% map( ~ {
    select(.x,-p_miss,-draw) %>%
      predict(fitted_mod$log$full, newdata = ., type = "response") %>%
      matrix(ncol = nrow(validation_set)) %>% colMeans()
  })
  # conditional imputation with rf model
  Y_pred_imp_rf <- imputations %>% map( ~ {
    select(.x,-p_miss,-draw) %>%
      predict(fitted_mod$rf$full,
              data = .,
              type = "response") %>% .[["predictions"]] %>%
      matrix(ncol = nrow(validation_set)) %>% colMeans()
  })
  ## non-imputation methods
  # pattern submodel methods
  ids_miss <-
    map(c(4, 6, 8), ~ {
      validation_set %>% filter(p_miss == .x) %>% .[, "id"]
    })
  # logistic model
  Y_pred_sub_log <-
    map2_dfr(ids_miss, fitted_mod$log[-1], ~ {
      validation_set[.x,-c(1, 2)] %>%
        predict(.y,
                newdata = .,
                type = "response",
                terms = c("id", "Y")) %>% 
        data.frame(Y_pred = ., id = .x)
    }) %>% dplyr::arrange(id)
  # random forest model
  Y_pred_sub_rf <-
    map2_dfr(ids_miss, fitted_mod$rf[-1], ~ {
      validation_set[.x,-c(1, 2)] %>%
        predict(.y,
                data = .,
                type = "response") %>% 
        .[["predictions"]] %>% 
        data.frame(Y_pred = ., id = .x)
    }) %>% dplyr::arrange(id)
  out <- data.frame(
    Y_true = Y_true,
    Y_prob = Y_prob,
    Y_pred_mean_log = Y_pred_imp_log$mean,
    Y_pred_sing_log = Y_pred_imp_log$sing,
    Y_pred_mult_log = Y_pred_imp_log$mult,
    Y_pred_mean_rf = Y_pred_imp_rf$mean,
    Y_pred_sing_rf = Y_pred_imp_rf$sing,
    Y_pred_mult_rf = Y_pred_imp_rf$mult,
    Y_pred_sub_log = Y_pred_sub_log$Y_pred,
    Y_pred_sub_rf = Y_pred_sub_rf$Y_pred
    )
  }
  # output
  return(out)
}

# function to impute the missing entries in three ways
# using the conditional mean or draw(s) from the conditional
impute_cond <- function(vals, dev_means, dev_cov, m) {
  imp_list <- purrr::map(c(4, 6, 8), function(p) {
    # process inputs
    miss <- (nrow(dev_cov) - p + 1):nrow(dev_cov)
    obs <-  1:(nrow(dev_cov) - p)
    x_obs <- vals[vals$p_miss == p,-c(1, 2)]
    # compute
    B <- dev_cov[miss, miss]
    C <- dev_cov[miss, obs, drop = FALSE]
    D <- dev_cov[obs, obs]
    CDinv <- C %*% solve(D)
    # conditionals
    c_var <- B - CDinv %*% t(C)
    c_mu <-
      purrr::map_dfr(1:nrow(x_obs), function(i) {
        c(dev_means[miss] + CDinv %*% (as.numeric(x_obs[i, obs]) - dev_means[obs])) %>%
          c(as.numeric(rownames(x_obs[i, obs])), p) %>%
          setNames(c(paste0("X", miss), "id", "p_miss"))
      })
    # impute mean
    imp_mean <- cbind(x_obs[, obs], c_mu, draw = NA)
    # impute draw
    imp_draw <-
      purrr::map_dfr(1:nrow(c_mu), function(i) {
        MASS::mvrnorm(m, mu = as.numeric(c_mu[i, c(paste0("X", miss))]), Sigma = c_var) %>%
          cbind(x_obs[i, obs],
                .,
                c_mu[i, c("id", "p_miss")],
                draw = 1:nrow(.),
                row.names = NULL)
      })
    list(mean = imp_mean, draw = imp_draw)
  }) %>% setNames(c("miss_4", "miss_6", "miss_8"))
  # output
  imp_means <- map_dfr(imp_list, "mean") %>% dplyr::arrange(id)
  imp_draws <- map_dfr(imp_list, "draw") %>% dplyr::arrange(id)
  singles <- imp_draws$draw == m
  return(list(
    mean = imp_means,
    sing = imp_draws[singles, ],
    mult = imp_draws[!singles, ]
  ))
}

###################
# new sim functions
###################

datasets <- function(DGM, p, n_devset, n_valset, miss_mech) {
  # create devset and fit models 
  dev <- generate_data(
    sample_size = n_devset,
    covariance_matrix = DGM$varcov,
    linear_bs = DGM$betas[1:p],
    non_linear_bs = DGM$betas[(p + 1):(2 * p)],
    interaction = TRUE
  ) 
  # create valset, make incomplete, and predict outcome
  val <- generate_data(
    sample_size = round(1.01 * n_valset, 0),
    DGM$varcov,
    DGM$betas[1:p],
    DGM$betas[(p + 1):(2 * p)],
    interaction = TRUE
  ) %>%
    create_miss(missingness_pat = DGM$miss_pat,
                missingness_type = DGM$miss_type,
                missingness_mech = miss_mech
    ) 
  # output
  return(list(devset=dev, valset = val))
}

analyses <- function(dev, val, p, m, surrogate_split){
  pred <- fit_mod(development_set = dev, n_predictors = p, just_sur = surrogate_split) %>% 
    pred_outcome(validation_set = val, fitted_mod = ., n_imp = m, just_sur = surrogate_split)
  return(pred)
}
  
##################
# start simulation
##################
# define population 
DGM <- define_DGM(p, seed)
set.seed(seed)

# create datasets
sim_data <- replicate(datasets(DGM, p, n_devset, n_valset, miss_mech), n = n_sim, simplify = FALSE)
saveRDS(sim_data, file = "Data/datasets_MNAR.RDS")

# run analyses
results <- map(sim_data, ~analyses(.x$devset, .x$valset, p, m, surrogate_split = FALSE))

# # repeat the simulation pipeline n_sim times
# simvec <- 1:n_sim
# simfun <- function(vec){simulate_once(DGM, n_devset, n_valset, m, p, i = vec)}
# results <- parallel::mclapply(simvec, simfun, mc.cores = 5)

# export results 
output <- list(DGM = DGM, results = results, seed = .Random.seed) 
saveRDS(output, file = "Results/output_MNAR.RDS")

##################
# surrogate splits
##################
# set.seed(output$seed)
# max_it <- 1
# results_sur <- map(sim_data[1:max_it], ~analyses(.x$devset, .x$valset, p, m, surrogate_split = TRUE))
# sur_out <- list(results_sur = results_sur, seed = .Random.seed, maxit = max_it)
# sur_name <- paste0("sur_", max_it, ".RDS")
# saveRDS(sur_out, file = sur_name)
# 
# set.seed(sur_out$seed)
# min_it <- sur_out$maxit + 1
# max_it <- 10
# results_sur <- map(sim_data[min_it:max_it], ~analyses(.x$devset, .x$valset, p, m, surrogate_split = TRUE))
# sur_out <- list(results_sur = results_sur, seed = .Random.seed, maxit = max_it)
# sur_name <- paste0("sur_", min_it, "to", max_it, ".RDS")
# saveRDS(sur_out, file = sur_name)