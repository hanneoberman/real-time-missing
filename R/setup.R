# setup simulation functions
# run/source this once to load 

#######################
# once used functions #
#######################

# function to define the data generating mechanism before starting the simulations
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

# wrapper function to run the simulation pipeline once
simulate_once <- function(DGM, n_devset, n_valset, m, p) {
  # fit models in dev set
  mod <- generate_data(
    sample_size = n_devset,
    covariance_matrix = DGM$varcov,
    linear_bs = DGM$betas[1:p],
    non_linear_bs = DGM$betas[(p + 1):(2 * p)],
    interaction = TRUE
  ) %>%
    fit_mod(n_predictors = p) %>% suppressWarnings()
  # predict outcome in val set
  pred <- generate_data(
    sample_size = round(1.01 * n_valset, 0),
    DGM$varcov,
    DGM$betas[1:p],
    DGM$betas[(p + 1):(2 * p)],
    interaction = TRUE
  ) %>%
    create_miss(missingness_pat = DGM$miss_pat,
                missingness_type = DGM$miss_type) %>%
    pred_outcome(fitted_mod = mod, n_imp = m) %>% suppressWarnings()
  # output
  return(pred)
}

#######################
# re-used functions ###
#######################

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
  betas <- linear_bs #runif(nrow(covariance_matrix), -10, 10)
  lin_pred <- -3 +
    betas[2] * log(abs(dat[, 2])) +
    dat[, c(1, 3:nrow(covariance_matrix))] %*% betas[c(1, 3:nrow(covariance_matrix))] +
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

# function to create missingness in each validation set
# according to the missingness parameters from define_DGM()
create_miss <- function(validation_set,
                        missingness_pat,
                        missingness_type) {
  # ampute the complete valset
  incomplete <- validation_set %>%
    mice::ampute(
      mech = "MAR",
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

# function to fit the prediction models on each development set
# and save the fitted models plus devset characteristics (means&covariance)
fit_mod <- function(development_set, n_predictors = 10) {
  # save the means and var-cov matrix for imputation
  means <- colMeans(development_set[,-c(1, 2)])
  varcov <- cov(development_set[,-c(1, 2)])
  # calculate the number of observed predictors
  p_obs <-
    list(2:(n_predictors + 2 - 4),
         2:(n_predictors + 2 - 6),
         2:(n_predictors + 2 - 8))
  # logistic model with splines
  log_mod <-
    glm(Y ~ rms::rcs(X10, 5) + ., family = "binomial", data = development_set[,-1])
  # random forest model
  rf_mod <-
    ranger::ranger(Y ~ .,
                   data = development_set[,-1],
                   num.trees = 100,
                   min.node.size = 10)
  # surrogate split model
  sur_mod <- party::cforest(Y ~ ., data = development_set[,-1])
  # box of submodels
  sub_mod <- purrr::map(p_obs, ~ {
    development_set[, .x] %>%
      glm(Y ~ ., family = "binomial", data = .)
  }) %>% setNames(c("miss_4", "miss_6", "miss_8"))
  # output
  return(list(
    mu = means,
    sigma = varcov,
    log = log_mod,
    rf = rf_mod,
    sur = sur_mod,
    sub = sub_mod
  ))
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

# function to predict the outcome from the incomplete validation set
# using two non-imputation methods and 3x2 imputation methods
pred_outcome <- function(validation_set, fitted_mod, n_imp) {
  # split off and save Y for revaluation
  Y_true <- validation_set$Y
  Y_prob <- validation_set$Y_prob
  validation_set <-
    validation_set[,-c(2:3)] %>% cbind(id = 1:nrow(.), .)
  ## non-imputation methods
  # pattern submodel method
  ids_miss <-
    map(c(4, 6, 8), ~ {
      validation_set %>% filter(p_miss == .x) %>% .[, "id"]
    })
  Y_pred_sub <-
    map2_dfr(ids_miss, fitted_mod$sub, ~ {
      validation_set[.x,-c(1, 2)] %>%
        predict(.y,
                newdata = .,
                type = "response",
                terms = c("id", "Y")) %>% data.frame(Y_pred = ., id = .x)
    }) %>% dplyr::arrange(id)
  # surrogate split method
  Y_pred_sur <-
    predict(fitted_mod$sur, newdata = validation_set)[, "Y"]
  ## imputation methods
  imputations <-
    impute_cond(
      vals = validation_set,
      dev_means = fitted_mod$mu,
      dev_cov = fitted_mod$sigma,
      m = n_imp
    )
  # conditional imputation with logistic model
  Y_pred_log <- imputations %>% map( ~ {
    select(.x,-p_miss,-draw) %>%
      predict(fitted_mod$log, newdata = ., type = "response") %>%
      matrix(ncol = nrow(validation_set)) %>% colMeans()
  })
  # conditional imputation with rf model
  Y_pred_rf <- imputations %>% map( ~ {
    select(.x,-p_miss,-draw) %>%
      predict(fitted_mod$rf,
              data = .,
              type = "response") %>% .[["predictions"]] %>%
      matrix(ncol = nrow(validation_set)) %>% colMeans()
  })
  # output
  return(
    data.frame(
      Y_true = Y_true,
      Y_prob = Y_prob,
      Y_pred_log_mean = Y_pred_log$mean,
      Y_pred_log_sing = Y_pred_log$sing,
      Y_pred_log_mult = Y_pred_log$mult,
      Y_pred_rf_mean = Y_pred_rf$mean,
      Y_pred_rf_sing = Y_pred_rf$sing,
      Y_pred_rf_mult = Y_pred_rf$mult,
      Y_pred_sub = Y_pred_sub$Y_pred,
      Y_pred_sur = Y_pred_sur
    )
  )
}
