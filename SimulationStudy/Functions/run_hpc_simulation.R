#######################
# simulation script hpc
# surrogate splits only
#######################

###################
# setup environment 
###################
# data
sim_data <- readRDS("./data/sim_data.RDS")

# parameters
set.seed(11)
it <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))

# packages
library("dplyr") #for wrangling
library("purrr") #for vectorization
library("party") #for prediction

# wrapper function to fit the models and predict the outcome
# only surrogate version is needed here (surrogate_split = TRUE)
analyses <- function(dev, val, p, m, surrogate_split){
  pred <- fit_mod(development_set = dev, n_predictors = p, just_sur = surrogate_split) %>% 
    pred_outcome(validation_set = val, fitted_mod = ., n_imp = m, just_sur = surrogate_split)
  return(pred)
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

##################
# start simulation
##################
results_sur <- purrr::map(sim_data[it], ~analyses(.x$devset, .x$valset, p, m, surrogate_split = TRUE))

# export results 
saveRDS(results_sur, file = paste0("./data/output_sur_it_", it, ".RDS"))

# # combine results later
# all_results_sur <- list(readRDS("./data/output_sur_it_1.RDS"), readRDS("./data/output_sur_it_2.RDS"))
