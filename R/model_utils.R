library(dplyr)
library(tidyr)
library(purrr)
library(pROC)
library(tidyverse)
library(glmnet)

stage1 = function(vars, dataset){
  
  # Create a list of participant-level summary datasets for Stage 1 analysis
  # -------------------------------------------------------------
  # Inputs:
  #   vars    : character vector of variables to summarize
  #   dataset : long-format dataset with day, participantidentifier, outcomes
  # Output:
  #   List of 90 data frame
  
  day_datasets <- vector("list", length = 90)
  for (d in 0:89) {
    day_datasets[[d+1]] <- dataset %>%
      filter(day <= d + 1) %>%
      group_by(participantidentifier) %>%
      summarise(
        across(all_of(vars),
               list(
                 mean = ~mean(., na.rm = TRUE),
                 sd   = ~sd(., na.rm = TRUE)
               ),
               .names = "{.col}_{.fn}"),
        across(phq9_cat, first),
        across(si, first),
        .groups = "drop"
      ) %>%
      drop_na()
  }
  return(day_datasets)
}

stage2 = function(vars, dataset, delta=11){
  
  # Create a list of participant-level summary datasets for Stage 2 analysis
  # -------------------------------------------------------------
  # Inputs:
  #   vars    : character vector of variables to summarize
  #   dataset : long-format dataset with day, participantidentifier, outcomes
  #   delta   : window length 
  # Output:
  #   List of 90-delta data frame

  day_datasets <- vector("list", length = 90)
  for (d in 0:90) {
    day_datasets[[d+1]] <- dataset %>%
      filter(day >= d, day < d + delta) %>%
      group_by(participantidentifier) %>%
      summarise(
        across(all_of(vars),
               list(
                 mean = ~mean(., na.rm = TRUE),
                 sd   = ~sd(., na.rm = TRUE)
               ),
               .names = "{.col}_{.fn}"),
        across(phq9_cat, first),
        across(si, first),
        .groups = "drop"
      ) %>%
      drop_na()
  }
  return(day_datasets)
}

get_roc <- function(df, response_var, covariates, model_fun, pred_fun = NULL,
                    folds = 5, repeats = 3, seed = 123, ...) {
  
  # Estimate cross-validated ROC AUC for a binary prediction model.
  # -------------------------------------------------------------
  # Inputs:
  #   df           : dataset
  #   response_var : binary outcome variable
  #   covariates   : predictor variables
  #   model_fun    : model fitting function (x, y)
  #   pred_fun     : prediction function
  # Output:
  #   Tibble with AUC and its confidence interval (ROC, lower, upper).
  
  set.seed(seed)
  
  df <- df %>%
    dplyr::filter(!is.na(.data[[response_var]])) %>%
    dplyr::mutate(!!response_var := factor(.data[[response_var]], levels = c(0, 1)))
  
  if (nrow(df) < 10 || length(unique(df[[response_var]])) < 2) {
    return(tibble::tibble(ROC = NA_real_, lower = NA_real_, upper = NA_real_))
  }
  
  n <- nrow(df)
  
  preds_all <- numeric()
  y_all <- numeric()
  
  for (r in 1:repeats) {
    
    set.seed(seed + r - 1)
    df$fold <- sample(rep(1:folds, length.out = n))
    
    for (k in 1:folds) {
      
      train <- df[df$fold != k, ]
      test  <- df[df$fold == k, ]
      
      x_train <- model.matrix(
        as.formula(paste("~", paste(covariates, collapse = " + "))),
        data = train
      )[, -1, drop = FALSE]
      
      y_train <- as.numeric(as.character(train[[response_var]]))
      
      model <- model_fun(x = x_train, y = y_train, ...)
      
      x_test <- model.matrix(
        as.formula(paste("~", paste(covariates, collapse = " + "))),
        data = test
      )[, -1, drop = FALSE]
      
      preds <- if (!is.null(pred_fun)) {
        pred_fun(model, x_test)
      } else {
        predict(model, newx = x_test, type = "response")
      }
      
      preds_all <- c(preds_all, preds)
      y_all <- c(y_all, as.numeric(as.character(test[[response_var]])))
    }
  }
  
  roc_obj <- pROC::roc(y_all, preds_all, ci = TRUE, quiet = TRUE)
  
  tibble::tibble(
    ROC = as.numeric(pROC::auc(roc_obj)),
    lower = roc_obj$ci[1],
    upper = roc_obj$ci[3]
  )
}

get_sens_spec <- function(df, response_var, covariates, model_fun, pred_fun = NULL,
                     folds = 5, seed = 123, boot.n = 2000, conf.level = 0.95, ...) {
  set.seed(seed)
  
  # Purpose: Estimate sensitivity and specificity at the Youden-optimal threshold.
  # -------------------------------------------------------------  
  # Inputs:
  #   df, response_var, covariates
  #   model_fun, pred_fun
  #   folds       : CV folds
  #   boot.n      : bootstrap iterations for CI
  # Output:
  #   Tibble with threshold, sensitivity, specificity, and bootstrap CIs.
  
  df <- df %>%
    dplyr::filter(!is.na(.data[[response_var]])) %>%
    dplyr::mutate(!!response_var := factor(.data[[response_var]], levels = c(0, 1)))
  
  if (nrow(df) < 10 || length(unique(df[[response_var]])) < 2) {
    return(tibble::tibble(
      threshold = NA_real_,
      Sensitivity = NA_real_, Sens_low = NA_real_, Sens_high = NA_real_,
      Specificity = NA_real_, Spec_low = NA_real_, Spec_high = NA_real_
    ))
  }
  
  df$fold <- sample(rep(1:folds, length.out = nrow(df)))
  preds_all <- rep(NA_real_, nrow(df))
  
  for (k in 1:folds) {
    train <- df[df$fold != k, , drop = FALSE]
    test  <- df[df$fold == k, , drop = FALSE]
    
    x_train <- model.matrix(
      stats::as.formula(paste("~", paste(covariates, collapse = " + "))),
      data = train
    )[, -1, drop = FALSE]
    
    y_train <- as.numeric(as.character(train[[response_var]]))
    model <- model_fun(x = x_train, y = y_train, ...)
    
    x_test <- model.matrix(
      stats::as.formula(paste("~", paste(covariates, collapse = " + "))),
      data = test
    )[, -1, drop = FALSE]
    
    preds <- if (!is.null(pred_fun)) pred_fun(model, x_test) else
      predict(model, newx = x_test, type = "response")
    
    preds_all[df$fold == k] <- as.numeric(preds)
  }
  
  y <- as.numeric(as.character(df[[response_var]]))
  roc_obj <- pROC::roc(y, preds_all, quiet = TRUE)
  
  best <- pROC::coords(
    roc_obj, x = "best", best.method = "youden",
    ret = c("threshold", "sensitivity", "specificity"),
    transpose = FALSE
  )
  if (is.data.frame(best) && nrow(best) > 1) best <- best[1, , drop = FALSE]
  th <- as.numeric(best[["threshold"]])
  
  pt <- pROC::coords(
    roc_obj, x = th, input = "threshold",
    ret = c("sensitivity", "specificity"),
    transpose = FALSE
  )
  if (is.data.frame(pt) && nrow(pt) > 1) pt <- pt[1, , drop = FALSE]
  sens_pt <- as.numeric(pt[["sensitivity"]])
  spec_pt <- as.numeric(pt[["specificity"]])
  
  ci_sens <- pROC::ci.coords(
    roc_obj, x = th, input = "threshold",
    ret = "sensitivity",
    boot.n = boot.n, conf.level = conf.level
  )
  ci_spec <- pROC::ci.coords(
    roc_obj, x = th, input = "threshold",
    ret = "specificity",
    boot.n = boot.n, conf.level = conf.level
  )
  
  ci_sens_vec <- as.numeric(ci_sens)
  ci_spec_vec <- as.numeric(ci_spec)
  
  sens_low  <- ci_both$sensitivity[1, "2.5%"]
  sens_high <- ci_both$sensitivity[1, "97.5%"]
  
  spec_low  <- ci_both$specificity[1, "2.5%"]
  spec_high <- ci_both$specificity[1, "97.5%"]
  
  tibble::tibble(
    threshold = th,
    Sensitivity = sens_pt,
    Sens_low = sens_low,
    Sens_high = sens_high,
    Specificity = spec_pt,
    Spec_low = spec_low,
    Spec_high = spec_high
  )
}

glm_fun <- function(x, y, ...) {
  df_tmp <- data.frame(y = y, x)
  glm(y ~ ., data = df_tmp, family = binomial(), ...)
}

glm_pred <- function(model, x) predict(model, newdata = data.frame(x), type = "response")

enr_fun <- function(x, y, ...) {
  alpha_grid <- seq(0, 1, length.out = 5)
  best_auc <- -Inf
  for (a in alpha_grid) {
    cvfit <- glmnet::cv.glmnet(x, y, alpha = a, family = "binomial", type.measure = "auc", ...)
    if (max(cvfit$cvm) > best_auc) {
      best_auc <- max(cvfit$cvm)
      best_model <- cvfit
    }
  }
  best_model
}
enr_pred <- function(model, x) predict(model, newx = x, s = "lambda.min", type = "response")

rf_fun <- function(x, y,
                   ntree = 300,
                   mtry_grid = NULL,
                   nodesize_grid = c(1, 5, 10),
                   ...) {
  
  x_df <- as.data.frame(x)
  y_f  <- factor(y)
  
  p <- ncol(x_df)
  
  if (is.null(mtry_grid)) {
    mtry_grid <- unique(pmax(1, round(c(
      sqrt(p),
      p/3,
      p/2
    ))))
  }
  
  best_oob <- Inf
  best_model <- NULL
  
  for (m in mtry_grid) {
    for (ns in nodesize_grid) {
      
      fit <- randomForest::randomForest(
        x = x_df,
        y = y_f,
        ntree = ntree,
        mtry = m,
        nodesize = ns,
        keep.forest = TRUE,
        ...
      )
      
      oob_err <- tail(fit$err.rate[, "OOB"], 1)
      
      if (is.finite(oob_err) && oob_err < best_oob) {
        best_oob <- oob_err
        best_model <- fit
      }
    }
  }
  
  best_model
}

rf_pred <- function(model, x) {
  predict(model, newdata = as.data.frame(x), type = "prob")[, 2]
}
