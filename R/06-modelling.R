

# Models for complete data ----------------------------------------------------------------------------------------

# Weighted models 
fit_weighted_model <- function(
    data, 
    exposure, 
    outcome, 
    weights, 
    method = ""
    ) {
  weight_formula <- stats::as.formula(paste0("~", weights))
  # Create a survey design object using the weights
  design <- svydesign(ids = ~1, data = data, weights = weight_formula)

  # Define the formula for the outcome model (only exposure)
  outcome_formula <- as.formula(paste(outcome, "~", exposure))

  # Fit the weighted model using survey-weighted generalized linear model (GLM)
  model_weighted <- svyglm(
    outcome_formula,
    design = design,
    family = quasipoisson(link = "log")
  )

  # Extract the results for the exposure variable
  result_weighted <- tidy(model_weighted, conf.int = TRUE) |>
    filter(term == exposure) |>
    mutate(
      method = method
    ) |>
    dplyr::select(term, estimate, std.error, conf.low, conf.high, method)

  return(result_weighted)
}

# Unweighted model
fit_unadjusted_model <- function(data, exposure, outcome) {
  # Define the formula
  unadjusted_formula <- as.formula(paste(outcome, "~", exposure))

  # Fit the model using standard GLM
  model_unadj <- glm(
    unadjusted_formula,
    data = data,
    family = quasipoisson(link = "log")
  )

  # Adjust standard errors using sandwich estimator
  model_unadj_se <- coeftest(model_unadj, vcov = sandwich)

  # Extract the results for the exposure variable
  result_unadj <- tidy(model_unadj_se) |>
    filter(term == exposure) |>
    mutate(
      conf.low = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error,
      method = "unadjusted"
    ) |>
    dplyr::select(term, estimate, std.error, conf.low, conf.high, method)

  return(result_unadj)
}

# Outcome adjustment
fit_adjusted_model <- function(data, exposure, outcome, covariates) {
  # Define the formula
  adjusted_formula <- as.formula(paste(outcome, "~", exposure, "+", paste(covariates, collapse = "+")))

  # Fit the model using standard GLM
  model_adj <- glm(
    adjusted_formula,
    data = data,
    family = quasipoisson(link = "log")
  )

  # Extract the results for the exposure variable
  result_adj <- tidy(coeftest(model_adj, vcov = sandwich), conf.int = TRUE) |>
    filter(term == exposure) |>
    mutate(
      method = "adjusted"
    ) |>
    dplyr::select(term, estimate, std.error, conf.low, conf.high, method)

  return(result_adj)
}

# Results for all models
fit_all_models <- function(
    data,
    exposure,
    outcome,
    covariates,
    methods # A character vector of all methods to run.
) {
  
  # 1. Validate the requested methods
  # Define all possible methods this function knows how to run.
  all_possible_methods <- c("unadjusted", "adjusted", "CBPS", "npCBPS", "energy", "multinom", "GBM")
  
  # Find the intersection of requested methods and supported methods.
  methods_to_run <- intersect(methods, all_possible_methods)
  
  # Stop if no valid methods were requested.
  if (length(methods_to_run) == 0) {
    warning("No valid/supported methods were requested. Returning empty tibble.")
    return(tibble())
  }
  
  # 2. Initialize a list to store results
  results <- list()
  
  # 3. Fit unweighted models
  # Check if "unadjusted" was requested and run it.
  if ("unadjusted" %in% methods_to_run) {
    results[["unadjusted"]] <- fit_unadjusted_model(
      data = data,
      exposure = exposure,
      outcome = outcome
    )
  }
  
  # Check if "adjusted" was requested and run it.
  if ("adjusted" %in% methods_to_run) {
    results[["adjusted"]] <- fit_adjusted_model(
      data = data,
      exposure = exposure,
      outcome = outcome,
      covariates = covariates
    )
  }
  
  # 4. Fit All Requested Weighted Models
  # Identify which of the requested methods are IPTW methods.
  weighted_methods_to_run <- intersect(
    methods_to_run,
    c("CBPS", "npCBPS", "energy", "multinom", "GBM")
  )
  
  # Loop through only the requested weighted methods.
  for (m in weighted_methods_to_run) {
    # Sanity-check that the required weight column exists in the data.
    if (!m %in% names(data)) {
      warning(sprintf("Weight column '%s' not found in data. Skipping this method.", m))
      next # Skip to the next iteration of the loop.
    }
    
    # Call the weighted model fitter.
    results[[m]] <- fit_weighted_model(
      data = data,
      exposure = exposure,
      outcome = outcome,
      weights = m, # The weight column is named after the method.
      method = m   # The method label is also the name of the method.
    )
  }
  
  # 5. Combine all results into a single output tibble 
  dplyr::bind_rows(results)
}


# Models with MI --------------------------------------------------------------------------------------------------

# Helper: If the binary outcome is coded as factor, convert it into a 0/1 numeric
# This is needed because mice only lets method = "logreg" for binary variables coded as factors,
# whereas modified Poisson regression requires numeric 0/1. 
recode_outcome_to_binary <- function(df, outcome) {
  if (is.factor(df[[outcome]])) {
    df[[outcome]] <- as.integer(df[[outcome]]) - 1
  }
  df
}

# Fit one model with MI
## This function will return the model fit object for a single imputed dataset.
## The code does not reuse the previous functions, as the entire model fit object is required for pooling later.
fit_model_mi_single <- function(
    data,
    exposure,
    outcome,
    covariates,
    method,
    outcome_to_num = TRUE
    ) {
  
  # optionally recode factor → numeric
  if (outcome_to_num) {
    data <- recode_outcome_to_binary(data, outcome)
  }
  
  # Check if the method is supported
  is_weighted_method <- method %in% c("CBPS", "npCBPS", "energy", "multinom", "GBM")
  
  if (is_weighted_method) {
    if (!method %in% names(data)) {
      warning(sprintf("Weight column '%s' not found for method '%s'. Returning NA for this model fit.", method, method))
      return(NA)
    }
    weight_formula <- as.formula(paste0("~", method))
  } else {
    weight_formula <- ~1
  }
  
  des <- svydesign(ids = ~1, data = data, weights = weight_formula)
  
  # build the right RHS for the formula
  rhs <- switch(
    method,
    adjusted = paste(exposure, "+", paste(covariates, collapse = "+")),
    exposure
  )
  
  form <- as.formula(paste(outcome, "~", rhs))
  
  # fit and return the model object
  svyglm(form, design = des, family = quasipoisson(link = "log"))
}

# Fit MI Models
# This function fits the models to all imputed datasets in a list (data_imp). 
fit_models_mi <- function(
    data_imp,
    exposure,
    outcome,
    covariates,
    methods,
    outcome_to_num = TRUE
    ) {
  # validate
  if (!is.list(data_imp) || length(data_imp) == 0) {
    stop("`data_imp` must be a non-empty list of data.frames")
  }
  
  # for each method, map over imputations
  set_names(methods) |>
    map(
      ~ map(
        data_imp,
        fit_model_mi_single,
        exposure = exposure,
        outcome = outcome,
        covariates = covariates,
        method = .x,
        outcome_to_num = outcome_to_num
        )
    )
  }

# Pool the results for one method
# This is a "worker" function that takes a list of raw model objects all corresponding to a single analysis method 
# (e.g. a list where the elements are model fits for a single method). 
# For example use, see the test files.
pool_method <- function(models, method) {
  coefs <- map(models, coef)
  vars <- map(models, vcov)
  
  pooled <- MIcombine(coefs, vars)
  est <- pooled$coefficients
  var_t <- pooled$variance
  se <- sqrt(diag(var_t))
  df_tot <- pooled$df
  
  tstat <- est / se
  pval <- 2 * pt(-abs(tstat), df = df_tot)
  tcrit <- qt(0.975, df = df_tot)
  ci_lo <- est - tcrit * se
  ci_hi <- est + tcrit * se
  
  tibble(
    term = names(est),
    estimate = unname(est),
    std.error = unname(se),
    df = df_tot,
    tval = unname(tstat),
    pval = unname(pval),
    conf.low = unname(ci_lo),
    conf.high = unname(ci_hi),
    method = method
  )
}

# Pool *all* methods into one table
# This is the primary entry point for pooling. It takes a nested list, where names are the methods
# and each element is a list of model fits for that method.
# The function then iterates over each method, applies pool_method, and returns a tibble with pooled results.
pool_models <- function(fit_list) {
  imap_dfr(
    fit_list,
    ~ pool_method(models = .x, method = .y),
    .id = "method"
  ) |>
    dplyr::filter(str_detect(term, "exposure"))
}
