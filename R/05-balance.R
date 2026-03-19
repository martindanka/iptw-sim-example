
# Balance diagnostics ---------------------------------------------------------------------------------------------


## Weighted correlations -------------------------------------------------------------------------------------------

# Helper to estimate weighted correlations for a SINGLE set of weights.
estimate_cors <- function(
    data,
    exposure,
    covariates,
    bin_vars, # A logical vector specifying which covariates are binary.
    weights
) {
  
  # Pull exposure column (bare name or string both OK)
  treat <- dplyr::pull(data, {{ exposure }})
  
  # Pull covariates – accept a character vector
  covs <- data |>
    dplyr::select(all_of(covariates)) |>
    dplyr::mutate(across(where(is.factor), ~ as.numeric(.x) - 1L)) |>
    as.matrix()
  
  # pull weights column (bare name or string both OK)
  wts <- dplyr::pull(data, {{ weights }})
  
  # compute weighted correlations
  cors <- cobalt::col_w_corr(
    mat = covs,
    treat = treat,
    bin.vars = bin_vars,
    weights = wts
  )
  
  tibble(
    var = colnames(covs),
    cor = cors
  )
}

# Wrapper to estimate weighted correlations for ALL specified methods.
estimate_cors_all <- function(
    data,
    exposure,
    covariates,
    bin_vars,
    methods,
    warn_if_missing = TRUE
) {
  
  # --- 1. Setup and Validation (Unchanged) ---
  if (!is.null(bin_vars) && length(bin_vars) != length(covariates)) {
    stop("`bin_vars` must be the same length as `covariates`.")
  }
  
  present <- intersect(methods, names(data))
  missing <- setdiff(methods, names(data))
  
  if (length(missing) && warn_if_missing) {
    warning("Skipping weight columns not found in `data`: ", paste(missing, collapse = ", "))
  }
  
  if (!length(present)) {
    stop("None of the requested `methods` are present in the data.")
  }
  
  # --- 2. Loop over methods and call the core helper function ---
  # This block is now much simpler and has no duplicated logic.
  purrr::map_dfr(present, ~{
    
    # For each method name (`.x`), call the simple, single-method function.
    # We pass the method name string '.x' as the `weights` argument.
    estimate_cors(
      data = data,
      exposure = exposure,
      covariates = covariates,
      bin_vars = bin_vars,
      weights = .x
    ) %>%
      # Add the identifying columns back to the result to match the original output.
      mutate(method = .x, .before = 1)
  })
}

# Wrapper to estimate and pool correlations across imputed datasets
estimate_cors_all_mi <- function(
    data_list,
    exposure,
    covariates,
    bin_vars,
    methods,
    pool,
    warn_if_missing = TRUE
) {
  
  treat_quo <- enquo(exposure) # capture the bare column name if supplied
  
  # Calculate correlations for each imputation.
  out <- imap_dfr(data_list, \(dat, imp) { # .y (imp) is the imputation index
    estimate_cors_all(
      data = dat,
      exposure = !!treat_quo,
      covariates = covariates,
      bin_vars = bin_vars,
      methods = methods,
      warn_if_missing = warn_if_missing
    ) |>
      mutate(.imp = imp, .before = 1)
  })
  
  # If requested, pool the results by taking the average correlation.
  if (pool) {
    out |>
      group_by(method, var) |>
      summarise(
        cor = mean(cor),
        .groups = "drop"
      )
  } else {
    out
  }
}

# Energy balance metrics ------------------------------------------------------------------------------------------

# Helper to calculate energy statistics for a SINGLE set of weights.
# This is a modified version of the source code from the `independenceWeights` package.
# Modifications were made to make the function accept a tibble and return a compact tibble output.

energy_stats <- function(exposure, covariates, weights, data, dimension_adj = TRUE) {
  ### MODIFICATIONS FOR PRE-PROCESSING ### 
  # Extract covariates as a matrix
  X <- data |>
    dplyr::select({{ covariates }}) |>
    as.matrix()
  
  # Extract exposure as a numeric vector
  A <- data |>
    dplyr::pull({{ exposure }}) |>
    as.numeric()
  
  # Extract weights as a numeric vector
  weights <- data |>
    dplyr::pull({{ weights }}) |>
    as.numeric()
  
  ### START OF THE ORIGINAL CODE ###
  
  Xdist <- as.matrix(dist(X))
  Adist <- as.matrix(dist(A))
  
  gamma <- 1
  
  ## Normalize weights
  weights <- weights / mean(weights)
  
  n <- NROW(A)
  p <- NCOL(X)
  
  ## Terms for energy-dist(Wtd A, A)
  Q_energy_A <- -Adist / n^2
  aa_energy_A <- 1 * as.vector(rowSums(Adist)) / (n^2)
  
  ## Terms for energy-dist(Wtd X, X)
  Q_energy_X <- -Xdist / n^2
  aa_energy_X <- 1 * as.vector(rowSums(Xdist)) / (n^2)
  
  mean_Adist <- mean(Adist)
  mean_Xdist <- mean(Xdist)
  
  Xmeans <- colMeans(Xdist)
  Xgrand_mean <- mean(Xmeans)
  XA <- Xdist + Xgrand_mean - outer(Xmeans, Xmeans, "+")
  
  Ameans <- colMeans(Adist)
  Agrand_mean <- mean(Ameans)
  AA <- Adist + Agrand_mean - outer(Ameans, Ameans, "+")
  
  ## Quadratic term for weighted total distance covariance
  P <- XA * AA / n^2
  
  if (dimension_adj) {
    Q_energy_A_adj <- 1 / sqrt(p)
    Q_energy_X_adj <- 1
    
    sum_adj <- Q_energy_A_adj + Q_energy_X_adj
    
    Q_energy_A_adj <- Q_energy_A_adj / sum_adj
    Q_energy_X_adj <- Q_energy_X_adj / sum_adj
  } else {
    Q_energy_A_adj <- Q_energy_X_adj <- 1 / 2
  }
  
  ## Quadratic part of the overall objective function
  QM <- (P + gamma * (Q_energy_A * Q_energy_A_adj + Q_energy_X * Q_energy_X_adj))
  quadpart <- drop(t(weights) %*% QM %*% weights)
  
  ## Linear part of the overall objective function
  qvec <- 2 * gamma * (aa_energy_A * Q_energy_A_adj + aa_energy_X * Q_energy_X_adj)
  linpart <- drop(weights %*% qvec)
  
  ## Objective function
  objective_history <- quadpart + linpart + gamma * (-1 * mean_Xdist * Q_energy_X_adj - mean_Adist * Q_energy_A_adj)
  
  qvec_full <- 2 * (aa_energy_A * Q_energy_A_adj + aa_energy_X * Q_energy_X_adj)
  
  quadpart_energy_A <- drop(t(weights) %*% Q_energy_A %*% weights) * Q_energy_A_adj
  quadpart_energy_X <- drop(t(weights) %*% Q_energy_X %*% weights) * Q_energy_X_adj
  
  quadpart_energy <- quadpart_energy_A * Q_energy_A_adj + quadpart_energy_X * Q_energy_X_adj
  
  distcov_history <- drop(t(weights) %*% P %*% weights)
  
  linpart_energy <- drop(weights %*% qvec_full)
  linpart_energy_A <- 2 * drop(weights %*% aa_energy_A) * Q_energy_A_adj
  linpart_energy_X <- 2 * drop(weights %*% aa_energy_X) * Q_energy_X_adj
  
  ## Sum of energy-dist(wtd A, A) + energy-dist(wtd X, X)
  energy_history <- quadpart_energy + linpart_energy - mean_Xdist * Q_energy_X_adj - mean_Adist * Q_energy_A_adj
  
  ## Energy-dist(wtd A, A)
  energy_A <- quadpart_energy_A + linpart_energy_A - mean_Adist * Q_energy_A_adj
  
  ## Energy-dist(wtd X, X)
  energy_X <- quadpart_energy_X + linpart_energy_X - mean_Xdist * Q_energy_X_adj
  
  ess <- (sum(weights))^2 / sum(weights^2)
  
  ### MODIFICATION: EXPORT AS A TIBBLE INSTEAD ###
  
  retobj <- tibble(
    D_w = objective_history, # The actual objective function value
    distcov_unweighted = sum(P),
    distcov_weighted = distcov_history, # Weighted total distance covariance
    energy_A = energy_A, # Energy(Wtd Treatment, Treatment)
    energy_X = energy_X, # Energy(Wtd X, X)
    ess = ess # Effective sample size
  )
  
  # retobj <- list(
  #   D_w = objective_history,            # The actual objective function value
  #   distcov_unweighted = sum(P),
  #   distcov_weighted = distcov_history, # Weighted total distance covariance
  #   energy_A = energy_A,                # Energy(Wtd Treatment, Treatment)
  #   energy_X = energy_X,                # Energy(Wtd X, X)
  #   ess = ess                           # Effective sample size
  # )
  #
  
  return(retobj)
}

# Wrapper to calculate energy stats for ALL specified methods.
energy_stats_all <- function(
    data,
    exposure,
    covariates,
    methods,
    dimension_adj = TRUE,
    warn_if_missing = TRUE
) {
  ## Keep present methods, warn (optionally) about the rest
  present <- intersect(methods, names(data))
  absent <- setdiff(methods, names(data))
  
  if (length(absent) && warn_if_missing) {
    warning(
      "Skipping weight columns not in `data`: ",
      paste(absent, collapse = ", ")
    )
  }
  
  if (!length(present)) {
    stop("None of the requested `methods` are present in the data.")
  }
  
  # Loop over present methods
  map_dfr(present, \(m) {
    result <- energy_stats(
      exposure = {{ exposure }},
      covariates = all_of(covariates), # turns a character vector into a tidy-select spec
      weights = !!sym(m), # convert column name (string) → symbol
      data = data,
      dimension_adj = dimension_adj
    )
    
    result |>
      mutate(
        method = m,
        .before = 1
      ) # add identifying column
  })
}

# Wrapper to calculate energy stats for imputed datasets.
energy_stats_all_mi <- function(
    data_list,
    exposure,
    covariates,
    methods,
    dimension_adj = TRUE,
    warn_if_missing = TRUE,
    pool = TRUE
) {
  
  treat_quo <- enquo(exposure)
  
  # Calculate stats for each imputation.
  out <- imap_dfr(data_list, \(dat, imp) {
    energy_stats_all(
      data = dat,
      exposure = !!treat_quo,
      covariates = covariates,
      methods = methods,
      dimension_adj = dimension_adj,
      warn_if_missing = warn_if_missing
    ) |>
      mutate(.imp = imp, .before = 1)
  })
  
  # If requested, pool by taking the average of each statistic.
  if (pool) {
    out |>
      group_by(method) |>
      summarise(
        D_w = mean(D_w),
        distcov_unweighted = mean(distcov_unweighted),
        distcov_weighted = mean(distcov_weighted),
        energy_A = mean(energy_A),
        energy_X = mean(energy_X),
        ess = mean(ess),
        .groups = "drop"
      )
  } else {
    out
  }
}
