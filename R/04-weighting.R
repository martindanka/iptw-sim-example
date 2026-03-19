

# Default args ----------------------------------------------------------------------------------------------------

# Provided for reference
# DEFAULT_WEIGHT_ARGS <- list(
#   exposure = "exposure",
#   covariates = c("c1", "c2", "c3"),
#   methods = c("CBPS", "npCBPS", "energy", "multinom", "GBM"),
#   collapse_prop_multinom = 0.01,
#   keep_data = FALSE
# )

# Estimate weights for complete data -----------------------------------------------------------------------------

# This function estimates stabilised, un-winsorised weights for a single complete dataset.
compute_weights <- function(
    data,
    exposure,
    covariates,
    methods,
    collapse_prop_multinom = NULL,
    keep_data
    ) {

  # Toggle for methods
  all_methods <- c("CBPS", "npCBPS", "energy", "multinom", "GBM")
  methods <- match.arg(methods, all_methods, several.ok = TRUE)

  # Prepare output container
  weights_df <- data.frame(ID = seq_len(nrow(data)))
  fmla <- stats::as.formula(paste(exposure, "~", paste(covariates, collapse = "+")))

  # CBPS
  if ("CBPS" %in% methods) {
    fit <- WeightIt::weightit(fmla, data = data, method = "cbps", estimand = "ATE")
    weights_df$CBPS <- fit$weights
  }

  # npCBPS
  if ("npCBPS" %in% methods) {
    fit <- WeightIt::weightit(fmla, data = data, method = "npcbps", estimand = "ATE")
    weights_df$npCBPS <- fit$weights
  }

  # energy
  if ("energy" %in% methods) {
    fit <- WeightIt::weightit(fmla, data = data, method = "energy", estimand = "ATE")
    weights_df$energy <- fit$weights
  }

  # Binning with multinomial weights
  if ("multinom" %in% methods) {
    # Collapse exposure so that each category has at least 1% of the sample
    var_sym <- rlang::ensym(exposure)
    # Create a table with counts and proportions
    tab <- data |>
      dplyr::count(!!var_sym) |>
      dplyr::mutate(prop = n / sum(n))

    # Join the prop column to the original data
    data_multi <- data |>
      dplyr::left_join(tab, by = rlang::as_name(var_sym)) |>
      dplyr::mutate(
        exposure_multi = dplyr::case_when(
          prop < collapse_prop_multinom ~ "other",
          TRUE ~ as.character(!!var_sym)
        ),
        exposure_multi = factor(exposure_multi)
      )

    formula_multinom <- stats::as.formula(paste("exposure_multi ~", paste(covariates, collapse = "+")))

    # Compute weights using multinomial logistic regression
    multinom_fit <- WeightIt::weightit(
      formula_multinom,
      data = data_multi,
      method = "glm",
      estimand = "ATE",
      stabilize = TRUE
    )
   weights_df$multinom <- multinom_fit$weights
  }

  # Generalized boosted models
  if ("GBM" %in% methods) {
    fit <- WeightIt::weightit(fmla, data = data, method = "gbm", estimand = "ATE", criterion = "p.rms")
    weights_df$GBM <- fit$weights
  }

  # Assemble and return
  out <- dplyr::as_tibble(weights_df)
  if (keep_data) out <- dplyr::bind_cols(data, out)
  return(out)
}

# Function to winsorise the weights
winsorise <- function(
    data, 
    wins_perc, 
    weight_cols
    ) {
  data |>
    mutate(across(dplyr::any_of(weight_cols), ~ WeightIt::trim(.x, at = wins_perc))
    )
}


# Estimate weights for multiply imputed datasets ------------------------------------------------------------------

# Wrapper to apply weight computation across a list of imputed datasets 
compute_weights_mi <- function(
    data,
    exposure,
    covariates,
    methods,
    collapse_prop_multinom = NULL,
    keep_data
    ) {
  
  # Extract the list of imputed datasets from the mids object.
  imps_complete <- mice::complete(data, action = "all", include = FALSE)
  
  # Iterate over imputed datasets: Estimate the weights.
  imps_complete |>
    purrr::map(
      ~ compute_weights(
        .x,
        exposure,
        covariates,
        methods,
        collapse_prop_multinom,
        keep_data
      )
    )
}

