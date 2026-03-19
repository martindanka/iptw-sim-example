# This code reuses the functions from the simulation pipeline.
# For a simpler code example, see 3-iptw-illustrative.R

# Clear environment
rm(list = ls())
invisible(gc())

# Packages --------------------------------------------------------------------------------------------------------

# Data manipulation
library(tidyverse)
library(broom)

# Imputation
library(mice)
library(mitools)

# Project path
library(here)

# Weighting and sandwich variance
library(survey)
library(WeightIt)
library(rootSolve) # A recommended WeightIt dependency, will improve optimisation with CBPS
library(sandwich)
library(lmtest)
library(cobalt) # Balance plots

# Parallel processing
library(future)
library(furrr)

# Tables and figures
library(flextable)
library(officer)

# Set up ----------------------------------------------------------------------------------------------------------

# Find the script if you have not set the working directory
here::i_am("scripts/4-iptw-main.R")
source(here("R", "utils.R"))

data_imp <- read_rds(here("data", "processed", "data_imp.rds"))
m <- data_imp$m

# Import functions from the simulation pipeline
# These functions are stored in local copies of several scripts used in the simulation pipeline.
source(here("R", "04-weighting.R"))
source(here("R", "05-balance.R"))
source(here("R", "06-modelling.R"))

# Prepare the Imputed Datasets
imp_list <- mice::complete(data_imp, "all", include = FALSE)

# Testing: Keep only six elements of the list (uncomment line below to only run a test with 6 imp datasets)
# imp_list <- imp_list[1:6]

# Convert outcome to numeric (0/1) - required by the quasipoisson family in svyglm.
imp_list <- imp_list |>
  map(~ mutate(.x, illness_w9 = as.integer(illness_w9) - 1))

# Define substantive variables to enter the analysis (post-imputation)
substantive_vars <- imp_list[[1]] |>
  dplyr::select(
    sex, birthweight_w1, smoking_pregnancy_w1, parental_class_w1, overcrowding_w2, tenure_w3, parental_education_w3,
    parental_income_ridit_w3, reading_raw_w3, maths_raw_w3, malaise_tot_w4, bmi_w6, partnership_w6, occupation_w6,
    net_pay_log_w6, exercise_total_w6, smoking_status_w6, alcohol_w6, drug_use_w6, ethnicity_w6, illness_w6, malaise_w7,
    illness_w9
  )

# Extract covariate names
preds_iptw <- substantive_vars |>
  dplyr::select(-illness_w9, -malaise_w7) |>
  names()

# Choose methods to request
methods_weights <- c("multinom", "CBPS", "npCBPS", "GBM") # Leaving out "energy" due to computational intensity.
methods_models <- dplyr::union(methods_weights, c("adjusted", "unadjusted")) # Weighted models + cov adjustment + crude.

# Parallel computation set up
n_core <- 6 # Specify the number of cores to use
# Requires a machine with at least 6 cores and sufficient RAM.
# Can also use:
# n_core <- parallel::detectCores() - 1
# If n_core != 6, the analysis may return slightly different results due to the analysis being run in parallel
# with a different number of workers (affecting the random seed streams).

plan(strategy = multisession, workers = n_core)

# Weight estimation -----------------------------------------------------------------------------------------------

# Takes several hours to run.

data_imp_weights <- imp_list |>
  future_map(
    ~ compute_weights(
      data = .x,
      exposure = "malaise_w7",
      covariates = preds_iptw,
      methods = methods_weights,
      collapse_prop_multinom = 0.01,
      keep_data = TRUE
    ),
    .options = furrr_options(seed = 39919), # Must define seed here for parallel processing.
    .progress = TRUE
  )

write_rds(
  x = data_imp_weights,
  file = here("data", "processed", "data_imp_weights.rds"),
  compress = "xz"
)

# Winsorised weights (at 99th percentile)
data_imp_weights_win <- data_imp_weights |>
  map(
    ~ winsorise(
      data = .x,
      wins_perc = 0.01, # Winsorise the top 1% of weights
      weight_cols = methods_weights
    )
  )

# Balance ---------------------------------------------------------------------------------------------------------

# Formula
formula_iptw <- reformulate(termlabels = preds_iptw, response = "malaise_w7")

# Convert from list of multiply imputed datasets to a long-format tibble with .imp indicator
# This is done because cobalt::bal.tab supports multiply imputed datasets in long format.
data_long_weights_raw <- list_rbind(data_imp_weights, names_to = ".imp") |>
  dplyr::mutate(.imp = as.integer(.imp))

data_long_weights_win <- list_rbind(data_imp_weights_win, names_to = ".imp") |>
  dplyr::mutate(.imp = as.integer(.imp))

# Combine into a list
data_long_list <- list(
  raw = data_long_weights_raw,
  win = data_long_weights_win
)

# Love plot using cobalt
balance_for_plots <- data_long_list |>
  map(
    ~ cobalt::bal.tab(
      formula_iptw,
      data = .x,
      weights = setdiff(methods_weights, "npCBPS"), # Remove npCBPS (wide range, not good for plotting)
      imp = ".imp", # Indicate imputation id column
      imp.summary = TRUE, # Keep the pooled summary
      stats = "correlation",
      un = TRUE, # Also get unadjusted correlations
      quick = FALSE, # Full set of balance statistics
      disp = NULL,
      abs = FALSE # Use absolute correlations
    ),
    .progress = TRUE
  )

love.plot(balance_for_plots[[1]], threshold = 0.1, var.order = "unadjusted")
love.plot(balance_for_plots[[2]], threshold = 0.1, var.order = "unadjusted")

# Love plot with npCBPS for supplementary figures
balance_for_plots_supl <- data_long_list |>
  map(
    ~ cobalt::bal.tab(
      formula_iptw,
      data = .x,
      weights = methods_weights, # Remove npCBPS (wide range, not good for plotting)
      imp = ".imp", # Indicate imputation id column
      imp.summary = TRUE, # Keep the pooled summary
      stats = "correlation",
      un = TRUE, # Also get unadjusted correlations
      quick = FALSE, # Full set of balance statistics
      disp = NULL,
      abs = FALSE # Use absolute correlations
    ),
    .progress = TRUE
  )

# Balance metrics for numeric summaries (absolute correlations)
balance_list <- data_long_list |>
  map(
    ~ cobalt::bal.tab(
      formula_iptw,
      data = .x,
      weights = methods_weights,
      imp = ".imp", # Indicate imputation id column
      imp.summary = TRUE, # Keep the pooled summary
      stats = "correlation",
      un = TRUE, # Also get unadjusted correlations
      quick = FALSE, # Full set of balance statistics
      disp = NULL,
      abs = TRUE # Use absolute correlations
    ),
    .progress = TRUE
  )

# Extract per-imputation correlations

## Extract correlations for each imputation
corr_all <- balance_list |>
  purrr::map( # Iterate over each element of the list (raw and win), returning results as a list.
    .f = \(bal_list) { # Placeholder for the balance object (one for raw, one for win).

      # For each balance object, iterate over each imputation within that object.
      bal_list$Imputation.Balance |>
        purrr::map(
          .f = \(imp) { # Placeholder for one imputation.

            # Extract the balance table for this imputation.
            imp$Balance |>
              tibble::as_tibble(rownames = "covariate") |> # Convert row names to a proper 'covariate' column.
              dplyr::select(
                covariate,
                dplyr::starts_with("Corr.") # Keep only correlation columns
              )
          }
        ) |>
        # After mapping over imputations, row-bind all imputations within a given set (raw / win)
        # and create a new column 'imp' with the imputation index.
        purrr::list_rbind(names_to = "imp")
    }
  ) |>
  # After mapping over sets (raw / win), row-bind the two sets together
  # and create a new column 'set' to indicate which set each row comes from.
  purrr::list_rbind(names_to = "set")

# Clean up the format
corr_all <- corr_all |>
  ## Pivot to long format
  tidyr::pivot_longer(
    -c(set, imp, covariate),
    names_to = "metric",
    values_to = "val"
  ) |>
  ## Split metrics into two columns: method (e.g. CBPS) and which (unadjusted vs adjusted)
  dplyr::mutate(method = if_else(metric == "Corr.Un", "unweighted", sub("^Corr\\.", "", metric))) |>
  dplyr::select(-metric)

# Obtain correlation metrics across imputations and covariates
corr_across <- corr_all |>
  summarise(
    mean = mean(val, na.rm = TRUE), # Same as averaging first across imps (pooling), then across covariates
    min = min(val, na.rm = TRUE),
    p5 = stats::quantile(val, 0.05, na.rm = TRUE),
    p95 = stats::quantile(val, 0.95, na.rm = TRUE),
    max = max(val, na.rm = TRUE),
    .by = c(set, method) # Summaries computed by set and method
  )

### Correlations > 1 for npCBPS are NOT a coding error.
### These occur when weights are extreme / degenerate, causing weighted covariance in the numerator
### to exceed the product of unweighted standard deviations in the denominator.
### This aligns with issues identified in simulations.
### It is rectified by winsorising the weights.

# Add effective sample size
ess_per_imp <- tidyr::crossing(
  set = c("raw", "win"),
  imp = seq_along(data_imp_weights),
  method = methods_weights
) |>
  dplyr::mutate(
    ess = purrr::pmap_dbl(
      .l = list(set, imp, method),
      .f = \(s, i, m) {
        dat <- if (s == "raw") data_imp_weights[[i]] else data_imp_weights_win[[i]]
        result <- WeightIt::ESS(dat[[m]])
      }
    )
  )

# Summarise ESS and merge with cors
ess_summary <- ess_per_imp |>
  summarise(
    mean_ess = mean(ess, na.rm = TRUE),
    min_ess = min(ess, na.rm = TRUE),
    p5_ess = stats::quantile(ess, 0.05, na.rm = TRUE),
    p95_ess = stats::quantile(ess, 0.95, na.rm = TRUE),
    max_ess = max(ess, na.rm = TRUE),
    .by = c(set, method)
  )

# Models ----------------------------------------------------------------------------------------------------------

models <- list(raw = data_imp_weights, win = data_imp_weights_win) |>
  future_map(
    ~ fit_models_mi(
      data_imp = .x,
      exposure = "malaise_w7",
      outcome = "illness_w9",
      covariates = preds_iptw,
      methods = methods_models,
      outcome_to_num = FALSE
    ),
    .progress = TRUE
  )

# Pooling -> modify the pooling function from 06-modelling.R to filter for the correct coefficient
pool_models <- function(fit_list) {
  imap_dfr(
    fit_list,
    ~ pool_method(models = .x, method = .y),
    .id = "method"
  ) |>
    dplyr::filter(str_detect(term, "malaise_w7"))
}

# Results per 1-point increase
results <- models |>
  map( # Iterate over raw/win
    ~pool_models(.x) |> # Pool models
      mutate(across(.cols = c(estimate, conf.low, conf.high), .fns = ~ exp(.x))) # Exponentiate coefficients
  )

# Results per 4-point increase
results_4pt <- models |>
  map(
    ~pool_models(.x) |>
      mutate(across(.cols = c(estimate, conf.low, conf.high), .fns = ~ exp(.x * 4)))
  )

# Export results --------------------------------------------------------------------------------------------------
list(
  results_per1pt = results,
  results_per4pt = results_4pt,
  balance_correlations = corr_all,
  balance_correlations_summary = corr_across,
  balance_ess_summary = ess_summary,
  balance_for_plots = balance_for_plots,
  balance_for_plots_supl = balance_for_plots_supl
) |>
  write_rds(
    file = here("data", "processed", "results.rds"),
    compress = "xz"
  )

