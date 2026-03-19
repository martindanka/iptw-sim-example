# This script demonstrates how to run the methods on the motivating example.
# The code is kept deliberately simple for illustration.
# It is organised into two parts:
#   1. A single test run using one imputed dataset.
#   2. A test run using multiply imputed datasets.
#
# We first show how to obtain weights using each method.
# Steps that are common across methods are shown only once (using CBPS as the example).
#
# Note:
# - This script is designed for illustration rather than efficiency.
# - The full analysis is available in 4-iptw-main.R.

# Clear environment
rm(list = ls())

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

# Set up ----------------------------------------------------------------------------------------------------------

# Find the script if you have not set the working directory
here::i_am("scripts/3-iptw-illustrative.R")
source(here("R", "utils.R"))

data_imp <- read_rds(here("data", "processed", "data_imp.rds"))
m <- data_imp$m

# Part 1 - Single imputed dataset ---------------------------------------------------------------------------------

# Randomly select an imputed dataset to work with
# pseudorandom_seed()
# [1] 27350

set.seed(27350)
imp_num <- sample(1:m, 1)
imp_num

# Extract one imputed dataset
data_imp_single <- mice::complete(data_imp, action = imp_num)
glimpse(data_imp_single)

# Recode outcome variable type to numeric 0/1 (accepted by the Poisson family)
data_imp_single <- data_imp_single |>
  mutate(illness_w9 = as.integer(illness_w9) - 1)

# Select substantive variables for analysis - will be used for equations
data_substantive_vars <- data_imp_single |>
  dplyr::select(
    sex, birthweight_w1, smoking_pregnancy_w1, parental_class_w1, overcrowding_w2, tenure_w3, parental_education_w3,
    parental_income_ridit_w3, reading_raw_w3, maths_raw_w3, malaise_tot_w4, bmi_w6, partnership_w6, occupation_w6,
    net_pay_log_w6, exercise_total_w6, smoking_status_w6, alcohol_w6, drug_use_w6, ethnicity_w6, illness_w6, malaise_w7,
    illness_w9
  )

## Weight estimation -----------------------------------------------------------------------------------------------

# Exposure model formula: malaise_w7 ~ C
preds_iptw <- data_substantive_vars |>
  dplyr::select(-illness_w9, -malaise_w7) |>
  names()
formula_iptw <- reformulate(termlabels = preds_iptw, response = "malaise_w7")
formula_iptw

# Estimate weights

# To skip re-running the following block, load pre-computed weights:
# iptw_weights_single <- read_rds(here("data", "processed", "iptw_weights_single.rds"))
# weights_cbps <- iptw_weights_single$cbps
# weights_npcbps <- iptw_weights_single$npcbps
# weights_multinom <- iptw_weights_single$multinom
# weights_gbm <- iptw_weights_single$gbm

## CBPS
weights_cbps <- weightit(formula = formula_iptw, data = data_imp_single, method = "cbps", estimand = "ATE")

## npCBPS - can be slow, expect tens of minutes
weights_npcbps <- weightit(formula = formula_iptw, data = data_imp_single, method = "npcbps", estimand = "ATE")

## GBM - expect around 10 minutes
weights_gbm <- weightit(formula = formula_iptw, data = data_imp_single, method = "gbm", estimand = "ATE", criterion = "p.rms")

## Energy balancing
# Removed for time considerations, absolute time for this single run was slightly below 1 hour, can be RAM intensive.
# weights_energy <- weightit(formula = formula_iptw, data = data_imp_single, method = "energy", estimand = "ATE")

## Multinomial weights

### Collapse rare exposure levels (<1% prevalence) into 'Other' for multinomial IPTW.
### In this imputed dataset, this leaves the original 0-9 scale unchanged, since only one level is <1%.
### This procedure should be adapted based on the actual exposure distribution.
### For example, if rare levels are present at both tails, those should not be lumped together.
data_multi <- data_imp_single |>
  mutate(
    exposure_multi = fct_lump_prop(
      f = factor(malaise_w7),
      prop = 0.01,
      other_level = "Other"
    )
  )

formula_iptw_multi <- reformulate(preds_iptw, response = "exposure_multi")

weights_multinom <- weightit(
  formula = formula_iptw_multi,
  data = data_multi,
  method = "glm",
  estimand = "ATE",
  stabilize = TRUE # Stabilisation must be explicitly requested (unlike for the other methods)
)

# Save weights for re-running
# list(
#   cbps = weights_cbps,
#   npcbps = weights_npcbps,
#   multinom = weights_multinom,
#   gbm = weights_gbm
# ) |>
#   write_rds(here("data", "processed", "iptw_weights_single.rds"), compress = "gz")

## Weight diagnostics ----------------------------------------------------------------------------------------------

# The WeightIt object provides useful summaries
summary(weights_cbps) # example, includes ESS

# Love plots for checking balance

## Per method (shown for CBPS)
love.plot(weights_cbps, var.order = "unadjusted")

## All methods together
love.plot(
  formula_iptw,
  data = data_imp_single,
  weights = list(
    CBPS = weights_cbps,
    npCBPS = weights_npcbps,
    Multinom = weights_multinom,
    GBM = weights_gbm
  ),
  var.order = "unadjusted"
)

# Weighted treatment-covariate correlations
balance <- bal.tab(weights_cbps)
balance
mean(abs(balance$Balance$Corr.Adj))
quantile(abs(balance$Balance$Corr.Adj), probs = c(0.05, 0.95))

# Alternatively, this can also be called as:
# data_imp_single2 <- data_imp_single |>
#   mutate(weights = weights_cbps$weights)
#
# balance <- bal.tab(
#   formula_iptw,
#   data = data_imp_single2,
#   weights = weights_cbps$weights
# )
#
# balance$Balance

# Distance correlations
## This was not used in the motivating example due to being computation-intensive.
# bal.tab(weights_cbps, stats = "distance.correlations")

## The estimator ---------------------------------------------------------------------------------------------------

### Weighted modified Poisson regression ###

## Define a survey design object with CBPS weights
design_cbps <- svydesign(ids = ~1, data = data_imp_single, weights = weights_cbps$weights)

## Fit the weighted Poisson regression model
model_cbps <- svyglm(illness_w9 ~ malaise_w7, design = design_cbps, family = quasipoisson(link = "log"))

## Extract effect estimate per 1-point increase in Malaise: RR (exp coefficient for malaise_w7)
results_cbps <- model_cbps |>
  tidy(conf.int = TRUE) |>
  filter(term == "malaise_w7") |>
  mutate(
    estimate = exp(estimate),
    conf.low = exp(conf.low),
    conf.high = exp(conf.high)
  )

results_cbps

## Note: The survey package uses sandwich variance estimation by default for quasipoisson models.

## Express per 4-point increase
results_cbps_4pt <- results_cbps |>
  mutate(
    estimate = estimate^4,
    conf.low = conf.low^4,
    conf.high = conf.high^4
  )

results_cbps_4pt

### Modified Poisson regression with simple covariate adjustment ###

## Formula: illness_w9 ~ malaise_w7 + C
preds_outcome_adjustment <- data_substantive_vars |>
  dplyr::select(-illness_w9) |>
  names()
formula_outcome_adjustment <- reformulate(preds_outcome_adjustment, response = "illness_w9")
formula_outcome_adjustment

## Model: modified Poisson regression
outcome_adjustment <- glm(formula_outcome_adjustment, data = data_imp_single, family = quasipoisson(link = "log"))

# Effect estimate: RR (exp coefficient for malaise_w7)
results_adjusted <- tidy(coeftest(outcome_adjustment, vcov = sandwich), conf.int = TRUE) |> # use sandwich variance
  mutate(
    estimate = exp(estimate),
    conf.low = exp(conf.low),
    conf.high = exp(conf.high)
  ) |>
  filter(term == "malaise_w7")

results_adjusted

## Express per 4-point increase
results_adjusted_4pt <- results_adjusted |>
  mutate(
    estimate = estimate^4,
    conf.low = conf.low^4,
    conf.high = conf.high^4
  )

results_adjusted_4pt

# Part 2 - Multiply imputed datasets ---------------------------------------------------------------------------------

# Prepare the Imputed Datasets
imp_long <- mice::complete(data_imp, "long", include = FALSE)
imp_list <- map(1:m, ~ imp_long |> filter(.imp == .x)) # Convert to list of data frames

# Use only 12 imputed datasets for testing
imp_list <- imp_list[1:12]

# Parallel computation set up
n_core <- 6 # Specify the number of cores to use
# Requires a machine with at least 6 cores and sufficient RAM.
# Can also use:
# n_core <- parallel::detectCores() - 1
# If n_core != 6, the analysis may return slightly different results due to the analysis being run in parallel
# with a different number of workers (affecting the random seed streams).

plan(strategy = multisession, workers = n_core)

## Weight estimation -----------------------------------------------------------------------------------------------

# Example shown with CBPS
imp_weights_cbps <- future_map(
  imp_list,
  ~ weightit(
    formula_iptw,
    data = .x,
    method = "cbps",
    estimand = "ATE",
  ),
  .options = furrr_options(seed = 39919) # Must define seed here for parallel processing.
)

# Append a weight column to each imputed dataset
imp_long_weights <- map2(
  imp_list,
  imp_weights_cbps,
  ~ .x |>
    mutate(
      weights = .y$weights,
      illness_w9 = (as.integer(illness_w9) - 1)
    )
)

# Check the first imputed dataset to see that weights have been added.
glimpse(imp_long_weights[[1]])

## Weight diagnostics ----------------------------------------------------------------------------------------------

# 1) Bind imputations and add .imp id
imp_long_weights_df <- imp_long_weights |>
  list_rbind(names_to = ".imp") |> # bind rows from lists, add .imp column for imputation id
  dplyr::mutate(.imp = as.integer(.imp)) # convert .imp to integer

# 2) Balance assessment using cobalt's bal.tab for multiply imputed data
bal_mi <- cobalt::bal.tab(
  formula_iptw,
  data = imp_long_weights_df,
  weights = "weights",
  imp = ".imp", # Indicate imputation id column
  imp.summary = TRUE, # Keep the pooled summary
  stats = "correlation",
  un = TRUE, # Also get unadjusted correlations
  quick = FALSE, # Full set of balance statistics
  disp = NULL,
  abs = TRUE # Use absolute correlations
)

# 3) Love plot of pooled balance
love.plot(bal_mi)

# 4) Inspect balance across imputations and covariates

## Extract all per-imputation balance tables into one long tibble
balance_per_imp <- map(
  bal_mi$Imputation.Balance,
  ~ as_tibble(.x$Balance, rownames = "covariate")
) |>
  list_rbind(names_to = "Imputation") |>
  dplyr::mutate(Imputation = as.integer(Imputation))

## Compute global summaries across imputations and covariates
corr_summary <- balance_per_imp |>
  summarise(
    Mean_Abs_Corr = mean(Corr.Adj, na.rm = TRUE),
    Min_Abs_Corr = min(Corr.Adj, na.rm = TRUE),
    Max_Abs_Corr = max(Corr.Adj, na.rm = TRUE),
    p5 = quantile(Corr.Adj, 0.05, na.rm = TRUE),
    p95 = quantile(Corr.Adj, 0.95, na.rm = TRUE)
  )

corr_summary

# 5) Effective sample size

## ESS per imputed dataset
ess_per_imp <- imp_long_weights |>
  map(~ WeightIt::ESS(w = .x$weights))

## Summaries
ess_summary <- tibble(
  Imp = 1:length(ess_per_imp),
  ESS = unlist(ess_per_imp)
) |>
  summarise(
    Mean_ESS = mean(ESS),
    Min_ESS = min(ESS),
    Max_ESS = max(ESS),
    p5 = quantile(ESS, probs = 0.05),
    p95 = quantile(ESS, probs = 0.95)
  )

ess_summary

## Estimator: Weighted regression ----------------------------------------------------------------------------------

# Function to fit svyglm models and pool results
fit_and_pool_models <- function(imp_data_list_with_weights, model_formula) {
  # Fit svyglm models over the list of datasets
  svyglm_models <- map(
    imp_data_list_with_weights,
    ~ {
      des <- svydesign(ids = ~1, data = .x, weights = ~weights)
      svyglm(model_formula, design = des, family = quasipoisson(link = "log"))
    }
  )

  # Extract estimates and variance-covariance matrices
  estimates_list <- map(svyglm_models, coef)
  variance_list <- map(svyglm_models, vcov)

  # Combine estimates and variances using MIcombine
  pooled_results <- MIcombine(estimates_list, variance_list)

  # Extract pooled coefficients and variance
  pooled_coefficients <- pooled_results$coefficients
  total_variance <- pooled_results$variance

  # Calculate standard errors
  pooled_se <- sqrt(diag(total_variance))

  # Degrees of freedom
  df_total <- pooled_results$df

  # Calculate t-statistics
  t_stats <- pooled_coefficients / pooled_se

  # Calculate p-values using t-distribution with df_total degrees of freedom
  p_values <- 2 * pt(-abs(t_stats), df = df_total)

  # Confidence intervals
  conf_level <- 0.95
  alpha <- 1 - conf_level
  t_crit <- qt(1 - alpha / 2, df = df_total)
  lower_ci <- pooled_coefficients - t_crit * pooled_se
  upper_ci <- pooled_coefficients + t_crit * pooled_se

  # Create a summary data frame
  summary_df <- tibble(
    Parameter = names(pooled_coefficients),
    Estimate = pooled_coefficients,
    StdError = pooled_se,
    DF = df_total,
    tValue = t_stats,
    pValue = p_values,
    CILower = lower_ci,
    CIUpper = upper_ci
  )

  return(summary_df)
}

# Run the function to fit and pool models
pooled_results <- fit_and_pool_models(
  imp_data_list_with_weights = imp_long_weights,
  model_formula = as.formula("illness_w9 ~ malaise_w7")
)

# Display pooled results (exponentiated)
pooled_results_exp <- pooled_results |>
  filter(Parameter == "malaise_w7") |>
  mutate(
    Estimate = exp(Estimate),
    CILower = exp(CILower),
    CIUpper = exp(CIUpper)
  )

pooled_results_exp

## Express per 4-point increase
pooled_results_exp_4pt <- pooled_results |>
  filter(Parameter == "malaise_w7") |>
  mutate(
    Estimate = exp(Estimate * 4),
    CILower = exp(CILower * 4),
    CIUpper = exp(CIUpper * 4)
  )
pooled_results_exp_4pt
