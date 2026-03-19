# Clear environment
rm(list = ls())

# Packages --------------------------------------------------------------------------------------------------------

library(tidyverse) # Data manipulation
library(here) # Project path
library(mice) # Imputation
library(rstan) # PSRF calculation
library(furrr) # Parallel processing

# Visualisation
library(scales)
library(GGally)

# Functions -------------------------------------------------------------------------------------------------------

# Find the script if you have not set the working directory
here::i_am("scripts/2-multiple-imputation.R")

# Source helpers
source(here("R", "utils.R"))

# Pre-processing --------------------------------------------------------------------------------------------------

# Import data
bcs70_eligible_analysis <- read_rds(here("data", "processed", "bcs70_eligible_analysis.rds"))

bcs70_analytical <- bcs70_eligible_analysis |>
  dplyr::select(
    # Remove ID variables
    -bcsid, -starts_with("nonresponse"), -starts_with("outcme"), -cumul_nonresponse_w9, -cumul_nonresponse_w10
  ) |>
  # Transformations before imputation
  mutate(
    # Log-transform net-pay
    net_pay_log_w6 = log(net_pay_w6)
  ) |>
  # Remove variables that will not be used in the analysis
  dplyr::select(-country, -parental_income_w3, -net_pay_w5, -net_pay_w6, -bmi_cat_w6, -malaise_w6)

glimpse(bcs70_analytical)

# Quick exploration - numeric variables

# bcs70_analytical |>
#   dplyr::select(malaise_w7, birthweight_w1, net_pay_log_w5, rutter_mother_score_w2,
#                 malaise_tot_w4, bmi_w6, parental_income_ridit_w3, net_pay_log_w6) |>
#   ggpairs(lower = list(continuous = wrap(ggally_smooth, method = "loess")),
#           upper = list(continuous = wrap("cor")))


# Set up imputations ----------------------------------------------------------------------------------------------


## Prediction matrix -----------------------------------------------------------------------------------------------

pred_matrix <- make.predictorMatrix(bcs70_analytical)

# Set up auxiliaries to impute missingness at each subsequent wave

# Get a list of variable names measured at each wave
wave_names_list <- map(1:9, ~ names(bcs70_analytical)[str_detect(names(bcs70_analytical), paste0("_w", .x))])
wave_names_list

# Create a list of variables measured up to each wave (cumulative)
# This was not used in the end, but kept for reference.
# wave_names_cumulative <- map(1:9, function(wave) {
#   # Create a pattern for waves up to the current one (cumulative)
#   cumulative_pattern <- paste0("_w", 1:wave)
#   # Select column names matching any of the patterns from wave 1 to the current wave
#   names(bcs70_analytical)[str_detect(names(bcs70_analytical), paste0(cumulative_pattern, collapse = "|"))]
# })
# wave_names_cumulative

# Set up nonresponse as auxiliary
cumul_nonresp_columns <- grepl("cumul", colnames(pred_matrix)) # Identify the columns
cumul_nonresp_column_names <- colnames(pred_matrix)[cumul_nonresp_columns] # Get their names
pred_matrix[cumul_nonresp_columns, ] <- 0 # Set rows to 0 (do not impute)
pred_matrix[, cumul_nonresp_columns] <- 0 # Set columns to 0 (do not use to impute other variables)

# Variables at each wave from 2 to 9 are imputed using cumulative non-response
# from the previous wave (lag of 1).
map2( # Iterate over
  .x = 2:9, # waves to impute
  .y = 1:8, # corresponding lagged waves for nonresponse
  ~ {
    pred_matrix[wave_names_list[[.x]], paste0("cumul_nonresponse_w", .y)] <<- 1 # Set predmat entries to 1 for [.x, .y]
  }
)

## Method vector ---------------------------------------------------------------------------------------------------

# Extract default methods used by mice
# mice does not recognise variable types in tibbles correctly, so the dataset must be a data frame.
meth <- make.method(as.data.frame(bcs70_analytical))
meth


## The visiting sequence --------------------------------------------------------------------------------------------

# It is best if imputations proceed in temporal order.

# Do an empty run of mice to extract the default visiting sequence
imp_viseq <- mice(bcs70_analytical, method = meth, predictorMatrix = pred_matrix, maxit = 0)
viseq <- imp_viseq$visitSequence
viseq

## Order by wave
viseq <- viseq[order(as.numeric(str_extract(viseq, "(?<=_w)\\d+")))]
viseq

## Sex should be visited first
viseq <- c("sex", viseq[!viseq %in% c("sex")])
viseq

## Impute W7 auxiliaries before the exposure
viseq <- move_var(viseq, "malaise_w7", "consent_parent_proj_w8", after = FALSE)
viseq

# Try running mice with 2 imputations and 2 iterations to check everything works
imp <- mice(bcs70_analytical,
  method = meth, predictorMatrix = pred_matrix, visitSequence = viseq, m = 2,
  maxit = 2, seed = 123
)

## Number of imputations -------------------------------------------------------------------------------------------

# Ideally, m = FMI * 100. This is unknown before imputation, so instead m is set to phi * 100,
# where phi is the percentage of incomplete rows. This can be fairly conservative,
# as the FMI can be reduced by adding auxiliary variables.

phi <- sum(!complete.cases(bcs70_analytical)) / nrow(bcs70_analytical)
m <- round(phi * 100, 0)
m # set m to 95

# Running multiple imputation -------------------------------------------------------------------------------------

## Pseudorandom number generator
# seed <- pseudorandom_seed()
# seed
# [1] 48922
seed <- 48922

# Run this on a machine with at least 6 cores for reproducibility.
# Number of cores may be adjusted, but this may result in random differences in the imputed values.
# For adjustments, good default can be number of available cores - 2.
# n_core <- parallel::detectCores() - 2

data_imp <- futuremice(
  data = bcs70_analytical,
  m = m,
  maxit = 30,
  parallelseed = seed,
  n.core = 6,
  future.plan = "multisession",
  method = meth,
  predictorMatrix = pred_matrix,
  visitSequence = viseq
)

write_rds(data_imp, here("data", "processed", "data_imp.rds"))

# Diagnostics -----------------------------------------------------------------------------------------------------

# Check convergence
get_psrf(data_imp)
# No PSRF > 1.1

plot(data_imp)

# Inspect the imputed datasets

## Density plots for interval and ratio variables

### All numeric variables
numeric_vars <- bcs70_analytical |>
  dplyr::select(where(is.numeric)) |>
  names()

### Create formula for all numeric variables
form_density <- as.formula(
  paste("~", paste(numeric_vars, collapse = " + "))
)

### Density plots for all numeric variables
### This takes some time to print.
densityplot(data_imp, form_density)

## Categorical variables

# plot_all_categorical_with_missing(imp_owd_70)
### This function is heavy on computations.
### You might wish to plot the variables one at a time, focusing on the variables of interest.
### Example:
plot_imputed_categorical(data_imp, "occupation_w6")
