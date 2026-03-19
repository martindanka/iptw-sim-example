# General ---------------------------------------------------------------------------------------------------------

# Pseudorandom generator function
pseudorandom_seed <- function() {
  initial_seed <- Sys.time()
  initial_seed <- as.integer(initial_seed)
  the_seed <- initial_seed %% 100000
  print(the_seed)
}

# Data cleaning ---------------------------------------------------------------------------------------------------

# Function to read .dta files and clean variable names
read_and_clean <- function(file_path) {
  read_dta(file_path) |>
    janitor::clean_names()
}

# Function to set negative values to NA for variables coded as numeric
negative_to_na <- function(data) {
  data |>
    dplyr::mutate(across(where(is.numeric), ~ dplyr::case_when(.x < 0 ~ NA, .default = .x)))
}

# Function to zap Stata attributes
zap_stata <- function(data) {
  data |>
    haven::zap_labels() |> # drop value labels (e.g., 1 = "Yes", 2 = "No"); keep the numeric codes
    haven::zap_label() |> # drop the variable-level label (the long description of a column)
    haven::zap_formats() # drop Stata/SPSS/SAS display formats (e.g., %9.0g)
}

# Function to check for duplicates
check_duplicates <- function(data, var) {
  data |>
    dplyr::count({{ var }}) |>
    dplyr::filter(n > 1)
}

# Function to check missing data
count_missing <- function(data) {
  n <- nrow(data)

  tibble::tibble(
    Names = names(data),
    n_missing = purrr::map_int(data, ~ sum(is.na(.x))),
    n_complete = purrr::map_int(data, ~ sum(!is.na(.x)))
  ) |>
    dplyr::mutate(
      `% missing` = round(n_missing / n * 100, 1),
      `% complete` = n_complete / n * 100
    ) |>
    dplyr::select(
      Names,
      `N missing` = n_missing,
      `% missing`,
      `N complete` = n_complete,
      `% complete`
    ) |>
    dplyr::arrange(dplyr::desc(`N missing`))
}

# Imputations -----------------------------------------------------------------------------------------------------

# Function to move a variable in a visiting sequence in mice
move_var <- function(viseq, var, ref_var, after = TRUE) {
  viseq <- viseq[viseq != var] # Remove the variable from its current position
  ref_position <- which(viseq == ref_var) # Find the position of the reference variable
  viseq <- append(viseq, var, after = ifelse(after, ref_position, ref_position - 1)) # Insert variable after/before
  return(viseq)
}

# Function to inspect distributions of selected imputed categorical variables
# Updated plot_imputed_categorical function (unchanged)
plot_imputed_categorical <- function(imp_data, var_name) {
  # Ensure the variable name is a string
  var_name <- as.character(var_name)

  # Convert the imputed data to long format, including the original data
  complete_long <- complete(imp_data, action = "long", include = TRUE)

  # Extract the original data to identify missingness
  original_data <- complete_long %>%
    filter(.imp == 0) %>%
    mutate(
      missingness = if_else(is.na(.data[[var_name]]), "Missing", "Complete")
    ) %>%
    dplyr::select(.id, missingness)

  # Merge missingness indicator back into the complete long data
  complete_long <- complete_long %>%
    left_join(original_data, by = ".id") %>%
    mutate(
      imputation = if_else(.imp == 0, "Original Data", paste("Imputation", .imp)),
      imputation = factor(imputation, levels = unique(imputation)),
      missingness = factor(missingness, levels = c("Missing", "Complete"))
    )

  # Compute proportions manually
  proportion_data <- complete_long %>%
    filter(!is.na(.data[[var_name]])) %>%
    group_by(imputation, missingness) %>%
    mutate(total_count = n()) %>%
    ungroup() %>%
    group_by(imputation, missingness, category = as.factor(.data[[var_name]])) %>%
    summarise(
      count = n(),
      total_count = first(total_count),
      proportion = count / total_count,
      .groups = "drop"
    )

  # Plot the data with proportions and dodged bars
  plot <- proportion_data %>%
    ggplot(aes(
      x = missingness,
      y = proportion,
      fill = category
    )) +
    geom_col(
      position = position_dodge(width = 0.8),
      width = 0.7,
      color = "black"
    ) +
    facet_wrap(~imputation) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
      x = "Missingness",
      y = "Proportion",
      fill = var_name,
      title = paste(
        "Proportional Distribution of",
        var_name,
        "by Missingness and Imputation"
      )
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  return(plot)
}

# Extend this to all categorical variables
## This function will automatically identify all categorical variables with missing data and create a plot for each.
## Variables must be factors to be successfully identified.
## The function is heavy on computation, so you might want to check only a few variables at a time.

# Updated function to plot only categorical variables with missing data
plot_all_categorical_with_missing <- function(imp_data) {
  # Extract the original data
  original_data <- imp_data$data

  # Identify categorical variables (factors or character)
  categorical_vars <- names(Filter(function(x) is.factor(x) || is.character(x), original_data))

  # Further filter to variables that have missing data
  vars_with_missing <- categorical_vars[sapply(original_data[categorical_vars], function(x) any(is.na(x)))]

  # If no categorical variables with missing data are found, return a message
  if (length(vars_with_missing) == 0) {
    cat("No categorical variables with missing data found in the dataset.\n")
    return(NULL)
  }

  # Optionally, print the variables
  cat("Categorical variables with missing data identified:\n")
  print(vars_with_missing)

  # Create an empty list to store plots
  plot_list <- list()

  # Loop over categorical variables with missing data and create plots
  for (var_name in vars_with_missing) {
    cat("Creating plot for variable:", var_name, "\n")
    plot <- plot_imputed_categorical(imp_data, var_name)
    print(plot) # Display the plot
    plot_list[[var_name]] <- plot # Store the plot in the list
  }

  # Optionally, return the list of plots
  return(plot_list)
}

# Get PSRF
get_psrf <- function(imp_dat, filter = TRUE, threshold = 1.1) {
  niter <- imp_dat$iteration
  psrf_mean <- convergence(
    imp_dat,
    diagnostic = "psrf",
    parameter = "mean",
    it = 1:niter # Use all iterations
  )
  psrf_sd <- convergence(
    imp_dat,
    diagnostic = "psrf",
    parameter = "sd",
    it = 1:niter # Use all iterations
  )
  if (filter) {
    psrf_mean <- psrf_mean |>
      filter(psrf >= threshold & .it == niter)
    psrf_sd <- psrf_sd |>
      filter(psrf >= threshold & .it == niter)
  }
  cat("\nAll PSRF ≥", threshold, "for the mean (at final iteration):\n")
  print(psrf_mean)

  cat("\nAll PSRF ≥", threshold, "for the SD (at final iteration):\n")
  print(psrf_sd)
}

# Weighting -------------------------------------------------------------------------------------------------------

# Function to collapse exposure values with prevalence below a threshold
# This function is only meant to be used when values with low prevalence are consecutive.

collapse_exposure_levels <- function(data, var, threshold = 0.01) {
  var_sym <- rlang::ensym(var)
  # Create a table with counts and proportions
  tab <- data |>
    dplyr::count(!!var_sym) |>
    dplyr::mutate(prop = n / sum(n))

  # Join the prop column to the original data
  data_multi <- data |>
    dplyr::left_join(tab, by = rlang::as_name(var_sym)) |>
    dplyr::mutate(
      exposure_multi = dplyr::case_when(
        prop < threshold ~ "other",
        TRUE ~ as.character(!!var_sym)
      ),
      exposure_multi = factor(exposure_multi)
    )
  data_multi
}

# IPTW using MIte (e.g., Leyrat et al., 2019)
## In MIte, the propensity score is estimated in each imputed dataset. Next, the inverse probability weights are derived.
## A weighted regression model is fitted in each imputed dataset using a robust variance estimator.
## The point estimates and variances are combined using Rubin's rules.

# Define the pooling function
## The function assumes we have a list of imputed datasets, with each element being one of the imputed datasets.
## We also have a list of the CBPS models, with each element being the CBPS model fitted on the corresponding imputed dataset.

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
