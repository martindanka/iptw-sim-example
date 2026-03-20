# This script produces result summaries, tables, and figures from the analysis.

# Clear environment
rm(list = ls())
invisible(gc())

# Packages --------------------------------------------------------------------------------------------------------

library(tidyverse)
library(here)
library(scales) # for comma() etc.
library(cobalt) # love.plot
library(glue)
library(kableExtra)

# Set up ----------------------------------------------------------------------------------------------------------

here::i_am("scripts/5-results-tables-figures.R")

source(here("R", "utils.R"))
source(here("R", "gen_desc_table.R"))
source(here("R", "report_helpers.R"))

dir.create(here("results", "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("results", "figures"), recursive = TRUE, showWarnings = FALSE)

# Import data from analysis ---------------------------------------------------------------------------------------

bcs70_eligible_descriptive <- read_rds(
  here("data", "processed", "bcs70_eligible_descriptive.rds")
)

results_obj <- read_rds(
  here("data", "processed", "results.rds")
)

# results_obj should contain:
# - results_per1pt
# - results_per4pt
# - balance_correlations
# - balance_correlations_summary
# - balance_ess_summary
# - balance_for_plots

# Data preparation -------------------------------------------------------------------------------------------------

bcs70_descriptive_selected <- bcs70_eligible_descriptive |>
  mutate(net_pay_log_w6 = log(net_pay_w6)) |>
  dplyr::select(
    country, sex, birthweight_w1, smoking_pregnancy_w1, parental_class_w1,
    overcrowding_w2, tenure_w3, parental_education_w3, parental_income_w3,
    parental_income_ridit_w3, reading_raw_w3, maths_raw_w3, malaise_tot_w4,
    bmi_w6, bmi_cat_w6, partnership_w6, occupation_w6, net_pay_log_w6,
    exercise_total_w6, smoking_status_w6, alcohol_w6, drug_use_w6,
    ethnicity_w6, illness_w6, malaise_w7, illness_w9
  )

# Clean up labels in a human-readable way for the descriptive table
bcs70_descriptive_clean <- bcs70_descriptive_selected |>
  mutate(
    sex = fct_recode(sex,
      "Female" = "female",
      "Male" = "male"
    ),
    country = fct_relevel(country, "England", "Scotland", "Wales", "Other"),
    smoking_pregnancy_w1 = fct_recode(
      smoking_pregnancy_w1,
      "Non-Smoker" = "non-smoker",
      "Smoker" = "smoker"
    ),
    parental_class_w1 = fct_recode(
      parental_class_w1,
      "Non-Manual" = "non-manual",
      "Manual/Unskilled" = "manual/unskilled/partly_skilled",
      "Other" = "other"
    ),
    overcrowding_w2 = fct_recode(
      overcrowding_w2,
      "Not Overcrowded" = "not-overcrowded",
      "Overcrowded" = "overcrowded"
    ),
    tenure_w3 = fct_recode(
      tenure_w3,
      "Rented or Other" = "rented_or_other",
      "Owned" = "owned"
    ),
    parental_education_w3 = fct_recode(
      parental_education_w3,
      "No Qualifications" = "noqual",
      "Secondary" = "secondary",
      "Tertiary" = "tertiary"
    ),
    parental_income_w3 = fct_collapse(parental_income_w3,
      "<£50" = c("<35", "35-49"),
      "£50-99" = "50-99",
      "£100-149" = "100-149",
      "£150-199" = "150-199",
      "£200+" = c("200-249", "250+")
    ),
    bmi_cat_w6 = fct_recode(
      bmi_cat_w6,
      "Underweight" = "underweight",
      "Normal Weight" = "normal",
      "Overweight" = "overweight",
      "Obese" = "obese"
    ),
    partnership_w6 = fct_recode(
      partnership_w6,
      "Married/Partnered" = "married/partnered",
      "No Partner" = "nopartner"
    ),
    occupation_w6 = fct_recode(
      occupation_w6,
      "Partly Skilled/Unskilled/Manual" = "other",
      "Skilled Non-Manual" = "skilled-nonmanual",
      "Managerial/Technical" = "managerial-technical",
      "Professional" = "professional"
    ),
    exercise_total_w6 = fct_recode(
      exercise_total_w6,
      "<1 Time/Week" = "<1",
      "1-3 Times/Week" = "1-3times",
      "4+ Times/Week" = "4+"
    ),
    smoking_status_w6 = fct_recode(
      smoking_status_w6,
      "Smoker" = "smoker",
      "Never Smoked" = "neversmoker",
      "Ex-Smoker/Occasional" = "ex-smoker_or_occasional"
    ),
    alcohol_w6 = fct_recode(
      alcohol_w6,
      "Rarely/Never" = "rarely_never",
      "2-3 Times/Month" = "2-3times",
      "Once/Week" = "once",
      "Most Days" = "most_days"
    ),
    across(
      c(drug_use_w6, starts_with("illness")),
      ~ fct_recode(.x, "No" = "no", "Yes" = "yes")
    )
  ) |>
  dplyr::select(-bmi_cat_w6)

# Variable labels for the descriptive table and for later plots
variable_labels <- list(
  sex = "Sex",
  country = "Country at Birth",
  birthweight_w1 = "Birth Weight (g) at Birth",
  smoking_pregnancy_w1 = "Smoking During Pregnancy",
  parental_class_w1 = "Parental Social Class at Birth",
  overcrowding_w2 = "Overcrowding (Age 5)",
  tenure_w3 = "Housing Tenure (Age 10)",
  parental_education_w3 = "Parental Education (Age 10)",
  parental_income_w3 = "Parental Income (Age 10)",
  maths_raw_w3 = "Friendly Maths Raw Score (Age 10)",
  reading_raw_w3 = "Reading Test Raw Score (Age 10)",
  malaise_tot_w4 = "Malaise Total Score (Age 16)",
  bmi_w6 = "BMI (Age 30)",
  partnership_w6 = "Partnership Status (Age 30)",
  occupation_w6 = "Occupation (Age 30)",
  exercise_total_w6 = "Exercise Frequency (Age 30)",
  smoking_status_w6 = "Smoking Status (Age 30)",
  alcohol_w6 = "Alcohol Use (Age 30)",
  drug_use_w6 = "Substance Use in Past Year (Age 30)",
  illness_w6 = "Longstanding Illness (Age 30)",
  net_pay_log_w6 = "Net Pay (log, Age 30)",
  malaise_w7 = "Malaise Score (Age 34)",
  illness_w9 = "Longstanding Illness (Age 42)"
)

# Grouping of variables to display
var_groups <- list(
  "Demographics" = c(
    "sex",
    "country",
    "partnership_w6"
  ),
  "Socioeconomic Variables" = c(
    "parental_class_w1",
    "overcrowding_w2",
    "tenure_w3",
    "parental_education_w3",
    "parental_income_w3",
    "occupation_w6"
  ),
  "Health Variables" = c(
    "birthweight_w1",
    "malaise_tot_w4",
    "bmi_w6",
    "illness_w6",
    "malaise_w7",
    "illness_w9"
  ),
  "Health Behaviours" = c(
    "smoking_pregnancy_w1",
    "exercise_total_w6",
    "smoking_status_w6",
    "alcohol_w6",
    "drug_use_w6"
  ),
  "Cognition" = c(
    "maths_raw_w3",
    "reading_raw_w3"
  )
)

# Table: Descriptive statistics (CSV) ------------------------------------------------------------------------------

descriptive_tbl <- gen_desc_table(
  data = bcs70_descriptive_clean,
  var_groups = var_groups,
  variable_labels = variable_labels,
  caption = "",
  output = "summary_table"
)

write_csv(
  descriptive_tbl,
  here("results", "tables", "descriptive_summary.csv")
)

# Table: Descriptive statistics (LaTeX) ----------------------------------------------------------------------------

# Collapse binary variables to single rows for a more compact table.
# Each call specifies which factor level to keep (1 = first, 2 = second) and the display label.
desc_for_latex <- descriptive_tbl |>
  collapse_one_binary("Sex", 1L, "Female") |>
  collapse_one_binary("Overcrowding (Age 5)", 2L, "Overcrowded (Age 5)") |>
  collapse_one_binary("Housing Tenure (Age 10)", 2L, "Housing tenure: Owned (Age 10)") |>
  collapse_one_binary("Partnership Status (Age 30)", 1L, "Married/Partnered (Age 30)") |>
  collapse_one_binary("Smoking During Pregnancy", 2L, "Smoking During Pregnancy") |>
  collapse_one_binary("Substance Use in Past Year (Age 30)", 2L, "Substance Use in Past Year (Age 30)") |>
  collapse_one_binary("Longstanding Illness (Age 30)", 2L, "Longstanding Illness (Age 30)") |>
  collapse_one_binary("Longstanding Illness (Age 42)", 2L, "Longstanding Illness (Age 42)")

# Identify group headers vs data rows
group_names <- names(var_groups)
is_group_header <- desc_for_latex$variable %in% group_names

# Track which rows are sub-levels (indented factor categories) before stripping spaces
is_sublevel <- startsWith(desc_for_latex$variable, "   ")

# Remove group header rows and strip leading spaces
desc_data_latex <- desc_for_latex |>
  dplyr::filter(!is_group_header) |>
  dplyr::mutate(variable = stringr::str_trim(variable))

# Recompute sublevel indices after filtering
is_sublevel_filtered <- is_sublevel[!is_group_header]
indent_rows_latex <- which(is_sublevel_filtered)

# Compute how many data rows belong to each group.
# Split the non-header flag by cumulative group count to get sizes per group.
group_id <- cumsum(is_group_header)
groups_index <- setNames(
  as.integer(table(group_id[!is_group_header])),
  group_names
)

# Build LaTeX table
n_sample <- scales::number(nrow(bcs70_eligible_descriptive), big.mark = ",")

footnote_tex <- glue(
  "Descriptive statistics for the eligible sample (N = {n_sample}). ",
  "N -- count; SD -- standard deviation."
)

tbl_tex <- desc_data_latex |>
  dplyr::rename(
    Variable = variable,
    `N (%) / Mean (SD)` = stat,
    Missing = missing
  ) |>
  kbl(
    format = "latex",
    booktabs = TRUE,
    longtable = TRUE,
    position = "htbp",
    caption = "Descriptive statistics for the BCS70 motivating example",
    label = "descriptives"
  ) |>
  pack_rows(index = groups_index) |>
  kable_styling(
    full_width = FALSE,
    latex_options = "repeat_header",
    font_size = 8,
    position = "left"
  ) |>
  add_indent(indent_rows_latex) |>
  column_spec(2:3, latex_column_spec = "c") |>
  row_spec(0, bold = TRUE) |>
  kableExtra::footnote(
    general = footnote_tex,
    footnote_as_chunk = TRUE,
    escape = FALSE,
    threeparttable = TRUE
  )

path_descriptive_tex <- here("results", "tables", "table_bcs70_descriptives.tex")
save_kable(tbl_tex, file = path_descriptive_tex)
place_footnote_after_note(path_descriptive_tex)

# Table: Balance (CSV) ---------------------------------------------------------------------------------------------

balance_tables <- summarise_balance_table(
  corr_all = results_obj$balance_correlations,
  corr_across = results_obj$balance_correlations_summary,
  ess_summary = results_obj$balance_ess_summary
)

balance_tables |>
  list_rbind(names_to = "winsorised") |>
  write_csv(here("results", "tables", "table_balance.csv"))

# Plots: Balance ---------------------------------------------------------------------------------------------------

# We saved the cobalt::bal.tab objects in results.rds so no need to reload separately
balance_for_plots <- results_obj$balance_for_plots
balance_for_plots_sens <- results_obj$balance_for_plots_supl

# Pretty mapping for variable/level labels on the y-axis of love.plot
pretty_names <- c(
  # continuous
  "birthweight_w1" = variable_labels$birthweight_w1,
  "parental_income_ridit_w3" = "Parental Income (Ridit Score, Age 10)",
  "reading_raw_w3" = variable_labels$reading_raw_w3,
  "maths_raw_w3" = variable_labels$maths_raw_w3,
  "malaise_tot_w4" = variable_labels$malaise_tot_w4,
  "bmi_w6" = variable_labels$bmi_w6,
  "net_pay_log_w6" = "Net Pay (log, Age 30)",

  # sex
  "sex_male" = "Sex: Male",

  # smoking in pregnancy
  "smoking_pregnancy_w1_smoker" =
    paste0(variable_labels$smoking_pregnancy_w1, ": Smoker"),

  # parental class
  "parental_class_w1_manual/unskilled/partly_skilled" =
    paste0(variable_labels$parental_class_w1, ": Manual / partly skilled"),

  # overcrowding
  "overcrowding_w2_overcrowded" =
    paste0(variable_labels$overcrowding_w2, ": Overcrowded"),

  # tenure
  "tenure_w3_owned" =
    paste0(variable_labels$tenure_w3, ": Owned"),

  # parental education
  "parental_education_w3_noqual" =
    paste0(variable_labels$parental_education_w3, ": No qualifications"),
  "parental_education_w3_secondary" =
    paste0(variable_labels$parental_education_w3, ": Secondary"),
  "parental_education_w3_tertiary" =
    paste0(variable_labels$parental_education_w3, ": Tertiary"),

  # partnership
  "partnership_w6_nopartner" =
    paste0(variable_labels$partnership_w6, ": No partner"),

  # occupation
  "occupation_w6_other" =
    paste0(variable_labels$occupation_w6, ": Partly skilled / unskilled"),
  "occupation_w6_skilled-nonmanual" =
    paste0(variable_labels$occupation_w6, ": Skilled non-manual"),
  "occupation_w6_managerial-technical" =
    paste0(variable_labels$occupation_w6, ": Managerial / technical"),
  "occupation_w6_professional" =
    paste0(variable_labels$occupation_w6, ": Professional"),

  # exercise
  "exercise_total_w6_<1" =
    paste0(variable_labels$exercise_total_w6, ": <1x/week"),
  "exercise_total_w6_1-3times" =
    paste0(variable_labels$exercise_total_w6, ": 1-3x/week"),
  "exercise_total_w6_4+" =
    paste0(variable_labels$exercise_total_w6, ": 4+x/week"),

  # smoking status
  "smoking_status_w6_smoker" =
    paste0(variable_labels$smoking_status_w6, ": Smoker"),
  "smoking_status_w6_neversmoker" =
    paste0(variable_labels$smoking_status_w6, ": Never smoked"),
  "smoking_status_w6_ex-smoker_or_occasional" =
    paste0(variable_labels$smoking_status_w6, ": Ex-smoker / occasional"),

  # alcohol
  "alcohol_w6_rarely_never" =
    paste0(variable_labels$alcohol_w6, ": Rarely / never"),
  "alcohol_w6_2-3times" =
    paste0(variable_labels$alcohol_w6, ": 2-3x/month"),
  "alcohol_w6_once" =
    paste0(variable_labels$alcohol_w6, ": Once/week"),
  "alcohol_w6_most_days" =
    paste0(variable_labels$alcohol_w6, ": Most days"),

  # drug use
  "drug_use_w6_yes" =
    paste0(variable_labels$drug_use_w6, ": Yes"),

  # ethnicity (manual label)
  "ethnicity_w6_non-white" =
    "Ethnicity: Ethnic minority background",

  # illness
  "illness_w6_yes" =
    paste0(variable_labels$illness_w6, ": Yes")
)

# love.plot for raw/win weights
# Print both with/without npCBPS included.

all_balance_plots <- list(absent = balance_for_plots, present = balance_for_plots_sens) |>
  enframe(name = "npcbps_included", value = "balance_for_plots") |>
  unnest_longer(balance_for_plots, indices_to = "winsorisation") |>
  mutate(
    plot = map(
      balance_for_plots,
      ~make_balance_plot(
        bal_obj = .x,
        pretty_names = pretty_names,
        panel_title = "",
        panel_subtitle = ""
      )
    ),
    filename = glue("figure_bcs70_balance_{winsorisation}{ifelse(npcbps_included == 'present', '_npcbps', '')}")
  )

# Print all balance plots (raw/win weights, with/without npCBPS)
walk2(
  all_balance_plots$plot,
  all_balance_plots$filename,
  \(p, fn) ggsave(
    filename = here("results", "figures", paste0(fn, ".pdf")),
    plot = p,
    width = 9,
    height = 7,
    units = "in"
  )
)

# Forest plot: Main results ---------------------------------------------------------------------------------------

# Helper: clean method labels for plotting
pretty_method <- function(x) {
  dplyr::case_when(
    x == "unadjusted" ~ "Unadjusted",
    x == "adjusted" ~ "Adjusted",
    x == "multinom" ~ "Multinomial",
    x == "CBPS" ~ "CBPS",
    x == "npCBPS" ~ "npCBPS",
    x == "GBM" ~ "GBM",
    TRUE ~ x
  )
}

# Desired ordering top-to-bottom
method_order <- c(
  "Unadjusted",
  "Adjusted",
  "Multinomial",
  "CBPS",
  "npCBPS",
  "GBM"
)

# Assemble plotting data from pooled results_per4pt
results_plot <- results_obj$results_per4pt |>
  bind_rows(.id = "set") |> # 'set' is "raw" or "win"
  mutate(
    method_label = pretty_method(method),
    method_label = factor(method_label, levels = method_order),
    set_label = dplyr::case_when(
      set == "raw" ~ "Raw weights",
      set == "win" ~ "Winsorised weights (99th pct)",
      TRUE ~ set
    ),
    rr_label = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high)
  ) |>
  select(
    set_label,
    method_label,
    estimate,
    conf.low,
    conf.high,
    rr_label
  )

# Plot both raw & winsorised (overlay)
p_forest_both <- create_forest_methods_overlay(
  data = results_plot,
  method_col = method_label,
  set_col = set_label,
  estimate_col = estimate,
  ci_lower_col = conf.low,
  ci_upper_col = conf.high,
  label_col = rr_label,
  which_sets = c("Raw weights", "Winsorised weights (99th pct)"),
  sort_methods = "custom",
  method_order = method_order
)

# Plot raw only
p_forest_raw <- create_forest_methods_overlay(
  data = results_plot,
  method_col = method_label,
  set_col = set_label,
  estimate_col = estimate,
  ci_lower_col = conf.low,
  ci_upper_col = conf.high,
  label_col = rr_label,
  which_sets = "Raw weights",
  duplicate_unweighted = FALSE,
  sort_methods = "custom",
  method_order = method_order,
  y_label = "Risk ratio (95% CI) per 4-point greater in Malaise Inventory score",
  x_label = NULL,
  title = "",
  caption = ""
)

# Save forest plots
ggsave(
  filename = here("results", "figures", "figure_bcs70_forest_combined.pdf"),
  plot = p_forest_both,
  width = 9,
  height = 7,
  units = "in"
)

ggsave(
  filename = here("results", "figures", "figure_bcs70_forest.pdf"),
  plot = p_forest_raw,
  width = 9,
  height = 7,
  units = "in"
)
