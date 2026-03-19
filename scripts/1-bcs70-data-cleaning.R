# Clear environment
rm(list = ls())

# R packages ------------------------------------------------------------------------------------------------------

# Data manipulation
library(tidyverse)
library(janitor)
library(haven)

# Project path
library(here)

# Descriptive tables
library(gtsummary)

# Set up ----------------------------------------------------------------------------------------------------------

# Find the script if you have not set the working directory
here::i_am("scripts/1-bcs70-data-cleaning.R")

# You should open the project file or set the working directory to the R project folder.
# here() is a safe fallback if users open only this script, so that file paths still work.
# You can test this by running:
# here::here("data", "0y", "bcs7072a.dta")
# This should return the full file path to the dataset on your computer.

# Source helper functions in the utils.R script.
source(here("R", "utils.R"))

# Sweep 1 ---------------------------------------------------------------------------------------------------------

wave1_birth_a <- read_and_clean(here::here("data", "0y", "bcs7072a.dta"))
wave1_birth_derived <- read_and_clean(here::here("data", "0y", "bcs1derived.dta"))

# Check for duplicates
check_duplicates(wave1_birth_a, bcsid)
check_duplicates(wave1_birth_derived, bcsid)

# Merge
wave1_birth <- wave1_birth_a |>
  full_join(wave1_birth_derived, by = "bcsid")

# Check if dimensions correct
dim(wave1_birth_a)
dim(wave1_birth_derived)
dim(wave1_birth)

# Extract the variables
wave1_birth_selected <- wave1_birth |>
  dplyr::select(bcsid, a0278, a0043b, bd1psoc, a0190, a0196) |>
  dplyr::rename(
    birthweight = a0278, smoking_pregnancy = a0043b, parental_class = bd1psoc, antenatal_visits = a0190,
    menstr_certainty = a0196
  )

glimpse(wave1_birth_selected)

# Recode
wave1_birth_clean <- wave1_birth_selected |>
  negative_to_na() |> # Set negative values to NAs
  mutate(
    smoking_pregnancy = case_when(
      smoking_pregnancy %in% 1:3 ~ "non-smoker",
      smoking_pregnancy %in% 4:6 ~ "smoker",
      .default = NA
    ),
    parental_class = case_when(
      parental_class %in% 3:5 ~ "manual/unskilled/partly_skilled",
      parental_class %in% 6:8 ~ "non-manual",
      parental_class %in% c(1, 2) ~ "other",
      .default = NA
    ),
    menstr_certainty = case_when(
      menstr_certainty == 1 ~ "certain",
      menstr_certainty == 2 ~ "uncertain",
      .default = NA
    )
  ) |>
  zap_stata() |> # Zap Stata format
  rename_with(~ if_else(.x != "bcsid", paste0(.x, "_w1"), .x))

str(wave1_birth_clean)

# Remove other dataframes to free up memory
rm(wave1_birth, wave1_birth_a, wave1_birth_derived, wave1_birth_selected)

# Sweep 2 ---------------------------------------------------------------------------------------------------------

# Import datasets
wave2_age6_a <- read_and_clean(here::here("data", "5y", "f699a.dta"))
wave2_age6_b <- read_and_clean(here::here("data", "5y", "f699b.dta"))
wave2_age6_c <- read_and_clean(here::here("data", "5y", "f699c.dta"))
wave2_age6_derived <- read_and_clean(here::here("data", "5y", "bcs2derived.dta"))

wave2_list <- list(wave2_age6_a, wave2_age6_b, wave2_age6_c, wave2_age6_derived)

# Check duplicates
wave2_list |>
  map(~ check_duplicates(.x, bcsid))

# Merge
wave2_age6 <- wave2_list |>
  reduce(full_join, by = "bcsid")

# Check if everyone is included
wave2_list |>
  map(~ dim(.x))

dim(wave2_age6)

# Select variables
wave2_age6_selected <- wave2_age6 |>
  dplyr::rename(copying_score = f119, rutter_mother_score = bd2rutt, n_per_room = e228b) |>
  dplyr::select(bcsid, copying_score, rutter_mother_score, n_per_room)

glimpse(wave2_age6_selected)

# Recode and derive variables
wave2_age6_clean <- wave2_age6_selected |>
  negative_to_na() |>
  dplyr::mutate(
    overcrowding = case_when(
      n_per_room > 1 ~ "overcrowded",
      n_per_room <= 1 & n_per_room > 0 ~ "not-overcrowded",
      .default = NA
    )
  ) |>
  zap_stata() |>
  dplyr::select(-n_per_room) |>
  rename_with(~ if_else(.x != "bcsid", paste0(.x, "_w2"), .x))

str(wave2_age6_clean)

# Remove other dataframes to free up memory
rm(wave2_age6_a, wave2_age6_b, wave2_age6_c, wave2_age6_derived)

# Sweep 3 ---------------------------------------------------------------------------------------------------------

# Import and merge datasets
wave3_age10_a <- read_and_clean(here::here("data", "10y", "sn3723.dta"))
wave3_age10_derived <- read_and_clean(here::here("data", "10y", "bcs3derived.dta"))

# Check duplicates
list(wave3_age10_a, wave3_age10_derived) |>
  map(~ check_duplicates(.x, bcsid))

# Merge
wave3_age10 <- wave3_age10_a |>
  full_join(wave3_age10_derived, by = "bcsid")

# Check dimensions
dim(wave3_age10_a)
dim(wave3_age10_derived)
dim(wave3_age10)

# Select variables
items_parental_income <- paste0("c9_", 1:8)
items_parental_educ <- paste0("c1_", 1:22)

wave3_age10_selected <- wave3_age10 |>
  rename(tenure = d2) |>
  dplyr::select(bcsid, tenure, all_of(items_parental_income), all_of(items_parental_educ))

# Derive parental qualifications

## First, derive the qualifications each parent has
wave3_age10_educ <- wave3_age10_selected |>
  mutate(
    f_educ_noqual = case_when(
      c1_9 == 1 | c1_8 == 9 ~ 1,
      .default = 0
    ),
    f_educ_secondary = case_when(
      c1_1 == 1 | c1_2 == 1 | c1_8 %in% c(1, 2) ~ 1,
      .default = 0
    ),
    f_educ_tertiary = case_when(
      c1_3 == 1 | c1_4 == 1 | c1_5 == 1 | c1_6 == 1 | c1_8 %in% c(3:6) ~ 1,
      .default = 0
    ),
    m_educ_noqual = case_when(
      c1_20 == 1 | c1_19 == 9 ~ 1,
      .default = 0
    ),
    m_educ_secondary = case_when(
      c1_12 == 1 | c1_13 == 1 | c1_19 %in% c(1, 2) ~ 1,
      .default = 0
    ),
    m_educ_tertiary = case_when(
      c1_14 == 1 | c1_15 == 1 | c1_16 == 1 | c1_17 == 1 | c1_19 %in% c(3:6) ~ 1,
      .default = 0
    )
  )

## Next, derive highest achieved qualifications, including combined qualifications
wave3_age10_educ2 <- wave3_age10_educ |>
  mutate(
    f_highest_qual = case_when(
      f_educ_tertiary == 1 ~ "tertiary",
      f_educ_secondary == 1 & f_educ_tertiary == 0 ~ "secondary",
      f_educ_noqual == 1 & f_educ_secondary == 0 & f_educ_tertiary == 0 ~ "noqual",
      .default = NA
    ),
    m_highest_qual = case_when(
      m_educ_tertiary == 1 ~ "tertiary",
      m_educ_secondary == 1 & m_educ_tertiary == 0 ~ "secondary",
      m_educ_noqual == 1 & m_educ_secondary == 0 & m_educ_tertiary == 0 ~ "noqual",
      .default = NA
    )
  )

table(wave3_age10_educ2$f_highest_qual, useNA = "ifany")
table(wave3_age10_educ2$m_highest_qual, useNA = "ifany")
table(wave3_age10_educ2$f_highest_qual, wave3_age10_educ2$m_highest_qual)

## Combined qualification
wave3_age10_educ3 <- wave3_age10_educ2 |>
  dplyr::mutate(
    parental_education = case_when(
      # At least one has tertiary
      f_highest_qual == "tertiary" | m_highest_qual == "tertiary" ~ "tertiary",
      # At least one has secondary, none has tertiary
      (f_highest_qual == "secondary" | m_highest_qual == "secondary") &
        !(f_highest_qual == "tertiary" | m_highest_qual == "tertiary") ~ "secondary",
      # Both have no qualifications
      f_highest_qual == "noqual" & m_highest_qual == "noqual" ~ "noqual",
      # All other combinations are NA.
      .default = NA
    )
  )

table(wave3_age10_educ3$parental_education)
table(wave3_age10_educ3$f_highest_qual, wave3_age10_educ3$m_highest_qual)

## Append derived education to the main dataset
wave3_age10_educ_clean <- wave3_age10_educ3 |>
  dplyr::select(bcsid, parental_education)

wave3_age10_selected <- wave3_age10_selected |>
  full_join(wave3_age10_educ_clean, by = "bcsid") |>
  dplyr::select(-all_of(items_parental_educ))


# Derive parental income
wave3_age10_income <- wave3_age10_selected |>
  mutate(
    parental_income = case_when(
      c9_1 == 1 ~ 0,
      c9_2 == 1 ~ 1,
      c9_3 == 1 ~ 2,
      c9_4 == 1 ~ 3,
      c9_5 == 1 ~ 4,
      c9_6 == 1 ~ 5,
      c9_7 == 1 ~ 6
    ),
    parental_income = factor(
      parental_income,
      levels = c(0:6),
      labels = c("<35", "35-49", "50-99", "100-149", "150-199", "200-249", "250+"),
      ordered = TRUE
    )
  )

table(wave3_age10_income$parental_income)
prop.table(table(wave3_age10_income$parental_income))

## Ridits

# Calculate the frequencies of each income level
income_frequencies <- wave3_age10_income |>
  count(parental_income) |>
  dplyr::filter(!is.na(parental_income))

# Derive the ridits
income_frequencies <- income_frequencies |>
  dplyr::mutate(
    cumulative_freq = cumsum(n),
    total = sum(n),
    cumulative_prop = cumulative_freq / total,
    ridit = round((cumulative_prop + lag(cumulative_prop, default = 0)) / 2, 4)
  )

income_frequencies

## Check if mean ridit is 0.5
income_frequencies |> summarise(mean_ridit = sum(ridit * n) / sum(n))

## Join the ridits to the main dataset
wave3_age10_complete <- wave3_age10_income |>
  left_join(income_frequencies |> dplyr::select(parental_income, ridit), by = "parental_income") |>
  dplyr::select(-all_of(items_parental_income))

table(wave3_age10_complete$ridit, wave3_age10_complete$parental_income)

# Cognition
## For simplicity, use the the derived math and reading test scores available in the dataset.
wave3_age10_cognition <- wave3_age10_derived |>
  dplyr::select(bcsid, bd3rread, bd3maths) |>
  mutate(
    across(
      c(bd3rread, bd3maths),
      ~ case_when(
        .x == -1 ~ NA,
        .default = .x
      )
    )
  ) |>
  rename(
    reading_raw = bd3rread,
    maths_raw = bd3maths
  )

# Join cognition
wave3_age10_complete <- wave3_age10_complete |>
  left_join(wave3_age10_cognition, by = "bcsid")

# Recode the rest
head(wave3_age10_complete$tenure)

wave3_age10_clean <- wave3_age10_complete |>
  mutate(
    tenure = case_when(
      tenure %in% c(1, 2) ~ "owned",
      tenure %in% c(3:7) ~ "rented_or_other",
      .default = NA
    )
  ) |>
  rename(parental_income_ridit = ridit) |>
  rename_with(~ if_else(.x != "bcsid", paste0(.x, "_w3"), .x)) |>
  zap_stata()

str(wave3_age10_clean)

# Remove other dataframes
rm(
  income_frequencies, wave3_age10, wave3_age10_a, wave3_age10_complete, wave3_age10_derived,
  wave3_age10_educ, wave3_age10_educ2, wave3_age10_educ3, wave3_age10_educ_clean, wave3_age10_selected,
  wave3_age10_income, wave3_age10_cognition
)

# Sweep 4 ---------------------------------------------------------------------------------------------------------

# Import datasets
wave4_age16_a <- read_and_clean(here::here("data", "16y", "bcs7016x.dta"))
wave4_age16_derived <- read_and_clean(here::here("data", "16y", "bcs4derived.dta"))

# Check duplicates
list(wave4_age16_a, wave4_age16_derived) |>
  map(~ check_duplicates(.x, bcsid))

# Merge
wave4_age16 <- wave4_age16_a |>
  full_join(wave4_age16_derived, by = "bcsid")

table(wave4_age16$bd4mal)

wave4_age16_clean <- wave4_age16 |>
  dplyr::rename(malaise_tot = bd4mal) |>
  dplyr::select(bcsid, malaise_tot) |>
  negative_to_na() |>
  rename_with(~ if_else(.x != "bcsid", paste0(.x, "_w4"), .x)) |>
  zap_stata()

# Remove other dataframes to free up memory
rm(wave4_age16, wave4_age16_derived)

# Sweep 5 ---------------------------------------------------------------------------------------------------------

# Import and inspect
wave5_age26 <- read_and_clean(here::here("data", "26y", "bcs96x.dta"))

# Check duplicates
check_duplicates(wave5_age26, bcsid)

# Check net pay
head(wave5_age26$b960312)
summary(wave5_age26$b960312)

# Check negative values
less_than_zero <- wave5_age26 |>
  dplyr::filter(b960312 < 0)

table(less_than_zero$b960312)

# -3 and -8 are missing.

# Cleaning
wave5_age26_clean <- wave5_age26 |>
  rename(net_pay = b960312) |>
  dplyr::select(bcsid, net_pay) |>
  negative_to_na() |>
  rename_with(~ if_else(.x != "bcsid", paste0(.x, "_w5"), .x)) |>
  zap_stata()

str(wave5_age26_clean)

# Inspect a log-plot
wave5_age26_clean |>
  ggplot(aes(x = log(net_pay_w5))) +
  geom_histogram() +
  theme_bw()

rm(wave5_age26, less_than_zero)

# Sweep 6 ---------------------------------------------------------------------------------------------------------

# Import data
wave6_age29_a <- read_and_clean(here::here("data", "29y", "bcs2000.dta"))
wave6_age29_derived <- read_and_clean(here::here("data", "29y", "bcs6derived.dta"))

# Check duplicates
list(wave6_age29_a, wave6_age29_derived) |>
  map(~ check_duplicates(.x, bcsid))

wave6_age29 <- wave6_age29_a |>
  full_join(wave6_age29_derived, by = "bcsid")

dim(wave6_age29_a)
dim(wave6_age29_derived)
dim(wave6_age29)

# Define substance use items
items_drugs <- c(
  "ecsacy", "amphet", "lsd", "popper", "magmush", "cocaine", "temaz", "semeron", "ketamine", "crack",
  "heroin", "methad", "othdrug"
)

# Select variables

## Note: The correct variable for longstanding illness is lsiany2 (not lsiany, which is missing for most participants).
## lsiany2 can be used to identify if any longstanding illness has been reported.

## Net pay: This variable is used for simplicity. In applied projects, users may wish to derive family income across
## many indicators included in BCS70. See the age 51 strategy in the user guide
## (https://cls.ucl.ac.uk/wp-content/uploads/2017/02/bcs70_sweep11_age51_user_guide_v1_.pdf)

wave6_age29_selected <- wave6_age29 |>
  dplyr::rename(
    occupation = sc, exercise_any = exercise, exercise_freq = breathls, alcohol = drinks, partnership = ms,
    malaise = bd6mal, country = bd6cntry, illness = lsiany2, net_pay_raw = cnetpay, net_pay_period = cnetprd
  ) |>
  dplyr::select(
    bcsid, partnership, occupation, exercise_any, exercise_freq, smoking, alcohol, weight2, wtkilos2, wtstone2,
    wtpound2, height2, htcms2, htmetre2, htfeet2, htinche2, all_of(items_drugs), vote97, malaise, ethnic, net_pay_raw,
    net_pay_period, illness
  )

# Deriving BMI

## 1) Derive weight/height in consistent units

wave6_age29_bmi <- wave6_age29_selected |>
  # Set missing values to NAs
  mutate(
    across(
      c(htfeet2, htinche2, wtstone2, wtpound2),
      ~ case_when(
        .x %in% c(98, 99) ~ NA,
        .default = .x
      )
    )
  ) |>
  # Convert to kilograms and metres if reported in other units
  mutate(
    weight_kg = case_when(
      weight2 == 1 ~ wtkilos2,
      weight2 == 2 ~ (wtstone2 * 14 + wtpound2) * 0.453592,
      .default = NA
    ),
    height_m = case_when(
      height2 == 1 ~ htmetre2 + (htcms2 / 100),
      height2 == 2 ~ (htfeet2 * 12 + htinche2) * 0.0254,
      .default = NA
    )
  )

## 2) Typos in height

# Extremes on the low and high ends of the distribution:
# Based on cross-checks with other sweeps, heights below 1.1m and above 2.2m are typos.
# For heights below 1.1. these are e.g. 1.05m -> 1.50m.
# Heights above 2.2m are data entry errors and will be replaced with height from nurse measurement at age 46
# or with missing if not available.

# Inspect low heights
low_heights <- wave6_age29_bmi |>
  dplyr::filter(height_m < 1.1) |>
  dplyr::select(bcsid, height_m, weight_kg, weight2, wtstone2, wtpound2, height2, htfeet2, htinche2, htmetre2, htcms2)

low_heights

# Inspect high heights
high_heights <- wave6_age29_bmi |>
  dplyr::filter(height_m > 2.2) |>
  dplyr::select(bcsid, height_m, weight_kg, weight2, wtstone2, wtpound2, height2, htfeet2, htinche2, htmetre2, htcms2)

high_heights

# Extract heights from nurse measurement at age 46
age46_data <- read_and_clean(here::here("data", "46y", "bcs_age46_main.dta"))

age46_heights <- age46_data |>
  mutate(height_m_nurse = b10heightcm / 100) |>
  dplyr::select(bcsid, height_m_nurse) |>
  negative_to_na()

wave6_age29_bmi <- wave6_age29_bmi |>
  left_join(age46_heights, by = "bcsid")

# Implement the fix
wave6_age29_bmi <- wave6_age29_bmi |>
  mutate(
    height_m = case_when(
      # if height < 1.1m, correct the decimal typo
      height_m < 1.1 ~ floor(height_m) + (height_m %% 1) * 10,
      # if height > 2.2m, replace with height from nurse measurement at age 46 if available
      height_m > 2.2 & !is.na(height_m_nurse) ~ height_m_nurse,
      # if height > 2.2m and no height from nurse measurement, set to NA
      height_m > 2.2 & is.na(height_m_nurse) ~ NA,
      # otherwise, keep original height
      .default = height_m
    )
  )

# Check the fix
wave6_age29_bmi |>
  dplyr::filter(bcsid %in% low_heights$bcsid | bcsid %in% high_heights$bcsid) |>
  dplyr::select(bcsid, height_m)

# Inspect the height distribution
wave6_age29_bmi |>
  ggplot(aes(x = height_m)) +
  geom_histogram() +
  theme_bw()

## 3) Check if extreme weight present
wave6_age29_bmi |>
  dplyr::filter(weight_kg < 25 | weight_kg > 200) |>
  dplyr::select(bcsid, height_m, weight_kg, weight2, wtstone2, wtpound2, height2, htfeet2, htinche2, htmetre2, htcms2)

## 4) Derive BMI
wave6_age29_bmi <- wave6_age29_bmi |>
  mutate(
    bmi = weight_kg / (height_m^2),
    # Categorise
    bmi_cat = case_when(
      bmi < 18.5 ~ 0,
      bmi >= 18.5 & bmi < 25 ~ 1,
      bmi >= 25 & bmi < 30 ~ 2,
      bmi >= 30 ~ 3,
      .default = NA
    ),
    bmi_cat = factor(
      bmi_cat,
      levels = c(0:3),
      labels = c("underweight", "normal", "overweight", "obese"),
      ordered = T
    )
  )

wave6_age29_bmi |>
  ggplot(aes(x = bmi)) +
  geom_histogram() +
  theme_bw()

wave6_age29_bmi |>
  dplyr::filter(is.na(bmi_cat) == F) |>
  ggplot(aes(x = bmi_cat, y = bmi)) +
  geom_boxplot() +
  theme_bw()

# Net pay

## Convert net pay to weekly (participants could choose the period to report)
## Note: conversions take into account if leap year or not (days = 365 vs 366)

convert_to_weekly <- function(inc, period, days) {
  case_when(
    period == 1 ~ inc, # One week: keep
    period == 2 ~ inc / 2, # Two weeks: divide by 2
    period == 3 ~ inc / 4, # Four weeks
    period == 4 ~ (inc * 12) * (7 / days), # Monthly: convert to annual first, then convert to weekly
    period == 5 ~ inc * (7 / days), # Annual
    TRUE ~ NA_real_ # Other period will be set to missing
  )
}

wave6_age29_pay <- wave6_age29_bmi |>
  mutate(
    net_pay_raw = if_else(net_pay_raw %in% c(9999998, 9999999), NA_real_, net_pay_raw), # First, fix missing codes
    net_pay = convert_to_weekly(net_pay_raw, net_pay_period, days = 366) # 2000 is a leap year
  )

wave6_age29_pay |>
  dplyr::select(net_pay_raw, net_pay_period, net_pay)

# Remove intermediate variables
wave6_age29_pay <- wave6_age29_pay |>
  dplyr::select(-net_pay_raw, -net_pay_period)

# Remaining sociodemographic and socioeconomic variables
wave6_age29_sociodem <- wave6_age29_pay |>
  mutate(
    occupation = case_when(
      occupation == 1 ~ "professional",
      occupation == 2 ~ "managerial-technical",
      occupation == 3.1 ~ "skilled-nonmanual",
      occupation == 3.2 | occupation %in% c(4:6) ~ "other",
      .default = NA
    ),
    partnership = case_when(
      partnership %in% c(1, 2) ~ "married/partnered",
      partnership %in% c(3:6) ~ "nopartner",
      .default = NA
    ),
    vote97 = case_when(
      vote97 == 1 ~ "voted",
      vote97 == 2 ~ "not-voted",
      .default = NA
    ),
    ethnicity = case_when(
      ethnic %in% c(1:3) ~ "white",
      ethnic %in% c(4:7) ~ "mixed",
      ethnic %in% c(8:10) ~ "southasian",
      ethnic %in% c(12:14) ~ "black",
      ethnic %in% c(11, 15, 16) ~ "other",
      .default = NA
    )
  )

# Recode health behaviours
wave6_age29_healthbeh <- wave6_age29_sociodem |>
  mutate(
    exercise_total = case_when(
      exercise_any == 2 | exercise_freq %in% c(5, 6) ~ 0,
      exercise_freq %in% c(3, 4) ~ 1,
      exercise_freq %in% c(1, 2) ~ 2,
      .default = NA
    ),
    exercise_total = factor(
      exercise_total,
      levels = c(0:2),
      labels = c("<1", "1-3times", "4+"),
      ordered = T
    ),
    smoking_status = case_when(
      smoking == 1 ~ "neversmoker",
      smoking %in% c(2, 3) ~ "ex-smoker_or_occasional",
      smoking == 4 ~ "smoker",
      .default = NA
    ),
    alcohol = case_when(
      alcohol %in% c(4, 7) ~ 0,
      alcohol == 3 ~ 1,
      alcohol == 2 ~ 2,
      alcohol == 1 ~ 3,
      .default = NA
    ),
    alcohol = factor(
      alcohol,
      levels = c(0:3),
      labels = c("rarely_never", "once", "2-3times", "most_days"),
      ordered = T
    )
  )

# Derive drug use

## Remove the "other drug" item as it does not specify if done in past year
items_drugs <- items_drugs[items_drugs != "othdrug"]

## Recode all substance use items
wave6_age29_drugs <- wave6_age29_healthbeh |>
  mutate(
    across(
      all_of(items_drugs),
      ~ case_when(
        # yes if used in past year (3)
        .x == 3 ~ 1,
        # no if never (1) or not in past year (2)
        .x %in% c(1, 2) ~ 0,
        .default = NA
      )
    )
  )

## Derive substance use as a single variable
wave6_age29_drugs2 <- wave6_age29_drugs %>% # must use magrittr pipe for rowSums to work
  mutate(
    # Create a logical matrix where TRUE represents a '1' (drug use)
    is_drug_use = rowSums(select(., all_of(items_drugs)) == 1, na.rm = TRUE),
    # Create a logical matrix where TRUE represents a '0' (no drug use) and count non-NA entries
    is_no_drug_use = rowSums(select(., all_of(items_drugs)) == 0, na.rm = TRUE),
    # Count of non-NA values in drug-related columns
    non_na_count = rowSums(!is.na(select(., all_of(items_drugs))))
  ) %>%
  # Now, derive the drug_use variable using the intermediate variables
  mutate(
    drug_use = case_when(
      is_drug_use > 0 ~ 1, # If any drug use is confirmed
      is_no_drug_use == 12 ~ 0, # If all non-NA values are '0', confirming no drug use
      .default = NA # Default to NA in cases of ambiguity
    )
  ) %>%
  # Remove the helper columns
  dplyr::select(-is_drug_use, -is_no_drug_use, -non_na_count)

table(wave6_age29_drugs2$drug_use, useNA = "ifany")
prop.table(table(wave6_age29_drugs2$drug_use))

## Finish W6 data cleaning
wave6_age29_clean <- wave6_age29_drugs2 |>
  # Select relevant variables
  dplyr::select(
    bcsid, bmi, bmi_cat, partnership, occupation, exercise_total, smoking_status, alcohol, drug_use, vote97,
    malaise, ethnicity, illness, net_pay
  ) |>
  mutate(
    # Set Malaise < 0 to NA
    malaise = case_when(
      malaise < 0 ~ NA,
      .default = malaise
    ),
    # Longstanding illness
    illness = case_when(
      illness == 1 ~ 1,
      illness == 2 ~ 0,
      .default = NA
    )
  ) |>
  # Add wave identifier to variable names
  rename_with(~ if_else(.x != "bcsid", paste0(.x, "_w6"), .x)) |>
  # Remove Stata format
  zap_stata()

str(wave6_age29_clean)

# Remove other dataframes to free up memory
rm(
  wave6_age29, wave6_age29_a, wave6_age29_selected, wave6_age29_healthbeh, wave6_age29_sociodem,
  wave6_age29_drugs, wave6_age29_drugs2, wave6_age29_bmi, age46_data
)

# Sweep 7 ---------------------------------------------------------------------------------------------------------
wave7_age34_a <- read_and_clean(here::here("data", "34y", "bcs_2004_followup.dta"))
wave7_age34_derived <- read_and_clean(here::here("data", "34y", "bcs7derived.dta"))

# Check duplicates
list(wave7_age34_a, wave7_age34_derived) |>
  map(~ check_duplicates(.x, bcsid))

# Merge and derive variables
wave7_age34_clean <- wave7_age34_a |>
  left_join(wave7_age34_derived, by = "bcsid") |>
  rename(malaise = bd7mal, works_overtime = b7otimny, behave_responsibly = b7eresp6, guilty = b7court) |>
  dplyr::select(bcsid, malaise, works_overtime, behave_responsibly, guilty) |>
  negative_to_na() |>
  mutate(
    works_overtime = case_when(
      works_overtime == 1 ~ "works-overtime",
      works_overtime == 2 ~ "no-overtime",
      .default = NA
    ),
    behave_responsibly = case_when(
      behave_responsibly == 1 ~ "yes",
      behave_responsibly == 2 ~ "no",
      .default = NA
    ),
    guilty = case_when(
      guilty == 0 ~ "not-guilty",
      guilty >= 1 ~ "guilty",
      .default = NA
    )
  ) |>
  rename_with(~ if_else(.x != "bcsid", paste0(.x, "_w7"), .x)) |>
  zap_stata()

str(wave7_age34_clean)

# Inspect malaise distribution
wave7_age34_clean |>
  ggplot(aes(x = malaise_w7)) +
  geom_histogram(binwidth = 1, fill = "steelblue") +
  theme_bw()

rm(wave7_age34_derived, wave7_age34_a)

# Sweep 8 ---------------------------------------------------------------------------------------------------------
wave8_age38 <- read_and_clean(here::here("data", "38y", "bcs_2008_followup.dta"))

check_duplicates(wave8_age38, bcsid)

wave8_age38_clean <- wave8_age38 |>
  rename(consent_parent_proj = b8parent) |>
  dplyr::select(bcsid, consent_parent_proj) |>
  negative_to_na() |>
  mutate(
    consent_parent_proj = case_when(
      consent_parent_proj == 1 ~ "consent",
      consent_parent_proj == 2 ~ "no-consent",
      .default = NA
    )
  ) |>
  rename_with(~ if_else(.x != "bcsid", paste0(.x, "_w8"), .x)) |>
  zap_stata()

str(wave8_age38_clean)


# Sweep 9 (end of follow-up) ---------------------------------------------------------------------------------------

wave9_age42 <- read_and_clean(here::here("data", "42y", "bcs70_2012_flatfile.dta"))
check_duplicates(wave9_age42, bcsid)

# Longstanding illness (outcome)
wave9_age42_clean <- wave9_age42 |>
  mutate(
    illness_w9 = case_when(
      b9loil == 1 ~ 1,
      b9loil == 2 ~ 0,
      .default = NA
    )
  ) |>
  select(bcsid, illness_w9)

# Response dataset ------------------------------------------------------------------------------------------------

response_dataset <- read_and_clean(here::here("data", "xwave", "bcs70_response_1970-2016.dta"))

check_duplicates(response_dataset, bcsid)

# Rename wave notation for consistency
response_dataset <- response_dataset |>
  rename_with(~ str_replace(.x, "^outcme0?(\\d+)$", "outcme_w\\1"))

glimpse(response_dataset)

response_dataset_select <- response_dataset |>
  mutate(
    # Derive non-response indicators at each sweep
    across(
      starts_with("outcme"),
      ~ case_when(
        .x %in% c(0, 2:8) ~ 1,
        .x == 1 ~ 0,
        .default = NA
      ),
      .names = "nonresponse_w{str_extract(.col, '\\\\d+$')}"
    ),
    sex = case_when(
      sex == 1 ~ "male",
      sex == 2 ~ "female",
      .default = NA
    ),
    country = case_when(
      cob == 1 ~ "England",
      cob == 2 ~ "Wales",
      cob == 3 ~ "Scotland",
      cob %in% c(4:6) ~ "Other",
      .default = NA
    )
  ) |>
  dplyr::select(-cob, -multipno, -twincode) |>
  zap_stata()

glimpse(response_dataset_select)

# Cumulative non-response

## Pivot into the long format
response_dataset_nonresp_long <- response_dataset_select |>
  pivot_longer(
    cols = starts_with("nonresponse_w"),
    names_to = "wave",
    values_to = "nonresponse"
  ) |>
  mutate(num = readr::parse_number(wave)) |> # Extract wave number for ordering (helper column added for safety)
  arrange(bcsid, num) |>
  group_by(bcsid) |>
  mutate(cumulative_nonresponse = ifelse(cumsum(nonresponse) > 0, 1, 0)) |>
  dplyr::select(-num) # Remove helper column

## Check if the cumulative non-response is correct

### Initial check
print(response_dataset_nonresp_long, n = 30)

### Verify that there are individuals who responded but have cumulative non-response of 1 due to prior non-response
cumulative_id <- response_dataset_nonresp_long |>
  dplyr::filter(nonresponse == 0 & cumulative_nonresponse == 1)

cumulative_id

### Verify that cumulative non-response works as expected for these individuals
response_dataset_nonresp_long |>
  dplyr::filter(bcsid %in% cumulative_id$bcsid)

## Pivot back into wide format
response_dataset_nonresp_wide <- response_dataset_nonresp_long |>
  dplyr::select(-nonresponse) |>
  ungroup() |>
  pivot_wider(
    names_from = wave,
    values_from = cumulative_nonresponse,
    names_prefix = "cumul_"
  ) |>
  dplyr::select(bcsid, starts_with("cumul"))

glimpse(response_dataset_nonresp_wide)

## Append cumulative non-response to the response dataset
response_dataset_clean <- response_dataset_select |>
  full_join(response_dataset_nonresp_wide, by = "bcsid")

## Check
response_dataset_clean |>
  dplyr::filter(nonresponse_w5 == 0 & cumul_nonresponse_w5 == 1) |>
  dplyr::select(bcsid, nonresponse_w1:nonresponse_w5, cumul_nonresponse_w1:cumul_nonresponse_w5)

## All good

str(response_dataset_clean)

# Combine --------------------------------------------------------------------------------------------------------


## Combine datasets ------------------------------------------------------------------------------------------------

datasets_list <- list(
  wave1_birth_clean, wave2_age6_clean, wave3_age10_clean, wave4_age16_clean, wave5_age26_clean, wave6_age29_clean,
  wave7_age34_clean, wave8_age38_clean, wave9_age42_clean
)

bcs70_combined_fullsample <- datasets_list |>
  reduce(full_join, by = "bcsid")

check_duplicates(bcs70_combined_fullsample, bcsid)
glimpse(bcs70_combined_fullsample)
# 18,645 observations

# The main sweep datasets include a small number of participants from Northern Ireland who were never followed up
# and are not part of the core BCS70 sample.
# The response dataset only includes the core of BCS70 without these participants.

bcs70_combined_fullsample <- response_dataset_clean |>
  left_join(bcs70_combined_fullsample, by = "bcsid")

dim(bcs70_combined_fullsample)
# 18,037 observations

bcs70_combined_fullsample <- bcs70_combined_fullsample |>
  mutate(
    # Where appropriate, convert remaining character variables to factors
    across(
      c(
        parental_class_w1, smoking_pregnancy_w1, parental_class_w1, menstr_certainty_w1, overcrowding_w2, tenure_w3,
        parental_education_w3, partnership_w6, occupation_w6, ethnicity_w6, smoking_status_w6, vote97_w6, ethnicity_w6,
        works_overtime_w7, behave_responsibly_w7, guilty_w7, consent_parent_proj_w8, sex, country
      ),
      ~ forcats::as_factor(.x)
    ),
    # Any remaining 0/1 numeric variables into factors with yes/no labels
    across(
      # Select all numeric columns that contain only 0/1 values (and possibly NAs)
      where(~ is.numeric(.x) && all(.x %in% c(0, 1) | is.na(.x))),
      # Convert them to factor with "no"/"yes" labels
      ~ factor(.x, levels = c(0, 1), labels = c("no", "yes"))
    )
  )

# Lose any remaining Stata dataset label
attr(bcs70_combined_fullsample, "label") <- NULL

str(bcs70_combined_fullsample)


## Keep the eligible ---------------------------------------------------------------------------------------------

# Exclude those who died or emigrated by end of follow-up
bcs70_eligible_descriptive <- bcs70_combined_fullsample |>
  dplyr::filter(outcme_w9 != 7 & outcme_w9 != 8)

# Export dataset
write_rds(
  bcs70_eligible_descriptive,
  here::here("data", "processed", "bcs70_eligible_descriptive.rds"),
  compress = "xz"
)

# Overall eligible
nrow(bcs70_combined_fullsample)
nrow(bcs70_eligible_descriptive)
nrow(bcs70_combined_fullsample) - nrow(bcs70_eligible_descriptive)
1 - nrow(bcs70_eligible_descriptive) / nrow(bcs70_combined_fullsample)

# Step by step eligibility for flow chart
## Deaths
n_died <- bcs70_combined_fullsample |>
  filter(outcme_w9 == 8) |>
  nrow()

n_died # 966

n_died / nrow(bcs70_combined_fullsample) # 5.4%

## Emigrations
n_emigrated <- bcs70_combined_fullsample |>
  filter(outcme_w9 == 7) |>
  nrow()

n_emigrated # 433

n_emigrated / (nrow(bcs70_combined_fullsample) - n_died) # 2.5% of those who were alive at end of follow-up



## Descriptives (exploratory) --------------------------------------------------------------------------------------

# Show missingness table
count_missing(bcs70_eligible_descriptive) |>
  print(n = Inf, width = Inf)

# Show descriptives table

theme_gtsummary_journal(journal = "jama")
theme_gtsummary_compact()

bcs70_eligible_descriptive |>
  dplyr::select(-bcsid, -starts_with("outcme")) |>
  tbl_summary(
    statistic = list(all_continuous() ~ "{mean} ({sd}), {median} ({p25}, {p75})"),
    digits = list(all_continuous() ~ 1, all_categorical() ~ 1),
    type = list(
      copying_score_w2 ~ "continuous", smoking_pregnancy_w1 ~ "categorical",
      overcrowding_w2 ~ "categorical", parental_income_ridit_w3 ~ "continuous"
    ),
    missing_text = "missing"
  )

## Collapse sparse categories -----------------------------------------------------------------------------------

# Parental class at W1

## Check
bcs70_eligible_descriptive$parental_class_w1 |>
  table(useNA = "ifany")

## Check distributions of malaise_w7 by parental social class
bcs70_eligible_descriptive |>
  dplyr::filter(!is.na(malaise_w7)) |>
  group_by(parental_class_w1) |>
  summarise(
    n = n(),
    mean_malaise = mean(malaise_w7, na.rm = TRUE)
  ) |>
  arrange(desc(n))

### Combine 'Other' with 'Manual' (more similar in malaise_w7)\

bcs70_eligible_analysis <- bcs70_eligible_descriptive |>
  mutate(
    parental_class_w1 = case_when(
      parental_class_w1 == "other" ~ "manual/unskilled/partly_skilled",
      TRUE ~ parental_class_w1
    ),
    parental_class_w1 = forcats::as_factor(parental_class_w1)
  )

# Ethnicity
bcs70_eligible_analysis$ethnicity_w6 |>
  table(useNA = "ifany")

bcs70_eligible_analysis <- bcs70_eligible_analysis |>
  mutate(
    ethnicity_w6 = case_when(
      ethnicity_w6 == "white" ~ "white",
      ethnicity_w6 %in% c("southasian", "black", "mixed", "other") ~ "non-white",
      .default = ethnicity_w6
    ),
    ethnicity_w6 = forcats::as_factor(ethnicity_w6)
  )

glimpse(bcs70_eligible_analysis)

# Export dataset of the eligible with variables for analysis
write_rds(bcs70_eligible_analysis, here::here("data", "processed", "bcs70_eligible_analysis.rds"), compress = "xz")

# Exploratory descriptives for the analytical dataset
theme_gtsummary_journal(journal = "jama")
theme_gtsummary_compact()

bcs70_eligible_analysis |>
  dplyr::select(-bcsid, -starts_with("outcme"), -contains("nonresponse")) |>
  tbl_summary(
    statistic = list(all_continuous() ~ "{mean} ({sd}), {median} ({p25}, {p75})"),
    digits = list(all_continuous() ~ 1, all_categorical() ~ 1),
    type = list(
      copying_score_w2 ~ "continuous", smoking_pregnancy_w1 ~ "categorical",
      overcrowding_w2 ~ "categorical", parental_income_ridit_w3 ~ "continuous"
    ),
    missing_text = "missing"
  )
