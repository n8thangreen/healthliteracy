# equivalise income in resident survey

library(dplyr)

file_loc <- here::here("../../data/Newham Resident Survey 2023/London Borough of Newham - Residents Survey - 2023 - Dataset v3.xlsx")

resident_survey <- readxl::read_xlsx(file_loc, sheet = "Labels")

# convert income bands to a numeric midpoint
convert_income_band_to_numeric <- function(income_label) {
  if (is.na(income_label) || income_label %in% c("Not answered", "Don't know", "Prefer not to say")) {
    return(NA)
  } else if (income_label == "Less than £10,000 per year") {
    return(5000) # Midpoint for 0 - 9999
  } else if (income_label == "£10,000 - £14,999 per year") {
    return(12500)
  } else if (income_label == "£15,000 - £19,999 per year") {
    return(17500)
  } else if (income_label == "£20,000 - £24,999 per year") {
    return(22500)
  } else if (income_label == "£25,000 - £29,999 per year") {
    return(27500)
  } else if (income_label == "£30,000 - £39,999 per year") {
    return(35000)
  } else if (income_label == "£40,000 - £49,999 per year") {
    return(45000)
  } else if (income_label == "£50,000 - £74,999 per year") {
    return(62500)
  } else if (income_label == "£75,000 - £99,999 per year") {
    return(87500)
  } else if (income_label == "£100,000 or more per year") {
    return(125000) # Use a reasonable upper bound for estimation
  } else {
    return(NA) # Handle any unexpected labels
  }
}

survey_data <- resident_survey %>%
  rowwise() %>%
  mutate(Household_Income_Numeric = convert_income_band_to_numeric(Q61)) %>%
  ungroup()


# Convert number of adults and children to numeric, handling non-numeric responses
# Assuming Q67A, Q67B, Q67C, Q67D are counts.
# If they contain "Not answered", "Don't know", "Prefer not to say", or "0", convert them.
# The `replace_na(0)` for children categories ensures that if a count is missing, it's treated as zero children in that category.
# However, for Q67A, if it's NA, the equivalisation cannot proceed for that household.

survey_data <- survey_data %>%
  mutate(
    # questions say "including yourself..."
    Q67A = ifelse(Q67A == "0", 1, Q67A),
    Q67B = ifelse(Q67B == "0", 1, Q67B),
    Q67C = ifelse(Q67C == "0", 1, Q67C),
    Q67D = ifelse(Q67D == "0", 1, Q67D),

    # it seems that "Not answered" for none
    Q67A = ifelse(Q67A == "Not answered", 1, Q67A),
    Q67B = ifelse(Q67B == "Not answered", 1, Q67B),
    Q67C = ifelse(Q67C == "Not answered", 1, Q67C),
    Q67D = ifelse(Q67D == "Not answered", 1, Q67D),

    # assume that 5+ is 5
    Q67A = ifelse(Q67A == "5+", 5, Q67A),
    Q67B = ifelse(Q67B == "5+", 5, Q67B),
    Q67C = ifelse(Q67C == "5+", 5, Q67C),
    Q67D = ifelse(Q67D == "5+", 5, Q67D),

    Adults_16_plus = as.numeric(Q67A),
    Children_0_4 = as.numeric(Q67B),
    Children_5_10 = as.numeric(Q67C),
    Children_11_15 = as.numeric(Q67D)
  )


# --- Step 2: Calculate Equivalence Scale Weights (Modified OECD scale) ---
# Weights:
#  First adult = 1.0
#  Additional adults (14+) = 0.5
#  Children (under 14) = 0.3

survey_data <- survey_data %>%
  rowwise() %>% # Process each row (household)
  mutate(
    base_adult_weight = 1.0,

    additional_adults_weight = ifelse(Adults_16_plus > 1, (Adults_16_plus - 1) * 0.5, 0),

    # Children aged 0-13 receive a weight of 0.3
    # mismatch in ranges
    children_weight = (Children_0_4 + Children_5_10) * 0.3,
    # children_under_14_weight = (Children_0_4 + Children_5_10 + Children_11_15) * 0.3,

    equiv_scale =
      base_adult_weight +
      additional_adults_weight +
      children_weight
  ) %>%
  ungroup()


survey_data <- survey_data %>%
  mutate(
    equiv_income = Household_Income_Numeric / equiv_scale
  )

{survey_data$equiv_income < 10000} |> table() |> prop.table() |> round(2)
