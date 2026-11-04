# Histogram of response variable ----
relevant_predictors <- c(
  "PAADVVOL", 
  "PAA_15A",
  "LBFDGHPW",
  "GEN_05",
  "GEN_01",
  "GEN_10",
  "INCDGHH",
  "EDDVH3",
  "FSCDVHF2",
  "DHH_SEX",
  "HWTDGBCC"
)

# Entire
ggplot(cleaned_data, aes(x = LSM_01)) + 
  geom_bar(stat = "count") + 
  theme_minimal() + 
  scale_x_continuous(
    breaks = 0:10
  ) +
  labs(
    x = "General Life Satisfaction: 0 (Very dissatisfied) to 10 (Very satisfied)",
    y = "Number of respondents who answered the question"
  )

# Subset
subsetted_df <- cleaned_data |>
  select(
    LSM_01, 
    all_of(relevant_predictors)
  ) |>
  drop_na()

ggplot(subsetted_df, aes(x = LSM_01)) + 
  geom_bar(stat = "count") + 
  theme_minimal() + 
  scale_x_continuous(
    breaks = 0:10
  ) +
  labs(
    x = "General Life Satisfaction: 0 (Very dissatisfied) to 10 (Very satisfied)",
    y = "Number of respondents who responded to all predictors"
  )


# Table
library(moments)

predictor_table_numeric <- cleaned_data |>
  select(all_of(relevant_predictors)) |>
  select(any_of(numeric_classes)) |>
  pivot_longer(cols = everything(), names_to = "predictor", values_to = "value") |>
  group_by(predictor) |>
  summarize(
    lowest = min(value, na.rm = TRUE),
    highest = max(value, na.rm = TRUE),
    range = paste0(round(lowest, 1), " - ", round(highest, 1)),
    variance = var(value, na.rm = TRUE),
    skewness = skewness(value, na.rm = TRUE),
    skipped = sum(1*is.na(value)),
    total = n(),
    num_skipped = paste0(skipped, " / ", total)
  ) |>
  mutate(
    description = dict[predictor]
  ) |>
  select(predictor, description, range, variance, skewness, num_skipped)

predictor_table_factored <- cleaned_data |>
  select(all_of(relevant_predictors)) |>
  select(any_of(factored_classes)) |>
  pivot_longer(cols = everything(), names_to = "predictor", values_to = "value") |>
  group_by(predictor) |>
  summarize(
    range = n_distinct(value),
    variance = var(as.numeric(levels(value)[value]), na.rm = TRUE),
    skewness = skewness(as.numeric(levels(value)[value]), na.rm = TRUE),
    skipped = sum(1*is.na(value)),
    total = n(),
    num_skipped = paste0(skipped, " / ", total)
  ) |>
  mutate(
    description = dict[predictor]
  ) |>
  select(predictor, description, range, variance, skewness, num_skipped)

predictor_table <- rbind(predictor_table_numeric, predictor_table_factored)

write_csv(predictor_table, file = "predictor_table.csv")