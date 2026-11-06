library(moments)
library(knitr)

source("data_cleaning.R", echo = FALSE)

# Figure 1. Histogram of all valid “general life satisfaction” responses ----
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
  "DHH_SEX"
)

# Subsetting data to just predictors and filtering out skipped values
model_data <- cleaned_data |>
  select(
    LSM_01, 
    all_of(relevant_predictors)
  ) |>
  drop_na()

ggplot(model_data, aes(x = LSM_01)) + 
  geom_bar(stat = "count", color = "black") + 
  theme_minimal() + 
  scale_x_continuous(
    breaks = 0:10
  ) +
  labs(
    x = "General Life Satisfaction: 0 (Very dissatisfied) to 10 (Very satisfied)",
    y = "# respondents who responded to all predictors"
  ) + 
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

ggsave(filename = "data_description_response_summary.png", width = 6, height = 4)


# Table 1. Summary of predictors used in preliminary linear model ----

# Numeric Table
numeric_table <- cleaned_data |>
  select(all_of(relevant_predictors)) |>
  select(any_of(numeric_classes)) |>
  pivot_longer(cols = everything(), names_to = "predictor", values_to = "value") |>
  group_by(predictor) |>
  summarize(
    lowest = min(value, na.rm = TRUE),
    highest = max(value, na.rm = TRUE),
    range = paste0(round(lowest, 1), " - ", round(highest, 1)),
    stdev = round(sd(value, na.rm = TRUE), 1),
    skewness = round(skewness(value, na.rm = TRUE), 1),
    skipped = sum(1*is.na(value)),
    total = n(),
    num_skipped = paste0(skipped, " / ", total),
    avg = round(mean(value, na.rm = TRUE), 1),
    median = median(value, na.rm = TRUE)
  ) |>
  mutate(
    description = dict[predictor], type = general_classes[predictor], distribution = "-", avg_metric = ifelse(type == "Ord", median, avg)
  ) |>
  select(
    "Predictor Code" = predictor,
    "Predictor Description" = description,
    "Type" = type,
    "Possible Values (Range)" = range,
    "Mean/Median/Mode" = avg_metric,
    "Stdev" = stdev,
    "Skewness" = skewness,
    "Distribution (% of answered)" = distribution, 
    "# Skipped" = num_skipped
  )

# Categorical Table
categorical_long <- cleaned_data |>
  select(any_of(intersect(relevant_predictors, factored_classes))) |>
  mutate(across(everything(), as.character)) |>
  pivot_longer(everything(), names_to = "predictor", values_to = "value")

level_summary <- categorical_long |>
  filter(!is.na(value)) |>
  count(predictor, value) |>                           
  mutate(.by = predictor, pct = 100 * n / sum(n)) |>  
  arrange(predictor, desc(n)) |>
  summarize(
    .by = predictor,
    n_levels     = n(),
    mode         = first(value),                      
    distribution = str_flatten(head(sprintf("%s %.0f%%", value, pct), 5), ", ") 
  )

categorical_table <- categorical_long |>
  group_by(predictor) |>
  summarize(
    skipped = sum(is.na(value)), 
    total = n()
  ) |>
  left_join(level_summary, by = "predictor") |> 
  mutate(
    description = dict[predictor],
    type        = "Cat",
    num_skipped = paste0(skipped, " / ", total),
  ) |>
  mutate(stdev = "-", skewness = "-", range = paste0(n_levels, " levels")) |>
  select(
    "Predictor Code" = predictor, 
    "Predictor Description" = description, 
    "Type" = type,
    "Possible Values (Range)" = range,
    "Mean/Median/Mode" = mode,
    "Stdev" = stdev,
    "Skewness" = skewness,
    "Distribution (% of answered)" = distribution, 
    "# Skipped" = num_skipped
  )

predictor_table <- rbind(numeric_table, categorical_table)

write.table(predictor_table, file = "data_description_predictor_table.txt", sep = "\t", row.names = FALSE, quote = FALSE)

rm(categorical_table, categorical_long, numeric_table, level_summary)
