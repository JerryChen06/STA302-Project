source("data_description.R", echo = FALSE)

# Adjusting dictionary temporarily for readability
temp_dict <- dict
temp_dict["PAA_15A"] <- "Hours of active transportation"
temp_dict["EDDVH3"] <- "Highest level of education"
num_responses <- sort(sapply(colnames(cleaned_data), FUN = function(column) {

  num_not_na <- sum(!is.na(cleaned_data[[column]]))
}), decreasing = TRUE)

names(num_responses) <- temp_dict[names(num_responses)]

num_vars <- c("LSM_01", "PAADVVOL", "PAA_15A", "LBFDGHPW", "GEN_05", "GEN_01", "GEN_10", "INCDGHH", "FSCDVHF2")

model_data <- cleaned_data |>
  drop_na(all_of(vars))

model <- lm(
  LSM_01 ~ PAADVVOL * GEN_05 + PAA_15A + LBFDGHPW + GEN_01 + GEN_10 + INCDGHH + EDDVH3 + FSCDVHF2 + DHH_SEX,
  data = model_data
)

# residuals and fitted values and qq plot
cap <- list("A) Residuals vs Fitted plot", "B) Normal Q-Q plot")

png("resid_qq.png", width = 1100, height = 500, res = 120)
par(mfrow = c(1, 2))
plot(model, which = 1, caption = cap)
plot(model, which = 2, caption = cap)
dev.off()

dev.off()

# Predictors x predictors 
png("pairs.png", width = 1000, height = 1000, res = 120)

pairs <- model_data %>%
  select(LSM_01, PAADVVOL, LBFDGHPW, GEN_05, GEN_01, GEN_10) %>%
  drop_na() %>%
  slice_sample(n = 2000) %>%
  mutate(across(everything(), as.numeric))

temp_temp_dict <- temp_dict
temp_temp_dict["LBFDGHPW"] <- "Usual hours worked\nper week"
colnames(pairs) <- paste0(colnames(pairs), "\n", temp_dict[colnames(pairs)])

pairs |> pairs(pch = 19, col = rgb(0, 0, 1, alpha = 0.2),
        main = "Figure 3: Pairwise Relationships of Selected Variables")

dev.off()

# residuals to predictors of numeric variables (scatterplots)
res <- resid(model)
mf <- model.frame(model)   # only the rows actually used in the fit

png("resid_vs_predictors_num.png", width = 900, height = 800, res = 120)
par(mfrow = c(3, 3))
for (v in num_vars) {
  x <- mf[[v]]
  pred_desc <- temp_dict[v]
  if (is.numeric(x)) {
    plot(x, res, xlab = pred_desc, ylab = "Residuals",
         main = paste("Residuals vs", v), pch = 16, col = rgb(0, 0, 0, 0.3))
    lines(lowess(x, res), col = "red", lwd = 2)
  } 
  abline(h = 0, lty = 2, col = "gray40")
}
mtext("Figure 4: Residuals vs Predictors", outer = TRUE, cex = 1.3, font = 2, line = 0.5)
dev.off()

png("resid_vs_pred_edu.png", width = 500, height = 700, res = 120)
par(mfrow = c(1, 1), mar = c(15, 4, 4, 2) + 0.1)

# helper function that draws x-axis labels rotated 45 degrees
rotated_x_labels <- function(labels, cex = 0.8) {
  usr <- par("usr")
  text(x = seq_along(labels),
       y = usr[3] - 0.04 * diff(usr[3:4]),
       labels = labels, srt = 45, adj = 1, xpd = TRUE, cex = cex)
}

# residuals vs predictors of the two categorical variables
png("resid_vs_pred_edu.png", width = 530, height = 500, res = 120)
par(mar = c(8, 4, 3, 1))  # extra bottom margin for the slanted labels
boxplot(res ~ mf[["EDDVH3"]], xlab = "", ylab = "Residuals",
        main = paste("Residuals vs", temp_dict["EDDVH3"]), xaxt = "n")
rotated_x_labels(levels(factor(mf[["EDDVH3"]])))

dev.off()

png("resid_vs_pred_sex.png", width = 500, height = 400, res = 120)
par(mfrow = c(1, 1), mar = c(5, 4, 3, 1))

boxplot(res ~ mf[["DHH_SEX"]], xlab = "", ylab = "Residuals",
        main = paste("Residuals vs", temp_dict["DHH_SEX"]), xaxt = "n")
rotated_x_labels(levels(factor(mf[["DHH_SEX"]])))

dev.off()