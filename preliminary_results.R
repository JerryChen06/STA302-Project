#' <relevant_predictors> Vector of the predictor codes that are being considered
#' <subsetted_df> Cleaned data subsetted to just relevant_predictors. Filtered out all rows with a missing value. 
#' <numeric_classes> Vector of the codes of all ord. cat. and numeric variables
#' <factored_classes> Vector of the codes of all categorical variables that are factored in the data (includes all Y/N variables)
source("data_description.R", echo = FALSE)

# Fitting the model
model <- lm(
  formula = LSM_01 ~ PAADVVOL * GEN_05 + PAA_15A + LBFDGHPW + GEN_01 + GEN_10 + INCDGHH + EDDVH3 + FSCDVHF2 + DHH_SEX,
  data = model_data
)

# If we ever change our model, this will prevent any discrepencies between documentation and model itself. 
if (!identical(sort(all.vars(delete.response(terms(model)))), sort(relevant_predictors))) stop("Mismatch between # predictors documented vs in model")

# Figure 2. Residual Plots (Residual vs Fitted and Normal QQ)
png("prelim_results_resid_qq.png", width = 1100, height = 500, res = 120)
par(mfrow = c(1, 2))
plot(model, which = 1, caption = "A) Residuals vs Fitted")
plot(model, which = 2, caption = "B) Normal QQ Plot")
dev.off()

dev.off()

# Predictors x predictors 
png("pairs.png", width = 800, height = 800, res = 120)

pairs <- model_data %>%
  select(LSM_01, PAADVVOL, LBFDGHPW, GEN_05, GEN_01, GEN_10) %>%
  drop_na() %>%
  slice_sample(n = 2000) %>%
  mutate(across(everything(), as.numeric))

temp_temp_dict <- temp_dict
temp_temp_dict["LBFDGHPW"] <- "Usual hours worked\nper week"
temp_temp_dict["LSM_01"] <- "Satisfaction with\nlife in general"
temp_temp_dict["PAADVVOL"] <- "Volume of\nweekly activity"
temp_temp_dict["GEN_05"] <- "Perceived mental\nhealth"
temp_temp_dict["GEN_10"] <- "Perceived life\nstress"
colnames(pairs) <- paste0(colnames(pairs), "\n", temp_temp_dict[colnames(pairs)])

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