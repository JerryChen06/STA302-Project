knitr::purl("part_one/part_1.Rmd", output = "part_one/part_1.R") # doing this to be able to sorce the .rmd file
source("part_one/part_1.R", echo = FALSE)

# before checking assumptions, look at the distribution of data
# plot histograms of response and predictors

png(filename = "fix-assumptions/hist-of-response.png", width = 6, height = 4, units = "in", res = 300)
par(mfrow = c(1, 1), mai = c(1, 1, 0.8, 0.4)) # reset graphical parameters so the code actually plots
hist(model_data$LSM_01, main = paste("Histogram of response (LSM_01)"), xlab = "LSM_01")

dev.off()

relevant_predictors <- c(
  "boxcox_PAADVVOL", 
  "boxcox_PAA_15A",
  "boxcox_LBFDGHPW",
  "GEN_05",
  "GEN_01",
  "GEN_10",
  "INCDGHH",
  "EDDVH3",
  "FSCDVHF2",
  "DHH_SEX"
)

png(filename = "fix-assumptions/hist-of-boxcox-num-preds.png", width = 12, height = 8, units = "in", res = 500)
par(mfrow = c(2, 5), mai = c(1, 1, 0.8, 0.4)) # reset graphical parameters so the code actually plots
for (p in relevant_predictors) {
    if (is.numeric(model_data[[p]])) {
        hist(model_data[[p]], main = p, xlab = p, las = 2)
    }
    else {
        barplot(table(model_data[[p]]), main = p, xlab = p, las = 2)
    }
}


dev.off()


# check conditional mean respose condition

# need to run model to get fitted vals
model <- lm(
  formula = LSM_01 ~ PAADVVOL * GEN_05 + PAA_15A + LBFDGHPW + GEN_01 + GEN_10 + INCDGHH + EDDVH3 + FSCDVHF2 + DHH_SEX,
  data = model_data
)
# plot scatterplot of response and fitted
png(filename = "fix-assumptions/scatterplot-response-fitted.png", width = 6, height = 4, units = "in", res = 300)
plot(fitted(model), model_data$LSM_01,
     xlab = "fitted values", ylab = "respose",
     main = "scatter plot of respose vs. fitted values",
     pch = 16, col = rgb(0, 0, 0, 0.3),
     abline(a = 0, b = 1, lty = 2, col = "grey40")  # y = x reference line)
)

dev.off()


# check conditional mean predictor condition
# plot pairwise plots of the predictors
png(filename = "fix-assumptions/pairwise-predictors.png", width = 15, height = 8, units = "in", res = 500)
pairs_raw <- model_data |>
select(all_of(pairwise_predictors)) |>
drop_na()

level_names <- lapply(pairs_raw, levels)   # labels of each factor column; NULL for numeric columns

pairs_df <- pairs_raw |>
mutate(across(everything(), as.numeric))

is_discrete <- function(v) length(unique(v)) <= 11

# Labels for the ticks of column k: factor labels if it was a factor, otherwise the numbers
tick_labels <- function(v, k) {
lv <- level_names[[k]]
if (is.null(lv)) TRUE else lv[ticks(v)]
}

lims  <- function(v) if (is_discrete(v)) range(v) + c(-0.6, 0.6) else extendrange(v)
ticks <- function(v) if (is_discrete(v)) sort(unique(v)) else pretty(v)

box_panel <- function(x, y, ...) {

# Replacing pairs' default limits 
par(usr = c(lims(x), lims(y)))

# Drawing new axes
pos <- par("mfg")                                # (row, col, n_rows, n_cols)
if (pos[1] == pos[3]) axis(1, at = ticks(x), labels = tick_labels(x, pos[2]), cex.axis = 0.7, xpd = NA)
if (pos[2] == 1)      axis(2, at = ticks(y), labels = tick_labels(y, pos[1]), cex.axis = 0.7, xpd = NA, las = 1)
if (pos[1] == 1 && pos[2] == pos[4]) {
    axis(3, at = ticks(x), labels = tick_labels(x, pos[2]), cex.axis = 0.7, xpd = NA)
    axis(4, at = ticks(y), labels = tick_labels(y, pos[1]), cex.axis = 0.7, xpd = NA, las = 1)
}

# If x is discrete but y is not, then draw barplot horizontally (y ~ x)
if (is_discrete(x) && !is_discrete(y)) {
    boxplot(y ~ x, at = sort(unique(x)), add = TRUE, axes = FALSE,
            col = "lightblue", outcex = 0.3, lwd = 0.5)
}

# If y is discrete but x isn't, then draw normal vertical barplots (x ~ y)
else if (!is_discrete(x) && is_discrete(y)) {
    boxplot(x ~ y, at = sort(unique(y)), horizontal = TRUE, add = TRUE, axes = FALSE,
            col = "lightblue", outcex = 0.3, lwd = 0.5)
}

# If both discrete, then make one bubble per combination whose area is proportional to count
else if (is_discrete(x) && is_discrete(y)) {
    counts <- as.data.frame(table(x, y))
    counts <- counts[counts$Freq > 0, ]
    symbols(as.numeric(as.character(counts$x)), as.numeric(as.character(counts$y)),
            circles = sqrt(counts$Freq),           # sqrt so the area tracks the count
            inches = 0.08,                         # largest bubble radius in this panel
            add = TRUE, bg = rgb(0, 0, 1, 0.4), fg = rgb(0, 0, 1, 0.6))
}

else {
    points(x, y, pch = 19, col = rgb(0, 0, 1, 0.2))
}
}

wrap_label <- function(x, y, labels, ...) {
chars_per_line <- floor(par("pin")[1] / strwidth("n", units = "inches", cex = 0.8))
desc <- paste(strwrap(dict[labels], width = chars_per_line), collapse = "\n")
text(x, y, paste0(labels, "\n", desc), cex = 0.8)
}

pairs(pairs_df, panel = box_panel, text.panel = wrap_label, xaxt = "n", yaxt = "n")

dev.off()

# now correcting assumptions. i will do a boxcox on numeric predictors to see what will happen

cont_preds <- c("PAADVVOL", "PAA_15A", "LBFDGHPW")

# powerTransform needs positive values, so shift anything with zeros
shift_pos <- function(x) if (min(x) <= 0) x + (1 - min(x)) else x
X <- as.data.frame(lapply(model_data[cont_preds], shift_pos))

# add them so i can fit a model with the shifted values
for (v in cont_preds) {
  model_data[[paste0(v, "_s")]] <- shift_pos(model_data[[v]])
}

p <- powerTransform(X)
summary(p)



# fitting new model with transformed preds.
model_data$boxcox_PAADVVOL <- (model_data$PAADVVO_s)**0.25
model_data$boxcox_PAA_15A <- (model_data$PAA_15A_s)**(-0.5)
model_data$boxcox_LBFDGHPW <- (model_data$LBFDGHPW_s)**2.25

boxcox_num_pred_model <- lm(
  formula = LSM_01 ~ boxcox_PAADVVOL * GEN_05 + boxcox_PAA_15A + boxcox_LBFDGHPW + GEN_01 + GEN_10 + INCDGHH + EDDVH3 + FSCDVHF2 + DHH_SEX,
  data = model_data
)