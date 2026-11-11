knitr::purl("part_one/part_1.Rmd", output = "part_one/part_1.R") # doing this to be able to sorce the .rmd file
source("part_one/part_1.R", echo = FALSE)
graphics.off() # restart from the part_1.R

# code for looking at plots

# plot histograms of response
png(filename = "fix-assumptions/hist-of-response.png", width = 6, height = 4, units = "in", res = 300)
par(mfrow = c(1, 1), mai = c(1, 1, 0.8, 0.4)) # reset graphical parameters so the code actually plots
hist(model_data$LSM_01, main = paste("Histogram of response (LSM_01)"), xlab = "LSM_01")
dev.off()
# plot histograms and barplots of preds
png(filename = "fix-assumptions/hist-of-skew-correct-preds.png", width = 12, height = 8, units = "in", res = 500)
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

# plot scatterplot of response and fitted
png(filename = "fix-assumptions/scatterplot-response-fitted-boxcox.png", width = 6, height = 4, units = "in", res = 300)
plot(fitted(skew_corrected_model), model_data$LSM_01, # เปลี่ยน
     xlab = "fitted values", ylab = "respose",
     main = "scatter plot of respose vs. fitted values",
     pch = 16, col = rgb(0, 0, 0, 0.3),
     abline(a = 0, b = 1, lty = 2, col = "grey40")  # y = x reference line)
)
dev.off()

# plot pairwise plots of the predictors
png(filename = "fix-assumptions/pairwise-predictors-skew.png", width = 15, height = 8, units = "in", res = 500)
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

# resids and fitted values
png(filename = "fix-assumptions/resid-fitted-qq-boxcox.png", width = 6, height = 4, units = "in", res = 300)
par(mfrow = c(1, 2))

# เปลี่ยน
plot(skew_corrected_model, which = 1, caption = "A) Residuals vs Fitted", id.n = 0, pch = 16, col = rgb(0, 0, 0, 0.3),
     )
abline(h = 0, lty = 2, col = "grey40")        # y = 0 line

# เปลี่ยน
plot(skew_corrected_model, which = 2, caption = c("", "B) Normal QQ Plot"), qqline = FALSE, id.n = 0)
abline(a = 0, b = 1, lty = 2, col = "grey40") # y = x line
dev.off()
# resid and preds

panel_w = 2.6   # inches: width of every plot region
panel_h = 2.0   # inches: height of every plot region
n_col = 3       # number of columns to show
gap = 0      # extra inches of spacing around each panel
cex_lab = 0.8   # size of axis numbers / category labels
cex_main = 0.9  # size of titles
dpi = 150
res <- resid(skew_corrected_model) # เปลี่ยน
mf  <- skew_corrected_model.frame(model)  # เปลี่ยน                 # only the rows used in the fit 

# Classes to use box plot
box_classes <- c("Ord", "Cat", "YN", "YNC", "YNDK")

# All predictors that should be rep. with a boxplot
is_box <- function(p) general_classes[p] %in% box_classes
x_labels <- function(x) levels(factor(x))   # same groups and order boxplot(res ~ factor(x)) uses

# Word wrap by measured width in inches
wrap_inches <- function(text, width, cex, font) {
words <- strsplit(text, " ")[[1]]
lines <- character(0); cur <- ""
for (w in words) {
    trial <- if (cur == "") w else paste(cur, w)
    if (cur != "" && strwidth(trial, units = "inches", cex = cex, font = font) > width) {
    lines <- c(lines, cur); cur <- w
    } else cur <- trial
}
c(lines, cur)
}

# Measuring text on device
pdf(NULL); plot.new(); par(cex = 1)
line_in <- par("csi")                       # height of one text line, in inches

titles <- lapply(relevant_predictors, function(p)
wrap_inches(paste0("Residuals vs ", p, " (", dict[p], ")"), panel_w, cex_main, font = 2))
names(titles) <- relevant_predictors 

x_tick_labs <- lapply(relevant_predictors, function(p) {
if (!is_box(p)) return(NULL)
vapply(x_labels(mf[[p]]),
        function(l) paste(wrap_inches(l, 1.2, cex_lab, font = 1), collapse = "\n"), "")
})
names(x_tick_labs) <- relevant_predictors

# Wrapping each x-axis label to the panel's width (panel_w)
x_desc <- lapply(relevant_predictors, function(p)
if (is_box(p)) NULL else wrap_inches(dict[p], panel_w, cex_lab, font = 1))
names(x_desc) <- relevant_predictors

bottom_needed <- vapply(relevant_predictors, function(p) {
if (is_box(p)) {
    labs <- x_tick_labs[[p]]
    w <- max(strwidth(labs,  units = "inches", cex = cex_lab))  # widest line
    h <- max(strheight(labs, units = "inches", cex = cex_lab))  # tallest block of lines
    0.1 + w * sin(pi / 4) + h * cos(pi / 4)                     # gap + block rotated 45°
} else {
    (1.8 + length(x_desc[[p]]) * 1.1 * cex_lab) * line_in       # tick numbers + wrapped label
}
}, numeric(1))
dev.off()

# Creating margins for each plot looking at the worst case
max_title_lines <- max(lengths(titles))
mai <- c(bottom = max(bottom_needed),
        left   = 3.2 * line_in,                                        # y numbers + "Residuals"
        top    = (0.6 + max_title_lines * 1.1 * cex_main) * line_in,   # wrapped title
        right  = 0.1) + gap

# Computing number of rows needed according to how many predictors we're using
n_row  <- ceiling(length(relevant_predictors) / n_col)

# Computing the width of each cell
cell_w <- panel_w + mai[2] + mai[4]
cell_h <- panel_h + mai[1] + mai[3]

png(filename = "fix-assumptions/resid-pred-boxcox.png", # เปลี่ยน
    width  = cell_w * n_col,
    height = cell_h * n_row,
    units  = "in", res = dpi)
par(mfrow = c(n_row, n_col))
par(mai = mai, mgp = c(2, 0.6, 0), cex = 1) # cex = 1 undoes mfrow's automatic text shrinking

for (p in relevant_predictors) {
x <- mf[[p]]

if (is_box(p)) {
    boxplot(res ~ factor(x), xaxt = "n", xlab = "", ylab = "Residuals", cex.axis = cex_lab)
    labs <- x_tick_labs[[p]]
    y_in <- grconvertY(par("usr")[3], "user", "inches") - 0.1   # 0.1 in below the plot box
    text(seq_along(labs), grconvertY(y_in, "inches", "user"), labs,
        srt = 45, adj = c(1, 1), xpd = NA, cex = cex_lab)

# If the class is numeric, then do simple plot
} else if (general_classes[p] == "Num") {
    plot(x, res, xlab = "", ylab = "Residuals", pch = 16,
        col = rgb(0, 0, 0, 0.3), cex.axis = cex_lab)
    lines(lowess(x, res), col = "red", lwd = 2)

    # Wrapped x-axis description (see x_desc above)
    xl <- x_desc[[p]]
    for (i in seq_along(xl)) {
    mtext(xl[i], side = 1, line = 1.6 + (i - 1) * 1.1 * cex_lab, cex = cex_lab)
    }

} else stop("Unknown class for ", p, ": ", general_classes[p])

# Adding a residual = 0 line for reference
abline(h = 0, lty = 2, col = "grey50") 

# Title lines, stacked upward from just above the plot box
tl <- titles[[p]]; n <- length(tl)
for (i in seq_len(n)) {
    mtext(tl[i], side = 3, line = 0.3 + (n - i) * 1.1 * cex_main,
        font = 2, cex = cex_main)
}
}

dev.off()

# TRANSFORMATIONS

# fitting original model
model <- lm(
  formula = LSM_01 ~ PAADVVOL * GEN_05 + PAA_15A + LBFDGHPW + GEN_01 + GEN_10 + INCDGHH + EDDVH3 + FSCDVHF2 + DHH_SEX,
  data = model_data
)

# handing skewness of the numerical predictors

# fitting new model with transformed preds.
# PAADVVOL, PAA_15A is right skewed
# LBFDGHPW is left skewed
c_val <- max(model_data$LBFDGHPW) + 1 # for log(c-x) transformation to handel left skew
model_data$PAADVVOL <- log1p(model_data$PAADVVOL)
model_data$PAA_15A <- log1p(model_data$PAA_15A)
model_data$LBFDGHPW <- log(c_val - (model_data$LBFDGHPW))

skew_corrected_model <- lm(
formula = LSM_01 ~ PAADVVOL * GEN_05 + PAA_15A + LBFDGHPW + GEN_01 + GEN_10 + INCDGHH + EDDVH3 + FSCDVHF2 + DHH_SEX,
data = model_data
)