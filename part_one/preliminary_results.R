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

#' Will generate figures 2, 3, and 4 given a fitted linear model. Will also include a table summary of results
#' 
#' @param model Linear model of relevant predictors
#' @param relevant_predictors Documented relavent predictors
#' @param output_dir The folder to which the figures will be created into
#' @param pairwise_predictors The predictors to plot in the pred x pred plot. Must be a subset of relevant_predictors
generate_plots <- function(model, relevant_predictors, output_dir, pairwise_predictors) {

  # Just catches if we accidentally don't keep relevant_predictors and pairwise_predictors agreeing with each other. 
  # The long statement just prints the predictors that weren't present in both vectors (there's defo a better way but it works lmao)
  if (FALSE %in% (pairwise_predictors %in% relevant_predictors)) stop(paste0("Pairwise predictors not found in relevant_predictors: ", paste(setdiff(union(pairwise_predictors, relevant_predictors), intersect(pairwise_predictors, relevant_predictors)), collapse = ", ")))
 
  # If we ever change our model, this will prevent any discrepencies between documentation and model itself. 
  if (!identical(sort(all.vars(delete.response(terms(model)))), sort(relevant_predictors))) stop("Mismatch between # predictors documented vs in model")

  # Figure 2. Residual Plots (Residual vs Fitted and Normal QQ) ----
  png(file.path(output_dir, "prelim_results_resid_qq.png"), width = 1000, height = 400, res = 120)
  par(mfrow = c(1, 3))

  plot(model, which = 1, caption = "A) Residuals vs Fitted", id.n = 0)
  abline(h = 0, lty = 2, col = "grey40")        # y = 0 line

  plot(model, which = 2, caption = c("", "B) Normal QQ Plot"), qqline = FALSE, id.n = 0)
  abline(a = 0, b = 1, lty = 2, col = "grey40") # y = x line

  y <- model$model[[1]]   # response exactly as used in the fit
  plot(fitted(model), jitter(y, amount = 0.2),
       main = "C) Response vs Fitted",
       xlab = "Fitted values", ylab = "Life satisfaction (jittered)",
       pch = 16, col = rgb(0, 0, 0, 0.25))
  abline(a = 0, b = 1, lty = 2, col = "grey40")   # y = x reference line

  dev.off()

  # Figure 3. Pairwise relationships of each predictor in linear model ----
  png(file.path(output_dir, "prelim_results_pred_pairs.png"), width = 1300, height = 1300, res = 240)

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
  # Figure 4. Residuals against predictor plots ----
  panel_w = 2.6   # inches: width of every plot region
  panel_h = 2.0   # inches: height of every plot region
  n_col = 3       # number of columns to show
  gap = 0      # extra inches of spacing around each panel
  cex_lab = 0.8   # size of axis numbers / category labels
  cex_main = 0.9  # size of titles
  dpi = 150
  res <- resid(model)
  mf  <- model.frame(model)                   # only the rows used in the fit

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

  png(file.path(output_dir, "prelim_results_resid_vs_pred.png"), width = n_col * cell_w, height = n_row * cell_h, units = "in", res = dpi)
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
  invisible(c(width_in = n_col * cell_w, height_in = n_row * cell_h))

}

output_dir <- "/Users/jerrychen/Desktop/StatsFinalProject"

generate_plots(model, relevant_predictors, output_dir, c("PAADVVOL", "PAA_15A", "GEN_05", "GEN_01", "GEN_10"))
