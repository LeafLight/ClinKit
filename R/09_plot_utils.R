#' Faceted Violin Plot
#'
#' Draws faceted violin plots for multiple indicators, grouped by a grouping variable,
#' with each indicator in a separate facet
#'
#' @param data    Data frame
#' @param vars    Character vector, names of indicator variables to plot
#' @param groupby Character scalar, name of grouping variable
#'
#' @return ggplot object
#'
#' @details
#' This function will:
#' 1. Select specified columns and convert to long format
#' 2. Convert grouping variable to factor
#' 3. Draw violin plots with boxplots inside, faceted by indicator
#'
#' @examples
#' \dontrun{
#' p <- facet_violin(iris, c("Sepal.Length", "Sepal.Width"), "Species")
#' print(p)
#' }
#'
#' @importFrom ggplot2 ggplot aes_string geom_violin geom_boxplot facet_wrap scale_fill_manual labs theme
#' @importFrom dplyr select
#' @importFrom tidyr pivot_longer
facet_violin <- function(data, vars, groupby) {

  # Select columns and convert to long format
  plot_dat <- data %>%
    dplyr::select(all_of(c(groupby, vars))) %>%
    tidyr::pivot_longer(cols = all_of(vars),
                        names_to = "indicator",
                        values_to = "value")

  # Convert grouping variable to factor
  plot_dat[[groupby]] <- factor(plot_dat[[groupby]])

  # Create plot
  ggplot(plot_dat,
         aes_string(x = groupby, y = "value", fill = groupby)) +
    geom_violin(trim = FALSE, alpha = 0.8) +
    geom_boxplot(width = 0.15, colour = "black", alpha = 0.9) +
    facet_wrap(~indicator, nrow = 3, scales = "free") +
    #scale_fill_manual(values = c("#FF6B6B", "#4ECDC4")) +
    labs(x = NULL, y = NULL,
         title = paste("Violin plots of indicators by", groupby)) +
    ggprism::theme_prism(base_size = 14) +
    theme(legend.position = "none",
          strip.text = element_text(face = "bold"))
}

#' Scatter Plot with Regression Line (Basic Version)
#'
#' Quickly draws X-Y scatter plot with linear regression confidence band,
#' and annotates R² and P value within the plot;
#' Supports rug plots and additional aesthetic parameters for points
#'
#' @param data     \code{data.frame}, must contain x and y columns
#' @param x,y      Character scalars, variable names
#' @param digits   Numeric, decimal places for R² and P value, default 3
#' @param label.x,label.y  Coordinates for annotation; when NULL, automatically placed at top-left 5% position
#' @param ...      Additional parameters passed to \code{geom_point()} and \code{geom_rug()},
#'                 e.g., \code{color = group, size = 2, alpha = 0.6}
#'
#' @return         \code{ggplot} object, can be further customized with \code{+} or saved
#' @details
#' \itemize{
#'   \item Uses \code{lm(y ~ x)} for fitting and draws 95% confidence band
#'   \item Rug plots at bottom and left with jitter to avoid overlap
#'   \item Annotation text uses Times font for consistent theming
#' }
#'
#' @examples
#' \dontrun{
#' p <- scatter_lm(mtcars, "wt", "mpg",
#'                 color = cyl, size = 3, alpha = 0.7)
#' print(p)
#' ggsave("scatter_lm.png", p, width = 5, height = 4)
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_rug geom_smooth annotate
#' @importFrom stats lm reformulate
#' @export
scatter_lm <- function(data, x, y,
                          digits = 3,
                          label.x = NULL,
                          label.y = NULL,
                          marginal = c("histogram", "density", "boxplot"),
                          bins = 15,
                          margin_fill = "grey60",
                          margin_color = "white", ...) {

  # ---- 0. Convert strings to symbols ----
  x_sym <- sym(x)
  y_sym <- sym(y)

  # ---- 1. Fit model & calculate metrics ----
  fit  <- lm(reformulate(x, y), data = data)
  summ <- summary(fit)
  r2   <- summ$r.squared
  pval <- summ$coefficients[2, 4]
  lab  <- sprintf("R² = %.*f\np  = %.*g", digits, r2, digits, pval)

  # ---- 2. Default label positions ----
  if (is.null(label.x))
    label.x <- min(data[[x]], na.rm = TRUE) + 0.05 * diff(range(data[[x]], na.rm = TRUE))
  if (is.null(label.y))
    label.y <- max(data[[y]], na.rm = TRUE) - 0.10 * diff(range(data[[y]], na.rm = TRUE))

  # ---- 3. Main plot ----
  p_main <- ggplot(data, aes(!!x_sym, !!y_sym)) +
    geom_point(..., na.rm = TRUE) +
    geom_rug(..., position = 'jitter', size = 0.1, na.rm = TRUE) +
    geom_smooth(method = "lm", se = TRUE, color = "firebrick") +
    annotate("text", x = label.x, y = label.y,
             label = lab, hjust = 0, vjust = 0, size = 4, color = "black")

  # ---- 4. Return main plot (marginal plots parameter kept for compatibility) ----
  p_main
}

#' Scatter Plot with Marginal Density/Histogram and Regression Equation (Enhanced Version)
#'
#' Main plot: scatter points + linear regression line + automatic annotation of equation, R², p value (ggpmisc::stat_poly_eq).
#' Marginal plots: bilateral densigram (density + histogram mix), supports grouping colors.
#' Color logic: uniform color when no color grouping; automatic coloring by group variable with hidden legend when color specified.
#'
#' @param data     Data frame, must contain x, y and optional color grouping variable
#' @param x,y      Character scalars, variable names
#' @param digits   Decimal places for equation/statistics, default 3 (controlled by stat_poly_eq)
#' @param label.x,label.y  Deprecated, kept for backward compatibility
#' @param color    Character scalar, grouping variable name; NULL for no grouping
#' @param marginal Marginal plot type, fixed as "densigram"
#' @param bins     Number of histogram bins, passed to ggExtra::ggMarginal
#' @param margin_fill,margin_color  Fill color and border color for marginal plots
#' @param ...      Additional aesthetic parameters passed to geom_point()
#'
#' @return         ggplot object (with marginal layers), can be further customized with + or saved
#' @details
#' \itemize{
#'   \item Uses ggpmisc::stat_poly_line() + stat_poly_eq() to automatically add regression line and equation
#'   \item Marginal plots are densigram (density + histogram), supports group colors/fill
#'   \item Uniform color `#479E88` when no grouping; uses scale_color_npg() with hidden legend when grouping
#'   \item Returned object already includes marginal layers, can be directly saved with ggsave()
#' }
#'
#' @examples
#' \dontrun{
#' p <- scatter_lm_marginal(mtcars, "wt", "mpg", color = "cyl", bins = 20)
#' print(p)
#' ggsave("scatter_marginal.png", p, width = 6, height = 5)
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_point theme_classic guides theme
#' @importFrom ggpmisc stat_poly_line stat_poly_eq
#' @importFrom ggExtra ggMarginal
#' @importFrom rlang sym
#' @export
scatter_lm_marginal <- function(data, x, y,
                          digits = 3,
                          label.x = NULL,
                          label.y = NULL,
                                color = NULL,
                          marginal = c("histogram", "density", "boxplot"),
                          bins = 15,
                          margin_fill = "grey60",
                          margin_color = "white", ...) {

  # ---- 0. Convert strings to symbols ----
  x_sym <- sym(x)
  y_sym <- sym(y)

  # ---- 1. Fit model & calculate metrics ----
  fit  <- lm(reformulate(x, y), data = data)
  summ <- summary(fit)
  r2   <- summ$r.squared
  pval <- summ$coefficients[2, 4]
  lab  <- sprintf("R² = %.*f\np  = %.*g", digits, r2, digits, pval)

  # ---- 2. Default label positions (kept for compatibility) ----
  if (is.null(label.x))
    label.x <- min(data[[x]], na.rm = TRUE) + 0.05 * diff(range(data[[x]], na.rm = TRUE))
  if (is.null(label.y))
    label.y <- max(data[[y]], na.rm = TRUE) - 0.10 * diff(range(data[[y]], na.rm = TRUE))

  # ---- 3. Main plot ----
  if(is.null(color)){
    # No color grouping - use uniform color
    data$`.no_color` <- "black"
    color <- ".no_color"
    p_main <- ggplot(data, aes(!!x_sym, !!y_sym, color = !!sym(color))) +
      geom_point(alpha = 0.5, na.rm = TRUE) +
      stat_poly_line(formula = y ~ x) +
      stat_poly_eq(
        aes(label = paste(..eq.label.., ..rr.label.., ..p.value.label.., sep = "*\", \"*")),
        formula = y ~ x,
        parse = TRUE
      ) +
      scale_color_manual(values = "#479E88") +
      guides(colour = "none", size = "none") +
      theme_classic()
  } else {
    # With color grouping
    p_main <- ggplot(data, aes(!!x_sym, !!y_sym, color = !!sym(color))) +
      geom_point(alpha = 0.5, na.rm = TRUE) +
      stat_poly_line(formula = y ~ x) +
      stat_poly_eq(
        aes(label = paste(..eq.label.., ..rr.label.., ..p.value.label.., sep = "*\", \"*")),
        formula = y ~ x,
        parse = TRUE
      ) +
      scale_color_npg() +
      guides(colour = "none", size = "none") +
      theme_classic()
  }

  # ---- 4. Add marginal plots ----
  ggExtra::ggMarginal(p_main,
                     type = 'densigram',
                     margins = 'both',
                     size = 5,
                     groupColour = TRUE,
                     groupFill = TRUE,
                     alpha = 0.4)
}
#p<-scatter_lm_marginal(mtcars, "disp", "cyl")
#ggsave("./test_output/1.tiff",p, width = 10, height = 10)
#facet_violin(mtcars, "disp", "gear")