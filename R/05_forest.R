#' Forest Plot for Subgroup Analysis
#'
#' Fits generalized linear models for specified exposure variable on outcome
#' and calculates effect sizes (default OR) with 95% CI, P-values, and sample
#' proportions within multiple subgroups. Returns formatted data and forest plot.
#'
#' @param data Data frame containing outcome, exposure and all subgroup/covariate variables
#' @param outcome Character scalar, outcome variable name
#' @param exposure Character scalar, exposure variable name (main effect)
#' @param subgroups Character vector, subgroup variable names
#' @param covariates Character vector, additional covariates, default NULL
#' @param family GLM family, default "binomial"; can be "gaussian" etc.
#' @param output_dir Output directory for saving results, default NULL
#' @param save_format Save format: "none", "data", "plot", "all", default "none"
#' @param decimal_estimate Decimal places for effect estimates, default 2
#' @param decimal_pvalue Decimal places for P-values, default 3
#' @param line Logical, whether to draw separation lines between subgroups, default FALSE
#' @param prepare_plot Logical, whether to prepare data for forestploter, default TRUE
#' @param tm Forest plot theme, default "blue", you can alse choose green, cyan or passing a custom theme by forestploter::forest_theme
#' @param plot_title Plot title, default "Forest Plot of Subgroup Analysis"
#' @param xlab X-axis label, default "Odds Ratio"
#'
#' @return List containing forest plot data, plot object, and optional saved file paths
#' @export
subgroup_forest <- function(data,
                           outcome,
                           exposure,
                           subgroups,
                           covariates = NULL,
                           family = "binomial",
                           output_dir = NULL,
                           save_format = c("none", "data", "plot", "all"),
                           decimal_estimate = 2,
                           decimal_pvalue = 3,
                           line = FALSE,
                           prepare_plot = TRUE,
                           tm = "blue",
                            xlim = NULL,
                            ticks_at = NULL,
                           plot_title = "Forest Plot of Subgroup Analysis",
                           xlab = "Odds Ratio",
                            CI_title = "OR(95%CI)",
                            math_font = NULL,  # Math font like "STIX Two Math"
                           ensure_italic_p = TRUE
) {

  # Parameter validation
  save_format <- match.arg(save_format)
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  # Check dependencies
  if (!requireNamespace("forestploter", quietly = TRUE)) {
    stop("Please install.packages('forestploter')")
  }
  if (!requireNamespace("jstable", quietly = TRUE)) {
    stop("Please install.packages('jstable')")
  }
  # font config for italic p
  font_config_file <- NULL
  old_font_config <- NULL

  if (!is.null(math_font) || ensure_italic_p) {
    if (!requireNamespace("sysfonts", quietly = TRUE)) {
      warning("Package 'sysfonts' not available. Font configuration skipped.")
    } else {
      # create font config
      font_config_file <- setup_font_config(math_font, ensure_italic_p)

      if (!is.null(font_config_file)) {
        # save old font config
        old_font_config <- Sys.getenv("FONTCONFIG_FILE")
        Sys.setenv(FONTCONFIG_FILE = font_config_file)

        # restore original config
        on.exit({
          if (!is.null(old_font_config) && nzchar(old_font_config)) {
            Sys.setenv(FONTCONFIG_FILE = old_font_config)
          } else {
            Sys.unsetenv("FONTCONFIG_FILE")
          }
          if (!is.null(font_config_file) && file.exists(font_config_file)) {
            unlink(font_config_file)
          }
        })
      }
    }
  }

  # Check if variables exist
  all_vars <- unique(c(outcome, exposure, subgroups, covariates))
  missing_vars <- setdiff(all_vars, names(data))
  if (length(missing_vars) > 0) {
    stop("Variables not found in data: ", paste(missing_vars, collapse = ", "))
  }

  # check the vars
  data_processed <- data
  for (subgroup_var in subgroups) {
    if (!is.factor(data_processed[[subgroup_var]])) {
      warning(sprintf("Subgroup variable '%s' is not a factor. Converting to factor automatically.", subgroup_var))
      data_processed[[subgroup_var]] <- as.factor(data_processed[[subgroup_var]])
    }
  }

  # construct formular
  formula <- as.formula(paste(outcome, "~", exposure))

  # analysis by jstable
  res <- jstable::TableSubgroupMultiGLM(
    formula        = formula,
    var_subgroups  = subgroups,
    var_cov        = covariates,
    data           = data_processed,
    family         = family,
    decimal.estimate = decimal_estimate,
    decimal.percent  = 1,
    decimal.pvalue   = decimal_pvalue,
    line             = line
  )
  # clean the data
  plot_res <- res
  cols_to_blank <- c(2, 3, 7, 8)
  plot_res[cols_to_blank][is.na(plot_res[cols_to_blank])] <- " "
  plot_res$` ` <- paste(rep(" ", nrow(plot_res)), collapse = " ")
  plot_res[, 4:6] <- apply(plot_res[, 4:6], 2, as.numeric)
  final_data <- plot_res
  if (prepare_plot) {
    final_data <- plot_res
    colnames(final_data)[1] <- "Subgroup"
    colnames(final_data)[7] <- "𝑃  value"
    colnames(final_data)[8] <- "𝑃  for interaction"
    final_data$Subgroup <- ifelse(is.na(final_data$OR),
                                 final_data$Subgroup,
                                 paste0("        ", final_data$Subgroup))

    final_data$` ` <- "                                                                  "

    final_data <- final_data %>%
      dplyr::mutate(
        OR = as.numeric(OR),
        Lower = as.numeric(Lower),
        Upper = as.numeric(Upper),
        !!CI_title :=  ifelse(
          !is.na(OR) & !is.na(Lower) & !is.na(Upper),
          sprintf("    %.2f (%.2f-%.2f)    ", OR, Lower, Upper),
          " "
        )
      )
    final_data[is.na(final_data$`OR(95%CI)`), "OR(95%CI)"] <- " "
  }
  if(tm %in% c("blue", "green", "cyan")) {
    tm <- get_forest_theme(tm, background_levels = generate_background_levels(final_data))
  }
  # Make Forest Plot ####
  forest_plot <- NULL
  if (nrow(final_data) > 0 && prepare_plot) {
    valid_rows <- !is.na(final_data$OR) & !is.na(final_data$Lower) & !is.na(final_data$Upper)
    if (sum(valid_rows) > 0) {
      forest_plot <- tryCatch({
        forestploter::forest(
          final_data[, c(1,10,7,9,8)],,
          est = final_data$OR,
          lower = final_data$Lower,
          upper = final_data$Upper,
          ci_column = 4,  # blank col for forest plot
          ref_line = 1,
          is_summary = c(TRUE, rep(FALSE, nrow(final_data) - 1)),
          xlab = xlab,
          title = plot_title,
          xlim = xlim,
          ticks_at = ticks_at,
          theme = tm
        )
      }, error = function(e) {
        warning("Forest plot generation failed: ", e$message)
        return(NULL)
      })
    } else {
      warning("No valid data available for forest plot generation")
    }
  }

  saved_files <- character(0)
  if (save_format != "none" && !is.null(output_dir)) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }

    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

    if (save_format %in% c("data", "all")) {
      # save raw data
      csv_file <- file.path(output_dir, sprintf("subgroup_forest_raw_%s.csv", timestamp))
      write.csv(plot_res, csv_file, row.names = FALSE)
      saved_files <- c(saved_files, csv_file)

      # save formated data
      if (prepare_plot) {
        formatted_file <- file.path(output_dir, sprintf("subgroup_forest_formatted_%s.csv", timestamp))
        write.csv(final_data, formatted_file, row.names = FALSE)
        saved_files <- c(saved_files, formatted_file)
      }
    }

    if (save_format %in% c("plot", "all") && !is.null(forest_plot)) {
  # Save pdf by Cairo
  pdf_file <- file.path(output_dir, sprintf("subgroup_forest_%s.pdf", timestamp))
  grDevices::cairo_pdf(pdf_file, width = 10, height = 6, family = "Times New Roman")
  print(forest_plot)
  grDevices::dev.off()
  saved_files <- c(saved_files, pdf_file)
}
  }

  # return list
  return(list(
    raw_data = plot_res,
    plot_data = final_data,
    forest_plot = forest_plot,
    saved_files = if (length(saved_files) > 0) saved_files else NULL,
    call = match.call()
  ))
}
#' Setup Font Configuration for Math Symbols (Internal)
#' @keywords internal
setup_font_config <- function(math_font = NULL, ensure_italic_p = TRUE) {
  if (is.null(math_font)) {
    math_font <- "STIX Two Math"
  }
  # check the availablity of font
  if (!sysfonts::font_families() %>% grepl(math_font, .) %>% any()) {
    warning("Math font '", math_font, "' not found. Using system default.")
    return(NULL)
  }

  # creat font config
  fc <- tempfile(fileext = ".conf")

  config_content <- sprintf(
'<?xml version="1.0"?>
<!DOCTYPE fontconfig SYSTEM "fonts.dtd">
<fontconfig>
  <!-- Math symbol fallback for italic P characters -->
  <alias>
    <family>serif</family>
    <prefer>
      <family>Times New Roman</family>
      <family>%s</family>
    </prefer>
  </alias>
  <alias>
    <family>sans-serif</family>
    <prefer>
      <family>Arial</family>
      <family>%s</family>
    </prefer>
  </alias>
  <alias>
    <family>monospace</family>
    <prefer>
      <family>Courier New</family>
      <family>%s</family>
    </prefer>
  </alias>

  <!-- Specific handling for mathematical italic P (U+1D443) -->
  <match target="pattern">
    <test name="charset" compare="contains">
      <string>U+1D443</string> <!-- Mathematical Italic Capital P -->
    </test>
    <edit name="family" mode="prepend" binding="strong">
      <string>%s</string>
    </edit>
  </match>
</fontconfig>', math_font, math_font, math_font, math_font)

  writeLines(config_content, fc)
  return(fc)
}
#' Get Predefined Forest Plot Themes
#'
#' Provides several predefined color themes for forest plots with Times New Roman font.
#'
#' @param theme_name Theme name: "blue", "green", "cyan", or "default"
#' @param base_size Base font size, default 12
#' @param background_levels Background color levels vector for subgroup hierarchy
#' @return Forest plot theme object
#' @export
#'
#' @examples
#' \dontrun{
#' tm <- get_forest_theme("blue")
#' result <- subgroup_forest(..., tm = tm)
#' }
get_forest_theme <- function(theme_name = c("blue", "green", "cyan", "default"),
                            base_size = 12,
                            background_levels = NULL) {

  theme_name <- match.arg(theme_name)

  # color schemes
  color_schemes <- list(
    blue = list(
      summary_fill = "#4575b4",
      summary_col = "#4575b4",
      footnote_col = "blue",
      background_colors = c("#f5fcff", "#c8f0f6", "#a2d2eb")  # blue from thin to thick
    ),
    green = list(
      summary_fill = "#2e8b57",
      summary_col = "#2e8b57",
      footnote_col = "darkgreen",
      background_colors = c("#f7fcf7", "#d8f0d8", "#a3d9a3")  # green from thin to thick
    ),
    cyan = list(
      summary_fill = "#008b8b",
      summary_col = "#008b8b",
      footnote_col = "darkcyan",
      background_colors = c("#f5fcfc", "#c8f8f8", "#92d0d0")  # cyan from thin to thick
    ),
    default = list(
      summary_fill = "#333333",
      summary_col = "#333333",
      footnote_col = "black",
      background_colors = c("#ffffff", "#f0f0f0", "#e0e0e0")  # gray from thin to thick
    )
  )

  scheme <- color_schemes[[theme_name]]

  # bg color
  if (!is.null(background_levels)) {
    names(scheme$background_colors) <- 1:3
    bg_fill <- scheme$background_colors[as.character(background_levels)]
  } else {
    bg_fill <- NULL
  }

  # create theme
  theme <- forestploter::forest_theme(
    base_size = base_size,
    base_family = "Times New Roman",

    # Confidence interval settings
    ci_pch = 16,
    ci_col = "black",
    ci_fill = "black",
    ci_alpha = 1,
    ci_lty = 1,
    ci_lwd = 2,
    ci_Theight = 0.2,

    # Reference line
    refline_gp = grid::gpar(lwd = 1.5, lty = "dashed", col = "grey20"),

    # Vertical lines
    vertline_lwd = 1,
    vertline_lty = "dashed",
    vertline_col = "grey20",

    # Summary row
    summary_fill = scheme$summary_fill,
    summary_col = scheme$summary_col,

    # Footnote
    footnote_gp = grid::gpar(cex = 0.7, fontface = "italic", col = scheme$footnote_col),

    # X-axis
    xaxis_gp = grid::gpar(fontsize = base_size, fontfamily = "Times New Roman", lwd = 1.5),

    # Core styling with background colors if provided
    core = list(
      bg_params = if (!is.null(bg_fill)) list(fill = bg_fill) else list()
    ),

    # Column headers
    colhead = list(
      fg_params = list(hjust = 0.5, x = 0.5, fontfamily = "Times New Roman")
    )
  )

  return(theme)
}
#' Generate Background Levels for Forest Plot (Internal)
#'
#' Creates background level vector based on data structure:
#' - First row: level 1 (Title)
#' - Rows with NA OR: level 3 (Subgroup Title)
#' - Other rows: alternating between 1 and 2 (row of data)
#'
#' @param forest_data Data frame from subgroup analysis
#' @return Numeric vector of background levels
#' @keywords internal
generate_background_levels <- function(forest_data) {
  if (nrow(forest_data) == 0) return(numeric(0))

  levels <- numeric(nrow(forest_data))
  data_row_counter <- 0  # calculate row of data

  for (i in 1:nrow(forest_dsata)) {
    if (i == 1) {
      levels[i] <- 1  # first row
    } else if (is.na(forest_data$OR[i])) {
      levels[i] <- 3  # subgroup title row
      data_row_counter <- 0  # reset the counter
    } else {
      data_row_counter <- data_row_counter + 1
      # 1 2 1 2 1 2....
      levels[i] <- ifelse(data_row_counter %% 2 == 1, 1, 2)
    }
  }

  return(levels)
}
# fast function
#' @rdname get_forest_theme
#' @export
tm_blue <- function(base_size = 12, background_levels = NULL) {
  get_forest_theme("blue", base_size, background_levels)
}

#' @rdname get_forest_theme
#' @export
tm_green <- function(base_size = 12, background_levels = NULL) {
  get_forest_theme("green", base_size, background_levels)
}

#' @rdname get_forest_theme
#' @export
tm_cyan <- function(base_size = 12, background_levels = NULL) {
  get_forest_theme("cyan", base_size, background_levels)
}

#' @rdname get_forest_theme
#' @export
tm_default <- function(base_size = 12, background_levels = NULL) {
  get_forest_theme("default", base_size, background_levels)
}


###### test
# data(cancer, package = "survival")
# result <- subgroup_forest(
#   data = colon,
#   outcome = "status",
#   exposure = "nodes",
#   subgroups = c("sex", "adhere", "rx"),
#   covariates = c("age"),
#   output_dir = "./test_output",
#   save_format = "all",
#   CI_title = "OR(95% CI)"
# )