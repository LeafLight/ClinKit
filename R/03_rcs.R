#' Restricted Cubic Splines (RCS) Logistic Regression with Plot
#'
#' Fits RCS logistic regression models and generates publication-ready plots
#'
#' @param data Data frame containing all variables
#' @param outcome Outcome variable name
#' @param predictor Continuous predictor variable name for RCS
#' @param covariates Covariates for adjusted model (optional)
#' @param knots Number of knots for RCS (default 4)
#' @param output_dir Output directory (optional)
#' @param save_format Save format: "none", "tiff", "svg", "pdf", "all" (default "none")
#' @param filename Custom filename prefix (optional)
#'
#' @return List containing plot object, p-values, and optional saved file paths
#' @export
generate_rcs_plot <- function(data,
                             outcome,
                             predictor,
                             covariates = NULL,
                             knots = 4,
                             output_dir = NULL,
                             save_format = c("none", "tiff", "svg", "pdf", "all"),
                             filename = NULL) {
  if (!is.null(output_dir) && nzchar(output_dir)) {
  output_dir <- normalizePath(output_dir, mustWork = FALSE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(output_dir)) stop("Cannot create output_dir: ", output_dir)
}

  # Parameter validation
  save_format <- match.arg(save_format)
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  # Check dependencies
  if (!requireNamespace("rms", quietly = TRUE)) {
    stop("Please install.packages('rms')")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Please install.packages('ggplot2')")
  }

  # Check if variables exist
  all_vars <- unique(c(outcome, predictor, covariates))
  missing_vars <- setdiff(all_vars, names(data))
  if (length(missing_vars) > 0) {
    stop("Variables not found in data: ", paste(missing_vars, collapse = ", "))
  }

  # Remove missing values
  complete_cases <- complete.cases(data[all_vars])
  if (sum(complete_cases) < 10) {
    stop("Insufficient complete cases for analysis")
  }
  data_complete <- data[complete_cases, ]

  # Set up rms environment
  options(rms = "4.0.0")
  dd <- rms::datadist(data_complete)
  options(datadist = dd)

  # Build formula
  if (is.null(covariates)) {
    formula <- as.formula(paste(outcome, "~ rms::rcs(", predictor, ",", knots, ")"))
  } else {
    formula <- as.formula(paste(outcome, "~ rms::rcs(", predictor, ",", knots, ") +",
                               paste(covariates, collapse = " + ")))
  }

  # Fit model
  fit <- tryCatch({
    model_fit <- rms::lrm(formula, data = data_complete)
    message("Model fitting successfully")
    model_fit
  }, error = function(e) {
    message("Model fitting failed",  e$message)
    stop("Model fitting failed: ", e$message)
  })

  # Generate predictions
  print(formula)
  print(fit)
  predictions <- tryCatch({
    model_prediction <- rms::Predict(fit, name = predictor,fun = exp, type = "predictions",
                 conf.int = 0.95, digits = 2, ref.zero = TRUE)
    message("Prediction generation successfully")
     model_prediction
  }, error = function(e) {
    message("Prediction generation failed")
    stop("Prediction generation failed: ", e$message)
  })

  # ANOVA for p-values
  anova_table <- stats::anova(fit)
  p_nl <- format_p_value(anova_table[2, 3], name = "p for nonlinear")
  p_oa <- format_p_value(anova_table[1, 3], name = "p for overall")

  # Create plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = predictions%>% dplyr::select(dplyr::all_of(predictor), yhat, lower, upper),
      ggplot2::aes(x = !!sym(predictor), y = yhat, color = "Prediction"),
      linetype = "solid", linewidth = 1, alpha = 0.9
    ) +
    ggplot2::geom_ribbon(
      data = predictions,
      ggplot2::aes(x = !!sym(predictor), ymin = lower, ymax = upper, fill = "Confidence Interval"),
      alpha = 0.2
    ) +
    ggplot2::scale_color_manual(values = c("Prediction" = "#d63031")) +
    ggplot2::scale_fill_manual(values = c("Confidence Interval" = "#e17055")) +
    ggplot2::geom_hline(yintercept = 1, linetype = 2, linewidth = 1) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 12),
      legend.position = "none",
      axis.title = ggplot2::element_text(size = 12),
      axis.line = ggplot2::element_line(linewidth = 1)
    ) +
    ggplot2::labs(
      x = predictor,
      y = "OR (95% CI)",
      title = if (!is.null(covariates)) "Adjusted RCS Plot" else "Unadjusted RCS Plot"
    )

  # Save files if requested
  saved_files <- character(0)
  if (save_format != "none" && !is.null(output_dir)) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }

    # Generate filename
    if (is.null(filename)) {
      base_name <- paste(predictor, outcome, sep = "_")
      if (!is.null(covariates)) {
        base_name <- paste0(base_name, "_adjusted")
      }
    } else {
      base_name <- filename
    }

    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

    # Handle different save formats
    if (save_format == "tiff") {
      tiff_file <- file.path(output_dir, sprintf("%s_%s.tiff", base_name, timestamp))
      ggplot2::ggsave(tiff_file, plot = p, width = 6, height = 5.5, dpi = 300)
      saved_files <- c(saved_files, tiff_file)

    } else if (save_format == "svg") {
      svg_file <- file.path(output_dir, sprintf("%s_%s.svg", base_name, timestamp))
      ggplot2::ggsave(svg_file, plot = p, width = 6, height = 5.5)
      saved_files <- c(saved_files, svg_file)

    } else if (save_format == "pdf") {
      pdf_file <- file.path(output_dir, sprintf("%s_%s.pdf", base_name, timestamp))
      ggplot2::ggsave(pdf_file, plot = p, width = 6, height = 5.5)
      saved_files <- c(saved_files, pdf_file)

    } else if (save_format == "all") {
      # Save all formats
      tiff_file <- file.path(output_dir, sprintf("%s_%s.tiff", base_name, timestamp))
      svg_file <- file.path(output_dir, sprintf("%s_%s.svg", base_name, timestamp))
      pdf_file <- file.path(output_dir, sprintf("%s_%s.pdf", base_name, timestamp))

      ggplot2::ggsave(tiff_file, plot = p, width = 6, height = 5.5, dpi = 300)
      ggplot2::ggsave(svg_file, plot = p, width = 6, height = 5.5)
      ggplot2::ggsave(pdf_file, plot = p, width = 6, height = 5.5)

      saved_files <- c(saved_files, tiff_file, svg_file, pdf_file)

      # Save results summary
      results_summary <- data.frame(
        predictor = predictor,
        outcome = outcome,
        covariates = if (!is.null(covariates)) paste(covariates, collapse = ", ") else "None",
        p_nonlinear = anova_table[2, 3],
        p_overall = anova_table[1, 3],
        n_observations = sum(complete_cases),
        stringsAsFactors = FALSE
      )

      csv_file <- file.path(output_dir, sprintf("%s_results_%s.csv", base_name, timestamp))
      utils::write.csv(results_summary, csv_file, row.names = FALSE)
      saved_files <- c(saved_files, csv_file)
    }
  }

  # Clean up rms environment
  options(datadist = NULL)

  # Return structured results
  return(list(
    plot = p,
    predictions = predictions,
    p_nonlinear = anova_table[2, 3],
    p_overall = anova_table[1, 3],
    p_nonlinear_str = p_nl,
    p_overall_str = p_oa,
    n_observations = sum(complete_cases),
    saved_files = if (length(saved_files) > 0) saved_files else NULL,
    call = match.call()
  ))
}


# format_p_value <- function(p_value) {
#   if (is.na(p_value)) return("NA")
#   if (p_value < 0.001) return("<0.001")
#   return(sprintf("%.3f", p_value))
# }

#' Batch RCS Analysis
#'
#' Runs multiple RCS analyses for different predictors and outcomes
#'
#' @param data Data frame containing all variables
#' @param predictors Character vector of predictor variable names
#' @param outcomes Character vector of outcome variable names
#' @param outcomes_map Outcome variable mapping (optional)
#' @param covariates Covariates for adjusted models (optional)
#' @param output_dir Output directory (optional)
#' @param save_format Save format: "none", "tiff", "svg", "pdf", "all" (default "none")
#'
#' @return Data frame with predictor, outcome, model_type, p-values, and optional saved file paths
#' @export
#'
#' @examples
#' \dontrun{
#' results <- run_analysis_rcs(
#'   data = mtcars,
#'   predictors = c("mpg", "wt"),
#'   outcomes = c("vs", "am"),
#'   covariates = c("cyl", "gear"),
#'   output_dir = tempdir(),
#'   save_format = "none"
#' )
#' }
run_analysis_rcs <- function(data,
                            predictors,
                            outcomes,
                            outcomes_map = NULL,
                            covariates = NULL,
                            output_dir = NULL,
                            save_format = c("none", "tiff", "svg", "pdf", "all")) {

  # Parameter validation
  save_format <- match.arg(save_format)
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  # Handle outcome mapping
  if (is.null(outcomes_map)) {
    outcomes_map <- setNames(outcomes, outcomes)
  }

  # Initialize results
  results_list <- list()
  all_saved_files <- character(0)

  # Iterate through predictors and outcomes
  for (predictor in predictors) {
    for (outcome in outcomes) {

      # Generate base filename
      base_filename <- paste(predictor, outcomes_map[[outcome]], sep = "_")

      # Run unadjusted model
      result_unadjusted <- tryCatch({
        generate_rcs_plot(
          data = data,
          outcome = outcome,
          predictor = predictor,
          covariates = NULL,
          output_dir = if (!is.null(output_dir)) file.path(output_dir, "unadjusted") else NULL,
          save_format = save_format,
          filename = paste0(base_filename, "_unadjusted")
        )
      }, error = function(e) {
        warning(sprintf("Unadjusted model failed for %s ~ %s: %s", outcome, predictor, e$message))
        return(NULL)
      })

      if (!is.null(result_unadjusted)) {
        # Add unadjusted results
        results_list[[paste(predictor, outcome, "unadjusted", sep = "__")]] <- data.frame(
          predictor = predictor,
          outcome = outcomes_map[[outcome]],
          model_type = "unadjusted",
          p_nonlinear = result_unadjusted$p_nonlinear,
          p_overall = result_unadjusted$p_overall,
          p_nonlinear_str = result_unadjusted$p_nonlinear_str,
          p_overall_str = result_unadjusted$p_overall_str,
          n_observations = result_unadjusted$n_observations,
          stringsAsFactors = FALSE
        )

        # Collect saved files
        if (!is.null(result_unadjusted$saved_files)) {
          all_saved_files <- c(all_saved_files, result_unadjusted$saved_files)
        }
      }

      # Run adjusted model if covariates provided
      if (!is.null(covariates) && length(covariates) > 0) {
        result_adjusted <- tryCatch({
          generate_rcs_plot(
            data = data,
            outcome = outcome,
            predictor = predictor,
            covariates = covariates,
            output_dir = if (!is.null(output_dir)) file.path(output_dir, "adjusted") else NULL,
            save_format = save_format,
            filename = paste0(base_filename, "_adjusted")
          )
        }, error = function(e) {
          warning(sprintf("Adjusted model failed for %s ~ %s: %s", outcome, predictor, e$message))
          return(NULL)
        })

        if (!is.null(result_adjusted)) {
          # Add adjusted results
          results_list[[paste(predictor, outcome, "adjusted", sep = "__")]] <- data.frame(
            predictor = predictor,
            outcome = outcomes_map[[outcome]],
            model_type = "adjusted",
            p_nonlinear = result_adjusted$p_nonlinear,
            p_overall = result_adjusted$p_overall,
            p_nonlinear_str = result_adjusted$p_nonlinear_str,
            p_overall_str = result_adjusted$p_overall_str,
            n_observations = result_adjusted$n_observations,
            stringsAsFactors = FALSE
          )

          # Collect saved files
          if (!is.null(result_adjusted$saved_files)) {
            all_saved_files <- c(all_saved_files, result_adjusted$saved_files)
          }
        }
      }
    }
  }

  # Combine all results
  if (length(results_list) > 0) {
    final_results <- dplyr::bind_rows(results_list)
  } else {
    final_results <- data.frame(
      predictor = character(),
      outcome = character(),
      model_type = character(),
      p_nonlinear = numeric(),
      p_overall = numeric(),
      p_nonlinear_str = character(),
      p_overall_str = character(),
      n_observations = integer(),
      stringsAsFactors = FALSE
    )
  }

  # Save summary table if requested
  if (save_format != "none" && !is.null(output_dir) && nrow(final_results) > 0) {
    summary_file <- file.path(output_dir,
                             sprintf("rcs_summary_%s.csv", format(Sys.time(), "%Y%m%d_%H%M%S")))
    utils::write.csv(final_results, summary_file, row.names = FALSE)
    all_saved_files <- c(all_saved_files, summary_file)
  }

  # Return structured results
  return(list(
    results = final_results,
    saved_files = if (length(all_saved_files) > 0) all_saved_files else NULL,
    call = match.call()
  ))
}

# data(package = "survival", cancer)
# # 保存所有结果
# results <- run_analysis_rcs(
#   data = colon,
#   predictors = c("nodes"),
#   outcomes = c("status"),
#   covariates = c("sex", "age"),
#   output_dir = "test_output/rcs",
#   save_format = "all"
# )