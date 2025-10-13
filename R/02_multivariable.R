#' Multivariable Logistic Regression (4-layer models)
#'
#' @param data          Data frame
#' @param outcomes      Outcome variables (character vector)
#' @param predictors    Predictor variables (character vector)
#' @param models_list   Named list of length 3 with covariates for model2, model3, model4
#' @param outcomes_map  Outcome variable mapping (optional)
#' @param output_dir    Output directory (optional, default no file saving)
#' @param save_format   Save format: "none", "txt", "csv" (default "none")
#'
#' @return List containing results data frame and optional saved file paths
#' @export
#'
#' @examples
#' \dontrun{
#' models_list <- list(
#'   model2 = c("cyl", "gear"),
#'   model3 = c("carb"),
#'   model4 = c("hp")
#' )
#'
#' result <- run_multivariable_logistic_regression(
#'   data = mtcars,
#'   outcomes = "vs",
#'   predictors = c("mpg", "wt"),
#'   models_list = models_list
#' )
#' }
run_multivariable_logistic_regression <- function(data,
                                                 outcomes,
                                                 predictors,
                                                 models_list,
                                                 outcomes_map = NULL,
                                                 output_dir = NULL,
                                                 save_format = c("none", "txt", "csv")) {

  # Parameter validation
  save_format <- match.arg(save_format)
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  stopifnot(
    length(models_list) == 3,
    identical(names(models_list), c("model2", "model3", "model4"))
  )

  # Check if variables exist
  all_vars <- unique(c(outcomes, predictors, unlist(models_list)))
  missing_vars <- setdiff(all_vars, names(data))
  if (length(missing_vars) > 0) {
    stop("Variables not found in data: ", paste(missing_vars, collapse = ", "))
  }

  # Handle outcome mapping
  if (is.null(outcomes_map)) {
    outcomes_map <- setNames(outcomes, outcomes)
  }

  # Initialize results container
  results_list <- list()
  saved_files <- character(0)

  # Iterate through predictors and outcomes
  for (predictor in predictors) {
    for (outcome in outcomes) {

      # Remove missing values for current variables
      current_vars <- unique(c(outcome, predictor, unlist(models_list)))
      complete_cases <- complete.cases(data[current_vars])

      if (sum(complete_cases) < 10) {
        warning(sprintf("Insufficient complete cases for %s ~ %s (n=%d)",
                       outcome, predictor, sum(complete_cases)))
        next
      }

      # Model 1: Unadjusted
      f1 <- as.formula(paste(outcome, "~", predictor))
      fit1 <- tryCatch({
        glm(f1, data = data[complete_cases, ], family = binomial())
      }, error = function(e) {
        warning(sprintf("Model1 failed for %s ~ %s: %s", outcome, predictor, e$message))
        return(NULL)
      })

      if (is.null(fit1)) next

      # Extract results with error handling
      model_results <- list()

      # Helper function to fit model and extract results
      fit_model <- function(formula, model_name) {
        fit <- tryCatch({
          glm(formula, data = data[complete_cases, ], family = binomial())
        }, error = function(e) {
          warning(sprintf("%s failed for %s ~ %s: %s", model_name, outcome, predictor, e$message))
          return(NULL)
        })

        if (is.null(fit)) return(NULL)

        coef_summary <- coef(summary(fit))
        if (!predictor %in% rownames(coef_summary)) return(NULL)

        ci <- tryCatch({
          ci_raw <- confint.default(fit)[predictor, ]
          setNames(as.numeric(exp(ci_raw)), NULL)
        }, error = function(e) c(NA, NA))

        return(list(
          OR = exp(coef_summary[predictor, 1]),
          lower_ci = ci[1],
          upper_ci = ci[2],
          p_value = coef_summary[predictor, 4]
        ))
      }

      # Model 1 results
      res1 <- list(
        OR = exp(coef(fit1)[predictor]),
        lower_ci = unname(exp(confint.default(fit1)[predictor, 1])),
        upper_ci = unname(exp(confint.default(fit1)[predictor, 2])),
        p_value = summary(fit1)$coefficients[predictor, 4]
      )

      # Model 2: Adjusted for model2 covariates
      cov2 <- setdiff(models_list[["model2"]], predictor)
      f2 <- reformulate(c(predictor, cov2), response = outcome)
      res2 <- fit_model(f2, "Model2")

      # Model 3: Model2 + model3 covariates
      cov3 <- setdiff(c(models_list[["model2"]], models_list[["model3"]]), predictor)
      f3 <- reformulate(c(predictor, cov3), response = outcome)
      res3 <- fit_model(f3, "Model3")

      # Model 4: All covariates
      cov4 <- setdiff(unlist(models_list), predictor)
      f4 <- reformulate(c(predictor, cov4), response = outcome)
      res4 <- fit_model(f4, "Model4")

      # Create result row
      result_row <- data.frame(
        predictor = predictor,
        outcome = outcomes_map[[outcome]],
        OR_m1 = res1$OR,
        LCI_m1 = res1$lower_ci,
        UCI_m1 = res1$upper_ci,
        p_m1 = res1$p_value,
        OR_m2 = if (!is.null(res2)) res2$OR else NA,
        LCI_m2 = if (!is.null(res2)) res2$lower_ci else NA,
        UCI_m2 = if (!is.null(res2)) res2$upper_ci else NA,
        p_m2 = if (!is.null(res2)) res2$p_value else NA,
        OR_m3 = if (!is.null(res3)) res3$OR else NA,
        LCI_m3 = if (!is.null(res3)) res3$lower_ci else NA,
        UCI_m3 = if (!is.null(res3)) res3$upper_ci else NA,
        p_m3 = if (!is.null(res3)) res3$p_value else NA,
        OR_m4 = if (!is.null(res4)) res4$OR else NA,
        LCI_m4 = if (!is.null(res4)) res4$lower_ci else NA,
        UCI_m4 = if (!is.null(res4)) res4$upper_ci else NA,
        p_m4 = if (!is.null(res4)) res4$p_value else NA,
        n_observations = sum(complete_cases),
        stringsAsFactors = FALSE
      )

      # Add to results
      if (requireNamespace("dplyr", quietly = TRUE)) {
        results_list <- dplyr::bind_rows(results_list, result_row)
      } else {
        results_list <- rbind(results_list, result_row)
      }

      # Save individual file if requested
      if (save_format != "none" && !is.null(output_dir)) {
        file_saved <- save_multivariable_result(
          result_row, output_dir, save_format, predictor, outcomes_map[[outcome]]
        )
        saved_files <- c(saved_files, file_saved)
      }
    }
  }

  # Reset row names if using base R
  if (!requireNamespace("dplyr", quietly = TRUE) && !is.null(results_list)) {
    row.names(results_list) <- NULL
  }

  # Return structured results
  return(list(
    results = results_list,
    saved_files = if (length(saved_files) > 0) saved_files else NULL,
    call = match.call()
  ))
}

#' Save Multivariable Analysis Results (Internal)
#' @keywords internal
save_multivariable_result <- function(result, output_dir, format, predictor, outcome_name) {
# nocov start
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  safe_predictor <- gsub("[^a-zA-Z0-9]", "_", predictor)
  safe_outcome <- gsub("[^a-zA-Z0-9]", "_", outcome_name)
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

  if (format == "txt") {
    filename <- file.path(output_dir,
                         sprintf("multivariable_%s_%s_%s.txt",
                                safe_predictor, safe_outcome, timestamp))

    content <- sprintf(
      "Multivariable Logistic Regression Analysis Results\n\nPredictor: %s\nOutcome: %s\nSample Size: %d\n\nModel 1 (Unadjusted):\nOR = %.2f, 95%% CI [%.2f-%.2f], p = %.4f\n\nModel 2 (Adjusted for model2):\nOR = %.2f, 95%% CI [%.2f-%.2f], p = %.4f\n\nModel 3 (+model3):\nOR = %.2f, 95%% CI [%.2f-%.2f], p = %.4f\n\nModel 4 (+model4):\nOR = %.2f, 95%% CI [%.2f-%.2f], p = %.4f",
      predictor, outcome_name, result$n_observations,
      result$OR_m1, result$LCI_m1, result$UCI_m1, result$p_m1,
      result$OR_m2, result$LCI_m2, result$UCI_m2, result$p_m2,
      result$OR_m3, result$LCI_m3, result$UCI_m3, result$p_m3,
      result$OR_m4, result$LCI_m4, result$UCI_m4, result$p_m4
    )

    writeLines(content, con = filename, useBytes = TRUE)

  } else if (format == "csv") {
    filename <- file.path(output_dir,
                         sprintf("multivariable_%s_%s_%s.csv",
                                safe_predictor, safe_outcome, timestamp))
    write.csv(result, file = filename, row.names = FALSE, fileEncoding = "UTF-8")
  }

  return(filename)
  # nocov end
}



#' Create Multivariate Analysis Table
#'
#' @param raw_df Results data frame from multivariable analysis
#' @param out_file Output Word file path (optional)
#' @param caption Table caption
#' @param merge_predictor Merge identical predictor cells vertically (default TRUE)
#'
#' @return If out_file is NULL, returns flextable object; otherwise writes to file and returns NULL
#' @export
make_multivariate_table <- function(raw_df,
                                    out_file = NULL,
                                    caption = "Multivariate analysis results",
                                    merge_predictor = TRUE) {

  # Check dependencies
  if (!requireNamespace("flextable", quietly = TRUE))
    stop("Please install.packages('flextable')")
  if (!requireNamespace("dplyr", quietly = TRUE))
    stop("Please install.packages('dplyr')")
  if (!requireNamespace("tidyr", quietly = TRUE))
    stop("Please install.packages('tidyr')")

  # Ensure data.frame
  raw_df <- as.data.frame(raw_df, stringsAsFactors = FALSE)

  # Data processing with separate OR and p-value columns
  dat <- raw_df %>%
    dplyr::mutate(
      Predictor = as.character(predictor),
      Outcome = as.character(outcome),
      `Model 1 OR (95% CI)` = sprintf("%.2f (%.2f–%.2f)", OR_m1, LCI_m1, UCI_m1),
      `Model 1 P-value` = sprintf("%.2e", p_m1),
      `Model 2 OR (95% CI)` = sprintf("%.2f (%.2f–%.2f)", OR_m2, LCI_m2, UCI_m2),
      `Model 2 P-value` = sprintf("%.2e", p_m2),
      `Model 3 OR (95% CI)` = sprintf("%.2f (%.2f–%.2f)", OR_m3, LCI_m3, UCI_m3),
      `Model 3 P-value` = sprintf("%.2e", p_m3),
      `Model 4 OR (95% CI)` = sprintf("%.2f (%.2f–%.2f)", OR_m4, LCI_m4, UCI_m4),
      `Model 4 P-value` = sprintf("%.2e", p_m4)
    ) %>%
    dplyr::select(Predictor, Outcome,
                  `Model 1 OR (95% CI)`, `Model 1 P-value`,
                  `Model 2 OR (95% CI)`, `Model 2 P-value`,
                  `Model 3 OR (95% CI)`, `Model 3 P-value`,
                  `Model 4 OR (95% CI)`, `Model 4 P-value`) %>%
    dplyr::arrange(Predictor, Outcome)

  # Create flextable

  ft <- flextable::flextable(dat) %>%
    flextable::hline_top(border = officer::fp_border(width = 2), part = "header") %>%
    flextable::hline_bottom(border = officer::fp_border(width = 2), part = "header") %>%
    flextable::hline_bottom(border = officer::fp_border(width = 1), part = "body") %>%
    flextable::border_remove() %>%  # 移除所有边框
    flextable::bold(part = "header") %>%
    flextable::align(align = "center", part = "all") %>%
    flextable::fontsize(size = 9, part = "all") %>%
    flextable::set_caption(caption)


  # Merge predictor cells if requested
  if (merge_predictor && nrow(dat) > 1) {
    ft <- flextable::merge_v(ft, j = "Predictor")
  }

  # Merge header for each model
  ft <- ft %>%
    flextable::set_header_labels(
      `Model 1 OR (95% CI)` = "OR (95% CI)",
      `Model 1 P-value` = "P-value",
      `Model 2 OR (95% CI)` = "OR (95% CI)",
      `Model 2 P-value` = "P-value",
      `Model 3 OR (95% CI)` = "OR (95% CI)",
      `Model 3 P-value` = "P-value",
      `Model 4 OR (95% CI)` = "OR (95% CI)",
      `Model 4 P-value` = "P-value"
    ) %>%
    flextable::add_header_row(
      values = c("", "", "Model 1", "Model 2", "Model 3", "Model 4"),
      colwidths = c(1, 1, 2, 2, 2, 2)
    )%>%
    flextable::hline_top(border = officer::fp_border(width = 2), part = "header") %>%
    flextable::hline_bottom(border = officer::fp_border(width = 1), part = "header") %>%
    flextable::hline_bottom(border = officer::fp_border(width = 2), part = "body")

  # Output
  if (!is.null(out_file)) {
    if (!requireNamespace("officer", quietly = TRUE))
      stop("Please install.packages('officer')")

    output_dir <- dirname(out_file)
    if (!dir.exists(output_dir) && output_dir != ".") {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }

    officer::read_docx() %>%
      flextable::body_add_flextable(ft) %>%
      print(target = out_file)
    message("Word table written to: ", normalizePath(out_file))
    invisible(NULL)
  } else {
    return(ft)
  }
}


#' Multivariable Multinomial Logistic Regression Analysis
#'
#' Fits multinomial logistic regression models for continuous predictors:
#' ① Model1 (unadjusted)
#' ② Model2/3/4 (sequentially adding covariates)
#' Calculates OR, 95% CI and P-value per unit increase.
#'
#' @param data Data frame containing all variables
#' @param outcomes Character vector of outcome variable names
#' @param predictors Character vector of continuous predictor variable names
#' @param models_list Named list of length 3 with covariates for model2, model3, model4
#' @param ref_level Reference level for multinomial outcome (optional)
#' @param output_dir Output directory (optional)
#' @param save_format Save format: "none", "docx", "csv" (default "none")
#'
#' @return List containing results data frame and optional saved file paths
#' @export
run_multivariable_multinomial_logistic_regression <- function(data,
                                                             outcomes,
                                                             predictors,
                                                             models_list,
                                                             ref_level = NULL,
                                                             output_dir = NULL,
                                                             save_format = c("none", "docx", "csv")) {

  # Parameter validation
  save_format <- match.arg(save_format)
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  # Check dependencies
  if (!requireNamespace("nnet", quietly = TRUE)) {
    stop("Please install.packages('nnet')")
  }
  if (!requireNamespace("broom", quietly = TRUE)) {
    stop("Please install.packages('broom')")
  }

  # Check if variables exist
  all_vars <- unique(c(outcomes, predictors, unlist(models_list)))
  missing_vars <- setdiff(all_vars, names(data))
  if (length(missing_vars) > 0) {
    stop("Variables not found in data: ", paste(missing_vars, collapse = ", "))
  }

  # Initialize results
  results_list <- list()
  saved_files <- character(0)

  # Create a working copy of data to avoid modifying original
  working_data <- data

  # Iterate through outcomes and predictors
  for (outcome in outcomes) {
    # Convert outcome to factor in working data
    working_data[[outcome]] <- factor(working_data[[outcome]])
    if (!is.null(ref_level)) {
      working_data[[outcome]] <- relevel(working_data[[outcome]], ref = ref_level)
    }

    # Get outcome levels for current outcome
    outcome_levels <- levels(working_data[[outcome]])

    for (predictor in predictors) {

      # Remove missing values for current variables
      current_vars <- unique(c(outcome, predictor, unlist(models_list)))
      complete_cases <- complete.cases(working_data[current_vars])

      if (sum(complete_cases) < 10) {
        warning(sprintf("Insufficient complete cases for %s ~ %s (n=%d)",
                       outcome, predictor, sum(complete_cases)))
        next
      }

      # Define models
      model_names <- c("model1", "model2", "model3", "model4")
      covariates_list <- list(
        model1 = character(0),
        model2 = setdiff(models_list[["model2"]], predictor),
        model3 = setdiff(c(models_list[["model2"]], models_list[["model3"]]), predictor),
        model4 = setdiff(unlist(models_list), predictor)
      )

      # Fit models and extract results
      model_results <- list()

      for (model_name in model_names) {
        covariates <- covariates_list[[model_name]]

        # Build formula
        if (length(covariates) == 0) {
          formula <- as.formula(paste(outcome, "~", predictor))
        } else {
          formula <- reformulate(c(predictor, covariates), response = outcome)
        }

        # Fit model
        fit <- tryCatch({
          nnet::multinom(formula, data = working_data[complete_cases, ], trace = FALSE)
        }, error = function(e) {
          warning(sprintf("%s failed for %s ~ %s: %s", model_name, outcome, predictor, e$message))
          return(NULL)
        })

        if (is.null(fit)) next

        # Extract results for the predictor
        tidy_result <- tryCatch({
          broom::tidy(fit, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95)
        }, error = function(e) {
          warning(sprintf("Result extraction failed for %s ~ %s: %s", outcome, predictor, e$message))
          return(NULL)
        })

        if (is.null(tidy_result)) next

        # Filter for current predictor and add model info
predictor_results <- tidy_result %>%
  dplyr::filter(term == predictor) %>%
  dplyr::mutate(
    predictor = predictor,
    outcome = outcome,
    model = model_name,
    n_observations = sum(complete_cases),
    level = as.character(y.level),  # y.level 就是水平名称
    comparison = paste0(level, " vs ", outcome_levels[1])
  )

        model_results[[model_name]] <- predictor_results
      }

      # Combine results for current predictor-outcome pair
      if (length(model_results) > 0) {
        combined_results <- dplyr::bind_rows(model_results)
        results_list <- append(results_list, list(combined_results))
      }
    }
  }

  # Combine all results
  if (length(results_list) > 0) {
    final_results <- dplyr::bind_rows(results_list) %>%
      dplyr::select(outcome, predictor, model, comparison,
                    OR = estimate, lower_ci = conf.low, upper_ci = conf.high,
                    p_value = p.value, n_observations) %>%
      dplyr::arrange(outcome, predictor, model)
  } else {
    final_results <- data.frame()
  }

  # Save results if requested
  if (save_format != "none" && !is.null(output_dir)) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }

    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

    if (save_format == "csv") {
      csv_file <- file.path(output_dir, sprintf("multinomial_results_%s.csv", timestamp))
      write.csv(final_results, csv_file, row.names = FALSE)
      saved_files <- c(saved_files, csv_file)
    } else if (save_format == "docx") {
      docx_file <- file.path(output_dir, sprintf("multinomial_results_%s.docx", timestamp))
      save_multinomial_table(final_results, docx_file)
      saved_files <- c(saved_files, docx_file)
    }
  }

  # Return structured results
  return(list(
    results = final_results,
    saved_files = if (length(saved_files) > 0) saved_files else NULL,
    call = match.call()
  ))
}

#' Save Multinomial Results Table (Internal)
#' @keywords internal
save_multinomial_table <- function(results, file_path) {
  # nocov start
  if (!requireNamespace("flextable", quietly = TRUE) ||
      !requireNamespace("officer", quietly = TRUE)) {
    stop("Please install.packages('flextable') and install.packages('officer')")
  }

  # Format data for table
  table_data <- results %>%
    dplyr::mutate(
      `OR (95% CI)` = sprintf("%.2f (%.2f–%.2f)", OR, lower_ci, upper_ci),
      `P-value` = sprintf("%.3f", p_value)
    ) %>%
    dplyr::select(Outcome = outcome, Predictor = predictor, Model = model,
                  Comparison = comparison, `OR (95% CI)`, `P-value`)

  # Create three-line table
  ft <- flextable::flextable(table_data) %>%
    flextable::border_remove() %>%
    flextable::hline_top(border = officer::fp_border(width = 2), part = "all") %>%
    flextable::hline_bottom(border = officer::fp_border(width = 2), part = "header") %>%
    flextable::hline_bottom(border = officer::fp_border(width = 2), part = "body") %>%
    flextable::bold(part = "header") %>%
    flextable::align(align = "center", part = "all") %>%
    flextable::fontsize(size = 9, part = "all") %>%
    flextable::set_caption("Multinomial Logistic Regression Results") %>%
    flextable::autofit()

  # Save to Word
  officer::read_docx() %>%
    flextable::body_add_flextable(ft) %>%
    print(target = file_path)
  # nocov end
}