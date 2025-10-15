#' Quartile-based Logistic Regression Analysis
#'
#' Analyzes continuous predictors by quartile groups, fitting:
#' 1. Model1 (unadjusted)
#' 2. Model2/3/4 (sequentially adding covariates)
#' Calculates OR, 95% CI for Q2/Q3/Q4 vs Q1 and trend P-value.
#'
#' @param data Data frame containing all variables
#' @param outcomes Character vector of binary outcome variable names
#' @param predictors Character vector of continuous predictor variable names
#' @param models_list Named list of length 3 with covariates for model2, model3, model4
#' @param output_dir Output directory (optional)
#' @param save_format Save format: "none", "docx", "csv", "all" (default "none")
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
#' result <- quartile_logistic_analysis(
#'   data = mtcars,
#'   outcomes = "vs",
#'   predictors = c("mpg", "wt"),
#'   models_list = models_list
#' )
#' }
quartile_logistic_analysis <- function(data,
                                      outcomes,
                                      predictors,
                                      models_list,
                                      output_dir = NULL,
                                      save_format = c("none", "docx", "csv", "all")) {

  # Parameter validation
  save_format <- match.arg(save_format)
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  # Check dependencies
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Please install.packages('dplyr')")
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
for (outcome in outcomes) {
    unique_vals <- unique(na.omit(data[[outcome]]))
    if (length(unique_vals) != 2) {
      stop(sprintf("Outcome variable '%s' must be binary (has %d unique values)",
                   outcome, length(unique_vals)))
    }
  }
  # Initialize results
  results_list <- list()
  saved_files <- character(0)

  # Iterate through outcomes and predictors
  for (outcome in outcomes) {
    for (predictor in predictors) {

      # Remove missing values for current variables
      current_vars <- unique(c(outcome, predictor, unlist(models_list)))
      complete_cases <- complete.cases(data[current_vars])

      if (sum(complete_cases) < 10) {
        warning(sprintf("Insufficient complete cases for %s ~ %s (n=%d)",
                       outcome, predictor, sum(complete_cases)))
        next
      }

      data_subset <- data[complete_cases, ]

      # Create quartile groups
      data_subset <- data_subset %>%
        dplyr::mutate(
          quartile = dplyr::ntile(!!sym(predictor), 4),
          quartile = factor(quartile, levels = 1:4, labels = c("Q1", "Q2", "Q3", "Q4")),
          quartile_num = as.numeric(quartile)
        )

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

        # Build formula for categorical analysis
        if (length(covariates) == 0) {
          formula_cat <- as.formula(paste(outcome, "~ quartile"))
        } else {
          formula_cat <- reformulate(c("quartile", covariates), response = outcome)
        }

        # Build formula for trend test
        if (length(covariates) == 0) {
          formula_trend <- as.formula(paste(outcome, "~ quartile_num"))
        } else {
          formula_trend <- reformulate(c("quartile_num", covariates), response = outcome)
        }

        # Fit categorical model
        fit_cat <- tryCatch({
          glm(formula_cat, data = data_subset, family = binomial())
        }, error = function(e) {
          warning(sprintf("Categorical model failed for %s ~ %s (%s): %s",
                         outcome, predictor, model_name, e$message))
          return(NULL)
        })

        # Fit trend model
        fit_trend <- tryCatch({
          glm(formula_trend, data = data_subset, family = binomial())
        }, error = function(e) {
          warning(sprintf("Trend model failed for %s ~ %s (%s): %s",
                         outcome, predictor, model_name, e$message))
          return(NULL)
        })

        if (is.null(fit_cat) || is.null(fit_trend)) next

        # Extract categorical results
        tidy_cat <- tryCatch({
          broom::tidy(fit_cat, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95)
        }, error = function(e) {
          warning(sprintf("Result extraction failed for %s ~ %s (%s): %s",
                         outcome, predictor, model_name, e$message))
          return(NULL)
        })

        if (is.null(tidy_cat)) next

        # Extract trend p-value
        trend_p <- tryCatch({
          broom::tidy(fit_trend)$p.value[2]
        }, error = function(e) {
          warning(sprintf("Trend p-value extraction failed for %s ~ %s (%s): %s",
                         outcome, predictor, model_name, e$message))
          return(NA)
        })

        # Filter for quartile comparisons and add model info
        quartile_results <- tidy_cat %>%
          dplyr::filter(grepl("^quartileQ[234]$", term)) %>%
          dplyr::mutate(
            predictor = predictor,
            outcome = outcome,
            model = model_name,
            p_trend = trend_p,
            n_observations = sum(complete_cases),
            comparison = dplyr::recode(term,
                                      "quartileQ2" = "Q2 vs Q1",
                                      "quartileQ3" = "Q3 vs Q1",
                                      "quartileQ4" = "Q4 vs Q1")
          )

        model_results[[model_name]] <- quartile_results
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
                    p_value = p.value, p_trend, n_observations) %>%
      dplyr::arrange(outcome, predictor, model, comparison)
  } else {
    final_results <- data.frame()
  }

  # Save results if requested
  if (save_format != "none" && !is.null(output_dir)) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }

    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

    if (save_format %in% c("docx", "all")) {
      docx_file <- file.path(output_dir, sprintf("quartile_results_%s.docx", timestamp))
      save_quartile_table(final_results, docx_file)
      saved_files <- c(saved_files, docx_file)
    }

    if (save_format %in% c("csv", "all")) {
      csv_file <- file.path(output_dir, sprintf("quartile_results_%s.csv", timestamp))
      utils::write.csv(final_results, csv_file, row.names = FALSE)
      saved_files <- c(saved_files, csv_file)
    }
  }

  # Return structured results
  return(list(
    results = final_results,
    saved_files = if (length(saved_files) > 0) saved_files else NULL,
    call = match.call()
  ))
}

#' Save Quartile Results Table (Internal)
#' @keywords internal
save_quartile_table <- function(results, file_path) {
  # nocov start
  if (!requireNamespace("flextable", quietly = TRUE) ||
      !requireNamespace("officer", quietly = TRUE)) {
    stop("Please install.packages('flextable') and install.packages('officer')")
  }

  # Format data for table
  table_data <- results %>%
    dplyr::mutate(
      `OR (95% CI)` = sprintf("%.2f (%.2f-%.2f)", OR, lower_ci, upper_ci),
      `P-value` = sprintf("%.3f", p_value),
      `P-trend` = sprintf("%.3f", p_trend)
    ) %>%
    dplyr::select(Outcome = outcome, Predictor = predictor, Model = model,
                  Comparison = comparison, `OR (95% CI)`, `P-value`, `P-trend`)

  # Create three-line table
  ft <- flextable::flextable(table_data) %>%
    flextable::border_remove() %>%
    flextable::hline_top(border = officer::fp_border(width = 2), part = "all") %>%
    flextable::hline_bottom(border = officer::fp_border(width = 2), part = "header") %>%
    flextable::hline_bottom(border = officer::fp_border(width = 2), part = "body") %>%
    flextable::bold(part = "header") %>%
    flextable::align(align = "center", part = "all") %>%
    flextable::fontsize(size = 9, part = "all") %>%
    flextable::set_caption("Quartile-based Logistic Regression Results") %>%
    flextable::autofit()

  # Save to Word
  officer::read_docx() %>%
    flextable::body_add_flextable(ft) %>%
    print(target = file_path)
  # nocov end
}

#' Quartile-based Multinomial Logistic Regression Analysis
#'
#' Analyzes continuous predictors by quartile groups for multinomial outcomes,
#' fitting multiple models with sequential covariate adjustment.
#'
#' @param data Data frame containing all variables
#' @param outcomes Character vector of multinomial outcome variable names
#' @param predictors Character vector of continuous predictor variable names
#' @param models_list Named list of covariates for different models
#' @param ref_level Reference level for multinomial outcome (optional)
#' @param output_dir Output directory (optional)
#' @param save_format Save format: "none", "docx", "csv", "all" (default "none")
#'
#' @return List containing results data frame and optional saved file paths
#' @export
#'
#' @examples
#' \dontrun{
#' # Create test data with multinomial outcome
#' set.seed(123)
#' test_data <- data.frame(
#'   outcome = sample(c("A", "B", "C"), 100, replace = TRUE),
#'   predictor1 = rnorm(100),
#'   predictor2 = rnorm(100),
#'   cov1 = rnorm(100),
#'   cov2 = sample(c("X", "Y"), 100, replace = TRUE)
#' )
#'
#' models_list <- list(
#'   model2 = c("cov1"),
#'   model3 = c("cov1", "cov2")
#' )
#'
#' result <- quartile_multinomial_analysis(
#'   data = test_data,
#'   outcomes = "outcome",
#'   predictors = c("predictor1", "predictor2"),
#'   models_list = models_list,
#'   output_dir = tempdir(),
#'   save_format = "all"
#' )
#' }
quartile_multinomial_analysis <- function(data,
                                         outcomes,
                                         predictors,
                                         models_list,
                                         ref_level = NULL,
                                         output_dir = NULL,
                                         save_format = c("none", "docx", "csv", "all")) {

  # Parameter validation
  save_format <- match.arg(save_format)
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  # Check dependencies
  if (!requireNamespace("nnet", quietly = TRUE)) {
    stop("Please install.packages('nnet')")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Please install.packages('dplyr')")
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

  # Create working copy of data to avoid modifying original
  working_data <- data

  # Iterate through outcomes and predictors
  for (outcome in outcomes) {
    # Convert outcome to factor in working data
    working_data[[outcome]] <- factor(working_data[[outcome]])
    if (!is.null(ref_level)) {
      working_data[[outcome]] <- relevel(working_data[[outcome]], ref = ref_level)
    }

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

      data_subset <- working_data[complete_cases, ]

      # Create quartile groups
      data_subset <- data_subset %>%
        dplyr::mutate(
          quartile = dplyr::ntile(!!sym(predictor), 4),
          quartile = factor(quartile, levels = 1:4, labels = c("Q1", "Q2", "Q3", "Q4")),
          quartile_num = as.numeric(quartile)
        )

      # Define models
      model_names <- c("model1", names(models_list))
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

        # Build formula for categorical analysis
        if (length(covariates) == 0) {
          formula_cat <- as.formula(paste(outcome, "~ quartile"))
        } else {
          formula_cat <- reformulate(c("quartile", covariates), response = outcome)
        }

        # Build formula for trend test
        if (length(covariates) == 0) {
          formula_trend <- as.formula(paste(outcome, "~ quartile_num"))
        } else {
          formula_trend <- reformulate(c("quartile_num", covariates), response = outcome)
        }

        # Fit categorical model
        fit_cat <- tryCatch({
          nnet::multinom(formula_cat, data = data_subset, trace = FALSE)
        }, error = function(e) {
          warning(sprintf("Categorical model failed for %s ~ %s (%s): %s",
                         outcome, predictor, model_name, e$message))
          return(NULL)
        })

        # Fit trend model
        fit_trend <- tryCatch({
          nnet::multinom(formula_trend, data = data_subset, trace = FALSE)
        }, error = function(e) {
          warning(sprintf("Trend model failed for %s ~ %s (%s): %s",
                         outcome, predictor, model_name, e$message))
          return(NULL)
        })

        if (is.null(fit_cat) || is.null(fit_trend)) next

        # Extract categorical results
        tidy_cat <- tryCatch({
          broom::tidy(fit_cat, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95)
        }, error = function(e) {
          warning(sprintf("Result extraction failed for %s ~ %s (%s): %s",
                         outcome, predictor, model_name, e$message))
          return(NULL)
        })

        if (is.null(tidy_cat)) next

        # Extract trend p-values
        tidy_trend <- tryCatch({
          broom::tidy(fit_trend) %>%
            dplyr::filter(term == "quartile_num") %>%
            dplyr::select(y.level, p_trend = p.value)
        }, error = function(e) {
          warning(sprintf("Trend p-value extraction failed for %s ~ %s (%s): %s",
                         outcome, predictor, model_name, e$message))
          return(data.frame(y.level = unique(tidy_cat$y.level), p_trend = NA))
        })

        # Filter for quartile comparisons and add model info
        quartile_results <- tidy_cat %>%
          dplyr::filter(grepl("^quartileQ[234]$", term)) %>%
          dplyr::mutate(
            predictor = predictor,
            outcome = outcome,
            model = model_name,
            n_observations = sum(complete_cases),
            level = y.level,
            comparison = dplyr::recode(term,
                                      "quartileQ2" = "Q2 vs Q1",
                                      "quartileQ3" = "Q3 vs Q1",
                                      "quartileQ4" = "Q4 vs Q1")
          ) %>%
          dplyr::left_join(tidy_trend, by = "y.level")

        model_results[[model_name]] <- quartile_results
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
      dplyr::select(outcome, predictor, model, level, comparison,
                    OR = estimate, lower_ci = conf.low, upper_ci = conf.high,
                    p_value = p.value, p_trend, n_observations) %>%
      dplyr::arrange(outcome, predictor, model, level, comparison)
  } else {
    final_results <- data.frame()
  }

  # Save results if requested
  if (save_format != "none" && !is.null(output_dir)) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }

    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

    if (save_format %in% c("docx", "all")) {
      docx_file <- file.path(output_dir, sprintf("quartile_multinomial_results_%s.docx", timestamp))
      save_multinomial_quartile_table(final_results, docx_file)
      saved_files <- c(saved_files, docx_file)
    }

    if (save_format %in% c("csv", "all")) {
      csv_file <- file.path(output_dir, sprintf("quartile_multinomial_results_%s.csv", timestamp))
      utils::write.csv(final_results, csv_file, row.names = FALSE)
      saved_files <- c(saved_files, csv_file)
    }
  }

  # Return structured results
  return(list(
    results = final_results,
    saved_files = if (length(saved_files) > 0) saved_files else NULL,
    call = match.call()
  ))
}

#' Save Multinomial Quartile Results Table (Internal)
#' @keywords internal
save_multinomial_quartile_table <- function(results, file_path) {
  if (!requireNamespace("flextable", quietly = TRUE) ||
      !requireNamespace("officer", quietly = TRUE)) {
    stop("Please install.packages('flextable') and install.packages('officer')")
  }

  # Format data for table
  table_data <- results %>%
    dplyr::mutate(
      `OR (95% CI)` = sprintf("%.2f (%.2f-%.2f)", OR, lower_ci, upper_ci),
      `P-value` = sprintf("%.3f", p_value),
      `P-trend` = sprintf("%.3f", p_trend)
    ) %>%
    dplyr::select(Outcome = outcome, Predictor = predictor, Model = model,
                  Level = level, Comparison = comparison,
                  `OR (95% CI)`, `P-value`, `P-trend`)

  # Create three-line table
  ft <- flextable::flextable(table_data) %>%
    flextable::border_remove() %>%
    flextable::hline_top(border = officer::fp_border(width = 2), part = "all") %>%
    flextable::hline_bottom(border = officer::fp_border(width = 2), part = "header") %>%
    flextable::hline_bottom(border = officer::fp_border(width = 2), part = "body") %>%
    flextable::bold(part = "header") %>%
    flextable::align(align = "center", part = "all") %>%
    flextable::fontsize(size = 9, part = "all") %>%
    flextable::set_caption("Quartile-based Multinomial Logistic Regression Results") %>%
    flextable::autofit()

  # Save to Word
  officer::read_docx() %>%
    flextable::body_add_flextable(ft) %>%
    print(target = file_path)
}

utils::globalVariables(c("quartile", "term", "model", "comparison", "estimate", "conf.low", "conf.high", "p.value", "p_trend", "n_observations", "y.level", "level", "OR", "lower_ci", "upper_ci", "p_value", "p_trend", "outcome", "predictor", "model", "comparison", "OR (95% CI)", "P-value", "P-trend"))
