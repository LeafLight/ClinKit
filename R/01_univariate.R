#' Univariate Logistic Regression Analysis
#'
#' @param data          Data frame
#' @param outcomes      Outcome variables (character vector)
#' @param predictors    Predictor variables (character vector)
#' @param outcomes_map  Outcome variable mapping (optional)
#' @param output_dir    Output directory (optional, default no file saving)
#' @param save_format   Save format: "none", "txt", "csv" (default "csv")
#'
#' @return List containing results data frame and optional saved file paths
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' result <- run_univariate_logistic_regression(
#'   data = mtcars,
#'   outcomes = "vs",
#'   predictors = c("mpg", "drat")
#' )
#'
#' # Save as text files
#' result <- run_univariate_logistic_regression(
#'   data = mtcars,
#'   outcomes = "vs",
#'   predictors = c("mpg", "drat")
#'   output_dir = tempdir(),
#'   save_format = "csv"
#' )
#' }
run_univariate_logistic_regression <- function(data,
                                              outcomes,
                                              predictors,
                                              outcomes_map = NULL,
                                              output_dir = NULL,
                                              save_format = c("csv", "txt", "none")) {

  # Parameter validation
  save_format <- match.arg(save_format)
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  # Check if variables exist
  missing_vars <- setdiff(c(outcomes, predictors), names(data))
  if (length(missing_vars) > 0) {
    stop("Variables not found in data: ", paste(missing_vars, collapse = ", "))
  }

  # Initialize results data frame
  results_df <- data.frame(
    predictor = character(),
    outcome = character(),
    p_value = numeric(),
    OR = numeric(),
    lower_ci = numeric(),
    upper_ci = numeric(),
    n_observations = integer(),
    stringsAsFactors = FALSE
  )

  # Handle outcome mapping
  if (is.null(outcomes_map)) {
    outcomes_map <- setNames(outcomes, outcomes)  # Use original names as default
  }

  # Store saved file paths
  saved_files <- character(0)

  # Iterate through predictors and outcomes
  for (predictor in predictors) {
    for (outcome in outcomes) {

      # Remove missing values
      complete_cases <- complete.cases(data[[predictor]], data[[outcome]])
      if (sum(complete_cases) < 10) {  # Skip if sample size too small
        warning(sprintf("Insufficient complete cases for %s ~ %s (n=%d)",
                       outcome, predictor, sum(complete_cases)))
        next
      }

      # Build model
      formula <- as.formula(paste(outcome, "~", predictor))
      fit <- tryCatch({
        glm(formula, data = data[complete_cases, ], family = binomial())
      }, error = function(e) {
        warning(sprintf("Model failed for %s ~ %s: %s", outcome, predictor, e$message))
        return(NULL)
      })

      if (is.null(fit)) next

      # Extract results
      coef_summary <- coef(summary(fit))
      conf_int <- tryCatch({
        ci <- confint(fit)[predictor, ]
        setNames(as.numeric(exp(ci)), NULL)  # 移除行名
      }, error = function(e) c(NA, NA))

      # Only process if predictor is in results
      if (predictor %in% rownames(coef_summary)) {
        result_row <- data.frame(
          predictor = predictor,
          outcome = outcomes_map[[outcome]],
          p_value = coef_summary[predictor, 4],
          OR = exp(coef_summary[predictor, 1]),
          lower_ci = conf_int[1],
          upper_ci = conf_int[2],
          n_observations = sum(complete_cases),
          stringsAsFactors = FALSE
        )

        if (requireNamespace("dplyr", quietly = TRUE)) {
          results_df <- dplyr::bind_rows(results_df, result_row)
        } else {
          results_df <- rbind(results_df, result_row)
          row.names(results_df) <- NULL
        }
        # Save file if needed
        if (save_format != "none" && !is.null(output_dir)) {
          file_saved <- save_analysis_result(
            result_row,
            output_dir,
            save_format,
            predictor,
            outcomes_map[[outcome]]
          )
          saved_files <- c(saved_files, file_saved)
        }
      }
    }
  }

  # Return structured results
  return(list(
    results = results_df,
    saved_files = if (length(saved_files) > 0) saved_files else NULL,
    call = match.call()
  ))
}

#' Save Analysis Results (Internal Function)
#' @keywords internal
save_analysis_result <- function(result, output_dir, format, predictor, outcome_name) {

  # Ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Generate safe file names
  safe_predictor <- gsub("[^a-zA-Z0-9]", "_", predictor)
  safe_outcome <- gsub("[^a-zA-Z0-9]", "_", outcome_name)
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

  if (format == "txt") {
    filename <- file.path(output_dir,
                         sprintf("univariate_%s_%s_%s.txt",
                                safe_predictor, safe_outcome, timestamp))

    content <- sprintf(
      "Univariate Logistic Regression Analysis Results\n\nPredictor: %s\nOutcome: %s\nSample Size: %d\nOR: %.3f\n95%% Confidence Interval: [%.3f, %.3f]\nP-value: %.4f\nAnalysis Time: %s",
      predictor, outcome_name, result$n_observations,
      result$OR, result$lower_ci, result$upper_ci, result$p_value,
      format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )

    writeLines(content, con = filename, useBytes = TRUE)

  } else if (format == "csv") {
    filename <- file.path(output_dir,
                         sprintf("univariate_%s_%s_%s.csv",
                                safe_predictor, safe_outcome, timestamp))
    utils::write.csv(result, file = filename, row.names = FALSE, fileEncoding = "UTF-8")
  }

  return(filename)
}