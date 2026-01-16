#' One-Click Baseline Characteristics Table (gtsummary version)
#'
#' Automatically generates baseline characteristics table with counts (percentages)
#' for categorical variables and appropriate statistics for continuous variables
#' based on normality testing. Outputs a three-line table in Word format with P-values and Overall column.
#'
#' @param data Data frame containing grouping variable and all variables to summarize
#' @param outcome Character scalar, grouping variable name (typically binary or factor)
#' @param vars Character vector, variables to display; if NULL (default) displays all variables except outcome
#' @param file Character, output Word file name (with path); parent directory must exist
#' @param pct_digits Decimal places for percentages, default 2
#' @param cont_digits Decimal places for continuous variables, default 2
#' @param p_digits Decimal places for P-values, default 3
#' @param label_list Named list, variable labels, e.g., list(age = "Age (years)");
#'                  if NULL, uses original column names
#' @param normality_test_method Character, method for normality test ("shapiro" or "ks"), default "shapiro"
#' @param normality_alpha Numeric, significance level for normality test, default 0.05
#' @param export_normality Logical, whether to export normality test results to a separate CSV file, default TRUE
#' @param normality_file Character, output CSV file name for normality test results; if NULL, uses same base name as main file
#' @importFrom stats as.formula binomial coef complete.cases confint confint.default glm ks.test median na.omit relevel sd setNames shapiro.test
#' @return gtsummary object (invisible), convenient for subsequent piping operations
#' @details
#' \itemize{
#'   \item Categorical variables displayed as n(%)
#'   \item Continuous variables: if all groups are normal, display as mean (SD); otherwise display as median (Q1, Q3)
#'   \item Missing values not displayed by default; P-values use appropriate tests (chi-square/t/ANOVA/Kruskal-Wallis)
#'   \item Final table exported via flextable, ready for use in papers or appendices
#'   \item Normality test results can be exported to CSV for reference
#' }
#'
#' @examples
#' \dontrun{
#' make_baseline_table(
#'   data     = mydata,
#'   outcome  = "Group",
#'   vars     = c("age", "sex", "smoke", "sbp"),
#'   file     = "Table1_Baseline.docx",
#'   label_list = list(age = "Age (years)", sbp = "Systolic BP (mmHg)")
#' )
#' }
#'
#' @importFrom dplyr all_of select filter pull
#' @importFrom gtsummary tbl_summary add_p modify_fmt_fun add_overall bold_labels as_flex_table
#' @importFrom officer read_docx
#' @importFrom flextable body_add_flextable
#' @importFrom purrr map_dfr
#' @importFrom tibble tibble
#' @export
make_baseline_table <- function(
  data,
  outcome,
  vars = NULL,
  file,
  pct_digits = 2,
  cont_digits = 2,
  p_digits = 3,
  label_list = NULL,
  normality_test_method = "shapiro",
  normality_alpha = 0.05,
  export_normality = TRUE,
  normality_file = NULL
) {
  # Input validation
  if (!outcome %in% names(data)) {
    stop("Outcome variable '", outcome, "' not found in data")
  }

  if (!is.null(vars) && !all(vars %in% names(data))) {
    missing_vars <- setdiff(vars, names(data))
    stop("The following variables not found in data: ", paste(missing_vars, collapse = ", "))
  }

  # 1. Variable selection
  if (is.null(vars)) {
    vars <- setdiff(names(data), outcome)
  }
  data <- data %>% dplyr::select(all_of(outcome), all_of(vars))

  # 2. Identify continuous variables
   # Let gtsummary decide variable types
  tbl_tmp <- gtsummary::tbl_summary(
    data = data,
    by = all_of(outcome),
    missing = "no"
  )

  var_types <- tbl_tmp$table_body %>%
    dplyr::distinct(variable, var_type)

  continuous_vars <- var_types$variable[var_types$var_type == "continuous"]
  categorical_vars <- var_types$variable[var_types$var_type == "categorical"]


  # 3. Perform normality tests for continuous variables
  normality_results <- perform_normality_tests(
    data = data,
    outcome = outcome,
    continuous_vars = continuous_vars,
    method = normality_test_method,
    alpha = normality_alpha
  )

  # 4. Determine statistics format based on normality
  statistic_list <- list(
    gtsummary::all_categorical() ~ "{n} ({p}%)"
  )

  # Add continuous variables with appropriate statistics
  for (var in continuous_vars) {
    if (is_normal_overall(normality_results, var)) {
      statistic_list[[ var ]] <- "{mean} ({sd})"
    } else {
      statistic_list[[ var ]] <- "{median} ({p25}, {p75})"
    }
  }

  # 5. Export normality test results if requested
  if (export_normality && length(continuous_vars) > 0) {
    if (is.null(normality_file)) {
      normality_file <- sub("\\.(docx|doc)$", "_normality.csv", file)
    }
    # Create directory if it doesn't exist
    dir.create(dirname(normality_file), showWarnings = FALSE, recursive = TRUE)
    utils::write.csv(normality_results, normality_file, row.names = FALSE)
    message("Normality test results exported to: ", normality_file)
  }

  # 6. Generate gtsummary table
  tbl <- data %>%
    gtsummary::tbl_summary(
      by = all_of(outcome),
      label = label_list,
      statistic = statistic_list,
      missing = "no",
      digits = list(
        gtsummary::all_continuous()  ~ cont_digits,
        gtsummary::all_categorical() ~ c(0, pct_digits)
      )
    ) %>%
    gtsummary::add_p() %>%
    gtsummary::modify_fmt_fun(
      p.value = function(x) ifelse(is.na(x), "", sprintf(paste0("%.", p_digits, "f"), x))
    ) %>%
    gtsummary::add_overall(last = TRUE) %>%
    gtsummary::bold_labels()

  # 7. Export to Word
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  ft <- gtsummary::as_flex_table(tbl)
  officer::read_docx() %>%
    flextable::body_add_flextable(ft) %>%
    print(target = file)

  message("Baseline table exported to: ", file)
  if (length(continuous_vars) > 0) {
    message("Normality summary:")
    print_normality_summary(normality_results)
  }

  invisible(tbl)
}

#' Perform normality tests for continuous variables by group
#'
#' @param data Data frame containing the data
#' @param outcome Character, grouping variable name
#' @param continuous_vars Character vector of continuous variable names
#' @param method Character, normality test method ("shapiro" or "ks")
#' @param alpha Numeric, significance level for normality test
#'
#' @return Data frame with normality test results
#' @importFrom dplyr filter pull
#' @importFrom purrr map_dfr
#' @importFrom tibble tibble
perform_normality_tests <- function(data, outcome, continuous_vars, method = "shapiro", alpha = 0.05) {
  if (length(continuous_vars) == 0) {
    return(tibble::tibble(
      variable = character(),
      group = character(),
      p_value = numeric(),
      is_normal = logical(),
      test_method = character(),
      note = character()
    ))
  }

  results_list <- list()

  for (var in continuous_vars) {
    groups <- unique(data[[outcome]])
    groups <- groups[!is.na(groups)]

    for (group_val in groups) {
      group_data <- data %>%
        dplyr::filter(.data[[outcome]] == group_val) %>%
        dplyr::pull(var)

      group_data <- group_data[!is.na(group_data)]

      if (length(group_data) < 3) {
        # Not enough data for normality test
        test_result <- tibble::tibble(
          variable = var,
          group = as.character(group_val),
          p_value = NA_real_,
          is_normal = FALSE,
          test_method = method,
          note = "Insufficient data (n < 3)"
        )
      } else {
        # Perform normality test
        if (method == "shapiro") {
          test <- shapiro.test(group_data)
          p_val <- test$p.value
        } else if (method == "ks") {
          # Kolmogorov-Smirnov test against normal distribution
          standardized <- (group_data - mean(group_data)) / sd(group_data)
          test <- ks.test(standardized, "pnorm")
          p_val <- test$p.value
        } else {
          stop("Unsupported normality test method. Use 'shapiro' or 'ks'.")
        }

        test_result <- tibble::tibble(
          variable = var,
          group = as.character(group_val),
          p_value = p_val,
          is_normal = p_val > alpha,
          test_method = method,
          note = ifelse(p_val > alpha, "Normal", "Non-normal")
        )
      }

      results_list[[paste0(var, "_", group_val)]] <- test_result
    }
  }

  purrr::map_dfr(results_list, identity)
}

#' Check if all groups for a variable are normal
#'
#' @param normality_results Data frame from perform_normality_tests
#' @param var Character, variable name to check
#'
#' @return Logical, TRUE if all groups are normal, FALSE otherwise
is_normal_overall <- function(normality_results, var) {
  var_results <- normality_results[normality_results$variable == var, ]

  if (nrow(var_results) == 0) {
    return(FALSE)
  }

  # Remove groups with insufficient data from consideration
  valid_results <- var_results[!grepl("Insufficient data", var_results$note), ]

  if (nrow(valid_results) == 0) {
    return(FALSE)  # All groups had insufficient data
  }

  all(valid_results$is_normal, na.rm = TRUE)
}

#' Print summary of normality test results
#'
#' @param normality_results Data frame from perform_normality_tests
print_normality_summary <- function(normality_results) {
  if (nrow(normality_results) == 0) {
    message("No continuous variables for normality testing.")
    return(invisible())
  }

  # Get unique variables
  unique_vars <- unique(normality_results$variable)

  for (var in unique_vars) {
    var_results <- normality_results[normality_results$variable == var, ]
    normal_groups <- sum(var_results$is_normal, na.rm = TRUE)
    total_groups <- nrow(var_results)

    if (normal_groups == total_groups) {
      message("  ", var, ": ALL groups normal: Mean (SD)")
    } else {
      message("  ", var, ": ", normal_groups, "/", total_groups,
              " groups normal: Median (Q1, Q3)")
    }
  }
}

# Example usage
make_baseline_table(
  data = mtcars,
  outcome = "vs",  # V-shaped vs Straight engine
  vars = NULL,     # Use all variables except outcome
  file = "./test_output/table1_complete.docx",
  label_list = list(
    mpg = "Miles per Gallon",
    cyl = "Number of Cylinders",
    disp = "Displacement (cu.in.)",
    hp = "Horsepower",
    drat = "Rear Axle Ratio",
    wt = "Weight (1000 lbs)",
    qsec = "Quarter Mile Time",
    gear = "Number of Forward Gears",
    carb = "Number of Carburetors"
  ),
  normality_test_method = "shapiro",
  export_normality = TRUE
)