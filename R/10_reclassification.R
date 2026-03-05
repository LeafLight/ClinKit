#' Reclassification Analysis (NRI & IDI)
#'
#' Evaluates the incremental predictive value of adding new markers to a base model
#' by calculating Categorical NRI, Continuous (Category-free) NRI, and IDI.
#' Outputs a three-line table in Word format and saves raw results to CSV.
#'
#' @param data Data frame containing outcome and all predictor variables
#' @param outcome Character scalar, binary outcome variable name (0/1)
#' @param base_vars Character vector, variables in the base model
#' @param new_vars Character vector, new variables added to the updated model
#' @param cutoffs Numeric vector, cutoff points for risk categories, default c(0, 0.2, 0.4, 1)
#' @param output_dir Output directory for saving results, default NULL
#' @param save_format Save format: "none", "docx", "csv", "all", default "none"
#' @param filename_base Base filename for outputs, default "reclassification"
#'
#' @return List containing results data frame, reclassification tables, and optional saved file paths
#' @export
#' @importFrom dplyr mutate select arrange
#' @importFrom stats glm as.formula binomial pnorm
run_reclassification_analysis <- function(
  data,
  outcome,
  base_vars,
  new_vars,
  cutoffs = c(0, 0.2, 0.4, 1),
  output_dir = NULL,
  save_format = c("none", "docx", "csv", "all"),
  filename_base = "reclassification"
) {

  # ---- 1. Parameter validation ----
  save_format <- match.arg(save_format)
  if (!is.data.frame(data)) stop("data must be a data frame")
  if (!requireNamespace("Hmisc", quietly = TRUE)) stop("Please install.packages('Hmisc')")

  all_vars <- unique(c(outcome, base_vars, new_vars))
  missing_vars <- setdiff(all_vars, names(data))
  if (length(missing_vars) > 0) {
    stop("Variables not found in data: ", paste(missing_vars, collapse = ", "))
  }

  saved_files <- character(0)

  # ---- 2. Data Preparation & Model Fitting ----
  # Remove missing values to ensure fair comparison
  complete_cases <- complete.cases(data[all_vars])
  if (sum(complete_cases) < 10) stop("Insufficient complete cases for analysis")
  data_subset <- data[complete_cases, ]

  # Fit Base Model
  f_base <- as.formula(paste(outcome, "~", paste(base_vars, collapse = " + ")))
  fit_base <- glm(f_base, data = data_subset, family = binomial())
  p_base <- fit_base$fitted.values

  # Fit Updated Model
  f_new <- as.formula(paste(outcome, "~", paste(c(base_vars, new_vars), collapse = " + ")))
  fit_new <- glm(f_new, data = data_subset, family = binomial())
  p_new <- fit_new$fitted.values

  y <- data_subset[[outcome]]

  # ---- 3. Calculate NRI & IDI using Hmisc ----
  c1 <- cut(p_base, breaks = cutoffs, include.lowest = TRUE, right = FALSE)
  c2 <- cut(p_new, breaks = cutoffs, include.lowest = TRUE, right = FALSE)
  c1_num <- as.numeric(factor(c1, levels = levels(c1)))
  c2_num <- as.numeric(factor(c2, levels = levels(c2)))

  # Categorical NRI
  res_cat <- tryCatch({
    Hmisc::improveProb(x1 = c1_num / length(levels(c1)),
                       x2 = c2_num / length(levels(c2)), y = y)
  }, error = function(e) { warning("Categorical NRI failed: ", e$message); NULL })

  # Continuous NRI and IDI
  res_cont <- tryCatch({
    Hmisc::improveProb(x1 = p_base, x2 = p_new, y = y)
  }, error = function(e) { stop("Continuous NRI/IDI failed: ", e$message) })

  # ---- 4. Format Results ----
  get_stat_row <- function(metric, est, se, z) {
    if (is.null(est)) return(NULL)
    low <- est - 1.96 * se
    upp <- est + 1.96 * se
    p_val <- 2 * pnorm(-abs(z))
    data.frame(
      Metric = metric,
      Estimate = est,
      Lower_CI = low,
      Upper_CI = upp,
      P_value = p_val,
      `Estimate (95% CI)` = sprintf("%.4f (%.4f-%.4f)", est, low, upp),
      `P value` = if(p_val < 0.001) "<0.001" else sprintf("%.4f", p_val),
      stringsAsFactors = FALSE, check.names = FALSE
    )
  }

  results_df <- rbind(
    if(!is.null(res_cat)) get_stat_row("NRI (Categorical)", res_cat$nri, res_cat$se.nri, res_cat$z.nri),
    get_stat_row("NRI (Continuous)", res_cont$nri, res_cont$se.nri, res_cont$z.nri),
    get_stat_row("IDI", res_cont$idi, res_cont$se.idi, res_cont$z.idi)
  )

  # Reclassification Tables
  reclass_tables <- list(
    Absent = table(Initial = c1[y == 0], Updated = c2[y == 0]),
    Present = table(Initial = c1[y == 1], Updated = c2[y == 1])
  )

  # ---- 5. Save Results ----
  if (save_format != "none" && !is.null(output_dir)) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    #timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

    if (save_format %in% c("csv", "all")) {
      #csv_file <- file.path(output_dir, sprintf("%s_metrics_%s.csv", filename_base, timestamp))
      csv_file <- generate_filepath(
          base_name =  sprintf("%s_metrics", filename_base),
          ext = "csv",
          output_dir = output_dir
      )
      utils::write.csv(results_df, csv_file, row.names = FALSE)
      saved_files <- c(saved_files, csv_file)
    }

    if (save_format %in% c("docx", "all")) {
      #docx_file <- file.path(output_dir, sprintf("%s_table_%s.docx", filename_base, timestamp))
      docx_file <- generate_filepath(
          base_name =  sprintf("%s_table", filename_base),
          ext = "docx",
          output_dir = output_dir
      )
      save_reclassification_table(results_df, docx_file)
      saved_files <- c(saved_files, docx_file)
    }
  }

  # ---- 6. Return Structured Object ----
  return(list(
    results = results_df,
    reclass_tables = reclass_tables,
    saved_files = if (length(saved_files) > 0) saved_files else NULL,
    call = match.call()
  ))
}

#' Save Reclassification Results Table (Internal)
#' @keywords internal
save_reclassification_table <- function(results_df, file_path) {
  if (!requireNamespace("flextable", quietly = TRUE) ||
      !requireNamespace("officer", quietly = TRUE)) {
    stop("Please install.packages('flextable') and install.packages('officer')")
  }

  # Clean data for printing
  table_data <- results_df[, c("Metric", "Estimate (95% CI)", "P value")]

  ft <- flextable::flextable(table_data) %>%
    flextable::border_remove() %>%
    flextable::hline_top(border = officer::fp_border(width = 2), part = "all") %>%
    flextable::hline_bottom(border = officer::fp_border(width = 1), part = "header") %>%
    flextable::hline_bottom(border = officer::fp_border(width = 2), part = "body") %>%
    flextable::bold(part = "header") %>%
    flextable::align(align = "center", part = "all") %>%
    flextable::align(j = 1, align = "left", part = "all") %>%
    flextable::fontsize(size = 10, part = "all") %>%
    flextable::font(fontname = "Times New Roman", part = "all") %>%
    flextable::set_caption("Reclassification and Discrimination Improvement") %>%
    flextable::autofit()

  officer::read_docx() %>%
    flextable::body_add_flextable(ft) %>%
    print(target = file_path)
}