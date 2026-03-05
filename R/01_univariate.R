#' Univariate Logistic Regression Analysis
#'
#' @param data          Data frame.
#' @param outcomes      Outcome variables (character vector).
#' @param predictors    Predictor variables (character vector).
#' @param outcomes_map  Optional named mapping from outcome variable names to
#'   display labels, e.g. c("A_B" = "A B").
#' @param predictors_map Optional named mapping from predictor variable names
#'   to display labels.
#' @param output_dir    Output directory (optional; files are not saved if NULL).
#' @param save_format   Save format: "csv", "txt", "docx", or "none" (default "csv").
#'
#' @return A list with components \code{results}, \code{saved_files} and
#'   \code{call}.
#' @export
run_univariate_logistic_regression <- function(data,
                                               outcomes,
                                               predictors,
                                               outcomes_map = NULL,
                                               predictors_map = NULL,
                                               output_dir = NULL,
                                               save_format = c("csv", "txt", "docx", "none")) {

  save_format <- match.arg(save_format)
  saved_files <- character()

  # =========================================================================
  # 1. Label Mapping (Variable Dictionaries)
  # =========================================================================
  pred_labels <- NULL
  if (!is.null(predictors_map)) {
    pred_labels <- as.list(predictors_map)
  }

  out_labels <- outcomes
  if (!is.null(outcomes_map)) {
    out_labels <- sapply(outcomes, function(x) {
      if (x %in% names(outcomes_map)) outcomes_map[[x]] else x
    })
  }

  # =========================================================================
  # 2. Core Engine: Fit univariable models using gtsummary
  # =========================================================================
  tables_list <- lapply(outcomes, function(out) {
    tbl <- data %>%
      dplyr::select(dplyr::all_of(c(out, predictors))) %>%
      gtsummary::tbl_uvregression(
        method = glm,
        y = dplyr::sym(out),
        method.args = list(family = binomial),
        exponentiate = TRUE,
        label = pred_labels
      ) %>%
      gtsummary::add_global_p() %>%
      # [ARCHITECTURAL FIX]: Apply reference styling BEFORE merging
      # This ensures 'reference_row' is always found perfectly.
      gtsummary::modify_table_styling(
        columns = dplyr::starts_with("estimate"),
        rows = reference_row %in% TRUE,
        missing_symbol = "1.00 (Ref)"
      ) %>%
      gtsummary::modify_table_styling(
        columns = dplyr::starts_with("conf.low"),
        rows = reference_row %in% TRUE,
        missing_symbol = ""
      )

    return(tbl)
  })

  # =========================================================================
  # 3. Merge Outcomes and Extract Data Frame
  # =========================================================================
  if (length(outcomes) == 1) {
    final_tbl <- tables_list[[1]]
  } else {
    final_tbl <- gtsummary::tbl_merge(
      tbls = tables_list,
      tab_spanner = out_labels
    )
  }

  results_df <- gtsummary::as_tibble(final_tbl, col_labels = TRUE)

  # =========================================================================
  # 4. Integrated I/O Routing
  # =========================================================================
  if (!is.null(output_dir) && save_format != "none") {

    # Ensure output directory exists
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }

    # Generate safe file prefix replacing non-alphanumeric characters
    raw_prefix <- ifelse(length(outcomes) == 1, outcomes[1], "Multiple_Outcomes")
    safe_prefix <- gsub("[^a-zA-Z0-9]", "_", raw_prefix)

    # Use ClinKit's internal generate_filepath if available, otherwise fallback to base R

    file_path <- generate_filepath(
      base_name  = sprintf("Univariate_Logistic_%s", safe_prefix),
      ext        = save_format,
      output_dir = output_dir
    )


    # Save according to the requested format
    if (save_format == "csv") {
      utils::write.csv(results_df, file = file_path, row.names = FALSE, fileEncoding = "UTF-8", na = "")
    } else if (save_format == "txt") {
      utils::write.table(results_df, file = file_path, sep = "\t", row.names = FALSE, quote = FALSE, fileEncoding = "UTF-8", na = "")
    } else if (save_format == "docx") {
      if (requireNamespace("flextable", quietly = TRUE)) {
        ft <- gtsummary::as_flex_table(final_tbl)
        ft <- flextable::font(ft, fontname = get_pkg_font(), part = "all")
        ft <- flextable::autofit(ft)
        flextable::save_as_docx(ft, path = file_path)
      } else {
        warning("Package 'flextable' is required to save as docx. Please install it.")
      }
    }

    saved_files <- file_path
    message(sprintf("Results successfully saved to: %s", file_path))
  }

  # =========================================================================
  # 5. Assemble Output List
  # =========================================================================
  return(list(
    results     = results_df,
    saved_files = saved_files,
    call        = match.call()
  ))
}