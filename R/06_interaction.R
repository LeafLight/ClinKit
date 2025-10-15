#' High-High vs Low-Low Four-Group Logistic Regression with Additive Interaction
#'
#' Analyzes joint effects of two continuous exposures by median dichotomization,
#' creating four exposure groups and fitting logistic regression models with
#' sequential covariate adjustment. Calculates additive interaction measures.
#'
#' @param data Data frame containing outcome, exposures and all covariates
#' @param outcome Character scalar, binary outcome variable name (0/1)
#' @param exposure_a Character scalar, continuous exposure A variable name
#' @param exposure_b Character scalar, continuous exposure B variable name
#' @param model2 Character vector, additional covariates for model 2
#' @param model3 Character vector, additional covariates for model 3
#' @param model4 Character vector, additional covariates for model 4
#' @param output_dir Output directory for saving results, default NULL
#' @param save_format Save format: "none", "docx", "all", default "none"
#' @param recode Logical, whether to recode interaction variables, default FALSE
#' @param filename_base Base filename for outputs, default "highlow_analysis"
#'
#' @return List containing analysis results, interaction measures, and optional saved file paths
#' @export
#'
#' @examples
#' \dontrun{
#' result <- highlow_analysis(
#'   data = mtcars,
#'   outcome = "vs",
#'   exposure_a = "mpg",
#'   exposure_b = "wt",
#'   model2 = c("cyl", "gear"),
#'   model3 = c("carb"),
#'   output_dir = tempdir(),
#'   save_format = "all"
#' )
#' }
highlow_analysis <- function(
  data,
  outcome,
  exposure_a,
  exposure_b,
  model2 = NULL,
  model3 = NULL,
  model4 = NULL,
  output_dir = NULL,
  save_format = c("none", "docx", "all"),
  recode = FALSE,
  filename_base = "highlow_analysis"
) {

  # Parameter validation
  save_format <- match.arg(save_format)
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  # Check dependencies
  if (!requireNamespace("interactionR", quietly = TRUE)) {
    stop("Please install.packages('interactionR')")
  }

  # Check if variables exist
  all_vars <- unique(c(outcome, exposure_a, exposure_b, model2, model3, model4))
  missing_vars <- setdiff(all_vars, names(data))
  if (length(missing_vars) > 0) {
    stop("Variables not found in data: ", paste(missing_vars, collapse = ", "))
  }

  # Initialize results
  saved_files <- character(0)

  # ---- 1. Create four exposure groups ----
  data_processed <- data %>%
    dplyr::mutate(
      A_bin = as.numeric(.data[[exposure_a]] > median(.data[[exposure_a]], na.rm = TRUE)),
      B_bin = as.numeric(.data[[exposure_b]] > median(.data[[exposure_b]], na.rm = TRUE)),
      group = factor(
        dplyr::case_when(
          A_bin == 0 & B_bin == 0 ~ "Low A / Low B",
          A_bin == 0 & B_bin == 1 ~ "Low A / High B",
          A_bin == 1 & B_bin == 0 ~ "High A / Low B",
          A_bin == 1 & B_bin == 1 ~ "High A / High B"
        ),
        levels = c("Low A / Low B", "Low A / High B", "High A / Low B", "High A / High B")
      )
    )

  # ---- 2. Case counts ----
  case_tab <- dplyr::count(data_processed, group,
                          wt = (.data[[outcome]] == 1),
                          name = "Cases")

  # ---- 3. Build 4-level logistic formulas ----
  rhs1 <- "group"
  rhs2 <- if (!is.null(model2)) paste(rhs1, paste(model2, collapse = " + "), sep = " + ") else rhs1
  rhs3 <- if (!is.null(model3)) paste(rhs2, paste(model3, collapse = " + "), sep = " + ") else rhs2
  rhs4 <- if (!is.null(model4)) paste(rhs3, paste(model4, collapse = " + "), sep = " + ") else rhs3

  formulas <- list(
    Model1 = stats::as.formula(paste(outcome, "~", rhs1)),
    Model2 = stats::as.formula(paste(outcome, "~", rhs2)),
    Model3 = stats::as.formula(paste(outcome, "~", rhs3)),
    Model4 = stats::as.formula(paste(outcome, "~", rhs4))
  )

  # Fit models with error handling
  mods <- list()
  for (model_name in names(formulas)) {
    mods[[model_name]] <- tryCatch({
      glm(formulas[[model_name]], data = data_processed, family = binomial())
    }, error = function(e) {
      warning(sprintf("Model %s failed: %s", model_name, e$message))
      return(NULL)
    })
  }

  # ---- 4. Extract OR, 95% CI, p-values ----
  extract_results <- function(mod) {
    if (is.null(mod)) return(NULL)

    broom::tidy(mod, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95) %>%
      dplyr::filter(grepl("^group", term)) %>%
      dplyr::mutate(
        OR_CI = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
        P_value = sprintf("%.3f", p.value)
      ) %>%
      dplyr::mutate(group = sub("^group", "", term))
  }

  or_df <- purrr::map_dfr(mods, extract_results, .id = "Model")

  # ---- 5. Merge case counts with OR table ----
  if (nrow(or_df) > 0) {
    table_results <- case_tab %>%
      dplyr::left_join(or_df, by = "group") %>%
      dplyr::mutate(
        OR_CI = dplyr::coalesce(OR_CI, "1.00 (ref)"),
        P_value = dplyr::coalesce(P_value, "")
      ) %>%
      tidyr::pivot_wider(
        names_from = group,
        values_from = c(Cases, OR_CI, P_value)
      )
  } else {
    table_results <- data.frame()
  }

  # ---- 6. Additive interaction analysis ----
  interaction_result <- NULL
  if (!is.null(mods$Model4)) {

      int_mod <- glm(
        stats::as.formula(paste0(outcome, " ~ A_bin * B_bin + ",
                                paste(c(model2, model3, model4), collapse = " + "))),
        data = data_processed, family = binomial
      )

      interaction_result <- interactionR::interactionR(
        int_mod,
        exposure_names = c("A_bin", "B_bin"),
        ci.type = "mover", ci.level = 0.95, em = FALSE, recode = recode
      )

  }

  # ---- 7. Save results if requested ----
  if (save_format != "none" && !is.null(output_dir)) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }

    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

    if (save_format %in% c("docx", "all")) {
      # Save main results table
      print(table_results)
      if (nrow(table_results) > 0) {
        main_file <- file.path(output_dir, sprintf("%s_main_%s.docx", filename_base, timestamp))
        save_highlow_table(table_results, main_file, "Joint Association Analysis")
        saved_files <- c(saved_files, main_file)
      }

      # Save interaction results
      if (!is.null(interaction_result)) {
        int_file <- file.path(output_dir, sprintf("%s_interaction", filename_base))
        dir.create(file.path(output_dir,"%s_interaction"))
        save_interaction_table(interaction_result, int_file)
        saved_files <- c(saved_files,  paste0(int_file, "\\interaction.docx", collapse = ""))
      }
    }

    if (save_format == "all") {
      # Save raw data
      data_file <- file.path(output_dir, sprintf("%s_data_%s.csv", filename_base, timestamp))
      utils::write.csv(table_results, data_file, row.names = FALSE)
      saved_files <- c(saved_files, data_file)
    }
  }

  # ---- 8. Return structured results ----
  return(list(
    group_vector = data_processed$group,
    main_results = table_results,
    interaction = interaction_result,
    models = mods,
    saved_files = if (length(saved_files) > 0) saved_files else NULL,
    call = match.call()
  ))
}

#' Save High-Low Analysis Table (Internal)
#' @keywords internal
save_highlow_table <- function(results, file_path, title) {
  if (!requireNamespace("flextable", quietly = TRUE) ||
      !requireNamespace("officer", quietly = TRUE)) {
    stop("Please install.packages('flextable') and install.packages('officer')")
  }

  ft <- flextable::flextable(results) %>%
    flextable::theme_booktabs() %>%
    flextable::autofit()

  officer::read_docx() %>%
    officer::body_add_par(title, style = "heading 1") %>%
    flextable::body_add_flextable(ft) %>%
    print(target = file_path)
}

#' Save Interaction Table (Internal)
#' @keywords internal
save_interaction_table <- function(interaction_result, file_path) {
  if (is.null(interaction_result)) return(NULL)

  # Round numeric values
  interaction_result$dframe <- as.data.frame(
    lapply(interaction_result$dframe, function(x) {
      if(is.numeric(x)) round(x, 4) else x
    })
  )
print(file_path)
  interactionR::interactionR_table(interaction_result, p.value = TRUE, file_path = file_path)
}

utils::globalVariables(c("group", "term", "estimate", "conf.low", "conf.high", "p.value", "OR_CI", "P_value", "Cases"))
