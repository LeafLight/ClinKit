#' Dynamic Sequential Multivariable Logistic Regression
#'
#' @description
#' Fits cumulative multivariable logistic regression models across N-layers defined by
#' \code{models_list}. The function ensures reference categories (1.00 (Ref)) are
#' consistently maintained and horizontally aligned across all adjustment levels.
#'
#' @param data A data frame containing the variables.
#' @param outcomes Character vector of outcome variable names (Binary).
#' @param predictors Character vector of primary predictors to be evaluated.
#' @param models_list A NAMED list where each element contains new covariates
#'                    to be added sequentially (e.g., list(Base = c("age"), Clinical = c("nodes"))).
#' @param outcomes_map Optional named mapping for outcome display labels.
#' @param predictors_map Optional named mapping for predictor display labels.
#' @param output_dir Optional path to save output files.
#' @param save_format Save format: "none", "docx", "csv", or "txt".
#'
#' @return A list containing a consolidated results data frame and saved file paths.
#'
#' @examples
#' \dontrun{
#' library(survival)
#' data(colon)
#' df <- colon %>% dplyr::filter(etype == 2) %>% dplyr::mutate(status = as.factor(status))
#'
#' models <- list(
#'   "Demographics" = c("age", "sex"),
#'   "Pathology"    = c("nodes", "extent")
#' )
#'
#' run_multivariable_logistic_regression(
#'   data = df,
#'   outcomes = "status",
#'   predictors = "adhere",
#'   models_list = models,
#'   save_format = "docx"
#' )
#' }
#' @export
run_multivariable_logistic_regression <- function(data,
                                                  outcomes,
                                                  predictors,
                                                  models_list,
                                                  outcomes_map = NULL,
                                                  predictors_map = NULL,
                                                  output_dir = NULL,
                                                  save_format = c("none", "docx", "csv", "txt")) {

  save_format <- match.arg(save_format)
  saved_files <- character()
  pred_labels <- if (!is.null(predictors_map)) as.list(predictors_map) else NULL

  # --- DYNAMIC LAYER SETUP ---
  # 1. Define Model Names: "Model 1" (Unadjusted) + names from models_list
  model_names <- c("Model 1", names(models_list))

  # 2. Build Cumulative Covariate Layers
  # Use Reduce to ensure Model N contains all variables from Model N-1
  cumulative_covs <- Reduce(function(a, b) unique(c(a, b)), models_list, accumulate = TRUE)
  # Prepend an empty vector for the Unadjusted Model (Model 1)
  all_layers_covs <- c(list(character(0)), cumulative_covs)
  names(all_layers_covs) <- model_names

  for (outcome in outcomes) {
    predictor_tables <- lapply(predictors, function(pred) {

      # Check all required variables exist for this predictor's full model
      current_vars <- unique(c(outcome, pred, unlist(models_list)))
      complete_data <- data[complete.cases(data[, current_vars, drop = FALSE]), ]
      if (nrow(complete_data) < 10) return(NULL)

      # 3. Fit models for each dynamic layer
      model_tbls <- lapply(seq_along(all_layers_covs), function(i) {

        # Remove the current predictor from covariates to avoid collinearity/redundancy
        current_covs <- setdiff(all_layers_covs[[i]], pred)

        formula_str <- paste0("`", outcome, "` ~ `", pred, "`")
        if (length(current_covs) > 0) {
          formula_str <- paste0(formula_str, " + ", paste0("`", current_covs, "`", collapse = " + "))
        }

        fit <- glm(as.formula(formula_str), data = complete_data, family = binomial)

        gtsummary::tbl_regression(fit, exponentiate = TRUE, include = dplyr::all_of(pred), label = pred_labels) %>%
          gtsummary::add_global_p(keep = TRUE) %>%
          gtsummary::modify_table_styling(
            columns = dplyr::starts_with("estimate"),
            rows = reference_row %in% TRUE, missing_symbol = "1.00 (Ref)"
          ) %>%
          gtsummary::modify_table_styling(
            columns = dplyr::starts_with("conf.low"),
            rows = reference_row %in% TRUE, missing_symbol = ""
          )
      })

      # 4. Horizontally merge based on dynamic count
      return(gtsummary::tbl_merge(tbls = model_tbls, tab_spanner = model_names))
    })

    # --- STACKING & I/O (Same as before but handles dynamic layers) ---
    predictor_tables <- Filter(Negate(is.null), predictor_tables)
    if (length(predictor_tables) == 0) next

    master_tbl <- gtsummary::tbl_stack(predictor_tables) %>%
      gtsummary::modify_caption(sprintf("**Sequential Models for: %s**", outcome))

    results_df <- gtsummary::as_tibble(master_tbl, col_labels = TRUE)

    if (save_format != "none" && !is.null(output_dir)) {
      if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
      safe_outcome <- gsub("[^a-zA-Z0-9]", "_", outcome)
      file_path <- generate_filepath(
        base_name  = sprintf("Sequential_Logistic_%s", safe_outcome),
        ext        = save_format,
        output_dir = output_dir
      )

      if (save_format == "docx") {
        ft <- gtsummary::as_flex_table(master_tbl)
        ft <- flextable::font(ft, fontname = "Times New Roman", part = "all")
        flextable::save_as_docx(flextable::autofit(ft), path = file_path)
      } else if (save_format == "csv") {
        utils::write.csv(results_df, file_path, row.names = FALSE)
      }
      saved_files <- c(saved_files, file_path)
    }
  }
  return(list(results = results_df, saved_files = saved_files, call=match.call()))
}

#' Dynamic Sequential Multivariable Multinomial Logistic Regression
#'
#' @description
#' Fits sequential multinomial logistic regression models using \code{nnet::multinom}.
#' Implements an "Anti-Cartesian" row-indexing strategy to ensure perfect alignment
#' of multi-level categorical outcomes across adjustment layers.
#'
#' @param data A data frame containing the variables.
#' @param outcomes Character vector of multinomial outcome variables.
#' @param predictors Character vector of primary predictors.
#' @param models_list A NAMED list defining sequential adjustment layers.
#' @param ref_level Optional string to set the reference level of the outcome.
#' @param output_dir Optional path to save output files.
#' @param save_format Save format: "none", "docx", "csv", or "txt".
#'
#' @return A list containing results data frame and saved file paths.
#'
#' @examples
#' \dontrun{
#' # Example with 3-level categorical outcome
#' library(nnet)
#' df_test <- survival::colon %>%
#'   dplyr::mutate(extent = factor(extent, labels = c("Sub", "Mus", "Ser", "Con")))
#'
#' run_multivariable_multinomial_logistic_regression(
#'   data = df_test,
#'   outcomes = "extent",
#'   predictors = "sex",
#'   models_list = list("Basic" = c("age"), "Clinical" = c("obstruct")),
#'   save_format = "docx"
#' )
#' }
#' @export
run_multivariable_multinomial_logistic_regression <- function(data,
                                                              outcomes,
                                                              predictors,
                                                              models_list,
                                                              ref_level = NULL,
                                                              output_dir = NULL,
                                                              save_format = c("none", "docx", "csv", "txt")) {

  save_format <- match.arg(save_format)
  saved_files <- character()
  if (!requireNamespace("nnet", quietly = TRUE)) stop("Please install.packages('nnet')")

  # --- DYNAMIC LAYER SETUP ---
  model_names <- c("Model 1", names(models_list))
  # Use Reduce to build cumulative covariate sets (Model N includes all previous)
  cumulative_covs <- Reduce(function(a, b) unique(c(a, b)), models_list, accumulate = TRUE)
  all_layers_covs <- c(list(character(0)), cumulative_covs)
  names(all_layers_covs) <- model_names

  working_data <- data

  for (outcome in outcomes) {
    # Set outcome factor and reference level
    working_data[[outcome]] <- factor(working_data[[outcome]])
    if (!is.null(ref_level)) {
      working_data[[outcome]] <- relevel(working_data[[outcome]], ref = ref_level)
    }

    predictor_tables <- lapply(predictors, function(pred) {
      current_vars <- unique(c(outcome, pred, unlist(models_list)))
      complete_data <- working_data[complete.cases(working_data[, current_vars, drop = FALSE]), ]
      if (nrow(complete_data) < 10) return(NULL)

      # 1. Fit models for each dynamic layer
      model_tbls <- lapply(seq_along(all_layers_covs), function(i) {
        current_covs <- setdiff(all_layers_covs[[i]], pred)

        # Build formula using backticks for safety
        formula_str <- paste0("`", outcome, "` ~ `", pred, "`")
        if (length(current_covs) > 0) {
          formula_str <- paste0(formula_str, " + ", paste0("`", current_covs, "`", collapse = " + "))
        }

        fit <- nnet::multinom(as.formula(formula_str), data = complete_data, trace = FALSE)

        suppressMessages({
          gtsummary::tbl_regression(fit, exponentiate = TRUE, include = dplyr::all_of(pred)) %>%
            gtsummary::add_global_p(keep = TRUE) %>%
            gtsummary::modify_table_styling(
              columns = dplyr::starts_with("estimate"),
              rows = reference_row %in% TRUE, missing_symbol = "1.00 (Ref)"
            ) %>%
            gtsummary::modify_table_styling(
              columns = dplyr::starts_with("conf.low"),
              rows = reference_row %in% TRUE, missing_symbol = ""
            ) %>%
            # [DYNAMIC ANTI-CARTESIAN]: Inject unique IDs based on row numbers to ensure clean merge
            gtsummary::modify_table_body(
              ~ .x %>% dplyr::mutate(
                variable = paste0(variable, "___", dplyr::row_number()),
                label = paste0(label, "___", dplyr::row_number())
              )
            )
        })
      })

      # 2. Merge all dynamically generated layers
      merged_pred_tbl <- gtsummary::tbl_merge(
        tbls = model_tbls,
        tab_spanner = model_names
      ) %>%
        # [RESTORE]: Clean up the temporary row IDs after merge is complete
        gtsummary::modify_table_body(
          ~ .x %>% dplyr::mutate(
            variable = gsub("___\\d+$", "", variable),
            label = gsub("___\\d+$", "", label)
          )
        )

      return(merged_pred_tbl)
    })

    # --- STACKING & I/O ---
    predictor_tables <- Filter(Negate(is.null), predictor_tables)
    if (length(predictor_tables) == 0) next

    master_tbl <- gtsummary::tbl_stack(predictor_tables) %>%
      gtsummary::modify_caption(sprintf("**Dynamic Multinomial Models for: %s**", outcome))

    results_df <- gtsummary::as_tibble(master_tbl, col_labels = TRUE)

    if (save_format != "none" && !is.null(output_dir)) {
      if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
      safe_outcome <- gsub("[^a-zA-Z0-9]", "_", outcome)
      file_path <- generate_filepath(
        base_name  = sprintf("Sequential_Multinomial_%s", safe_outcome),
        ext        = save_format,
        output_dir = output_dir
      )

      if (save_format == "docx") {
        ft <- gtsummary::as_flex_table(master_tbl)
        ft <- flextable::font(ft, fontname = "Times New Roman", part = "all")
        flextable::save_as_docx(flextable::autofit(ft), path = file_path)
      } else if (save_format == "csv") {
        utils::write.csv(results_df, file_path, row.names = FALSE)
      }
      saved_files <- c(saved_files, file_path)
    }
  }
  return(list(results = results_df, saved_files = saved_files, call=match.call()))
}

#' Dynamic Sequential Multivariable Cox Proportional Hazards Regression
#'
#' Fits sequential Cox models across N-layers. Automatically handles survival
#' objects and ensures reference categories are maintained across adjustment levels.
#'
#' @param data          Data frame.
#' @param time          Character scalar, name of the follow-up time variable.
#' @param status        Character scalar, name of the event status variable (0/1).
#' @param predictors    Character vector of primary predictors to be evaluated.
#' @param models_list   A NAMED list defining sequential adjustment layers.
#' @param predictors_map Optional named mapping for predictor display labels.
#' @param output_dir    Output directory.
#' @param save_format   Save format: "none", "docx", "csv", "txt".
#'
#' @return A list containing results dataframe and saved file paths.
#' @export
run_multivariable_cox_regression <- function(data,
                                             time,
                                             status,
                                             predictors,
                                             models_list,
                                             predictors_map = NULL,
                                             output_dir = NULL,
                                             save_format = c("none", "docx", "csv", "txt")) {

  save_format <- match.arg(save_format)
  saved_files <- character()
  pred_labels <- if (!is.null(predictors_map)) as.list(predictors_map) else NULL

  # --- DYNAMIC LAYER SETUP ---
  model_names <- c("Model 1", names(models_list))
  cumulative_covs <- Reduce(function(a, b) unique(c(a, b)), models_list, accumulate = TRUE)
  all_layers_covs <- c(list(character(0)), cumulative_covs)
  names(all_layers_covs) <- model_names

  # Process survival outcome: Ensure status is numeric 0/1 for survival package
  data[[status]] <- as.numeric(as.character(data[[status]]))

  # Iterate over each predictor to generate a stacked table
  predictor_tables <- lapply(predictors, function(pred) {

    # Check all required variables for this predictor's full model
    current_vars <- unique(c(time, status, pred, unlist(models_list)))
    complete_data <- data[complete.cases(data[, current_vars, drop = FALSE]), ]
    if (nrow(complete_data) < 10) return(NULL)

    # 1. Fit Cox models for each dynamic layer
    model_tbls <- lapply(seq_along(all_layers_covs), function(i) {
      current_covs <- setdiff(all_layers_covs[[i]], pred)

      # Build Survival Formula: Surv(time, status) ~ Predictor + Covariates
      formula_str <- sprintf("survival::Surv(`%s`, `%s`) ~ `%s`", time, status, pred)
      if (length(current_covs) > 0) {
        formula_str <- paste0(formula_str, " + ", paste0("`", current_covs, "`", collapse = " + "))
      }

      fit <- survival::coxph(as.formula(formula_str), data = complete_data)

      # Generate table (gtsummary automatically handles HR and 95% CI)
      gtsummary::tbl_regression(fit, exponentiate = TRUE, include = dplyr::all_of(pred), label = pred_labels) %>%
        gtsummary::add_global_p(keep = TRUE) %>%
        gtsummary::modify_table_styling(
          columns = dplyr::starts_with("estimate"),
          rows = reference_row %in% TRUE, missing_symbol = "1.00 (Ref)"
        ) %>%
        gtsummary::modify_table_styling(
          columns = dplyr::starts_with("conf.low"),
          rows = reference_row %in% TRUE, missing_symbol = ""
        )
    })

    # 2. Merge all adjustment layers horizontally
    return(gtsummary::tbl_merge(tbls = model_tbls, tab_spanner = model_names))
  })

  # --- STACKING & I/O ---
  predictor_tables <- Filter(Negate(is.null), predictor_tables)
  if (length(predictor_tables) == 0) return(NULL)

  master_tbl <- gtsummary::tbl_stack(predictor_tables) %>%
    gtsummary::modify_caption(sprintf("**Sequential Cox PH Models for: %s**", status))

  results_df <- gtsummary::as_tibble(master_tbl, col_labels = TRUE)

  if (save_format != "none" && !is.null(output_dir)) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    file_path <- generate_filepath(
      base_name  = sprintf("Sequential_Cox_%s", status),
      ext        = save_format,
      output_dir = output_dir
    )

    if (save_format == "docx") {
      ft <- gtsummary::as_flex_table(master_tbl)
      ft <- flextable::font(ft, fontname = "Times New Roman", part = "all")
      flextable::save_as_docx(flextable::autofit(ft), path = file_path)
    } else if (save_format == "csv") {
      utils::write.csv(results_df, file_path, row.names = FALSE)
    }
    saved_files <- c(saved_files, file_path)
    message(sprintf("Saved Cox analysis results to: %s", file_path))
  }

  return(list(results = results_df, saved_files = saved_files, call=match.call()))
}
# Ensure global variables for R CMD check
utils::globalVariables(c("reference_row", "variable", "label"))