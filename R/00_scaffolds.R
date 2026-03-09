# =========================================================================
# ClinKit Project Scaffolding System
# =========================================================================

#' Initialize a ClinKit Analysis Project
#'
#' @description
#' Creates the core directory structure and the master configuration file
#' for a standardized clinical research workflow.
#'
#' @param path Target directory path (defaults to current working directory).
#' @export
use_clinkit_project <- function(path = ".") {

  if (!dir.exists(path)) dir.create(path, recursive = TRUE)

  # 1. Create Core Directories
  dirs <- c("data", "scripts", "results/tables", "results/figures")
  lapply(file.path(path, dirs), dir.create, recursive = TRUE, showWarnings = FALSE)

  # 2. Define Master Configuration Template
  config_content <- c(
    "# =========================================================================",
    "# ClinKit: Master Configuration & Control Center",
    "# =========================================================================",
    "library(ClinKit)",
    "library(dplyr)",
    "library(survival) # For dataset demonstration",
    "",
    "# --- 1. GLOBAL VARIABLES -------------------------------------------------",
    "# Define these once, and all downstream module scripts will use them.",
    "DATA_PATH     <- 'data/my_dataset.csv' # Replace with your data path",
    "OUTCOME_VAR   <- 'status'",
    "TIME_VAR      <- 'time'                # Only needed for Cox regression",
    "PREDICTORS    <- c('age', 'sex', 'nodes', 'adhere')",
    "",
    "# Hierarchical adjustment layers (for Model 1 -> Model N)",
    "MODELS_LIST   <- list(",
    "  'Demographics' = c('age', 'sex'),",
    "  'Clinical'     = c('obstruct', 'perfor')",
    ")",
    "",
    "# Variables specific to Subgroup & High-Low analysis",
    "SUBGROUPS     <- c('sex', 'adhere')",
    "MAIN_EXPOSURE <- 'nodes'",
    "",
    "# --- 2. GENERATE ANALYSIS MODULES ----------------------------------------",
    "# Run the functions below to generate template scripts for each step.",
    "# The generated scripts will be saved in the 'scripts/' folder.",
    "",
    "# [Module 1]: Baseline Characteristics (Table 1)",
    "# ClinKit::use_module_baseline()",
    "",
    "# [Module 2]: Univariate Screening (Table 2)",
    "# ClinKit::use_module_univariate()",
    "",
    "# [Module 3]: Multivariable Hierarchical Models (Table 3)",
    "# ClinKit::use_module_multivariable()",
    "",
    "# [Module 4]: Non-linear Trend Analysis (RCS Plots)",
    "# ClinKit::use_module_rcs()",
    "",
    "# [Module 5]: Subgroup Forest Plots",
    "# ClinKit::use_module_forest()",
    "",
    "# [Module 6]: Reclassification & Incremental Value (NRI/IDI)",
    "# ClinKit::use_module_nri()",
    "",
    "# [Module 7]: Joint Exposure Analysis (High-Low Interaction)",
    "# ClinKit::use_module_highlow()"
  )

  config_file <- file.path(path, "00_Master_Config.R")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(config_content, config_file)

  message("[SUCCESS] ClinKit project initialized.")
  message("-> Created directories: data/, scripts/, results/")
  message("-> Created file: 00_Master_Config.R")
  message("Please open '00_Master_Config.R' to start your analysis workflow.")
}

#' Generate Baseline Table Module Script
#' @param path Character. Target file path for the generated script.
#' @export
use_module_baseline <- function(path = "scripts/01_baseline_table.R") {
  content <- c(
    "# Module 1: Baseline Characteristics",
    "source('00_Master_Config.R')",
    "",
    "# Load data (using colon dataset as example)",
    "df_clean <- survival::colon %>% filter(etype == 2)",
    "",
    "make_baseline_table(",
    "  data = df_clean,",
    "  outcome = OUTCOME_VAR,",
    "  file = 'results/tables/Table1_Baseline.docx'",
    ")"
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(content, path)
  message(sprintf("[SUCCESS] Module created: %s", path))
}

#' Generate Univariate Analysis Module Script
#' @param path Character. Target file path for the generated script.
#' @export
use_module_univariate <- function(path = "scripts/02_univariate_analysis.R") {
  content <- c(
    "# Module 2: Univariate Analysis",
    "source('00_Master_Config.R')",
    "",
    "df_clean <- survival::colon %>% filter(etype == 2)",
    "",
    "run_univariate_logistic_regression(",
    "  data = df_clean,",
    "  outcomes = OUTCOME_VAR,",
    "  predictors = PREDICTORS,",
    "  output_dir = 'results/tables',",
    "  save_format = 'docx'",
    ")"
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(content, path)
  message(sprintf("[SUCCESS] Module created: %s", path))
}

#' Generate Multivariable Analysis Module Script
#' @param path Character. Target file path for the generated script.
#' @export
use_module_multivariable <- function(path = "scripts/03_multivariable_analysis.R") {
  content <- c(
    "# Module 3: Multivariable Hierarchical Models",
    "source('00_Master_Config.R')",
    "",
    "df_clean <- survival::colon %>% filter(etype == 2)",
    "",
    "# Choose your engine: Logistic, Multinomial, or Cox",
    "run_multivariable_cox_regression(",
    "  data = df_clean,",
    "  time = TIME_VAR,",
    "  status = OUTCOME_VAR,",
    "  predictors = PREDICTORS,",
    "  models_list = MODELS_LIST,",
    "  output_dir = 'results/tables',",
    "  save_format = 'docx'",
    ")"
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(content, path)
  message(sprintf("[SUCCESS] Module created: %s", path))
}

#' Generate RCS Plot Module Script
#' @param path Character. Target file path for the generated script.
#' @export
use_module_rcs <- function(path = "scripts/04_rcs_analysis.R") {
  content <- c(
    "# Module 4: Restricted Cubic Splines (Non-linear trend)",
    "source('00_Master_Config.R')",
    "",
    "df_clean <- survival::colon %>% filter(etype == 2) %>% filter(!is.na(nodes))",
    "",
    "run_analysis_rcs(",
    "  data = df_clean,",
    "  predictors = 'nodes',",
    "  outcomes = OUTCOME_VAR,",
    "  covariates = unlist(MODELS_LIST),",
    "  knots = 4,",
    "  output_dir = 'results/figures',",
    "  save_format = 'all'",
    ")"
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(content, path)
  message(sprintf("[SUCCESS] Module created: %s", path))
}

#' Generate Subgroup Forest Plot Module Script
#' @param path Character. Target file path for the generated script.
#' @export
use_module_forest <- function(path = "scripts/05_subgroup_forest.R") {
  content <- c(
    "# Module 5: Subgroup Analysis and Interaction Forest Plot",
    "source('00_Master_Config.R')",
    "",
    "df_clean <- survival::colon %>% filter(etype == 2)",
    "",
    "subgroup_forest(",
    "  data = df_clean,",
    "  outcome = OUTCOME_VAR,",
    "  exposure = MAIN_EXPOSURE,",
    "  subgroups = SUBGROUPS,",
    "  covariates = setdiff(unlist(MODELS_LIST), SUBGROUPS),",
    "  output_dir = 'results/figures',",
    "  save_format = 'all',",
    "  tm = 'blue'",
    ")"
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(content, path)
  message(sprintf("[SUCCESS] Module created: %s", path))
}

#' Generate Reclassification Analysis Module Script
#' @param path Character. Target file path for the generated script.
#' @export
use_module_reclassification <- function(path = "scripts/06_nri_idi_analysis.R") {
  content <- c(
    "# Module 6: Reclassification & Discrimination (NRI/IDI)",
    "source('00_Master_Config.R')",
    "",
    "df_clean <- survival::colon %>% filter(etype == 2)",
    "df_clean[[OUTCOME_VAR]] <- as.numeric(as.character(df_clean[[OUTCOME_VAR]]))",
    "",
    "# Define base model variables and the new marker you want to evaluate",
    "base_model_vars <- unlist(MODELS_LIST)",
    "new_marker <- MAIN_EXPOSURE",
    "",
    "run_reclassification_analysis(",
    "  data = df_clean,",
    "  outcome = OUTCOME_VAR,",
    "  base_vars = base_model_vars,",
    "  new_vars = new_marker,",
    "  output_dir = 'results/tables',",
    "  save_format = 'all'",
    ")"
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(content, path)
  message(sprintf("[SUCCESS] Module created: %s", path))
}

#' Generate High-Low Joint Analysis Module Script
#' @param path Character. Target file path for the generated script.
#' @export
use_module_highlow <- function(path = "scripts/07_highlow_analysis.R") {
  content <- c(
    "# Module 7: Joint Exposure & Additive Interaction (High-Low)",
    "source('00_Master_Config.R')",
    "",
    "df_clean <- survival::colon %>% filter(etype == 2)",
    "df_clean[[OUTCOME_VAR]] <- as.numeric(as.character(df_clean[[OUTCOME_VAR]]))",
    "",
    "highlow_analysis(",
    "  data = df_clean,",
    "  outcome = OUTCOME_VAR,",
    "  exposure_a = 'age',",
    "  exposure_b = 'nodes',",
    "  model2 = unlist(MODELS_LIST),",
    "  output_dir = 'results/tables',",
    "  save_format = 'all'",
    ")"
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(content, path)
  message(sprintf("[SUCCESS] Module created: %s", path))
}

#' Generate ROC Analysis Module Script
#' @param path Character. Target file path for the generated script.
#' @export
use_module_roc <- function(path = "scripts/08_roc_analysis.R") {
  content <- c(
    "# Module 8: ROC Curve Analysis and AUC Comparison",
    "source('00_Master_Config.R')",
    "",
    "df_clean <- survival::colon %>% filter(etype == 2)",
    "# Ensure outcome is numeric 0/1 for pROC",
    "df_clean[[OUTCOME_VAR]] <- as.numeric(as.character(df_clean[[OUTCOME_VAR]]))",
    "",
    "run_roc_analysis(",
    "  data = df_clean,",
    "  outcome = OUTCOME_VAR,",
    "  predictors = PREDICTORS, # Individual predictors defined in SSOT",
    "  combined_models = list(",
    "    'Full_Model' = PREDICTORS",
    "  ),",
    "  output_dir = 'results/figures',",
    "  save_format = 'all',",
    "  delong_test = TRUE,",
    "  direction = 'auto'",
    ")"
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(content, path)
  message(sprintf("[SUCCESS] Module created: %s", path))
}