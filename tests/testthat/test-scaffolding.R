# =========================================================================
# Unit Tests for ClinKit Project Scaffolding
# =========================================================================

test_that("use_clinkit_project initializes directory and config correctly", {
  # Create a safe temporary directory for testing
  tmp_dir <- withr::local_tempdir()
  
  # Run the initialization
  expect_message(
    use_clinkit_project(path = tmp_dir),
    "\\[SUCCESS\\] ClinKit project initialized"
  )
  
  # 1. Verify directory structure
  expect_true(dir.exists(file.path(tmp_dir, "data")))
  expect_true(dir.exists(file.path(tmp_dir, "scripts")))
  expect_true(dir.exists(file.path(tmp_dir, "results", "tables")))
  expect_true(dir.exists(file.path(tmp_dir, "results", "figures")))
  
  # 2. Verify Master Config file existence and content
  config_path <- file.path(tmp_dir, "00_Master_Config.R")
  expect_true(file.exists(config_path))
  
  config_lines <- readLines(config_path)
  expect_true(any(grepl("DATA_PATH", config_lines)))
  expect_true(any(grepl("MODELS_LIST", config_lines)))
})

test_that("module generators create correct scripts", {
  tmp_dir <- withr::local_tempdir()
  
  # Manually create a 'scripts' dir in the temp folder to mimic real environment
  scripts_dir <- file.path(tmp_dir, "scripts")
  dir.create(scripts_dir, recursive = TRUE)
  
  # List of module generator functions and their expected output filenames
  modules <- list(
    list(func = use_module_baseline,       file = "01_baseline_table.R"),
    list(func = use_module_univariate,     file = "02_univariate_analysis.R"),
    list(func = use_module_multivariable,  file = "03_multivariable_analysis.R"),
    list(func = use_module_rcs,            file = "04_rcs_analysis.R"),
    list(func = use_module_forest,         file = "05_subgroup_forest.R"),
    list(func = use_module_nri,            file = "06_nri_idi_analysis.R"),
    list(func = use_module_highlow,        file = "07_highlow_analysis.R")
  )
  
  # Iterate through all module generators
  for (mod in modules) {
    target_path <- file.path(scripts_dir, mod$file)
    
    # Check if the function runs and throws the success message
    expect_message(
      mod$func(path = target_path),
      "\\[SUCCESS\\] Module created"
    )
    
    # Check if the script file was actually created
    expect_true(file.exists(target_path))
    
    # Check if the script contains the essential config sourcing line
    script_lines <- readLines(target_path)
    expect_true(any(grepl("source\\('00_Master_Config\\.R'\\)", script_lines)))
  }
})
