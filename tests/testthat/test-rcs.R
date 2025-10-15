library(ClinKit)
library(testthat)

if (tolower(Sys.info()[['sysname']]) == "darwin") skip("skip all tests on macOS: known segfaults in dependencies")

test_that("rcs works", {
  data(package = "survival", cancer)
  tmp_fp <- withr::local_tempdir()
  output_file <- file.path(tmp_fp, "table1.docx")

  out <- run_analysis_rcs(
    data = mtcars,
    predictors = c("mpg"),
    outcomes = c("vs"),
    covariates = c("gear", "hp"),
    output_dir = tmp_fp,
    save_format = "all"
)
  expect_true("call" %in% names(out))
})
