if (tolower(Sys.info()[['sysname']]) == "darwin") skip("skip on macOS: segfault in interactionR")

library(ClinKit)
library(testthat)
test_that("interaction works", {
  tmp_fp <- withr::local_tempdir()
  output_file <- file.path(tmp_fp, "table1.docx")
  data(package = "survival", "cancer")
  is_win <- tolower(Sys.info()[['sysname']]) == "windows"
  is_mac <- tolower(Sys.info()[['sysname']]) == "darwin"
  skip_save <- is_win || is_mac
  out <- highlow_analysis(colon,
                          outcome = "status",
                          exposure_a = "nodes",
                          exposure_b = "time",
                          model2 = "age",
                          model3 = "sex",
                          model4 = NULL,
                          recode = TRUE,
                          save_format = if (skip_save) "none" else "all",
                          filename_base = "hhll",
                          output_dir = tmp_fp
                          )
  if (!skip_save) {
    print(file.exists(out$saved_files))
    print((out$saved_files))
    expect_true(all(file.exists(out$saved_files)))
  }
})
