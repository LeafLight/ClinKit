library(ClinKit)
library(testthat)
test_that("interaction works", {
  tmp_fp <- withr::local_tempdir()
  output_file <- file.path(tmp_fp, "table1.docx")
  data(package = "survival", "cancer")
  out <- highlow_analysis(colon,
                          outcome = "status",
                          exposure_a = "nodes",
                          exposure_b = "time",
                          model2 = "age",
                          model3 = "sex",
                          model4 = NULL,
                          recode = TRUE,
                          save_format = "all",
                          filename_base = "hhll",
                          output_dir = tmp_fp
                          )
  print(file.exists(out$saved_files))
  print((out$saved_files))
  expect_true(all(file.exists(out$saved_files)))
})
