if (tolower(Sys.info()[['sysname']]) == "darwin") skip("skip all tests on macOS: known segfaults in dependencies")
test_that("roc works", {
  tmp_fp <- withr::local_tempdir()
  out <- roc_analysis(
  data = mtcars,
  outcome = "vs",
  predictors = c("mpg", "wt", "disp"),
  combined_models = list(Combined = c("mpg", "wt")),
  output_dir = tmp_fp,
  save_format = "all",
  delong_test = TRUE,
  direction = "auto"
)
  expect_true(all(file.exists(out$saved_files)))
})
