if (tolower(Sys.info()[['sysname']]) == "darwin") skip("skip all tests on macOS: known segfaults in dependencies")

library(ClinKit)
library(testthat)
test_that("forest works", {
    tmp_fp <- withr::local_tempdir()
    data(cancer, package = "survival")
    out <- subgroup_forest(
      data = colon,
      outcome = "status",
      exposure = "nodes",
      subgroups = c("sex", "adhere", "rx"),
      covariates = c("age"),
      output_dir = tmp_fp,
      save_format = "all",
      CI_title = "OR(95% CI)"
    )
    expect_true(all(file.exists(out$saved_files)))
})
test_that("forest theme works", {
    tmp_fp <- withr::local_tempdir()
    data(cancer, package = "survival")
    out <- subgroup_forest(
      data = colon,
      outcome = "status",
      exposure = "nodes",
      subgroups = c("sex", "adhere", "rx"),
      covariates = c("age"),
      output_dir = tmp_fp,
      save_format = "all",
      CI_title = "OR(95% CI)",
      tm = "cyan"
    )
    expect_true(all(file.exists(out$saved_files)))
    data(cancer, package = "survival")
    out <- subgroup_forest(
      data = colon,
      outcome = "status",
      exposure = "nodes",
      subgroups = c("sex", "adhere", "rx"),
      covariates = c("age"),
      output_dir = tmp_fp,
      save_format = "all",
      CI_title = "OR(95% CI)",
      tm = tm_green(base_size = 20)
    )
    expect_true(all(file.exists(out$saved_files)))

    tm <- tm_default(base_size = 20, background_levels = c(1, 2, 1, 2, 1))
    expect_true(is.list(tm))
})

test_that("create font config works",{
font_func <- get("setup_font_config", envir = asNamespace("ClinKit"))
fc <- font_func("STIX Two Math")
expect_true(file.exists(fc))
})
