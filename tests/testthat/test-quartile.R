library(ClinKit)
library(testthat)
test_that("quartile analysis works", {
    tmp_fp <- withr::local_tempdir()
    output_file <- file.path(tmp_fp, "table1.docx")
    data(package = "survival", "cancer")
    out <- quartile_logistic_analysis(colon, 
                               outcomes = c("status", "perfor"),
                               predictors = c("time"),
                               models_list = list(
                                                  model2 = c("sex"),
                                                  model3 = c("age"),
                                                  model4 = NULL
                               ),
                               output_dir = tmp_fp,
                               save_format = "all"

    )
    expect_true(grepl("docx", paste(out$saved_files, collapse = "")))
    expect_true(all(file.exists(out$saved_files)))
})
test_that("multinomial quartile analysis works", {
    tmp_fp <- withr::local_tempdir()
    output_file <- file.path(tmp_fp, "table1.docx")
    data(package = "survival", "cancer")
    out <- quartile_multinomial_analysis(colon, 
                               outcomes = c("rx"),
                               predictors = c("time"),
                               models_list = list(
                                                  model2 = c("sex"),
                                                  model3 = c("age"),
                                                  model4 = NULL
                               ),
                               output_dir = tmp_fp,
                               save_format = "all"

    )
    expect_true(grepl("docx", paste(out$saved_files, collapse = "")))
    expect_true(all(file.exists(out$saved_files)))
})
