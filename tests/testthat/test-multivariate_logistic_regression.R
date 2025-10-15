# Set flextable default font to NULL to avoid macOS font segfaults
if (requireNamespace("flextable", quietly = TRUE)) {
  flextable::set_flextable_defaults(font.family = NULL)
}

# Ensure survival package and colon dataset are available for tests
if (!requireNamespace("survival", quietly = TRUE)) {
  stop("Package 'survival' is required for these tests.")
}
library(survival)
if (!exists("colon")) data(colon, package = "survival")

library(ClinKit)
library(testthat)
test_that("mulivaraite logistic regression works", {

              tmp_fp <- withr::local_tempdir()
              #out_file <- file.path(tmp, "table1.docx")
              data(package = "survival", "cancer")
              out <- run_multivariable_logistic_regression(colon,
                                                 outcomes = c("status", "perfor"),
                                                 predictors = c("age", "nodes"),
                                                 outcomes_map = c("status" = "Status",
                                                                  "perfor" = "Perfor"
                                                 ),
                                                models_list = list(
                                                                          model2 = c("sex"),
                                                                          model3 = NULL,
                                                                          model4 = NULL
                                                                        ),
                                                 output_dir = tmp_fp,
                                                 save_format = "csv"
              )
            expect_type(out, "list")
            # out is a list containing the path of the output file when output_dir is specified
            expect_true(all(file.exists(out$saved_files)))
        #data(package = "survival", "cancer")
          out <- run_multivariable_logistic_regression(colon,
                                             outcomes = c("status", "perfor"),
                                             predictors = c("age", "nodes"),
                                             outcomes_map = c("status" = "Status",
                                                              "perfor" = "Perfor"
                                             ),
                                                       models_list = list(
                                                                          model2 = c("sex"),
                                                                          model3 = NULL,
                                                                          model4 = NULL
                                                       )
                                             #output_dir = tmp_fp,
                                             #save_format = "csv"
          )

        expect_type(out, "list")
        # out is a list of results output_dir is not specified
        expect_true(all(c("results", "call") %in% names(out)))
        make_multivariate_table(out$results, out_file=file.path(tmp_fp, "table1.docx"))
        expect_true(file.exists(file.path(tmp_fp, "table1.docx")))

              tmp_fp <- withr::local_tempdir()
              #out_file <- file.path(tmp, "table1.docx")
              data(package = "survival", "cancer")
              out <- run_multivariable_multinomial_logistic_regression(colon,
                                                 outcomes = c("rx"),
                                                 predictors = c("age", "nodes"),
                                                 #outcomes_map = c("rx" = "Rx"),
                                                models_list = list(
                                                                          model2 = c("sex"),
                                                                          model3 = NULL,
                                                                          model4 = NULL
                                                                        ),
                                                 output_dir = tmp_fp,
                                                 save_format = "csv"
              )
            expect_type(out, "list")
            # out is a list containing the path of the output file when output_dir is specified
            expect_true(all(file.exists(out$saved_files)))
        #data(package = "survival", "cancer")
          out <- run_multivariable_multinomial_logistic_regression(colon,
                                             outcomes = c("rx"),
                                             predictors = c("age", "nodes"),
                                             models_list = list(
                                                          model2 = c("sex"),
                                                          model3 = NULL,
                                                          model4 = NULL
                                                       )
                                             #output_dir = tmp_fp,
                                             #save_format = "csv"
          )

        expect_type(out, "list")
        # out is a list of results output_dir is not specified
        expect_true(all(c("results", "call") %in% names(out)))
})
