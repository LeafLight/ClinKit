if (tolower(Sys.info()[['sysname']]) == "darwin") skip("skip all tests on macOS: known segfaults in dependencies")

library(testthat)
library(ClinKit)
test_that("univariate logistic regression works", {
              tmp_fp <- withr::local_tempdir()
              #out_file <- file.path(tmp, "table1.docx")
              data(package = "survival", "cancer")
              out <- run_univariate_logistic_regression(colon,
                                                 outcomes = c("status", "perfor"),
                                                 predictors = c("age", "nodes"),
                                                 outcomes_map = c("status" = "Status",
                                                                  "perfor" = "Perfor"
                                                 ),
                                                 output_dir = tmp_fp,
                                                 save_format = "csv"
              )
            expect_type(out, "list")
            # out is a list containing the path of the output file when output_dir is specified
            expect_true(all(file.exists(out$saved_files)))
        #data(package = "survival", "cancer")
          out <- run_univariate_logistic_regression(colon,
                                             outcomes = c("status", "perfor"),
                                             predictors = c("age", "nodes"),
                                             outcomes_map = c("status" = "Status",
                                                              "perfor" = "Perfor"
                                             ),
                                             #output_dir = tmp_fp,
                                             #save_format = "csv"
          )

        expect_type(out, "list")
        # out is a list of results output_dir is not specified
        expect_true(all(c("results", "call") %in% names(out)))

})
