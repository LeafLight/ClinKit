library(ClinKit)
library(testthat)
if (tolower(Sys.info()[['sysname']]) == "darwin") skip("skip all tests on macOS: known segfaults in dependencies")
test_that("format P value works", {
               expect_equal(format_p_value(0.0005, "p"), "p < 0.001")
               result <- format_p_value(0.005, "p")
               expect_true(grepl('p~"="~', result))
               expect_true(grepl('10\\^', result))
               expect_equal(format_p_value(0.05, "p"), "p~\"=\"~0.05")
               expect_equal(format_p_value(0.1, "p"), "p~\"=\"~0.1")

})
