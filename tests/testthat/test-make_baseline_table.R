if (tolower(Sys.info()[['sysname']]) == "darwin") skip("skip all tests on macOS: known segfaults in dependencies")
library(testthat)
library(ClinKit)
#library(vdiffr)      
#library(jsonlite)
test_that("make baseline table", {
    tmp_fp <- withr::local_tempdir()
    output_file <- file.path(tmp_fp, "table1.docx")
    make_baseline_table(data = mtcars, outcome = "vs",  # V-shaped vs Straight engine
      vars = NULL,     # Use all variables except outcome
      file = output_file,
      label_list = list(
        mpg = "Miles per Gallon",
        cyl = "Number of Cylinders",
        disp = "Displacement (cu.in.)",
        hp = "Horsepower",
        drat = "Rear Axle Ratio",
        wt = "Weight (1000 lbs)",
        qsec = "Quarter Mile Time",
        gear = "Number of Forward Gears",
        carb = "Number of Carburetors"
      ),
      normality_test_method = "shapiro",
      export_normality = TRUE
    )
     expect_true(file.exists(output_file))
    })
