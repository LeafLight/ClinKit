# Reclassification Analysis (NRI & IDI)

Evaluates the incremental predictive value of adding new markers to a
base model by calculating Categorical NRI, Continuous (Category-free)
NRI, and IDI. Outputs a three-line table in Word format and saves raw
results to CSV.

## Usage

``` r
run_reclassification_analysis(
  data,
  outcome,
  base_vars,
  new_vars,
  cutoffs = c(0, 0.2, 0.4, 1),
  output_dir = NULL,
  save_format = c("none", "docx", "csv", "all"),
  filename_base = "reclassification"
)
```

## Arguments

- data:

  Data frame containing outcome and all predictor variables

- outcome:

  Character scalar, binary outcome variable name (0/1)

- base_vars:

  Character vector, variables in the base model

- new_vars:

  Character vector, new variables added to the updated model

- cutoffs:

  Numeric vector, cutoff points for risk categories, default c(0, 0.2,
  0.4, 1)

- output_dir:

  Output directory for saving results, default NULL

- save_format:

  Save format: "none", "docx", "csv", "all", default "none"

- filename_base:

  Base filename for outputs, default "reclassification"

## Value

List containing results data frame, reclassification tables, and
optional saved file paths
