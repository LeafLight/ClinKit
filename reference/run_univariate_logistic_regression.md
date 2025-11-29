# Univariate Logistic Regression Analysis

Univariate Logistic Regression Analysis

## Usage

``` r
run_univariate_logistic_regression(
  data,
  outcomes,
  predictors,
  outcomes_map = NULL,
  output_dir = NULL,
  save_format = c("csv", "txt", "none")
)
```

## Arguments

- data:

  Data frame

- outcomes:

  Outcome variables (character vector)

- predictors:

  Predictor variables (character vector)

- outcomes_map:

  Outcome variable mapping (optional)

- output_dir:

  Output directory (optional, default no file saving)

- save_format:

  Save format: "none", "txt", "csv" (default "csv")

## Value

List containing results data frame and optional saved file paths

## Examples
