# Univariate Logistic Regression Analysis

Univariate Logistic Regression Analysis

## Usage

``` r
run_univariate_logistic_regression(
  data,
  outcomes,
  predictors,
  outcomes_map = NULL,
  predictors_map = NULL,
  output_dir = NULL,
  save_format = c("csv", "txt", "none")
)
```

## Arguments

- data:

  Data frame.

- outcomes:

  Outcome variables (character vector).

- predictors:

  Predictor variables (character vector).

- outcomes_map:

  Optional named mapping from outcome variable names to display labels,
  e.g. c("A_B" = "A B").

- predictors_map:

  Optional named mapping from predictor variable names to display
  labels.

- output_dir:

  Output directory (optional; files are not saved if NULL).

- save_format:

  Save format: "csv", "txt", or "none" (default "csv").

## Value

A list with components `results`, `saved_files` and `call`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage
result <- run_univariate_logistic_regression(
  data = mtcars,
  outcomes = "vs",
  predictors = c("mpg", "drat")
)

# Save as CSV files
result <- run_univariate_logistic_regression(
  data = mtcars,
  outcomes = "vs",
  predictors = c("mpg", "drat"),
  output_dir = tempdir(),
  save_format = "csv"
)
} # }
```
