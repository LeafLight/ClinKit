# Multivariable Logistic Regression (4-layer models)

Multivariable Logistic Regression (4-layer models)

## Usage

``` r
run_multivariable_logistic_regression(
  data,
  outcomes,
  predictors,
  models_list,
  outcomes_map = NULL,
  output_dir = NULL,
  save_format = c("none", "txt", "csv")
)
```

## Arguments

- data:

  Data frame

- outcomes:

  Outcome variables (character vector)

- predictors:

  Predictor variables (character vector)

- models_list:

  Named list of length 3 with covariates for model2, model3, model4

- outcomes_map:

  Outcome variable mapping (optional)

- output_dir:

  Output directory (optional, default no file saving)

- save_format:

  Save format: "none", "txt", "csv" (default "none")

## Value

List containing results data frame and optional saved file paths

## Examples

``` r
if (FALSE) { # \dontrun{
models_list <- list(
  model2 = c("cyl", "gear"),
  model3 = c("carb"),
  model4 = c("hp")
)

result <- run_multivariable_logistic_regression(
  data = mtcars,
  outcomes = "vs",
  predictors = c("mpg", "wt"),
  models_list = models_list
)
} # }
```
