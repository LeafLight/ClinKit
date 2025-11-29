# Quartile-based Logistic Regression Analysis

Analyzes continuous predictors by quartile groups, fitting:

1.  Model1 (unadjusted)

2.  Model2/3/4 (sequentially adding covariates) Calculates OR, 95% CI
    for Q2/Q3/Q4 vs Q1 and trend P-value.

## Usage

``` r
quartile_logistic_analysis(
  data,
  outcomes,
  predictors,
  models_list,
  output_dir = NULL,
  save_format = c("none", "docx", "csv", "all")
)
```

## Arguments

- data:

  Data frame containing all variables

- outcomes:

  Character vector of binary outcome variable names

- predictors:

  Character vector of continuous predictor variable names

- models_list:

  Named list of length 3 with covariates for model2, model3, model4

- output_dir:

  Output directory (optional)

- save_format:

  Save format: "none", "docx", "csv", "all" (default "none")

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

result <- quartile_logistic_analysis(
  data = mtcars,
  outcomes = "vs",
  predictors = c("mpg", "wt"),
  models_list = models_list
)
} # }
```
