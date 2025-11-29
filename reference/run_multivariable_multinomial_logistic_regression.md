# Multivariable Multinomial Logistic Regression Analysis

Fits multinomial logistic regression models for continuous predictors:

1.  Model1 (unadjusted)

2.  Model2/3/4 (sequentially adding covariates) Calculates OR, 95% CI
    and P-value per unit increase.

## Usage

``` r
run_multivariable_multinomial_logistic_regression(
  data,
  outcomes,
  predictors,
  models_list,
  ref_level = NULL,
  output_dir = NULL,
  save_format = c("none", "docx", "csv")
)
```

## Arguments

- data:

  Data frame containing all variables

- outcomes:

  Character vector of outcome variable names

- predictors:

  Character vector of continuous predictor variable names

- models_list:

  Named list of length 3 with covariates for model2, model3, model4

- ref_level:

  Reference level for multinomial outcome (optional)

- output_dir:

  Output directory (optional)

- save_format:

  Save format: "none", "docx", "csv" (default "none")

## Value

List containing results data frame and optional saved file paths
