# Dynamic Sequential Multivariable Multinomial Logistic Regression

Fits sequential multinomial logistic regression models using
[`nnet::multinom`](https://rdrr.io/pkg/nnet/man/multinom.html).
Implements an "Anti-Cartesian" row-indexing strategy to ensure perfect
alignment of multi-level categorical outcomes across adjustment layers.

## Usage

``` r
run_multivariable_multinomial_logistic_regression(
  data,
  outcomes,
  predictors,
  models_list,
  ref_level = NULL,
  output_dir = NULL,
  save_format = c("none", "docx", "csv", "txt")
)
```

## Arguments

- data:

  A data frame containing the variables.

- outcomes:

  Character vector of multinomial outcome variables.

- predictors:

  Character vector of primary predictors.

- models_list:

  A NAMED list defining sequential adjustment layers.

- ref_level:

  Optional string to set the reference level of the outcome.

- output_dir:

  Optional path to save output files.

- save_format:

  Save format: "none", "docx", "csv", or "txt".

## Value

A list containing results data frame and saved file paths.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example with 3-level categorical outcome
library(nnet)
df_test <- survival::colon %>%
  dplyr::mutate(extent = factor(extent, labels = c("Sub", "Mus", "Ser", "Con")))

run_multivariable_multinomial_logistic_regression(
  data = df_test,
  outcomes = "extent",
  predictors = "sex",
  models_list = list("Basic" = c("age"), "Clinical" = c("obstruct")),
  save_format = "docx"
)
} # }
```
