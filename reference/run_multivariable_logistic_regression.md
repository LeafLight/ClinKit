# Dynamic Sequential Multivariable Logistic Regression

Fits cumulative multivariable logistic regression models across N-layers
defined by `models_list`. The function ensures reference categories
(1.00 (Ref)) are consistently maintained and horizontally aligned across
all adjustment levels.

## Usage

``` r
run_multivariable_logistic_regression(
  data,
  outcomes,
  predictors,
  models_list,
  outcomes_map = NULL,
  predictors_map = NULL,
  output_dir = NULL,
  save_format = c("none", "docx", "csv", "txt")
)
```

## Arguments

- data:

  A data frame containing the variables.

- outcomes:

  Character vector of outcome variable names (Binary).

- predictors:

  Character vector of primary predictors to be evaluated.

- models_list:

  A NAMED list where each element contains new covariates to be added
  sequentially (e.g., list(Base = c("age"), Clinical = c("nodes"))).

- outcomes_map:

  Optional named mapping for outcome display labels.

- predictors_map:

  Optional named mapping for predictor display labels.

- output_dir:

  Optional path to save output files.

- save_format:

  Save format: "none", "docx", "csv", or "txt".

## Value

A list containing a consolidated results data frame and saved file
paths.

## Examples

``` r
if (FALSE) { # \dontrun{
library(survival)
data(colon)
df <- colon %>% dplyr::filter(etype == 2) %>% dplyr::mutate(status = as.factor(status))

models <- list(
  "Demographics" = c("age", "sex"),
  "Pathology"    = c("nodes", "extent")
)

run_multivariable_logistic_regression(
  data = df,
  outcomes = "status",
  predictors = "adhere",
  models_list = models,
  save_format = "docx"
)
} # }
```
