# Quartile-based Multinomial Logistic Regression Analysis

Analyzes continuous predictors by quartile groups for multinomial
outcomes, fitting multiple models with sequential covariate adjustment.

## Usage

``` r
quartile_multinomial_analysis(
  data,
  outcomes,
  predictors,
  models_list,
  ref_level = NULL,
  output_dir = NULL,
  save_format = c("none", "docx", "csv", "all")
)
```

## Arguments

- data:

  Data frame containing all variables

- outcomes:

  Character vector of multinomial outcome variable names

- predictors:

  Character vector of continuous predictor variable names

- models_list:

  Named list of covariates for different models

- ref_level:

  Reference level for multinomial outcome (optional)

- output_dir:

  Output directory (optional)

- save_format:

  Save format: "none", "docx", "csv", "all" (default "none")

## Value

List containing results data frame and optional saved file paths

## Examples

``` r
if (FALSE) { # \dontrun{
# Create test data with multinomial outcome
set.seed(123)
test_data <- data.frame(
  outcome = sample(c("A", "B", "C"), 100, replace = TRUE),
  predictor1 = rnorm(100),
  predictor2 = rnorm(100),
  cov1 = rnorm(100),
  cov2 = sample(c("X", "Y"), 100, replace = TRUE)
)

models_list <- list(
  model2 = c("cov1"),
  model3 = c("cov1", "cov2")
)

result <- quartile_multinomial_analysis(
  data = test_data,
  outcomes = "outcome",
  predictors = c("predictor1", "predictor2"),
  models_list = models_list,
  output_dir = tempdir(),
  save_format = "all"
)
} # }
```
