# Batch RCS Analysis

Runs multiple RCS analyses for different predictors and outcomes

## Usage

``` r
run_analysis_rcs(
  data,
  predictors,
  outcomes,
  outcomes_map = NULL,
  covariates = NULL,
  output_dir = NULL,
  save_format = c("none", "tiff", "svg", "pdf", "all")
)
```

## Arguments

- data:

  Data frame containing all variables

- predictors:

  Character vector of predictor variable names

- outcomes:

  Character vector of outcome variable names

- outcomes_map:

  Outcome variable mapping (optional)

- covariates:

  Covariates for adjusted models (optional)

- output_dir:

  Output directory (optional)

- save_format:

  Save format: "none", "tiff", "svg", "pdf", "all" (default "none")

## Value

Data frame with predictor, outcome, model_type, p-values, and optional
saved file paths

## Examples

``` r
if (FALSE) { # \dontrun{
results <- run_analysis_rcs(
  data = mtcars,
  predictors = c("mpg", "wt"),
  outcomes = c("vs", "am"),
  covariates = c("cyl", "gear"),
  output_dir = tempdir(),
  save_format = "none"
)
} # }
```
