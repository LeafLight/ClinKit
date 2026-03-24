# Batch RCS Analysis

Runs multiple RCS analyses for different predictors and outcomes.
Supports both binary outcomes (Logistic) and time-to-event outcomes
(Cox).

## Usage

``` r
run_analysis_rcs(
  data,
  predictors,
  outcomes,
  time = NULL,
  outcomes_map = NULL,
  predictors_map = NULL,
  covariates = NULL,
  knots = 4,
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

- time:

  Optional. Character scalar of the follow-up time variable.

- outcomes_map:

  Optional named mapping from outcome variable names to display labels.

- predictors_map:

  Optional named mapping from predictor variable names to display
  labels.

- covariates:

  Covariates for adjusted models (optional)

- knots:

  Number of knots for RCS (default 4)

- output_dir:

  Output directory (optional)

- save_format:

  Save format: "none", "tiff", "svg", "pdf", "all" (default "none")

## Value

Data frame with predictor, outcome, model_type, p-values, and optional
saved file paths
