# Restricted Cubic Splines (RCS) Regression with Plot (Logistic & Cox)

Fits RCS logistic or Cox proportional hazards regression models and
generates publication-ready plots. Automatically switches to Cox
regression if a `time` variable is provided.

## Usage

``` r
generate_rcs_plot(
  data,
  outcome,
  predictor,
  time = NULL,
  outcomes_map = NULL,
  predictors_map = NULL,
  covariates = NULL,
  knots = 4,
  output_dir = NULL,
  save_format = c("none", "tiff", "svg", "pdf", "all"),
  filename = NULL
)
```

## Arguments

- data:

  Data frame containing all variables

- outcome:

  Outcome variable name (status/event indicator)

- predictor:

  Continuous predictor variable name for RCS

- time:

  Optional. Follow-up time variable name. If provided, fits a Cox model.

- outcomes_map:

  Optional named mapping from outcome variable names to display labels.

- predictors_map:

  Optional named mapping from predictor variable names to display
  labels.

- covariates:

  Covariates for adjusted model (optional)

- knots:

  Number of knots for RCS (default 4)

- output_dir:

  Output directory (optional)

- save_format:

  Save format: "none", "tiff", "svg", "pdf", "all" (default "none")

- filename:

  Custom filename prefix (optional)

## Value

List containing plot object, p-values, and optional saved file paths
