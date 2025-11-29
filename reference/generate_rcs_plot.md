# Restricted Cubic Splines (RCS) Logistic Regression with Plot

Fits RCS logistic regression models and generates publication-ready
plots

## Usage

``` r
generate_rcs_plot(
  data,
  outcome,
  predictor,
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

  Outcome variable name

- predictor:

  Continuous predictor variable name for RCS

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
