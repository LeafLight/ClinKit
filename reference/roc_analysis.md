# ROC Curve Analysis with AUC Comparison and Optimal Cutoff

Performs comprehensive ROC analysis for single or multiple continuous
indicators and custom combination models, including AUC calculation,
optimal cutoff determination, and DeLong test for AUC comparisons.

## Usage

``` r
roc_analysis(
  data,
  outcome,
  predictors,
  combined_models = NULL,
  output_dir = NULL,
  save_format = c("none", "plot", "data", "all"),
  delong_test = FALSE,
  colors = NULL,
  legend_labels = NULL,
  seed = 123,
  plot_width = 2000,
  plot_height = 2000,
  plot_res = 300,
  direction = "<"
)
```

## Arguments

- data:

  Data frame containing outcome and all predictor variables

- outcome:

  Character scalar, binary outcome variable name (0/1)

- predictors:

  Character vector, continuous predictor variable names

- combined_models:

  Named list of combined models, e.g., list(Combined = c("X1", "X2"))

- output_dir:

  Output directory for saving results, default NULL

- save_format:

  Save format: "none", "plot", "data", "all", default "none"

- delong_test:

  Logical, whether to perform DeLong test for pairwise AUC comparisons,
  default FALSE

- colors:

  Custom color vector for ROC curves; if NULL, colors are automatically
  sampled

- legend_labels:

  Named vector for custom legend labels, e.g., c(PIV = "Systemic
  Inflammation Index")

- seed:

  Random seed for reproducibility, default 123

- plot_width:

  Plot width in pixels, default 2000

- plot_height:

  Plot height in pixels, default 2000

- plot_res:

  Plot resolution in DPI, default 300

- direction:

  roc direction, "auto" for automatic selection, default `<`

## Value

List containing ROC objects, AUC summary, and optional saved file paths
