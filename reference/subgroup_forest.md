# Forest Plot for Subgroup Analysis

Fits generalized linear models for specified exposure variable on
outcome and calculates effect sizes (default OR) with 95% CI, P-values,
and sample proportions within multiple subgroups. Returns formatted data
and forest plot.

## Usage

``` r
subgroup_forest(
  data,
  outcome,
  exposure,
  subgroups,
  covariates = NULL,
  family = "binomial",
  output_dir = NULL,
  save_format = c("none", "data", "plot", "all"),
  decimal_estimate = 2,
  decimal_pvalue = 3,
  line = FALSE,
  prepare_plot = TRUE,
  tm = "blue",
  xlim = NULL,
  ticks_at = NULL,
  plot_title = "Forest Plot of Subgroup Analysis",
  xlab = "Odds Ratio",
  CI_title = "OR(95%CI)",
  math_font = NULL,
  ensure_italic_p = TRUE
)
```

## Arguments

- data:

  Data frame containing outcome, exposure and all subgroup/covariate
  variables

- outcome:

  Character scalar, outcome variable name

- exposure:

  Character scalar, exposure variable name (main effect)

- subgroups:

  Character vector, subgroup variable names

- covariates:

  Character vector, additional covariates, default NULL

- family:

  GLM family, default "binomial"; can be "gaussian" etc.

- output_dir:

  Output directory for saving results, default NULL

- save_format:

  Save format: "none", "data", "plot", "all", default "none"

- decimal_estimate:

  Decimal places for effect estimates, default 2

- decimal_pvalue:

  Decimal places for P-values, default 3

- line:

  Logical, whether to draw separation lines between subgroups, default
  FALSE

- prepare_plot:

  Logical, whether to prepare data for forestploter, default TRUE

- tm:

  Forest plot theme, default "blue", you can alse choose green, cyan or
  passing a custom theme by forestploter::forest_theme

- xlim:

  xlim of the forest plot, default NULL

- ticks_at:

  a numeric vector to specify the x tick of the forestplot

- plot_title:

  Plot title, default "Forest Plot of Subgroup Analysis"

- xlab:

  X-axis label, default "Odds Ratio"

- CI_title:

  title of Confidence Interval, default "OR(95%CI)"

- math_font:

  the font you want to use to display math charactor(those not included
  in normal fonts)

- ensure_italic_p:

  TRUE if you want to use italic p

## Value

List containing forest plot data, plot object, and optional saved file
paths
