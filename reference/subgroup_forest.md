# Forest Plot for Subgroup Analysis (Dynamic Logistic/Cox Routing)

Fits generalized linear models or Cox models for specified exposure
variable and calculates effect sizes (OR or HR) with 95% CI.

## Usage

``` r
subgroup_forest(
  data,
  outcome,
  exposure,
  subgroups,
  time = NULL,
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
  xlab = NULL,
  CI_title = NULL
)
```

## Arguments

- data:

  Data frame containing outcome, exposure and all subgroup/covariate
  variables

- outcome:

  Character scalar, outcome/status variable name

- exposure:

  Character scalar, exposure variable name (main effect)

- subgroups:

  Character vector, subgroup variable names

- time:

  Optional. Character scalar, follow-up time variable. If provided, fits
  Cox models (HR).

- covariates:

  Character vector, additional covariates, default NULL

- family:

  GLM family, default "binomial". Ignored if time is provided.

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

  Forest plot theme, default "blue"

- xlim:

  xlim of the forest plot, default NULL

- ticks_at:

  a numeric vector to specify the x tick of the forestplot

- plot_title:

  Plot title, default "Forest Plot of Subgroup Analysis"

- xlab:

  X-axis label, default NULL (auto-detected)

- CI_title:

  title of Confidence Interval, default NULL (auto-detected)

## Value

List containing forest plot data, plot object, and optional saved file
paths
