# High-High vs Low-Low Four-Group Logistic Regression with Additive Interaction

Analyzes joint effects of two continuous exposures by median
dichotomization, creating four exposure groups and fitting logistic
regression models with sequential covariate adjustment. Calculates
additive interaction measures.

## Usage

``` r
highlow_analysis(
  data,
  outcome,
  exposure_a,
  exposure_b,
  model2 = NULL,
  model3 = NULL,
  model4 = NULL,
  output_dir = NULL,
  save_format = c("none", "docx", "all"),
  recode = FALSE,
  filename_base = "highlow_analysis"
)
```

## Arguments

- data:

  Data frame containing outcome, exposures and all covariates

- outcome:

  Character scalar, binary outcome variable name (0/1)

- exposure_a:

  Character scalar, continuous exposure A variable name

- exposure_b:

  Character scalar, continuous exposure B variable name

- model2:

  Character vector, additional covariates for model 2

- model3:

  Character vector, additional covariates for model 3

- model4:

  Character vector, additional covariates for model 4

- output_dir:

  Output directory for saving results, default NULL

- save_format:

  Save format: "none", "docx", "all", default "none"

- recode:

  Logical, whether to recode interaction variables, default FALSE

- filename_base:

  Base filename for outputs, default "highlow_analysis"

## Value

List containing analysis results, interaction measures, and optional
saved file paths

## Examples

``` r
if (FALSE) { # \dontrun{
result <- highlow_analysis(
  data = mtcars,
  outcome = "vs",
  exposure_a = "mpg",
  exposure_b = "wt",
  model2 = c("cyl", "gear"),
  model3 = c("carb"),
  output_dir = tempdir(),
  save_format = "all"
)
} # }
```
