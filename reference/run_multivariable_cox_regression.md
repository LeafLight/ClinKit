# Dynamic Sequential Multivariable Cox Proportional Hazards Regression

Fits sequential Cox models across N-layers. Automatically handles
survival objects and ensures reference categories are maintained across
adjustment levels.

## Usage

``` r
run_multivariable_cox_regression(
  data,
  time,
  status,
  predictors,
  models_list,
  predictors_map = NULL,
  output_dir = NULL,
  save_format = c("none", "docx", "csv", "txt")
)
```

## Arguments

- data:

  Data frame.

- time:

  Character scalar, name of the follow-up time variable.

- status:

  Character scalar, name of the event status variable (0/1).

- predictors:

  Character vector of primary predictors to be evaluated.

- models_list:

  A NAMED list defining sequential adjustment layers.

- predictors_map:

  Optional named mapping for predictor display labels.

- output_dir:

  Output directory.

- save_format:

  Save format: "none", "docx", "csv", "txt".

## Value

A list containing results dataframe and saved file paths.
