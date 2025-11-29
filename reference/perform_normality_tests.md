# Perform normality tests for continuous variables by group

Perform normality tests for continuous variables by group

## Usage

``` r
perform_normality_tests(
  data,
  outcome,
  continuous_vars,
  method = "shapiro",
  alpha = 0.05
)
```

## Arguments

- data:

  Data frame containing the data

- outcome:

  Character, grouping variable name

- continuous_vars:

  Character vector of continuous variable names

- method:

  Character, normality test method ("shapiro" or "ks")

- alpha:

  Numeric, significance level for normality test

## Value

Data frame with normality test results
