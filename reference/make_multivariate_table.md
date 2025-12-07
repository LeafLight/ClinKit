# Create Multivariate Analysis Table

Create Multivariate Analysis Table

## Usage

``` r
make_multivariate_table(
  result_df,
  out_file = NULL,
  caption = "Multivariate analysis results",
  merge_predictor = TRUE
)
```

## Arguments

- result_df:

  Results data frame from multivariable analysis

- out_file:

  Output Word file path (optional)

- caption:

  Table caption

- merge_predictor:

  Merge identical predictor cells vertically (default TRUE)

## Value

If out_file is NULL, returns flextable object; otherwise writes to file
and returns NULL
