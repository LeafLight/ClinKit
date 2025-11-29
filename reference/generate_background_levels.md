# Generate Background Levels for Forest Plot (Internal)

Creates background level vector based on data structure:

- First row: level 1 (Title)

- Rows with NA OR: level 3 (Subgroup Title)

- Other rows: alternating between 1 and 2 (row of data)

## Usage

``` r
generate_background_levels(forest_data)
```

## Arguments

- forest_data:

  Data frame from subgroup analysis

## Value

Numeric vector of background levels
