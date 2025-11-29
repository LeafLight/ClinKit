# Faceted Violin Plot

Draws faceted violin plots for multiple indicators, grouped by a
grouping variable, with each indicator in a separate facet

## Usage

``` r
facet_violin(data, vars, groupby)
```

## Arguments

- data:

  Data frame

- vars:

  Character vector, names of indicator variables to plot

- groupby:

  Character scalar, name of grouping variable

## Value

ggplot object

## Details

This function will:

1.  Select specified columns and convert to long format

2.  Convert grouping variable to factor

3.  Draw violin plots with boxplots inside, faceted by indicator

## Examples

``` r
if (FALSE) { # \dontrun{
p <- facet_violin(iris, c("Sepal.Length", "Sepal.Width"), "Species")
print(p)
} # }
```
