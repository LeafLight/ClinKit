# Scatter Plot with Regression Line (Basic Version)

Quickly draws X-Y scatter plot with linear regression confidence band,
and annotates R^2 and P value within the plot; Supports rug plots and
additional aesthetic parameters for points

## Usage

``` r
scatter_lm(data, x, y, digits = 3, label.x = NULL, label.y = NULL, ...)
```

## Arguments

- data:

  `data.frame`, must contain x and y columns

- x, y:

  Character scalars, variable names

- digits:

  Numeric, decimal places for R^2 and P value, default 3

- label.x, label.y:

  Coordinates for annotation; when NULL, automatically placed at
  top-left 5% position

- ...:

  Additional parameters passed to `geom_point()` and `geom_rug()`, e.g.,
  `color = group, size = 2, alpha = 0.6`

## Value

        \code{ggplot} object, can be further customized with \code{+} or saved

## Details

- Uses `lm(y ~ x)` for fitting and draws 95% confidence band

- Rug plots at bottom and left with jitter to avoid overlap

- Annotation text uses Times font for consistent theming

## Examples

``` r
if (FALSE) { # \dontrun{
p <- scatter_lm(mtcars, "wt", "mpg",
                color = cyl, size = 3, alpha = 0.7)
print(p)
ggsave("scatter_lm.png", p, width = 5, height = 4)
} # }
```
