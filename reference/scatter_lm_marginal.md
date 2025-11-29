# Scatter Plot with Marginal Density/Histogram and Regression Equation (Enhanced Version)

Main plot: scatter points + linear regression line + automatic
annotation of equation, R^2, p value (ggpmisc::stat_poly_eq). Marginal
plots: bilateral densigram (density + histogram mix), supports grouping
colors. Color logic: uniform color when no color grouping; automatic
coloring by group variable with hidden legend when color specified.

## Usage

``` r
scatter_lm_marginal(
  data,
  x,
  y,
  digits = 3,
  label.x = NULL,
  label.y = NULL,
  color = NULL,
  marginal = c("histogram", "density", "boxplot"),
  bins = 15,
  margin_fill = "grey60",
  margin_color = "white",
  ...
)
```

## Arguments

- data:

  Data frame, must contain x, y and optional color grouping variable

- x, y:

  Character scalars, variable names

- digits:

  Decimal places for equation/statistics, default 3 (controlled by
  stat_poly_eq)

- label.x, label.y:

  Deprecated, kept for backward compatibility

- color:

  Character scalar, grouping variable name; NULL for no grouping

- marginal:

  Marginal plot type, fixed as "densigram"

- bins:

  Number of histogram bins, passed to ggExtra::ggMarginal

- margin_fill, margin_color:

  Fill color and border color for marginal plots

- ...:

  Additional aesthetic parameters passed to geom_point()

## Value

        ggplot object (with marginal layers), can be further customized with + or saved

## Details

- Uses ggpmisc::stat_poly_line() + stat_poly_eq() to automatically add
  regression line and equation

- Marginal plots are densigram (density + histogram), supports group
  colors/fill

- Uniform color `#479E88` when no grouping; uses scale_color_npg() with
  hidden legend when grouping

- Returned object already includes marginal layers, can be directly
  saved with ggsave()

## Examples

``` r
if (FALSE) { # \dontrun{
p <- scatter_lm_marginal(mtcars, "wt", "mpg", color = "cyl", bins = 20)
print(p)
ggsave("scatter_marginal.png", p, width = 6, height = 5)
} # }
```
