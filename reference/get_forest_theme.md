# Get Predefined Forest Plot Themes

Provides several predefined color themes for forest plots with Times New
Roman font.

## Usage

``` r
get_forest_theme(
  theme_name = c("blue", "green", "cyan", "default"),
  base_size = 12,
  background_levels = NULL
)

tm_blue(base_size = 12, background_levels = NULL)

tm_green(base_size = 12, background_levels = NULL)

tm_cyan(base_size = 12, background_levels = NULL)

tm_default(base_size = 12, background_levels = NULL)
```

## Arguments

- theme_name:

  Theme name: "blue", "green", "cyan", or "default"

- base_size:

  Base font size, default 12

- background_levels:

  Background color levels vector for subgroup hierarchy

## Value

Forest plot theme object

## Examples

``` r
if (FALSE) { # \dontrun{
tm <- get_forest_theme("blue")
result <- subgroup_forest(..., tm = tm)
} # }
```
