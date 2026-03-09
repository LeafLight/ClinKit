# Format p-value as plotmath expression string

Returns p-values in common journal formats:

- p \< 0.001 → "name \< 0.001"

- 0.001 ≤ p \< 0.01 → scientific notation (with ×10⁻ⁿ)

- p ≥ 0.01 → keep 3 decimal places

Outputs as `plotmath` expressions that can be directly used in
[`ggplot2::annotate()`](https://ggplot2.tidyverse.org/reference/annotate.html)
or
[`ggpmisc::stat_poly_eq()`](https://docs.r4photobiology.info/ggpmisc/reference/stat_poly_eq.html)
label parameters.

## Usage

``` r
format_p_value(p, name)
```

## Arguments

- p:

  Numeric vector, p-values to format (typically 0-1)

- name:

  Character scalar, p-value prefix, e.g., "p", "P interaction", "P
  trend"

## Value

    Character vector, valid \code{plotmath} expressions, e.g.,
            \code{"p < 0.001"}, \code{"p~\"=\"~3.14\times10^{-4}"},
            \code{"p~\"=\"~0.03"}

## Examples

``` r
format_p_value(0.0001, "p")
#> [1] "p < 0.001"
```
