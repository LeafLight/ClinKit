# Map variable name to display label

Safely map a variable name using an optional named mapping. If the
mapping is NULL or the name is not present in the mapping, the original
name is returned.

## Usage

``` r
map_label(name, mapping = NULL)
```

## Arguments

- name:

  Character scalar, the variable name.

- mapping:

  Optional named vector/list, e.g. c("A_B" = "A B").

## Value

A single character string (label).
