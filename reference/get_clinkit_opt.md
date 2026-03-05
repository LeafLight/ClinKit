# Get Global Options for ClinKit

Retrieves package-specific global options. If the user has not set a
specific option globally (using
[`options()`](https://rdrr.io/r/base/options.html)), the function
returns a provided default value.

## Usage

``` r
get_clinkit_opt(opt_name, default)
```

## Arguments

- opt_name:

  Character scalar. The name of the option to retrieve (without the
  "ClinKit." prefix).

- default:

  The default value to return if the option is not found.

## Value

The value of the specified global option, or the default value if it is
not set.
