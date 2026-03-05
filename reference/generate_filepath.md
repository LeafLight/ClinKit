# Generate Unified File Paths

An internal utility to generate standardized file paths across the
ClinKit package. It automatically handles appending timestamps (based on
global package options) and ensures the target output directory exists.

## Usage

``` r
generate_filepath(base_name, ext, output_dir = NULL)
```

## Arguments

- base_name:

  Character scalar. The core name of the file (e.g., "rcs_summary").

- ext:

  Character scalar. The file extension without the dot (e.g., "csv",
  "docx").

- output_dir:

  Character scalar. The path to the output directory. Defaults to NULL.

## Value

A character scalar representing the complete file path.
