# One-Click Baseline Characteristics Table (gtsummary version)

Automatically generates baseline characteristics table with counts
(percentages) for categorical variables and appropriate statistics for
continuous variables based on normality testing. Outputs a three-line
table in Word format with P-values and Overall column.

## Usage

``` r
make_baseline_table(
  data,
  outcome,
  vars = NULL,
  file,
  pct_digits = 2,
  cont_digits = 2,
  p_digits = 3,
  label_list = NULL,
  normality_test_method = "shapiro",
  normality_alpha = 0.05,
  export_normality = TRUE,
  normality_file = NULL
)
```

## Arguments

- data:

  Data frame containing grouping variable and all variables to summarize

- outcome:

  Character scalar, grouping variable name (typically binary or factor)

- vars:

  Character vector, variables to display; if NULL (default) displays all
  variables except outcome

- file:

  Character, output Word file name (with path); parent directory must
  exist

- pct_digits:

  Decimal places for percentages, default 2

- cont_digits:

  Decimal places for continuous variables, default 2

- p_digits:

  Decimal places for P-values, default 3

- label_list:

  Named list, variable labels, e.g., list(age = "Age (years)"); if NULL,
  uses original column names

- normality_test_method:

  Character, method for normality test ("shapiro" or "ks"), default
  "shapiro"

- normality_alpha:

  Numeric, significance level for normality test, default 0.05

- export_normality:

  Logical, whether to export normality test results to a separate CSV
  file, default TRUE

- normality_file:

  Character, output CSV file name for normality test results; if NULL,
  uses same base name as main file

## Value

gtsummary object (invisible), convenient for subsequent piping
operations

## Details

- Categorical variables displayed as n(%)

- Continuous variables: if all groups are normal, display as mean (SD);
  otherwise display as median (Q1, Q3)

- Missing values not displayed by default; P-values use appropriate
  tests (chi-square/t/ANOVA/Kruskal-Wallis)

- Final table exported via flextable, ready for use in papers or
  appendices

- Normality test results can be exported to CSV for reference

## Examples

``` r
if (FALSE) { # \dontrun{
make_baseline_table(
  data     = mydata,
  outcome  = "Group",
  vars     = c("age", "sex", "smoke", "sbp"),
  file     = "Table1_Baseline.docx",
  label_list = list(age = "Age (years)", sbp = "Systolic BP (mmHg)")
)
} # }
```
