# ClinKit: Streamlined Clinical Data Analysis in R

## Introduction

**ClinKit** is an integrated R package designed to transform cleaned
clinical datasets directly into publication-ready figures and
statistical outputs, while preserving access to raw analysis objects for
complete methodological transparency and customization. It is tailored
for clinical research workflows, providing a one-stop solution for
routine statistical analysis and reporting.

## Installation

You can install the development version of ClinKit from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("LeafLight/ClinKit")
```

## Example Data

For demonstration, we will use a simulated dataset:

``` r
library(ClinKit)
set.seed(123)
data <- data.frame(
  group = rep(c("A", "B"), each = 50),
  age = rnorm(100, 60, 10),
  gender = sample(c("Male", "Female"), 100, replace = TRUE),
  outcome = rbinom(100, 1, 0.4)
)
head(data)
```

## Baseline Table Generation

Generate a baseline characteristics table with automatic test selection:

``` r
result <- make_baseline_table(data, group_var = "group")
print(result$summary)
# Export to Word
gtsummary::as_flextable(result$summary) |> flextable::save_as_docx(path = "baseline_table.docx")
```

## Regression Analysis

### Univariate Logistic Regression

``` r
uni_res <- run_univariate_logistic_regression(data, outcome = "outcome", predictors = c("age", "gender"))
print(uni_res$summary)
```

### Multivariable Logistic Regression

``` r
multi_res <- run_multivariable_logistic_regression(data, outcome = "outcome", predictors = c("age", "gender"), covariates = c("group"))
print(multi_res$summary)
```

## Visualization

### Scatter Plot with Linear Regression

``` r
scatter_lm(data, x = "age", y = "outcome")
```

### Forest Plot

``` r
# Example: subgroup_forest
# (Assume you have a suitable regression result)
# subgroup_forest(model_result, subgroup_var = "gender")
```

## Specialized Analyses

### High/Low Group Analysis

``` r
# highlow_analysis(data, outcome = "outcome", exposure1 = "age", exposure2 = "group")
```

### Quartile-Based Analysis

``` r
# quartile_logistic_analysis(data, outcome = "outcome", predictor = "age")
```

## ROC Curve Analysis

``` r
# roc_analysis(data, outcome = "outcome", predictors = c("age", "group"))
```

## Exporting Results

- Tables can be exported to Word (`.docx`) or CSV.
- Figures can be saved as PNG, PDF, or SVG.

## Advanced Usage

- All functions return raw analysis objects for further customization.
- Supports integration with tidyverse workflows.
- Publication-ready outputs with minimal code.

## Conclusion

ClinKit streamlines the process of clinical data analysis and reporting
in R, making it accessible, reproducible, and publication-ready. For
more details, visit the [ClinKit documentation
website](https://leaflight.github.io/ClinKit/).

## References

- [ClinKit GitHub](https://github.com/LeafLight/ClinKit)
- [gtsummary](https://www.danieldsjoberg.com/gtsummary/)
