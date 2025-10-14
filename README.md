
# ClinKit

<!-- badges: start -->
[![Codecov test coverage](https://codecov.io/gh/LeafLight/ClinKit/graph/badge.svg)](https://app.codecov.io/gh/LeafLight/ClinKit)
<!-- badges: end -->

ClinKit is an integrated R package designed to transform cleaned clinical datasets directly into publication-ready figures and statistical outputs, while preserving access to raw analysis objects for complete methodological transparency and customization.

Built specifically for clinical research workflows, ClinKit begins where data cleaning ends, providing:

## Core Analytical Modules:

- make_baseline_table() - Automated baseline characteristic tables with intelligent test selection. Wraps gtsummary::tbl_summary() with built-in normality testing to automatically choose appropriate parametric or non-parametric tests, exporting directly to formatted Word documents.

- Regression Analysis Suite - Comprehensive modeling with progressive adjustment:

- run_univariate_logistic_regression() - Single predictor analysis

- run_multivariable_logistic_regression() - Sequential multivariable models (Model 1-4) with incremental covariate adjustment

- run_multivariable_multinomial_logistic_regression() - Multi-category outcome analysis

- Visualization Tools - Flexible plotting options:

- scatter_lm() - Clean scatter plots with linear regression

- scatter_lm_marginal() - Enhanced versions with marginal distributions

- subgroup_forest() - Publication-ready forest plots with 4 built-in themes and comprehensive p-value reporting

- Specialized Analytical Methods:

- highlow_analysis() - Variable interaction analysis with intuitive high/low grouping

- quartile_logistic_analysis(), quartile_multinomial_analysis() - Streamlined quartile-based analysis (Q1-Q4) for continuous predictors

- roc_analysis() - ROC curve visualization with tidy DeLong test results

## Key Advantages:

- Progressive Modeling: Sequential multivariable regression with incremental covariate adjustment (Models 1-4)

- Reproducible Research: All functions return raw analysis objects alongside formatted outputs

- Methodological Rigor: Automated statistical appropriateness checks

- Publication-Ready: Direct export to journal-compatible formats

Clinical Focus: Specialized methods for common clinical research scenarios

ClinKit bridges the gap between statistical analysis and manuscript preparation, ensuring analytical rigor while dramatically reducing the time from clean data to submission-ready results.
## Installation

You can install the development version of ClinKit from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("LeafLight/ClinKit")
```

## Documents & Tutorial
[ClinKit Documents](https://leaflight.github.io/ClinKit/index.html)
