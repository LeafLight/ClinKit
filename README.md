# ClinKit

<!-- badges: start -->
[![Codecov test coverage](https://codecov.io/gh/LeafLight/ClinKit/graph/badge.svg)](https://app.codecov.io/gh/LeafLight/ClinKit)
[![R-CMD-check](https://github.com/LeafLight/ClinKit/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/LeafLight/ClinKit/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

ClinKit is an integrated R package designed to transform cleaned clinical datasets directly into publication-ready figures and statistical outputs, while preserving access to raw analysis objects for complete methodological transparency and customization.

## Installation

You can install the development version of ClinKit from GitHub:

```r
# install.packages("devtools")
devtools::install_github("LeafLight/ClinKit")
```

## Example Usage

Below is a minimal example using built-in or example data:

```r
library(ClinKit)
# Example: Generate a baseline table
data <- data.frame(
  group = rep(c("A", "B"), each = 50),
  age = rnorm(100, 60, 10),
  gender = sample(c("Male", "Female"), 100, replace = TRUE)
)
result <- make_baseline_table(data, group_var = "group")
print(result$summary)
# Export to Word
gtsummary::as_flextable(result$summary) |> flextable::save_as_docx(path = "baseline_table.docx")
```

## Core Analytical Modules

- **make_baseline_table()**: Automated baseline characteristic tables with intelligent test selection. Wraps gtsummary::tbl_summary() with built-in normality testing to automatically choose appropriate parametric or non-parametric tests, exporting directly to formatted Word documents.
- **Regression Analysis Suite**: Comprehensive modeling with progressive adjustment:
  - run_univariate_logistic_regression(): Single predictor analysis
  - run_multivariable_logistic_regression(): Sequential multivariable models (Model 1-4) with incremental covariate adjustment
  - run_multivariable_multinomial_logistic_regression(): Multi-category outcome analysis
- **Visualization Tools**:
  - scatter_lm(): Clean scatter plots with linear regression
  - scatter_lm_marginal(): Enhanced versions with marginal distributions
  - subgroup_forest(): Publication-ready forest plots with 4 built-in themes and comprehensive p-value reporting
- **Specialized Analytical Methods**:
  - highlow_analysis(): Variable interaction analysis with intuitive high/low grouping
  - quartile_logistic_analysis(), quartile_multinomial_analysis(): Streamlined quartile-based analysis (Q1-Q4) for continuous predictors
  - roc_analysis(): ROC curve visualization with tidy DeLong test results

## Key Advantages

- Progressive Modeling: Sequential multivariable regression with incremental covariate adjustment (Models 1-4)
- Reproducible Research: All functions return raw analysis objects alongside formatted outputs
- Methodological Rigor: Automated statistical appropriateness checks
- Publication-Ready: Direct export to journal-compatible formats
- Clinical Focus: Specialized methods for common clinical research scenarios

## Dependencies

ClinKit depends on several R packages, including `gtsummary`, `flextable`, `ggplot2`, and others. All dependencies will be installed automatically.

## Example Data

You can use your own cleaned clinical dataset, or refer to the example in the usage section above.

## Contributing

Contributions, bug reports, and feature requests are welcome! Please open an issue or submit a pull request on GitHub.

## Contact

For questions or support, please contact [LeafLight](mailto:leaflight@domain.com) or open an issue on the GitHub repository.

## License

This project is licensed under the MIT License. See the LICENSE file for details.

## Documents & Tutorial
[ClinKit Documents](https://leaflight.github.io/ClinKit/index.html)
