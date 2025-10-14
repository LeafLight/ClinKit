# R/utils_import.R: Unified import of external functions depended on by the package
# Purpose: Declare all required external functions in one place to avoid "undefined global function" warnings in R CMD check
# Note: Use roxygen2's @importFrom tag to import specific functions (avoids namespace pollution)

#' @importFrom dplyr `%>%` mutate select filter all_of any_of
# Import dplyr functions: pipe operator (%>%), data manipulation (mutate/select/filter), and column selection helpers (all_of/any_of)

#' @importFrom stats as.formula binomial coef complete.cases confint confint.default glm ks.test median na.omit relevel sd setNames shapiro.test
# Import stats functions: formula creation (as.formula), GLM families (binomial), model coefficients (coef), missing value handling (complete.cases/na.omit),
# confidence intervals (confint/confint.default), regression models (glm), statistical tests (ks.test/shapiro.test),
# descriptive stats (median/sd), factor reordering (relevel), and name assignment (setNames)

#' @importFrom utils write.csv
# Import utils function: CSV file writing (write.csv) for saving analysis results

#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_ribbon geom_hline geom_violin geom_smooth scale_color_manual scale_fill_manual theme_classic theme element_text labs facet_wrap facet_grid
# Import ggplot2 functions: plot initialization (ggplot), aesthetic mapping (aes), geoms (point/line/ribbon/hline/violin/smooth),
# color scales (scale_color_manual/scale_fill_manual), themes (theme_classic/theme), text styling (element_text),
# labels (labs), and faceting (facet_wrap/facet_grid)

#' @importFrom rlang sym := .data  
# Critical: Import rlang functions for non-standard evaluation (NSE):
# - sym: Convert character strings to symbols (for dynamic variable references)
# - :=: Operator for dynamic column naming (resolves warnings in subgroup_forest)
# - .data: Pronoun to explicitly reference data frame columns (avoids "undefined global variable" warnings)

# Import ggpubr function: Nature Publishing Group (NPG) color scale (resolves warnings in scatter_lm_marginal)

#' @importFrom forestploter forest forest_theme  
# Import forestploter functions: Core forest plot generation (forest) and theme customization (forest_theme) (required for subgroup_forest)

#' @importFrom jstable TableSubgroupMultiGLM  
# Import jstable function: Subgroup analysis with GLM (core dependency for subgroup_forest to generate subgroup-specific results)

NULL  # Empty object to avoid "no visible return value" notes (only for import declaration, no functional code)
