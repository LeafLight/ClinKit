#' Format p-value as plotmath expression string
#'
#' Returns p-values in common journal formats:
#' \itemize{
#'   \item p < 0.001  →  "name < 0.001"
#'   \item 0.001 ≤ p < 0.01  →  scientific notation (with ×10⁻ⁿ)
#'   \item p ≥ 0.01  →  keep 3 decimal places
#' }
#' Outputs as \code{plotmath} expressions that can be directly used in
#' \code{ggplot2::annotate()} or \code{ggpmisc::stat_poly_eq()} label parameters.
#'
#' @param p    Numeric vector, p-values to format (typically 0-1)
#' @param name Character scalar, p-value prefix, e.g., "p", "P interaction", "P trend"
#'
#' @return     Character vector, valid \code{plotmath} expressions, e.g.,
#'             \code{"p < 0.001"}, \code{"p~\"=\"~3.14\\times10^{-4}"},
#'             \code{"p~\"=\"~0.03"}
#'
#' @examples
#' format_p_value(0.0001, "p")
#' @export
format_p_value <- function(p, name) {
  if (p < 0.001) {
    return(paste0(name, ' < 0.001'))
  } else if (p < 0.01) {
    p = formatC(p, format = "e", digits = 3)
    p = strsplit(p, "e")[[1]]
    return(paste0(name, '~"="~', p[1], '~"\u00d7"~10^', p[3]))
  } else {
    return(paste0(name, '~"="~', round(p, 3)))
  }
}