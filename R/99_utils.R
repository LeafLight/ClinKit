#' 将 p 值格式化为可绘图的表达式字符串
#'
#' 按期刊常见格式返回：
#' \itemize{
#'   \item p < 0.001  →  "name < 0.001"
#'   \item 0.001 ≤ p < 0.01  →  scientific notation（含 ×10⁻ⁿ）
#'   \item p ≥ 0.01  →  保留 3 位小数
#' }
#' 输出为 \code{plotmath} 表达式，可直接用于 \code{ggplot2::annotate()} 或
#' \code{ggpmisc::stat_poly_eq()} 的 \code{label} 参数。
#'
#' @param p    数值向量，待格式化的 p 值（通常 0–1）
#' @param name 字符标量，p 值前缀，如 "p"、"P interaction"、"P trend"
#'
#' @return     字符向量，\code{plotmath} 合法表达式，例如
#'             \code{"p < 0.001"}、\code{"p~\"=\"~3.14\\times10^{-4}"}、
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
