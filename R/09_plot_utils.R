#' 分面小提琴图
#'
#' @param data    数据框
#' @param vars    指标向量
#' @param groupby 分组变量名
#'
#' @return        ggplot 对象
facet_violin <- function(data, vars, groupby) {

  # 2. 选列 + 变长
  plot_dat <- data %>%
    dplyr::select(all_of(c(groupby, vars))) %>%
    tidyr::pivot_longer(cols = all_of(vars),
                        names_to = "indicator",
                        values_to = "value")

  # 3. 把分组变量变成因子
  plot_dat[[groupby]] <- factor(plot_dat[[groupby]])

  # 4. 绘图
  ggplot(plot_dat,
         aes_string(x = groupby, y = "value", fill = groupby)) +
    geom_violin(trim = FALSE, alpha = 0.8) +
    geom_boxplot(width = 0.15, colour = "black", alpha = 0.9) +
    facet_wrap(~indicator, nrow = 3, scales = "free") +
    scale_fill_manual(values = c("#FF6B6B", "#4ECDC4")) +
    labs(x = NULL, y = NULL,
         title = paste("各指标按", groupby, "分组的小提琴图")) +
    ggprism::theme_prism(base_size = 14) +
    theme(legend.position = "none",
          strip.text = element_text(face = "bold"))
}

#' 带回归线的散点图（基础版）
#'
#' 快速绘制 X-Y 散点 + 线性回归置信带，并在图内标注 R² 与 P 值；
#' 支持 rug、点形/颜色等额外美学参数透传。
#'
#' @param data     \code{data.frame}，必须包含 x、y 列
#' @param x,y      字符标量，变量名
#' @param digits   数值，R² 与 P 值保留小数位，默认 3
#' @param label.x,label.y  图内标注坐标；NULL 时自动置于左上角 5% 处
#' @param ...      额外参数，透传给 \code{geom_point()} 与 \code{geom_rug()}，
#'                 例如 \code{color = group, size = 2, alpha = 0.6}
#'
#' @return         \code{ggplot} 对象，可继续用 \code{+} 叠加图层或保存
#' @details
#' \itemize{
#'   \item 使用 \code{lm(y ~ x)} 拟合，并绘制 95% 置信带
#'   \item rug 位于底部与左侧，位置 jitter 避免重叠
#'   \item 图内文本固定为 Times 风格，方便后续主题统一
#' }
#'
#' @examples
#' \dontrun{
#' p <- scatter_lm(mtcars, "wt", "mpg",
#'                 color = cyl, size = 3, alpha = 0.7)
#' print(p)
#' ggsave("scatter_lm.png", p, width = 5, height = 4)
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_rug geom_smooth annotate
#' @importFrom stats lm reformulate
#' @export
scatter_lm <- function(data, x, y,
                          digits = 3,
                          label.x = NULL,
                          label.y = NULL,
                          marginal = c("histogram", "density", "boxplot"),
                          bins = 15,
                          margin_fill = "grey60",
                          margin_color = "white", ...) {

  # ---- 0. 字符串转符号 ----
  x_sym <- sym(x)
  y_sym <- sym(y)

  # ---- 1. 拟合 & 指标 ----
  fit  <- lm(reformulate(x, y), data = data)
  summ <- summary(fit)
  r2   <- summ$r.squared
  pval <- summ$coefficients[2, 4]
  lab  <- sprintf("R² = %.*f\np  = %.*g", digits, r2, digits, pval)

  # ---- 2. 默认标签位置 ----
  if (is.null(label.x))
    label.x <- min(data[[x]], na.rm = TRUE) + 0.05 * diff(range(data[[x]], na.rm = TRUE))
  if (is.null(label.y))
    label.y <- max(data[[y]], na.rm = TRUE) - 0.10 * diff(range(data[[y]], na.rm = TRUE))

  # ---- 3. 主图 ----
  p_main <- ggplot(data, aes(!!x_sym, !!y_sym)) +
    geom_point(...)+
    geom_rug(...,position = 'jitter', size = 0.1) +
    geom_smooth(method = "lm", se = TRUE, color = "firebrick") +
    annotate("text", x = label.x, y = label.y,
             label = lab, hjust = 0, vjust = 0, size = 4, color = "black")

  # ---- 4. 加边缘图 ----
 p_main

}

#' 带边缘密度/直方图与回归方程的散点图（增强版）
#'
#' 主图：散点 + 线性回归线 + 自动标注方程、R²、p 值（ggpmisc::stat_poly_eq）。
#' 边缘：双侧 densigram（密度+直方图混合），支持分组着色。
#' 颜色逻辑：无 color 时统一配色；有 color 时按分组变量自动着色并隐藏图例。
#'
#' @param data     数据框，必须包含 x、y 及可选 color 分组变量
#' @param x,y      字符标量，变量名
#' @param digits   方程/统计量小数位，默认 3（已由 stat_poly_eq 控制）
#' @param label.x,label.y  弃用，保留参数位以便兼容旧接口
#' @param color    字符标量，分组变量名；NULL 时无分组
#' @param marginal 边缘图类型，固定为 "densigram"
#' @param bins     直方图分箱数，透传给 ggExtra::ggMarginal
#' @param margin_fill,margin_color  边缘图填充色/边框色
#' @param ...      额外美学参数，透传给 geom_point()
#'
#' @return         ggplot 对象（含边缘图层），可继续 + 主题或保存
#' @details
#' \itemize{
#'   \item 使用 ggpmisc::stat_poly_line() + stat_poly_eq() 自动添加回归线及方程
#'   \item 边缘图为 densigram（密度+直方图），支持分组颜色/填充
#'   \item 无分组时统一 `#479E88` 颜色；有分组时调用 scale_color_npg() 并隐藏图例
#'   \item 返回对象已含边缘图层，可直接 ggsave()
#' }
#'
#' @examples
#' \dontrun{
#' p <- scatter_lm_marginal(mtcars, "wt", "mpg", color = "cyl", bins = 20)
#' print(p)
#' ggsave("scatter_marginal.png", p, width = 6, height = 5)
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_point theme_classic guides theme
#' @importFrom ggpmisc stat_poly_line stat_poly_eq
#' @importFrom ggExtra ggMarginal
#' @importFrom rlang sym
#' @export
scatter_lm_marginal <- function(data, x, y,
                          digits = 3,
                          label.x = NULL,
                          label.y = NULL,
                                color = NULL,
                          marginal = c("histogram", "density", "boxplot"),
                          bins = 15,
                          margin_fill = "grey60",
                          margin_color = "white", ...) {

  # ---- 0. 字符串转符号 ----
  x_sym <- sym(x)
  y_sym <- sym(y)

  # ---- 1. 拟合 & 指标 ----
  fit  <- lm(reformulate(x, y), data = data)
  summ <- summary(fit)
  r2   <- summ$r.squared
  pval <- summ$coefficients[2, 4]
  lab  <- sprintf("R² = %.*f\np  = %.*g", digits, r2, digits, pval)

  # ---- 2. 默认标签位置 ----
  if (is.null(label.x))
    label.x <- min(data[[x]], na.rm = TRUE) + 0.05 * diff(range(data[[x]], na.rm = TRUE))
  if (is.null(label.y))
    label.y <- max(data[[y]], na.rm = TRUE) - 0.10 * diff(range(data[[y]], na.rm = TRUE))

  # ---- 3. 主图 ----
  if(is.null(color)){
    data$`.no_color` <- "black" #
    color <- ".no_color"
    p_main <- ggplot(data, aes(!!x_sym, !!y_sym, color = !!sym(color))) +          # 顶层无 color
  geom_point(, alpha = 0.5) +   # 颜色只给点
  stat_poly_line(formula = y ~ x) +
stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., ..p.value.label.., sep = "*\", \"*")),
    formula = y ~ x,
    parse = TRUE
  ) +
  scale_color_manual(values="#479E88")+
  guides(colour = FALSE, size = FALSE) +
  theme_classic()
  }
  else{
    p_main <- ggplot(data, aes(!!x_sym, !!y_sym, color = !!sym(color))) +          # 顶层无 color
  geom_point(, alpha = 0.5) +   # 颜色只给点
  stat_poly_line(formula = y ~ x) +
stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., ..p.value.label.., sep = "*\", \"*")),
    formula = y ~ x,
    parse = TRUE
  ) +
scale_color_npg() +
  guides(colour = FALSE, size = FALSE) +
  theme_classic()
  }
  # p_main <- ggplot(data, aes(!!x_sym, !!y_sym, color=!!sym(color))) +
  #   geom_point(...)+
  #   #geom_rug(...,position = 'jitter', size = 0.1) +
  #   geom_smooth(method = "lm", se = TRUE, color = "firebrick") +
  #   annotate("text", x = label.x, y = label.y,
  #            label = lab, hjust = 0, vjust = 0, size = 4, color = "black")+
  #   ggprism::theme_prism()

  # ---- 4. 加边缘图 ----
  ggExtra::ggMarginal(p_main, type = 'densigram',
  margins = 'both',
  size = 5, groupColour = TRUE, groupFill = TRUE, alpha = 0.4

  )

}
