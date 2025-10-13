#' 多亚组 GLM 森林图数据生成器，绘图结合forForest与forestPDF
#'
#' 以指定暴露变量对结局进行广义线性模型拟合，并在多个亚组内分别计算效应量
#' （默认 OR）及 95%CI、P 值、样本占比，最终返回可直接用于 \code{forestploter} 绘图的长数据框。
#'
#' @param data            \code{data.frame}，必须包含结局、暴露及所有亚组/协变量
#' @param outcome         字符标量，结局变量名
#' @param exposure        字符标量，暴露变量名（模型主效应）
#' @param var_subgroups   字符向量，亚组变量名（函数在每个水平内单独拟合）
#' @param var_cov         字符向量，额外协变量，默认 \code{NULL}
#' @param family          广义线性模型族，默认 \code{"binomial"}；可改 \code{"gaussian"} 等
#' @param decimal.estimate 效应量小数位，默认 8
#' @param decimal.percent  百分比小数位，默认 1
#' @param decimal.pvalue   P 值小数位，默认 3
#' @param line             逻辑值，是否在亚组间画分隔线，默认 \code{FALSE}
#'
#' @return                 \code{data.frame}，含亚组、效应量、95%CI、P 值、样本量等列，
#'                         可直接喂给 \code{forestploter::forest()} 绘图（见示例）
#'
#' @details
#' \itemize{
#'   \item 依赖 \code{TableSubgroupMultiGLM} 完成亚组拟合；返回对象再经缺失值填充、数值化整理
#'   \item 返回框已预留对齐空列，并保证百分比为数值型，方便后续绘图
#'   \item 森林图绘制部分已注释，用户拿到结果后自行调用 \code{forestploter} 即可
#' }
#'
#' @examples
#' \dontrun{
#' forest_df <- forest_multiGLM(
#'   data           = mydata,
#'   outcome        = "death",
#'   exposure       = "SIRI",
#'   var_subgroups  = c("性别", "年龄组", "高血压"),
#'   var_cov        = c("age", "smoke"),
#'   family         = "binomial",
#'   decimal.estimate = 3,
#'   decimal.pvalue   = 3
#' )
#'
#' # 绘图（需安装 forestploter）
#' library(forestploter)
#' forest(forest_df,
#'        est = 4, lower = 5, upper = 6,
#'        ci_column = 2, zero = 1,
#'        xlab = "Odds Ratio")
#' }
#'
#' @importFrom stats as.formula
#' @export
forest_multiGLM <- function(data,
                            outcome,
                            exposure,
                            var_subgroups,
                            var_cov = NULL,
                            family = "binomial",
                            decimal.estimate = 8,
                            decimal.percent  = 1,
                            decimal.pvalue   = 3,
                            line = FALSE) {

  # # ---------- 1. 检查必要包 ----------
  # if (!requireNamespace("TableSubgroupMultiGLM", quietly = TRUE))
  #   stop("请先安装 TableSubgroupMultiGLM")
  # if (!requireNamespace("forestploter", quietly = TRUE))
  #   stop("请先安装 forestploter")
  # if (!requireNamespace("grid", quietly = TRUE))
  #   stop("请先安装 grid")

  # ---------- 2. 构造公式 ----------
  formula <- as.formula(paste(outcome, "~", exposure))

  # ---------- 3. 运行多亚组 GLM ----------
  res <- TableSubgroupMultiGLM(
    formula        = formula,
    var_subgroups  = var_subgroups,
    var_cov        = var_cov,
    data           = data,
    family         = family,
    decimal.estimate = decimal.estimate,
    decimal.percent  = decimal.percent,
    decimal.pvalue   = decimal.pvalue,
    line             = line
  )

  # ---------- 4. 整理森林图数据 ----------
  plot_res <- res

  # 把缺失值替换为空格
  cols_to_blank <- c(2, 3, 7, 8)
  plot_res[cols_to_blank][is.na(plot_res[cols_to_blank])] <- " "

  # 生成空列用于对齐
  plot_res$` ` <- paste(rep(" ", nrow(plot_res)), collapse = " ")

  # 把百分比列转成数值
  plot_res[, 4:6] <- apply(plot_res[, 4:6], 2, as.numeric)

  # # ---------- 5. 绘制森林图 ----------
  # fp <- forestploter::forest(
  #   plot_res,
  #   est          = 4,  # 效应量列
  #   lower        = 5,  # 置信区间下限列
  #   upper        = 6,  # 置信区间上限列
  #   pval         = 7,  # P 值列
  #   sizes        = 3,  # 样本量列
  #   ci_column    = 2,  # 置信区间列
  #   ref_group    = 1,  # 参照组列
  #   xlab         = "Odds Ratio",
  #   title        = paste("Forest plot: ", outcome, " vs ", exposure),
  #   zero         = 1,
  #   line_height  = unit(8, "mm"),
  #   col          = forestploter::fpColors(lines = "black", zero = "red")
  # )

  # ---------- 6. 返回结果 ----------
  # invisible(list(model = res, plot = fp))
  return(plot_res)
}

#' 森林图专用数据后处理
#'
#' 将亚组分析结果数据框快速调整为适合 \code{forestploter} 绘图的格式：
#' - 给非总计行添加 8 空格缩进
#' - 生成用于对齐的空白列
#' - 合并 OR、Lower、Upper 为可读的 "OR(95%CI)" 字符串
#' - 保证空行/NA 仍留空格，防止绘图错位
#'
#' @param subgroup_df  \code{data.frame}，必须包含 Subgroup、OR、Lower、Upper 列
#'                    （通常来自 \code{TableSubgroupMultiGLM} 或自定义汇总）
#'
#' @return             \code{data.frame}，新增 \code{OR(95%CI)} 与空白对齐列，
#'                     可直接喂给 \code{forestploter::forest()}
#'
#' @examples
#' \dontrun{
#' forest_df <- forForest(sub_result)
#' forestploter::forest(forest_df,
#'                      est = "OR", lower = "Lower", upper = "Upper",
#'                      ci_column = "OR(95%CI)", zero = 1)
#' }
#'
#' @export
forForest <- function(
subgroup_df
){
  subgroup_df$Subgroup <- ifelse(is.na(subgroup_df$OR),
                      subgroup_df$Subgroup,
                      paste0("        ", subgroup_df$Subgroup))
  subgroup_df$` ` <-  "                                                                  "
subgroup_df <- subgroup_df %>%
  mutate(
    OR     = as.numeric(OR),
    Lower  = as.numeric(Lower),
    Upper  = as.numeric(Upper),
    `OR(95%CI)` = ifelse(
      OR != " " & Lower != " " & Upper != " ",          # 非空
      sprintf("    %.2f (%.2f–%.2f)    ", OR, Lower, Upper),
      " "                                                # 空就留空串
    )
  )
subgroup_df[is.na(subgroup_df$`OR(95%CI)`),"OR(95%CI)"] <- " "
  subgroup_df
}
