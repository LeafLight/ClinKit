#' 一键基线特征表（gtsummary 版）
#'
#' 自动计算各组例数（百分比）或中位数（四分位距），
#' 并输出带 P 值、Overall 列的三线表 Word 文档。
#'
#' @param data        \code{data.frame}，必须包含分组变量及所有待汇总变量
#' @param outcome     字符标量，分组变量名（通常为一二分类或分组因子）
#' @param vars        字符向量，需展示的变量；若为 NULL（默认）则展示除 outcome 外的全部变量
#' @param file        字符，输出 Word 文件名（含路径）；父目录须已存在
#' @param pct_digits  百分比小数位，默认 2
#' @param cont_digits 连续变量小数位，默认 2
#' @param p_digits    P 值小数位，默认 3
#' @param label_list  命名列表，变量标签，如 \code{list(age = "Age (years)")}；NULL 时使用原列名
#'
#' @return            \code{gtsummary} 对象（invisible），方便后续继续管道操作
#' @details
#' \itemize{
#'   \item 分类变量展示为 {n} ({p}%)，连续变量展示为 median (p25, p75)
#'   \item 缺失值默认不显示；P 值使用相应检验（卡方/t/ANOVA/Kruskal-Wallis）
#'   \item 最终表格经 \code{flextable} 输出，可直接用于论文或附录
#' }
#'
#' @examples
#' \dontrun{
#' make_baseline_table(
#'   data     = mydata,
#'   outcome  = "Group",
#'   vars     = c("age", "sex", "smoke", "sbp"),
#'   file     = "Table1_Baseline.docx",
#'   label_list = list(age = "Age (years)", sbp = "Systolic BP (mmHg)")
#' )
#' }
#'
#' @importFrom dplyr all_of
#' @importFrom gtsummary tbl_summary add_p modify_fmt_fun add_overall bold_labels as_flex_table
#' @importFrom officer read_docx
#' @importFrom flextable body_add_flextable
#' @export
make_baseline_table <- function(
  data,
  outcome,
  vars = NULL,
  file,
  pct_digits = 2,
  cont_digits = 2,
  p_digits = 3,
  label_list = NULL
) {
  # 1. 变量选择
  if (is.null(vars)) {
    vars <- setdiff(names(data), outcome)
  }
  data <- data %>% dplyr::select(all_of(outcome), all_of(vars))

  # 2. 生成 gtsummary 表
  tbl <- data %>%
    tbl_summary(
      by = all_of(outcome),
      label = label_list,
      statistic = list(
        all_categorical() ~ "{n} ({p}%)",
        all_continuous()  ~ "{median} ({p25}, {p75})"
      ),
      missing = "no",
      digits = list(
        all_continuous()  ~ cont_digits,
        all_categorical() ~ c(0, pct_digits)
      )
    ) %>%
    add_p(
      #test.args = all_categorical() ~ list(workspace = 2e9, hybrid = TRUE)
    ) %>%
    modify_fmt_fun(
      p.value = function(x) ifelse(is.na(x), "", sprintf(paste0("%.", p_digits, "f"), x))
    ) %>%
    add_overall(last = TRUE) %>%
    bold_labels()

  # 3. 导出 Word
  ft <- as_flex_table(tbl)
  officer::read_docx() %>%
    flextable::body_add_flextable(ft) %>%
    print(target = file)

  invisible(tbl)
}
