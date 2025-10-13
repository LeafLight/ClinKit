#' High-High vs Low-Low 四分组逻辑回归 + 相加交互作用
#'
#' 对两个连续暴露变量 A、B 同时按中位数二分类，生成 4 种暴露组合：
#' Low A/Low B（参照）、Low A/High B、High A/Low B、High A/High B，
#' 依次拟合 4 层逻辑回归模型（单因素 + 3 个协变量梯度），
#' 并计算各组 OR(95%CI) 及相加交互作用（interactionR）。
#' 结果汇总为发表级表格并写入 Word。
#'
#' @param data      \code{data.frame}，必须包含结局变量、A、B 及所有协变量
#' @param A         字符标量，连续暴露 A 列名
#' @param B         字符标量，连续暴露 B 列名
#' @param outcome   字符标量，二分类结局变量名（0/1）
#' @param model2    字符向量，Model2 新增协变量，默认 NULL
#' @param model3    字符向量，Model3 额外协变量，默认 NULL
#' @param model4    字符向量，Model4 额外协变量，默认 NULL
#' @param file_res  输出 Word 文件名（含路径），默认 \code{"result.docx"}
#' @param file_int  可选，交互作用单独报告文件名；若 NULL 则仅内部计算不单独输出
#' @param recode    逻辑值，是否对交互作用变量重新编码，默认 FALSE
#'
#' @return          四分组因子向量（invisible），方便后续二次分析
#' @details
#' \enumerate{
#'   \item 中位数自动计算，NA 会被忽略；二分类变量命名为 A_bin/B_bin
#'   \item 4 种暴露组合按 Low/Low → High/High 顺序因子化，Low/Low 为参照
#'   \item 使用 \code{glm(..., family = binomial())} 拟合，OR 及 95%CI 通过 \code{broom::tidy(exponentiate = TRUE)} 提取
#'   \item 相加交互作用由 \code{interactionR} 完成，输出 RERI、AP、SI 及其 95%CI
#'   \item Word 表含病例数、OR(95%CI)、P 值，可直接用于论文
#' }
#'
#' @examples
#' \dontrun{
#' highhighlowlow(
#'   data   = OCdata,
#'   A      = "alc",
#'   B      = "smk",
#'   outcome = "oc",
#'   model2 = c("age", "sex"),
#'   model3 = "bmi",
#'   model4 = "edu",
#'   file_res = "joint.docx",
#'   file_int = "interactionR.docx"
#' )
#' }
#'
#' @importFrom dplyr mutate count case_when left_join across everything
#' @importFrom broom tidy
#' @importFrom purrr map map_dfr
#' @importFrom stats as.formula glm
#' @importFrom interactionR interactionR interactionR_table
#' @importFrom officer read_docx body_add_par
#' @importFrom flextable body_add_flextable
#' @importFrom flextable flextable theme_booktabs autofit
#' @export
highhighlowlow <- function(
  data,
  A,               # 连续暴露 A 的列名
  B,               # 连续暴露 B 的列名
  outcome,         # 0/1 结局列名
  model2 = NULL,   # Model2 新增协变量（字符向量）
  model3 = NULL,
  model4 = NULL,
  file_res   = "result.docx",
  file_int  = NULL,    # 可选：interactionR 单独存一份，不填则合并
  recode=FALSE
) {

  # ---- 0. 依赖包 ----
  if (!requireNamespace("interactionR", quietly = TRUE))
    stop("请先安装 interactionR 包：install.packages('interactionR')")

  # ---- 1. 四分组：Low/Low 为参照 ----
  data <- dplyr::mutate(data,
                        A_bin = as.numeric(.data[[A]] > median(.data[[A]], na.rm = TRUE)),
                        B_bin = as.numeric(.data[[B]] > median(.data[[B]], na.rm = TRUE)),
                        group = factor(
                          dplyr::case_when(
                            A_bin == 0 & B_bin == 0 ~ "Low A / Low B",
                            A_bin == 0 & B_bin == 1 ~ "Low A / High B",
                            A_bin == 1 & B_bin == 0 ~ "High A / Low B",
                            A_bin == 1 & B_bin == 1 ~ "High A / High B"
                          ),
                          levels = c("Low A / Low B",
                                     "Low A / High B",
                                     "High A / Low B",
                                     "High A / High B")
                        )
  )

  # ---- 2. Case 数 ----
  case_tab <- dplyr::count(data, group,
                           wt = (.data[[outcome]] == 1),
                           name = "Cases")

  # ---- 3. 构建 4 层 logistic 公式 ----
  rhs1 <- "group"
  rhs2 <- if (!is.null(model2)) paste(rhs1, paste(model2, collapse = " + "), sep = " + ") else rhs1
  rhs3 <- if (!is.null(model3)) paste(rhs2, paste(model3, collapse = " + "), sep = " + ") else rhs2
  rhs4 <- if (!is.null(model4)) paste(rhs3, paste(model4, collapse = " + "), sep = " + ") else rhs3

  formulas <- list(
    Model1 = stats::as.formula(paste(outcome, "~", rhs1)),
    Model2 = stats::as.formula(paste(outcome, "~", rhs2)),
    Model3 = stats::as.formula(paste(outcome, "~", rhs3)),
    Model4 = stats::as.formula(paste(outcome, "~", rhs4))
  )

  mods <- purrr::map(formulas, ~ glm(.x, data = data, family = binomial))

  # ---- 4. 提取 OR、95%CI、p 值 ----
  extract <- function(mod) {
    broom::tidy(mod, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95) %>%
      dplyr::filter(grepl("^group", term)) %>%
      dplyr::mutate(
        OR_CI_P = sprintf("%.2f (%.2f–%.2f), p=%.3g",
                          estimate, conf.low, conf.high, p.value)
      ) %>%
      dplyr::mutate(group = sub("^group", "", term))
  }

  or_df <- purrr::map_dfr(mods, extract, .id = "Model")

  # ---- 5. 合并 Case 与 OR 表 ----
  table1 <- case_tab %>%
    dplyr::left_join(or_df, by = "group") %>%
    dplyr::mutate(OR_CI_P = dplyr::coalesce(OR_CI_P, "1.00 (ref)")) %>%
    tidyr::pivot_wider(names_from = group,
                       values_from = c(Cases, OR_CI_P)) %>%
    dplyr::mutate(dplyr::across(tidyselect::everything(), as.character))

  # ---- 6. interactionR：相加交互作用 ----
  int_mod <- glm(
    stats::as.formula(paste0(outcome, " ~ A_bin * B_bin + ",
                             paste(c(model2, model3, model4), collapse = " + "))),
    data = data, family = binomial
  )

  int_out <- interactionR::interactionR(
    int_mod,
    exposure_names = c("A_bin", "B_bin"),
    ci.type = "mover", ci.level = 0.95, em = FALSE, recode = recode
  )
  int_out$dframe <- as.data.frame(lapply(int_out$dframe, function(x) if(is.numeric(x)) round(x, 4) else x))
  interactionR::interactionR_table(int_out, p.value = TRUE, file_path = file_int)

  # ---- 7. 写入 Word ----
  doc <- officer::read_docx() %>%
    officer::body_add_par("Joint association of A and B (Low/Low reference)", style = "heading 1") %>%
    flextable::body_add_flextable(flextable::flextable(table1) %>%
                                    flextable::theme_booktabs() %>%
                                    flextable::autofit())

  print(doc, target = file_res)
  message("Word 已保存：", normalizePath(file_res))

  invisible(data$group)
}

#' 交互作用表：双向亚组 OR 与交互 P 值
#'
#' 对两个连续变量 A、B 分别按中位数二分类，生成交叉亚组，
#' 依次计算 A 在 B 分层下的效应（OR、95%CI）及交互 P 值，
#' 并反向计算 B 在 A 分层下的效应，最终合并为一张三线表写入 Word。
#'
#' @param data      \code{data.frame}，必须包含 A、B、结局变量及可选协变量
#' @param outcome   字符标量，二分类结局变量名（0/1）
#' @param A         字符标量，连续暴露 A 列名
#' @param B         字符标量，连续暴露 B 列名
#' @param cov       字符向量，需调整的协变量，默认 NULL
#' @param digit     数值，OR 及 95%CI 的小数位，默认 3
#' @param fp        字符，输出 Word 路径（含文件名），父目录须已存在
#'
#' @return          列表，元素 1 = A 分层结果，元素 2 = B 分层结果（invisible）
#' @details
#' \enumerate{
#'   \item 中位数自动计算，NA 被忽略；二分类变量命名为 A_bin/B_bin
#'   \item 使用 \code{TableSubgroupMultiGLM} 完成分层回归，自动输出交互 P 值
#'   \item 返回表含 aOR(95%CI) 及交互 P，可直接用于论文或附录
#' }
#'
#' @examples
#' \dontrun{
#' interaction_table(
#'   data = mydata,
#'   outcome = "death",
#'   A = "SIRI",
#'   B = "hsCRP",
#'   cov = c("age", "sex"),
#'   digit = 2,
#'   fp = "交互作用表.docx"
#' )
#' }
#'
#' @importFrom dplyr mutate if_else filter select row_number bind_rows
#' @importFrom flextable flextable border_outer autofit save_as_docx
#' @export
interaction_table <- function(data,
                              outcome,
                              A,
                              B,
                              cov = NULL,
                              digit = 3,
                              fp) {

  ## 0. 依赖 -----------------------------------------------------------
  suppressPackageStartupMessages({
    library(tableone)
    library(dplyr)
    library(flextable)
  })

  ## 1. 把 A、B 按中位数二分类 ----------------------------------------
  med_a <- median(data[[A]], na.rm = TRUE)
  med_b <- median(data[[B]], na.rm = TRUE)

  dat <- data %>%
    mutate(
      A_bin = factor(if_else(.data[[A]] >= med_a,
                             paste0(A, "≥median"),
                             paste0(A, "<median")),
                     levels = c(paste0(A, "<median"), paste0(A, "≥median"))),
      B_bin = factor(if_else(.data[[B]] >= med_b,
                             paste0(B, "≥median"),
                             paste0(B, "<median")),
                     levels = c(paste0(B, "<median"), paste0(B, "≥median")))
    )

  ## 2. 通用公式 -------------------------------------------------------
  make_formula <- function(x) {
    rhs <- x
    as.formula(paste(outcome, "~", rhs))
  }
  print(make_formula(A))
  ## 3. 用 TableSubgroupMultiGLM 计算 ----------------------------------
  tab_A <- TableSubgroupMultiGLM(
    formula       =  make_formula(A),
    var_subgroups = "B_bin",
    data          = dat,
    var_cov    = cov,
    family = "binomial",
    decimal.estimate = digit,
    decimal.percent  = 1,
    decimal.pvalue   = 3
    #addOverallCol = FALSE
  )


  tab_B <- TableSubgroupMultiGLM(
    formula       = make_formula(B),
    var_subgroups = "A_bin",
    data          = dat,
    var_cov    = cov,
    family = "binomial",
    decimal.estimate = digit,
    decimal.percent  = 1,
    decimal.pvalue   = 3
    #addOverallCol = FALSE
  )
  print(tab_A)
  print(tab_A[2, "P for interaction"])
  ## 4. 提取结果 -------------------------------------------------------
  extract <- function(tb, var_name, other) {
    if (is.null(tb)) return(NULL)               # 防止 NULL 导致的 mutate 报错

    tb %>%                               # 新版用 $Result
      mutate(
        Groups       = c(var_name, other,paste(other,"< median"), paste(other, "> median")),
        `aOR (95% CI)` = ifelse(row_number() %in% c(2, 6),"", paste(OR, "(",Lower, ",", Upper, ")")),
        `P interaction` = ifelse(row_number() == 2,
                                 tb$`P for interaction`, "")
      ) %>%
      dplyr::select(Groups, `aOR (95% CI)`, `P interaction`)
  }

  res <- bind_rows(
    extract(tab_A, A, B),
    extract(tab_B, B, A)
  )

  ## 5. 写入 Word ------------------------------------------------------
  ft <- flextable(res) %>% border_outer() %>% autofit()
  flextable::save_as_docx(ft, path = fp)

  invisible(res)
  list(tab_A, tab_B)
}
