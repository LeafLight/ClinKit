#' 多因素逻辑回归（4 层模型）
#'
#' @param related_index   预测变量
#' @param all_outcomes    结局变量
#' @param data            数据框
#' @param folder_path     结果保存目录（每个预测变量×结局生成一个txt)
#' @param outcomes_map    结局变量中文名映射（list）
#' @param models_list     协变量列表，长度 3（model2、3、4）
#'
#' @return 包含预测变量×结局的多因素逻辑回归结果的dataframe
#'                        names 必须与 model2、model3、model4 对应
run_multivariable_logistic_regression <- function(related_index,
                                                  all_outcomes,
                                                  data,
                                                  folder_path,
                                                  outcomes_map,
                                                  models_list) {

  stopifnot(
    length(models_list) == 3,
    identical(names(models_list), c("model2", "model3", "model4"))
  )

  # 初始化结果容器
  results <- list()

  # 遍历预测变量和结局
  for (i in related_index) {
    for (o in all_outcomes) {

      # 当前行唯一标识
      row_id <- paste(i, o, sep = "__")

      # 保存 4 个模型的结果
      res_row <- list(predictor = i,
                      outcome   = outcomes_map[[o]])

      # 1. model1：单因素
      f1 <- as.formula(paste(o, "~", i))
      fit1 <- glm(f1, data = data, family = binomial())
      ci1 <- confint.default(fit1)[i, ]
      res_row <- c(res_row,
                   OR_m1    = unname(exp(coef(fit1)[i])),
                   LCI_m1   = unname(exp(ci1[1])),
                   UCI_m1   = unname(exp(ci1[2])),
                   p_m1     = unname(summary(fit1)$coefficients[i, 4]))

      # 2. model2：model2 协变量
      cov2 <- setdiff(models_list[["model2"]], i)
      f2 <- reformulate(c(i, cov2), response = o)
      fit2 <- glm(f2, data = data, family = binomial())
      ci2 <- confint.default(fit2)[i, ]
      res_row <- c(res_row,
                   OR_m2    = unname(exp(coef(fit2)[i])),
                   LCI_m2   = unname(exp(ci2[1])),
                   UCI_m2   = unname(exp(ci2[2])),
                   p_m2     = unname(summary(fit2)$coefficients[i, 4])
      )

      # 3. model3：model2+model3
      cov3 <- setdiff(c(models_list[["model2"]], models_list[["model3"]]), i)
      f3 <- reformulate(c(i, cov3), response = o)
      fit3 <- glm(f3, data = data, family = binomial())
      ci3 <- confint.default(fit3)[i, ]
      res_row <- c(res_row,
                   OR_m3    = unname(exp(coef(fit3)[i])),
                   LCI_m3   = unname(exp(ci3[1])),
                   UCI_m3   = unname(exp(ci3[2])),
                   p_m3     = unname(summary(fit3)$coefficients[i, 4]))

      # 4. model4：model2+3+4
      cov4 <- setdiff(unlist(models_list), i)
      f4 <- reformulate(c(i, cov4), response = o)
      fit4 <- glm(f4, data = data, family = binomial())
      ci4 <- confint.default(fit4)[i, ]
      res_row <- c(res_row,
                   OR_m4    = unname(exp(coef(fit4)[i])),
                   LCI_m4   = unname(exp(ci4[1])),
                   UCI_m4   = unname(exp(ci4[2])),
                   p_m4     = unname(summary(fit4)$coefficients[i, 4]))

      # 保存
      results[[row_id]] <- res_row

      # 写单个 txt 文件
      out_txt <- file.path(folder_path,
                           paste(i, outcomes_map[[o]], "result.txt", sep = "_"))
      writeLines(
        c(paste("Predictor:", i),
          paste("Outcome:",  outcomes_map[[o]]),
          sprintf("Model1 (unadjusted): OR = %.2f, 95%%CI [%.2f-%.2f], p = %.4f",
                  res_row[["OR_m1"]], res_row[["LCI_m1"]], res_row[["UCI_m1"]], res_row[["p_m1"]]),
          sprintf("Model2 (adjusted for model2): OR = %.2f, 95%%CI [%.2f-%.2f], p = %.4f",
                  res_row[["OR_m2"]], res_row[["LCI_m2"]], res_row[["UCI_m2"]], res_row[["p_m2"]]),
          sprintf("Model3 (+model3): OR = %.2f, 95%%CI [%.2f-%.2f], p = %.4f",
                  res_row[["OR_m3"]], res_row[["LCI_m3"]], res_row[["UCI_m3"]], res_row[["p_m3"]]),
          sprintf("Model4 (+model4): OR = %.2f, 95%%CI [%.2f-%.2f], p = %.4f",
                  res_row[["OR_m4"]], res_row[["LCI_m4"]], res_row[["UCI_m4"]], res_row[["p_m4"]])),
        con = out_txt)
    }
  }

  # 合并为 data.frame
  #do.call(rbind.data.frame, results)
  as.data.frame(Reduce(rbind, results))
}



#######
#' 批量多因素结果 -> Word 三线表
#'
#' @param raw_df        原始数据框，一行一个分析结果，18 列。
#' @param out_file      输出的 docx 文件名；若为 NULL，则返回 flextable 对象。
#' @param caption       表格标题
#' @param merge_predictor 是否纵向合并相同的 Predictor 单元格（TRUE=合并）
#'
#' @return 若无 out_file，返回 flextable 对象；否则写入磁盘并返回 NULL（invisible）。
#'
#' @examples
#' # make_multivariate_table(my_raw, "table1.docx")
#'
make_multivariate_table <- function(raw_df,
                                    out_file   = NULL,
                                    caption    = "Multivariate analysis results",
                                    merge_predictor = TRUE) {

  # ---------- 0. 依赖检查 ----------
  if (!requireNamespace("flextable", quietly = TRUE))
    stop("Please install.packages('flextable')")
  if (!requireNamespace("dplyr", quietly = TRUE))
    stop("Please install.packages('dplyr')")
  if (!requireNamespace("stringr", quietly = TRUE))
    stop("Please install.packages('stringr')")

  # ---------- 1. 确保是 data.frame ----------
  raw_df <- as.data.frame(raw_df, stringsAsFactors = FALSE)

  # ---------- 2. 数据整理 ----------
  dat <- raw_df %>%
    dplyr::mutate(
      Predictor = sub("^X(\\..*)?$", "", as.character(.[[1]])),  # 按位置取列
      Outcome   = as.character(.[[2]]),
      OR_m1  = .[[3]],  LCI_m1 = .[[4]],  UCI_m1 = .[[5]],  p_m1 = .[[6]],
      OR_m2  = .[[7]],  LCI_m2 = .[[8]],  UCI_m2 = .[[9]],  p_m2 = .[[10]],
      OR_m3  = .[[11]], LCI_m3 = .[[12]], UCI_m3 = .[[13]], p_m3 = .[[14]],
      OR_m4  = .[[15]], LCI_m4 = .[[16]], UCI_m4 = .[[17]], p_m4 = .[[18]]
    ) %>%
    dplyr::transmute(
      Predictor,
      Outcome,
      `Model 1` = sprintf("%.2f (%.2f–%.2f)\np=%.2e", OR_m1, LCI_m1, UCI_m1, p_m1),
      `Model 2` = sprintf("%.2f (%.2f–%.2f)\np=%.2e", OR_m2, LCI_m2, UCI_m2, p_m2),
      `Model 3` = sprintf("%.2f (%.2f–%.2f)\np=%.2e", OR_m3, LCI_m3, UCI_m3, p_m3),
      `Model 4` = sprintf("%.2f (%.2f–%.2f)\np=%.2e", OR_m4, LCI_m4, UCI_m4, p_m4)
    ) %>%
    dplyr::arrange(Predictor, Outcome)

  # ---------- 3. 构建 flextable ----------
  ft <- flextable::flextable(dat) %>%
    flextable::set_header_labels(
      Predictor = "Predictor",
      Outcome   = "Outcome",
      `Model 1` = "Unadjusted",
      `Model 2` = "Model 2",
      `Model 3` = "Model 3",
      `Model 4` = "Model 4"
    ) %>%
    flextable::theme_zebra() %>%
    flextable::bold(part = "header") %>%
    flextable::align(align = "center", part = "all") %>%
    flextable::fontsize(size = 9, part = "all") %>%
    flextable::set_caption(caption)

  if (merge_predictor && nrow(dat) > 1)
    ft <- flextable::merge_v(ft, j = "Predictor")

  # ---------- 4. 输出 ----------
  if (!is.null(out_file)) {
    if (!requireNamespace("officer", quietly = TRUE))
      stop("Please install.packages('officer')")
    officer::read_docx() %>%
      flextable::body_add_flextable(ft) %>%
      print(target = out_file)
    message("Word table written to: ", normalizePath(out_file))
    invisible(NULL)
  } else {
    return(ft)
  }
}


#' 连续变量多分类 Logistic 回归批量分析
#'
#' 对连续预测变量（无需分组）直接拟合：
#' ① Model1（单因素）  
#' ② Model2/3/4（依次叠加协变量）  
#' 计算每单位增加的 OR、95%CI 及 P 值，并进行多分类趋势检验。
#' 结果汇总为可发表的三线表并写入 Word。
#'
#' @param data          \code{data.frame}，必须包含结局变量、预测变量及所有协变量
#' @param all_index     字符向量，指定需要分析的连续预测变量名
#' @param all_outcomes  字符向量，指定多分类结局变量名（可多个）；函数会将其转为因子
#' @param models_list   命名列表，长度 3，依次给出 Model2/3/4 的协变量。
#'                      示例：\code{list(model2 = c("age","sex"), model3 = c("bmi"), model4 = c("LDL"))}
#' @param ref_level     字符标量，可手动指定多分类结局的参照水平；默认 NULL（按因子默认第一水平）
#' @param output_docx   输出 Word 文件名（含路径），默认 \code{"continuous_multinomial_results.docx"}
#'
#' @return              \code{data.frame}，包含 Outcome/Level/Index/Model/Comparison/OR/95%CI/P-value
#'                      同时把三线表写入指定 Word 文件（invisible 返回）
#'
#' @details
#' \enumerate{
#'   \item 预测变量保持连续，不切割，解释为单位增量下的 OR
#'   \item 所有模型均使用 \code{nnet::multinom()} 拟合，OR 及 95%CI 通过 \code{broom::tidy(exponentiate = TRUE)} 提取
#'   \item Word 表使用 \code{flextable} 绘制，可直接用于论文或补充材料
#' }
#'
#' @examples
#' \dontrun{
#' models <- list(
#'   model2 = c("age", "sex"),
#'   model3 = c("bmi", "smoke"),
#'   model4 = c("sbp", "ldl")
#' )
#'
#' res <- run_multivariable_multinomial_logistic_regression(
#'   data         = mydata,
#'   all_index    = c("PIV", "SIRI"),
#'   all_outcomes = "stroke_subtype",   # 三分类变量
#'   models_list  = models,
#'   ref_level    = "normal",
#'   output_docx  = "Table_Continuous_Multi.docx"
#' )
#' }
#'
#' @importFrom dplyr %>% mutate across bind_rows select arrange transmute
#' @importFrom broom tidy
#' @importFrom glue glue
#' @importFrom flextable flextable theme_zebra set_caption colformat_num autofit
#' @importFrom officer read_docx body_add_par
#' @importFrom flextable flextable theme_zebra autofit
#' @importFrom nnet multinom
#' @export
run_multivariable_multinomial_logistic_regression <- function(data,
                                            all_index,
                                            all_outcomes,
                                            models_list,
                                            ref_level = NULL,
                                            output_docx = "continuous_multinomial_results.docx") {

  # ---------- 依赖 ----------
  if (!requireNamespace("nnet", quietly = TRUE))
    stop("请安装 nnet 包：install.packages('nnet')")

  suppressPackageStartupMessages({
    library(dplyr)
    library(broom)
    library(glue)
    library(flextable)
    library(officer)
  })

  results <- list()

  for (outcome in all_outcomes) {
    data[[outcome]] <- factor(data[[outcome]])
    if (!is.null(ref_level))
      data[[outcome]] <- relevel(data[[outcome]], ref = ref_level)

    for (index in all_index) {

      model_names <- c("model1", names(models_list))
      covariates  <- c("", unlist(models_list))

      res_models <- list()

      for (i in seq_along(model_names)) {

        covar_expr <- if (covariates[i] == "") "" else
          paste0(" + ", paste(covariates[[i]], collapse = " + "))

        f_expr <- as.formula(glue::glue("{outcome} ~ {index}{covar_expr}"))
        model  <- nnet::multinom(f_expr, data = data, trace = FALSE)

        tidy_or <- broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
          filter(term == index) %>%
          mutate(
            term   = paste0(index, " (per unit increase)"),
            Model  = model_names[i],
            index  = index,
            outcome = outcome
          )

        res_models[[model_names[i]]] <- tidy_or
      }
      res_combined <- bind_rows(res_models) %>%
        dplyr::select(outcome, index, Model, y.level, term,
               estimate, conf.low, conf.high, p.value)

      results[[glue::glue("{outcome}_{index}")]] <- res_combined
    }
  }

  # ---------- 最终表 ----------
  final_df <- bind_rows(results) %>%
    mutate(across(c(estimate, conf.low, conf.high, p.value), round, 3)) %>%
    transmute(
      Outcome    = outcome,
      Level      = y.level,
      Index      = index,
      Model      = Model,
      Comparison = term,
      OR         = estimate,
      `95% CI`   = sprintf("%.2f (%.2f–%.2f)", estimate, conf.low, conf.high),
      `P-value`  = p.value
    )

  # ---------- 写 Word ----------
  ft <- flextable(final_df) %>%
    theme_zebra() %>%
    set_caption("Continuous Multinomial Logistic Regression Results") %>%
    colformat_num(j = c("OR", "P-value"), digits = 3) %>%
    autofit()

  officer::read_docx() %>%
    body_add_par("Continuous Multinomial Logistic Regression Results", style = "heading 1") %>%
    flextable::body_add_flextable(ft) %>%
    print(target = output_docx)

  message(glue::glue("✅ 连续变量多分类结果已写入：{output_docx}"))
  invisible(final_df)
}
