#' 四分位 Logistic 回归批量分析
#'
#' 将连续预测变量按四分位分组后，依次拟合：
#' ① Model1（单因素）  
#' ② Model2/3/4（依次叠加协变量）  
#' 并计算 Q2/Q3/Q4 相对于 Q1 的 OR、95%CI 及趋势检验 P 值，
#' 最终结果汇总成可发表的三线表并写入 Word。
#'
#' @param data          \code{data.frame}，必须包含结局变量、预测变量及所有协变量
#' @param all_index     字符向量，指定需要分析的连续预测变量名
#' @param all_outcomes  字符向量，指定二分类结局变量名（可多个）
#' @param models_list   命名列表，长度 3，依次给出 Model2/3/4 的协变量。
#'                      示例：\code{list(model2 = c("age","sex"), model3 = c("bmi"), model4 = c("LDL"))}
#' @param output_docx   输出 Word 文件名（含路径），默认 \code{"quartile_logistic_results.docx"}
#'
#' @return              \code{data.frame}，包含 Outcome/Index/Model/Comparison/OR/95%CI/P-value/P-trend
#'                      同时把三线表写入指定 Word 文件（invisible 返回）
#'
#' @details
#' \enumerate{
#'   \item 每组四分位标签为 Q1/Q2/Q3/Q4，Q1 作为参照组
#'   \item 趋势检验使用 quartile_num（1-4）作为连续变量纳入模型
#'   \item 所有模型均使用 \code{stats::glm(..., family = binomial())} 拟合
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
#' res <- quartile_logistic_analysis(
#'   data         = mydata,
#'   all_index    = c("PIV", "SIRI"),
#'   all_outcomes = c("death", "stroke"),
#'   models_list  = models,
#'   output_docx  = "Table_Quartile.docx"
#' )
#' }
#'
#' @importFrom dplyr %>% mutate across bind_rows select arrange
#' @importFrom broom tidy
#' @importFrom glue glue
#' @importFrom flextable flextable theme_zebra set_caption colformat_num autofit
#' @importFrom officer read_docx body_add_par 
#' @importFrom flextable body_add_flextable 
#' @export
quartile_logistic_analysis <- function(data, all_index, all_outcomes, models_list, output_docx = "quartile_logistic_results.docx") {

  results <- list()

  for (outcome in all_outcomes) {
    for (index in all_index) {

      # 四分位分组
      data <- data %>%
        mutate(
          quartile = ntile(!!sym(index), 4),
          quartile = factor(quartile, levels = 1:4, labels = c("Q1","Q2","Q3","Q4")),
          quartile_num = as.numeric(quartile)
        )

      # 构建模型列表
      model_names <- c("model1", names(models_list))
      covariates <- c("", unlist(models_list))

      res_models <- list()

      for (i in seq_along(model_names)) {
        covar_expr <- if (covariates[i] == "") "" else paste0(" + ", paste(covariates[[i]], collapse = " + "))
        formula_expr <- glue::glue("{outcome} ~ quartile{covar_expr}")

        model <- glm(as.formula(formula_expr), data = data, family = binomial)

        # 分类 OR
        tidy_model <- broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
          filter(term %in% c("quartileQ2", "quartileQ3", "quartileQ4"))

        # 趋势检验
        trend_model <- glm(as.formula(glue::glue("{outcome} ~ quartile_num{covar_expr}")),
                           data = data, family = binomial)
        trend_p <- broom::tidy(trend_model)$p.value[2]

        res_models[[model_names[i]]] <- tidy_model %>%
          mutate(
            Model = model_names[i],
            p_trend = trend_p,
            index = index,
            outcome = outcome
          )
      }

      # 合并所有模型
      res_combined <- bind_rows(res_models) %>%
        dplyr::select(outcome, index, Model, term, estimate, conf.low, conf.high, p.value, p_trend) %>%
        mutate(
          term = recode(term, "quartileQ2" = "Q2 vs Q1", "quartileQ3" = "Q3 vs Q1", "quartileQ4" = "Q4 vs Q1")
        )

      results[[glue::glue("{outcome}_{index}")]] <- res_combined

      # # 写入 Word
      # doc <- flextable(res_combined) %>%
      #   flextable::theme_zebra() %>%
      #   flextable::set_caption(glue::glue("Association between {index} (quartiles) and {outcome}"))
      #
      # print(doc, preview = FALSE)
      #
      # read_docx() %>%
      #   body_add_flextable(doc) %>%
      #   print(target = glue::glue("{outcome}_{index}_Quantile.docx"))
    }

  }
  # 合并所有结果
  final_df <- bind_rows(results) %>%
    mutate(across(c(estimate, conf.low, conf.high, p.value, p_trend), ~ round(.x, 3))) %>%
    transmute(
      Outcome = outcome,
      Index = index,
      Model = Model,
      Comparison = term,
      OR = estimate,
      `95% CI` = sprintf("%.2f (%.2f–%.2f)", estimate, conf.low, conf.high),
      `P-value` = p.value,
      `P-trend` = p_trend
    )

  # 写入 Word
  ft <- flextable(final_df) %>%
    theme_zebra() %>%
    set_caption("Quartile-based Logistic Regression Results") %>%
    colformat_num(j = c("OR", "P-value", "P-trend"), digits = 3) %>%
    autofit()

 officer::read_docx() %>%
    officer::body_add_par("Quartile-based Logistic Regression Results", style = "heading 1") %>%
    flextable::body_add_flextable(ft) %>%
    print(target = output_docx)

  message(glue::glue("✅ 结果已写入：{output_docx}"))


  return(final_df)
}

#' 基于四分位数的多分类结局 Logistic 回归批量分析
#'
#' 对指定的多个暴露指标（连续变量）按四分位分组后，依次针对多个多分类结局变量
#' 拟合多项 Logistic 回归（multinomial logistic regression），
#' 可自动调整多组协变量，计算 OR 及 95% CI（以 Q1 为参照），并给出趋势检验 P-value。
#' 结果整理成统一表格并输出至 Word 文档。
#'
#' @param data \code{data.frame}，包含结局变量、暴露指标及所有协变量。
#' @param all_index 字符向量，指定需要按四分位分组的连续型暴露指标名。
#' @param all_outcomes 字符向量，指定多分类结局变量名（可多个）。
#' @param models_list 命名列表，用于定义多组协变量调整模型。
#'   列表名为模型名称，列表元素为字符向量，给出对应模型的协变量名。
#'   例如 \code{list(model2 = c("age", "sex"), model3 = c("age", "sex", "bmi"))}。
#'   注意：函数会自动生成一个仅含 quartile 的“model1”（无额外协变量）。
#' @param ref_level 字符，指定结局变量的参考水平；若为 \code{NULL}（默认），
#'   则使用因子本身的第一个水平作为参照。
#' @param output_docx 字符，输出 Word 文件名（含路径），默认
#'   \code{"quartile_multinomial_results.docx"}。
#'
#' @return 不可见（invisible）返回一个整理好的 \code{data.frame}，包含以下列：
#'   \itemize{
#'     \item \code{Outcome}：结局变量名。
#'     \item \code{Level}：结局变量的当前水平。
#'     \item \code{Index}：暴露指标名。
#'     \item \code{Model}：模型名称。
#'     \item \code{Comparison}：比较组（Q2 vs Q1、Q3 vs Q1、Q4 vs Q1）。
#'     \item \code{OR}： odds ratio。
#'     \item \code{95% CI}：OR 的 95% 置信区间，格式为 “estimate (low–high)”。
#'     \item \code{P-value}：当前比较对应的 Wald 检验 P 值。
#'     \item \code{P-trend}：趋势检验（以 quartile_num 为连续变量）P 值。
#'   }
#'   同时把格式化表格写入指定 Word 文件。
#'
#' @details
#' \enumerate{
#'   \item 将每个暴露指标按四分位分组（Q1–Q4），生成因子变量 \code{quartile} 及
#'         连续变量 \code{quartile_num}（1–4）。
#'   \item 对每个结局变量、每个暴露指标、每个模型组合，依次拟合
#'         \code{nnet::multinom}。
#'   \item 计算各组（Q2/Q3/Q4）相对于 Q1 的 OR 及 95% CI。
#'   \item 趋势检验：将 quartile_num 作为连续变量纳入模型，提取其 P 值。
#'   \item 最终整理成一张长表，自动四舍五入并输出到 Word（基于 \code{flextable}）。
#' }
#'
#' @section 注意事项:
#' \itemize{
#'   \item 需要安装 \pkg{nnet}、\pkg{dplyr}、\pkg{broom}、\pkg{flextable}、\pkg{officer} 等包。
#'   \item 若某个模型因共线性或样本量不足导致拟合失败，函数会抛出错误；建议提前清洗数据。
#'   \item 所有结局变量会被强制转换为因子；若需自定义水平顺序或参考水平，请提前在数据集中设置或在 \code{ref_level} 参数中指定。
#' }
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#' df <- data.frame(
#'   age  = rnorm(500, 50, 10),
#'   sex  = sample(c("M", "F"), 500, replace = TRUE),
#'   bmi  = rnorm(500, 25, 3),
#'   expo1 = rnorm(500),
#'   expo2 = rnorm(500),
#'   outcome = sample(letters[1:3], 500, replace = TRUE)
#' )
#'
#' models <- list(
#'   model2 = c("age", "sex"),
#'   model3 = c("age", "sex", "bmi")
#' )
#'
#' res <- quartile_multinomial_analysis(
#'   data        = df,
#'   all_index   = c("expo1", "expo2"),
#'   all_outcomes = "outcome",
#'   models_list = models,
#'   ref_level   = "a",
#'   output_docx = "demo_results.docx"
#' )
#'
#' print(res)
#' }
#'
#' @importFrom dplyr %>% mutate ntile left_join filter select bind_rows transmute
#' @importFrom broom tidy
#' @importFrom glue glue
#' @importFrom flextable flextable theme_zebra colformat_num autofit
#' @importFrom officer read_docx body_add_par
#' @importFrom flextable body_add_flextable
#' @export
quartile_multinomial_analysis <- function(data,
                                          all_index,
                                          all_outcomes,   # 可以是一个字符向量，也可以是一个多分类变量名
                                          models_list,
                                          ref_level = NULL,   # 可手动指定参考水平
                                          output_docx = "quartile_multinomial_results.docx") {

  # ---------- 依赖 ----------
  if (!requireNamespace("nnet", quietly = TRUE))
    stop("请安装 nnet 包：install.packages('nnet')")

  # ---------- 初始化 ----------
  results <- list()

  for (outcome in all_outcomes) {

    # 如果 outcome 不是因子，强制转换
    data[[outcome]] <- factor(data[[outcome]])
    if (!is.null(ref_level))
      data[[outcome]] <- relevel(data[[outcome]], ref = ref_level)

    for (index in all_index) {

      # -------- 四分位分组 --------
      data <- data %>%
        mutate(
          quartile = ntile(!!sym(index), 4),
          quartile = factor(quartile, levels = 1:4, labels = c("Q1","Q2","Q3","Q4")),
          quartile_num = as.numeric(quartile)
        )

      # -------- 模型循环 --------
      model_names <- c("model1", names(models_list))
      covariates  <- c("", unlist(models_list))
      res_models  <- list()

      for (i in seq_along(model_names)) {

        covar_expr <- if (covariates[i] == "") "" else
          paste0(" + ", paste(covariates[[i]], collapse = " + "))

        f_expr <- as.formula(glue::glue("{outcome} ~ quartile{covar_expr}"))
        model  <- nnet::multinom(f_expr, data = data, trace = FALSE)

        # ---- OR 与 95%CI（相对 Q1） ----
        tidy_or <- broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
          filter(term != "(Intercept)") %>%          # 去掉截距
          mutate(term = recode(term,
                               "quartileQ2" = "Q2 vs Q1",
                               "quartileQ3" = "Q3 vs Q1",
                               "quartileQ4" = "Q4 vs Q1"))

        # ---- 趋势检验（quartile_num 代替 quartile） ----
        f_trend <- as.formula(glue::glue("{outcome} ~ quartile_num{covar_expr}"))
        model_trend <- nnet::multinom(f_trend, data = data, trace = FALSE)
        # 每个 level 都会有一个 p.value，我们取 quartile_num 对应的
        tidy_trend <- broom::tidy(model_trend) %>%
          filter(term == "quartile_num") %>%
          mutate(level = y.level) %>%
          dplyr::select(level, p_trend = p.value)

        tidy_or <- tidy_or %>%
          left_join(tidy_trend, by = c("y.level" = "level")) %>%
          mutate(
            Model  = model_names[i],
            index  = index,
            outcome = outcome
          )

        res_models[[model_names[i]]] <- tidy_or
      }
      # -------- 合并 --------
      res_combined <- bind_rows(res_models) %>%
        dplyr::select(outcome, index, Model, y.level, term,
               estimate, conf.low, conf.high, p.value, p_trend)

      results[[glue::glue("{outcome}_{index}")]] <- res_combined
    }
  }

  # ---------- 整理最终表 ----------
  final_df <- bind_rows(results) %>%
    mutate(across(c(estimate, conf.low, conf.high, p.value, p_trend), round, 3)) %>%
    transmute(
      Outcome   = outcome,
      Level     = y.level,
      Index     = index,
      Model     = Model,
      Comparison = term,
      OR        = estimate,
      `95% CI`  = sprintf("%.2f (%.2f–%.2f)", estimate, conf.low, conf.high),
      `P-value` = p.value,
      `P-trend` = p_trend
    )

  # ---------- 写 Word ----------
  ft <- flextable(final_df) %>%
    theme_zebra() %>%
    set_caption("Quartile-based Multinomial Logistic Regression Results") %>%
    colformat_num(j = c("OR", "P-value", "P-trend"), digits = 3) %>%
    autofit()

  officer::read_docx() %>%
    officer::body_add_par("Quartile-based Multinomial Logistic Regression Results", style = "heading 1") %>%
    flextable::body_add_flextable(ft) %>%
    print(target = output_docx)

  message(glue::glue("✅ 多分类结果已写入：{output_docx}"))
  invisible(final_df)
}
