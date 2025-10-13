#' 单因素 Logistic
#'
#' @param related_index 预测变量
#' @param all_outcomes  结局变量
#' @param data          数据框
#' @param folder_path   结果保存目录
#' @param outcomes_map  中文映射
#'
#' @return              data.frame（OR / 95%CI / p）
run_univariate_logistic_regression <- function(related_index, all_outcomes, data, folder_path, outcomes_map) {
  # 初始化结果数据框
  results_df <- data.frame(predictor = character(),
                           outcome = character(),
                           p_value = numeric(),
                           OR = numeric(),
                           lower_ci = numeric(),
                           upper_ci = numeric(),
                           stringsAsFactors = FALSE)

  # 遍历相关指标和结局
  for (i in related_index) {
    for (o in all_outcomes) {
      # 构建逻辑回归模型
      formula <- as.formula(paste(o, "~", i))
      fit <- glm(formula, data = data, family = binomial())

      # 提取模型结果
      summary_fit <- summary(fit)
      coef_fit <- coef(summary_fit)

      # 获取OR值和置信区间
      OR <- exp(coef_fit[i, 1])  # OR值
      lower_ci <- exp(coef_fit[i, 1] - 1.96 * coef_fit[i, 2])  # 95% CI下限
      upper_ci <- exp(coef_fit[i, 1] + 1.96 * coef_fit[i, 2])  # 95% CI上限
      p_value <- coef_fit[i, 4]  # p值

      # 添加结果到数据框
      results_df <- rbind(results_df, data.frame(
        predictor = i,
        outcome = outcomes_map[[o]],
        p_value = p_value,
        OR = OR,
        lower_ci = lower_ci,
        upper_ci = upper_ci,
        stringsAsFactors = FALSE
      ))

      # 保存结果到文件
      output_file <- file.path(folder_path,
                               paste(i, outcomes_map[[o]], "txt",
                                sep = "_"))
      writeLines(paste("Predictor: ", i, "\nOutcome: ", outcomes_map[[o]], "\nOR: ", round(OR, 2),
                       "\n95% CI: [", round(lower_ci, 2), ", ", round(upper_ci, 2), "]\np-value: ", round(p_value, 4)),
                 con = output_file)
    }
  }

  # 返回结果数据框
  return(results_df)
}
