#' RCS 逻辑回归 + 图
#'
#' @param outcome      结局变量名
#' @param predictor    连续预测变量名
#' @param data         数据框
#' @param output_folder 输出目录
#' @param models       协变量向量，默认 NULL
#' @param use_model    是否加协变量，默认 FALSE
#' @param filename     自定义文件名，默认 NULL（自动生成）
#'
#' @return             列表：plot + 非线性/总体 P 值（字符 + 数值）
#' @examples
#' \dontrun{
#' generate_rcs_plot("death", "SIRI", data, "./RCS")
#' }
generate_rcs_plot <- function(outcome, predictor, data, output_folder, models=covariates, use_model=FALSE, filename=NULL) {
  # 检查输出文件夹是否存在，如果不存在则创建
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }
  formula <- as.formula(paste(outcome, "~ rcs(", predictor, ", 4)"))
  print(use_model)
  if(use_model){
    print("use model")
    formula <- as.formula(paste(outcome, "~ rcs(", predictor, ", 4) +", paste(models, collapse = " + ")))
  }
  # 构建公式
  print(formula)
  # 拟合逻辑回归模型
  fit <- rms::lrm(formula, data = data)
  print(fit)
  # 预测并计算OR值
  predictions <- do.call(Predict, list(fit, predictor, fun = exp, type = "predictions",
                                       conf.int = 0.95, digits = 2, ref.zero = TRUE))
  # predictions <-Predict(fit, formula, fun = exp, type = "predictions",
  #         conf.int = 0.95, digits = 2, ref.zero = TRUE)
  # 方差分析
  anova_table <- anova(fit)
  p_nl <- format_p_value(anova_table[2, 3], "p for nonlinear")
  p_oa <- format_p_value(anova_table[1, 3], "p for overall")

  # 提取原始p值（数值型）
  p_nl_value <- anova_table[2, 3]
  p_oa_value <- anova_table[1, 3]

  # 绘制RCS图
  p <- ggplot() +
    geom_line(data = predictions, aes(x = !!sym(predictor), y = yhat, color = "预测曲线"),
              linetype = "solid", size = 1, alpha = 0.9) +
    geom_ribbon(data = predictions,
                aes(x = !!sym(predictor), ymin = lower, ymax = upper, fill = "置信区间"),
                alpha = 0.2) +
    scale_color_manual(values = c("预测曲线" = "#d63031")) +
    scale_fill_manual(values = c("置信区间" = "#e17055")) +
    geom_hline(yintercept = 1, linetype = 2, size = 1) +
    theme_classic() +
    theme(axis.text = element_text(size = 25, family = "Arial"),
          legend.position = "none",
          axis.title = element_text(size = 25, family = "Arial"),
          axis.line = element_line(linewidth = 1)) +
    labs(x = predictor, y = "OR (95%CI)")

  # 保存图片
  tiff_path <- file.path(output_folder, paste(predictor, outcome, ".tiff", sep = "_"))
  pdf_path <- file.path(output_folder, paste(predictor, outcome, ".pdf", sep = "_"))
  if(use_model){
    tiff_path <- file.path(output_folder, paste(predictor, outcome, "_use_model.tiff", sep = "_"))
    pdf_path <- file.path(output_folder, paste(predictor, outcome, "_use_model.pdf", sep = "_"))
  }
  if(!is.null(filename)){
    tiff_path <- file.path(output_folder, paste0(filename, ".tiff"))
    pdf_path <- file.path(output_folder, paste0(filename, ".pdf"))
    svg_path <- file.path(output_folder, paste0(filename, ".svg"))
    if(use_model){
      tiff_path <- file.path(output_folder, paste(filename, "use_model.tiff", sep = "_"))
      pdf_path <- file.path(output_folder, paste(filename, "use_model.pdf", sep = "_"))
      svg_path <- file.path(output_folder, paste(filename, "use_model.svg", sep = "_"))
    }
  }
  ggsave(tiff_path,
         plot = p, width = 1600, height = 1500, dpi = 300, units = "px")
  ggsave(pdf_path,
         plot = p, width = 1600, height = 1500, dpi = 300, units = "px")
  ggsave(svg_path,
         plot = p, width = 1600, height = 1500, dpi = 300, units = "px")
  # 返回结果列表：包含图形和p值
  return(list(
    plot = p,
    p_nl_str = p_nl,    # 格式化字符串
    p_oa_str = p_oa,    # 格式化字符串
    p_nl_value = p_nl_value,  # 原始数值
    p_oa_value = p_oa_value   # 原始数值
  ))
}

#' 批量 RCS 分析
#'
#' @param data         数据框
#' @param related_index 预测变量向量
#' @param all_outcomes  结局变量向量
#' @param folder_path   根目录
#' @param outcomes_map  结局中文映射 named vector/list
#' @param covariates    协变量向量
#'
#' @return             data.frame：predictor / outcome / model_type / P 值
#' @examples
#' \dontrun{
#' run_analysis_rcs(data, c("SIRI"), c("death"), "./RCS", outcomes_map, covariates)
#' }
run_analysis_rcs <- function(data, related_index, all_outcomes, folder_path, outcomes_map, covariates) {
  # 初始化结果数据框
  results_df <- data.frame(predictor = character(),
                           outcome = character(),
                           model_type = character(),
                           p_nl_value = character(),
                           p_oa_value = character(), stringsAsFactors = FALSE)

  # 遍历相关指标和结局
  for (i in related_index) {
    for (o in all_outcomes) {
      fn <- paste(i, outcomes_map[o], sep = "_")

      # 无模型版本
      result_no_model <- generate_rcs_plot(
        outcome = o,
        predictor = i,
        data = data,
        output_folder = file.path(folder_path, "NoModel"),
        filename = fn
      )

      # 添加结果到数据框
      results_df <- rbind(results_df, data.frame(
        predictor = i,
        outcome = outcomes_map[o],
        model_type = "NoModel",
        p_nl_value = result_no_model$p_nl_value,
        p_oa_value = result_no_model$p_oa_value,
        stringsAsFactors = FALSE
      ))

      # 有模型版本
      result_with_model <- generate_rcs_plot(
        outcome = o,
        predictor = i,
        data = data,
        output_folder = file.path(folder_path, "WithModel4"),
        models = covariates,
        use_model = TRUE,
        filename = fn
      )

      # 添加结果到数据框
      results_df <- rbind(results_df, data.frame(
        predictor = i,
        outcome = outcomes_map[o],
        model_type = "WithModel4",
        p_nl_value = result_with_model$p_nl_value,
        p_oa_value = result_with_model$p_oa_value,
        stringsAsFactors = FALSE
      ))
    }
  }

  # 返回结果数据框
  return(results_df)
}
