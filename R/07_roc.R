#' 批量 ROC 曲线分析、AUC 比较与最佳截断点输出
#'
#' 对单个或多个连续指标（及自定义组合模型）一次性完成：
#' \itemize{
#'   \item ROC 拟合、AUC 及 95%CI 计算
#'   \item 最佳截断值（Youden 指数）
#'   \item 可选 DeLong 检验进行两两 AUC 比较
#'   \item 自动绘制 ROC 曲线并保存 TIFF/SVG
#'   \item 控制台打印灵敏度/特异度/截断值
#' }
#'
#' @param data               \code{data.frame}，必须包含结局变量及所有待评估指标
#' @param outcome            字符标量，二分类结局变量名（0/1）
#' @param index              字符向量，单个连续指标名，将作为 ROC 的 predictor
#' @param index_combined     命名列表，组合模型；如 \code{list(Combined = c("X1","X2"))}，
#'                           函数内部用 \code{glm(..., family = binomial)} 建模，
#'                           以预测概率作为 ROC 的 predictor
#' @param file               输出 ROC 曲线 TIFF 文件名（含路径），默认 \code{"ROC_curve.tiff"}
#' @param width,height,res   图形尺寸与分辨率（像素），默认 2000×2000×300 dpi
#' @param DeLong             逻辑值，是否执行 DeLong 检验，默认 FALSE
#' @param DeLong_fp          字符，DeLong 检验结果保存路径，默认 \code{"./DeLong_result.csv"}
#' @param cols               自定义曲线颜色向量；NULL 时自动采样
#' @param legend_map         命名向量，曲线图例别名，如 \code{c(PIV = "Systemic Inflammation Index")}
#' @param seed               随机种子，确保颜色与 DeLong 抽样可复现，默认 123
#'
#' @return                   列表：
#' \describe{
#'   \item{roc_list}{命名列表，各指标/组合的 \code{pROC} 对象}
#'   \item{auc_summary}{数据框，AUC、95%CI、最佳截断值、灵敏度、特异度}
#' }
#' 同时把 ROC 图写入 TIFF/SVG，控制台打印最佳截断点信息（invisible）
#'
#' @details
#' \itemize{
#'   \item 单个指标直接使用原始值；组合指标先拟合 logistic 再用预测概率
#'   \item 最佳截断值采用 Youden 指数最大点；多解时取第一个
#'   \item DeLong 检验使用 \code{pROC::roc.test(..., method = "delong")}，结果含差值及 CI
#'   \item 图形设备为 TIFF + SVG 双输出，满足期刊矢量/位图要求
#' }
#'
#' @examples
#' \dontrun{
#' roc_indexes(mtcars, "vs", "mpg", file = tempfile(".tiff"))
#' }
roc_indexes <- function(data,
                        outcome,               # 结局列名（字符）
                        index,                 # 单个指标名字向量
                        index_combined = NULL, # list("AB" = c("A","B"))
                        file = "ROC_curve.tiff",
                        width = 2000,
                        height = 2000,
                        res = 300,
                        DeLong = FALSE,        # 新增：是否做 DeLong 检验
                        DeLong_fp = "./DeLong_result.csv",
                        cols = NULL,
                        legend_map = NULL,
                        seed = 123             # 新增：设置随机种子确保结果可复现
) {
  # 设置随机种子确保结果可复现
  set.seed(seed)

  # ---------- 依赖包 ----------
  if (!requireNamespace("pROC", quietly = TRUE))
    stop("请安装 'pROC' 包：install.packages('pROC')")

  # ---------- 检查 ----------
  if (!outcome %in% names(data))
    stop("outcome 列不存在")
  if (!all(index %in% names(data)))
    stop("index 中的某些列不存在")

  # ---------- 建立单个指标的模型 ----------
  roc_list <- list()
  auc_summary <- data.frame(Indicator = character(),
                            AUC = numeric(),
                            Lower_CI = numeric(),
                            Upper_CI = numeric(),
                            Optimal_Cut_off = numeric(),
                            Sensitivity = numeric(),
                            Specificity = numeric(),
                            stringsAsFactors = FALSE)

  for (v in index) {
    # 关键修改：直接使用原始指标值构建ROC对象
    roc_obj <- pROC::roc(
      response = data[[outcome]],
      predictor = data[[v]],  # 使用原始指标值而非预测概率
      ci = TRUE,
      direction = "<"  # 明确方向：值越大阳性概率越高
    )
    roc_list[[v]] <- roc_obj

    # 计算最佳截断值、敏感性和特异性
    coords_result <- pROC::coords(
      roc_obj,
      x = "best",
      ret = c("threshold", "sensitivity", "specificity"),
      best.method = "youden"  # 明确使用Youden指数
    )

    # 处理可能的多解情况
    optimal_cutoff <- if (is.list(coords_result)) {
      coords_result$threshold[1]
    } else {
      coords_result["threshold"]
    }

    auc_summary <- rbind(auc_summary, data.frame(
      Indicator = v,
      AUC = as.numeric(roc_obj$auc),
      Lower_CI = roc_obj$ci[1],
      Upper_CI = roc_obj$ci[3],
      Optimal_Cut_off = optimal_cutoff,
      Sensitivity = coords_result$sensitivity[1],
      Specificity = coords_result$specificity[1]
    ))
  }

  # ---------- 建立组合的模型 ----------
  if (!is.null(index_combined)) {
    for (nm in names(index_combined)) {
      vars <- index_combined[[nm]]
      if (!all(vars %in% names(data)))
        stop(sprintf("组合 %s 中的某些列不存在", nm))
      rhs <- paste(vars, collapse = " + ")
      f <- as.formula(paste(outcome, "~", rhs))
      fit <- glm(f, data = data, family = binomial())
      roc_obj <- pROC::roc(data[[outcome]], fit$fitted.values, ci = TRUE)
      roc_list[[nm]] <- roc_obj

      # 计算最佳截断值、敏感性和特异性
      optimal_cutoff <- pROC::coords(roc_obj, x = "best", ret = "threshold")$threshold
      sensitivity <- pROC::coords(roc_obj, x = optimal_cutoff, ret = "sensitivity")$sensitivity
      specificity <- pROC::coords(roc_obj, x = optimal_cutoff, ret = "specificity")$specificity

      auc_summary <- rbind(auc_summary, data.frame(
        Indicator = nm,
        AUC = roc_obj$auc,
        Lower_CI = roc_obj$ci[1],
        Upper_CI = roc_obj$ci[3],
        Optimal_Cut_off = optimal_cutoff,
        Sensitivity = sensitivity,
        Specificity = specificity
      ))
    }
  }

  # ---------- 颜色 ----------
  if(length(cols)==0){
    cols <- grDevices::colors()[grep("gr(a|e)y|white", grDevices::colors(), invert = TRUE)]
    cols <- sample(cols, length(roc_list))
  }

  # ---------- 画图 ----------
  aucs <- sapply(roc_list, function(r) r$auc)
  plot_order <- order(aucs, decreasing = TRUE)

  # 保存为 TIFF 格式
  tiff(file, width = width, height = height, units = "px", bg = "white", res = res)
  first <- TRUE
  for (i in plot_order) {
    if (first) {
      plot(roc_list[[i]],
           col = cols[i],
           legacy.axes = TRUE,
           xlim = c(1, 0), ylim = c(0, 1),
           font = 2, cex = 1.4, font.lab = 2, font.axis = 2,
           cex.lab = 1.5, cex.axis = 1.5, cex.main = 2)
      first <- FALSE
    } else {
      lines(roc_list[[i]], col = cols[i])
    }
  }
  plot_legend <- names(roc_list)
  if(length(legend_map) > 0){
    plot_legend <- legend_map[plot_legend]
  }
  legend("bottomright",
         legend = plot_legend,
         col = cols,
         lty = 1,
         cex = 1.2)
  dev.off()

  # 保存为 SVG 格式
  svg_file <- gsub("\\.tiff$", ".svg", file)
  svg(svg_file, width = width / res, height = height / res)
  first <- TRUE
  for (i in plot_order) {
    if (first) {
      plot(roc_list[[i]],
           col = cols[i],
           legacy.axes = TRUE,
           xlim = c(1, 0), ylim = c(0, 1),
           font = 2, cex = 1.4, font.lab = 2, font.axis = 2,
           cex.lab = 1.5, cex.axis = 1.5, cex.main = 2)
      first <- FALSE
    } else {
      lines(roc_list[[i]], col = cols[i])
    }
  }
  legend("bottomright",
         legend = plot_legend,
         col = cols,
         lty = 1,
         cex = 1.2)
  dev.off()

  # ---------- DeLong 两两比较 ----------
  if (DeLong) {
    delong_res <- list()
    keys <- names(roc_list)
    n <- length(keys)
    for (i in seq_len(n - 1)) {
      for (j in (i + 1):n) {
        test <- pROC::roc.test(roc_list[[keys[i]]],
                               roc_list[[keys[j]]],
                               method = "delong")
        delong_res[[paste(keys[i], "vs", keys[j])]] <- data.frame(
          Comparison = paste(keys[i], "vs", keys[j]),
          AUC1 = as.numeric(pROC::auc(roc_list[[keys[i]]])),
          AUC2 = as.numeric(pROC::auc(roc_list[[keys[j]]])),
          Difference = as.numeric(pROC::auc(roc_list[[keys[i]]])) -
            as.numeric(pROC::auc(roc_list[[keys[j]]])),
          p.value = as.numeric(test$p.value),
          Lower_CI = test$conf.int[1],
          Upper_CI = test$conf.int[2],
          row.names = NULL
        )
      }
    }
    delong_df <- do.call(rbind, delong_res)
    write.csv(delong_df, file = DeLong_fp, row.names = FALSE)
  }

  # ---------- 打印最佳截断点 ----------
  for (i in names(roc_list)) {
    roc_obj <- roc_list[[i]]
    coords_result <- pROC::coords(
      roc_obj,
      x = "best",
      ret = c("threshold", "sensitivity", "specificity"),
      best.method = "youden"
    )

    optimal_cutoff <- if (is.list(coords_result)) {
      coords_result$threshold[1]
    } else {
      coords_result["threshold"]
    }

    cat(sprintf("Indicator: %s\n", i))
    cat(sprintf("  Optimal Cut-off: %.3f\n", optimal_cutoff))
    cat(sprintf("  Sensitivity: %.3f\n", coords_result$sensitivity[1]))
    cat(sprintf("  Specificity: %.3f\n", coords_result$specificity[1]))
    cat("\n")
  }

  # ---------- 返回对象 ----------
  list(roc_list = roc_list, auc_summary = auc_summary)
}
