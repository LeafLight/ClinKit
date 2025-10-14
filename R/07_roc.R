#' ROC Curve Analysis with AUC Comparison and Optimal Cutoff
#'
#' Performs comprehensive ROC analysis for single or multiple continuous indicators
#' and custom combination models, including AUC calculation, optimal cutoff determination,
#' and DeLong test for AUC comparisons.
#'
#' @param data Data frame containing outcome and all predictor variables
#' @param outcome Character scalar, binary outcome variable name (0/1)
#' @param predictors Character vector, continuous predictor variable names
#' @param combined_models Named list of combined models, e.g., list(Combined = c("X1", "X2"))
#' @param output_dir Output directory for saving results, default NULL
#' @param save_format Save format: "none", "plot", "data", "all", default "none"
#' @param delong_test Logical, whether to perform DeLong test for pairwise AUC comparisons, default FALSE
#' @param colors Custom color vector for ROC curves; if NULL, colors are automatically sampled
#' @param legend_labels Named vector for custom legend labels, e.g., c(PIV = "Systemic Inflammation Index")
#' @param seed Random seed for reproducibility, default 123
#' @param plot_width Plot width in pixels, default 2000
#' @param plot_height Plot height in pixels, default 2000
#' @param plot_res Plot resolution in DPI, default 300
#' @param direction roc direction, "auto" for automatic selection, default `<`
#' @return List containing ROC objects, AUC summary, and optional saved file paths
#' @export
roc_analysis <- function(data,
                        outcome,
                        predictors,
                        combined_models = NULL,
                        output_dir = NULL,
                        save_format = c("none", "plot", "data", "all"),
                        delong_test = FALSE,
                        colors = NULL,
                        legend_labels = NULL,
                        seed = 123,
                        plot_width = 2000,
                        plot_height = 2000,
                        plot_res = 300,
                         direction = "<") {

  # Parameter validation
  save_format <- match.arg(save_format)
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  # Check dependencies
  if (!requireNamespace("pROC", quietly = TRUE)) {
    stop("Please install.packages('pROC')")
  }

  # Check if variables exist
  all_vars <- unique(c(outcome, predictors, unlist(combined_models)))
  missing_vars <- setdiff(all_vars, names(data))
  if (length(missing_vars) > 0) {
    stop("Variables not found in data: ", paste(missing_vars, collapse = ", "))
  }

  # Set random seed for reproducibility
  set.seed(seed)

  # Initialize results
  roc_list <- list()
  saved_files <- character(0)

  # Initialize AUC summary data frame
  auc_summary <- data.frame(
    predictor = character(),
    auc = numeric(),
    lower_ci = numeric(),
    upper_ci = numeric(),
    optimal_cutoff = numeric(),
    sensitivity = numeric(),
    specificity = numeric(),
    stringsAsFactors = FALSE
  )

  # ---- Analyze individual predictors ----
  for (predictor in predictors) {
    roc_obj <- tryCatch({
      pROC::roc(
        response = data[[outcome]],
        predictor = data[[predictor]],
        ci = TRUE,
        direction = direction
      )
    }, error = function(e) {
      warning(sprintf("ROC analysis failed for %s: %s", predictor, e$message))
      return(NULL)
    })

    if (is.null(roc_obj)) next

    roc_list[[predictor]] <- roc_obj

    # Calculate optimal cutoff and performance metrics
    coords_result <- tryCatch({
      pROC::coords(
        roc_obj,
        x = "best",
        ret = c("threshold", "sensitivity", "specificity"),
        best.method = "youden"
      )
    }, error = function(e) {
      warning(sprintf("Coordinate calculation failed for %s: %s", predictor, e$message))
      return(NULL)
    })

    if (is.null(coords_result)) next

    # Handle multiple solutions
    optimal_cutoff <- if (is.list(coords_result)) {
      coords_result$threshold[1]
    } else {
      coords_result["threshold"]
    }

    auc_summary <- rbind(auc_summary, data.frame(
      predictor = predictor,
      auc = as.numeric(roc_obj$auc),
      lower_ci = roc_obj$ci[1],
      upper_ci = roc_obj$ci[3],
      optimal_cutoff = optimal_cutoff,
      sensitivity = coords_result$sensitivity[1],
      specificity = coords_result$specificity[1],
      stringsAsFactors = FALSE
    ))
  }

  # ---- Analyze combined models ----
  if (!is.null(combined_models)) {
    for (model_name in names(combined_models)) {
      model_vars <- combined_models[[model_name]]

      # Fit logistic regression model
      fit <- tryCatch({
        formula <- as.formula(paste(outcome, "~", paste(model_vars, collapse = " + ")))
        glm(formula, data = data, family = binomial())
      }, error = function(e) {
        warning(sprintf("Model fitting failed for %s: %s", model_name, e$message))
        return(NULL)
      })

      if (is.null(fit)) next

      # Create ROC object from predicted probabilities
      roc_obj <- tryCatch({
        pROC::roc(data[[outcome]], fit$fitted.values, ci = TRUE)
      }, error = function(e) {
        warning(sprintf("ROC analysis failed for %s: %s", model_name, e$message))
        return(NULL)
      })

      if (is.null(roc_obj)) next

      roc_list[[model_name]] <- roc_obj

      # Calculate performance metrics
      coords_result <- tryCatch({
        pROC::coords(
          roc_obj,
          x = "best",
          ret = c("threshold", "sensitivity", "specificity"),
          best.method = "youden"
        )
      }, error = function(e) {
        warning(sprintf("Coordinate calculation failed for %s: %s", model_name, e$message))
        return(NULL)
      })

      if (is.null(coords_result)) next

      optimal_cutoff <- if (is.list(coords_result)) {
        coords_result$threshold[1]
      } else {
        coords_result["threshold"]
      }

      auc_summary <- rbind(auc_summary, data.frame(
        predictor = model_name,
        auc = as.numeric(roc_obj$auc),
        lower_ci = roc_obj$ci[1],
        upper_ci = roc_obj$ci[3],
        optimal_cutoff = optimal_cutoff,
        sensitivity = coords_result$sensitivity[1],
        specificity = coords_result$specificity[1],
        stringsAsFactors = FALSE
      ))
    }
  }

  # ---- Generate ROC plot ----
  if (length(roc_list) > 0) {
    # Generate colors if not provided
    if (is.null(colors)) {
      available_colors <-  ggsci::pal_npg("nrc")(10)
      colors <- sample(available_colors, length(roc_list))
    }
    # Order by AUC (descending)
    aucs <- sapply(roc_list, function(r) r$auc)
    plot_order <- order(aucs, decreasing = TRUE)

    # Prepare legend labels
    plot_legend <- names(roc_list)
    if (!is.null(legend_labels)) {
      plot_legend <- legend_labels[plot_legend]
      plot_legend[is.na(plot_legend)] <- names(roc_list)[is.na(plot_legend)]
    }
  }
  # legend with auc
  plot_legend_with_auc <- sprintf("%s (AUC=%.3f)",
                               plot_legend[plot_order],
                               as.numeric(sapply(roc_list[plot_order], function(x) x$auc)))
  # ---- Perform DeLong test if requested ----
  delong_results <- NULL
  if (delong_test && length(roc_list) > 1) {
    delong_results <- list()
    keys <- names(roc_list)
    n <- length(keys)

    for (i in seq_len(n - 1)) {
      for (j in (i + 1):n) {
        test <- tryCatch({
          pROC::roc.test(roc_list[[keys[i]]], roc_list[[keys[j]]], method = "delong")
        }, error = function(e) {
          warning(sprintf("DeLong test failed for %s vs %s: %s", keys[i], keys[j], e$message))
          return(NULL)
        })

        if (is.null(test)) next

        delong_results[[paste(keys[i], "vs", keys[j])]] <- data.frame(
          comparison = paste(keys[i], "vs", keys[j]),
          auc1 = as.numeric(pROC::auc(roc_list[[keys[i]]])),
          auc2 = as.numeric(pROC::auc(roc_list[[keys[j]]])),
          difference = as.numeric(pROC::auc(roc_list[[keys[i]]])) -
            as.numeric(pROC::auc(roc_list[[keys[j]]])),
          p_value = as.numeric(test$p.value),
          lower_ci = test$conf.int[1],
          upper_ci = test$conf.int[2],
          stringsAsFactors = FALSE
        )
      }
    }

    if (length(delong_results) > 0) {
      delong_results <- do.call(rbind, delong_results)
    } else {
      delong_results <- NULL
    }
  }

  # ---- Save results if requested ----
  if (save_format != "none" && !is.null(output_dir)) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }

    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

    if (save_format %in% c("data", "all")) {
      # Save AUC summary
      auc_file <- file.path(output_dir, sprintf("roc_auc_summary_%s.csv", timestamp))
      utils::write.csv(auc_summary, auc_file, row.names = FALSE)
      saved_files <- c(saved_files, auc_file)

      # Save DeLong test results
      if (!is.null(delong_results)) {
        delong_file <- file.path(output_dir, sprintf("roc_delong_test_%s.csv", timestamp))
        utils::write.csv(delong_results, delong_file, row.names = FALSE)
        saved_files <- c(saved_files, delong_file)
      }
    }

    if (save_format %in% c("plot", "all") && length(roc_list) > 0) {
      # Save plots
      tiff_file <- file.path(output_dir, sprintf("roc_curve_%s.tiff", timestamp))
      svg_file <- file.path(output_dir, sprintf("roc_curve_%s.svg", timestamp))

      # Save TIFF
      grDevices::tiff(tiff_file, width = plot_width, height = plot_height,
                     units = "px", bg = "white", res = plot_res)

      first <- TRUE
      for (i in plot_order) {
        if (first) {
          pROC::plot.roc(roc_list[[i]],
                         col = colors[i],
                         legacy.axes = TRUE,
                         xlab = "1 - Specificity",
                         ylab = "Sensitivity",
                         font = 2, cex = 1.4, font.lab = 2, font.axis = 2,
                         cex.lab = 1.5, cex.axis = 1.5, cex.main = 2)
          first <- FALSE
        } else {
          pROC::lines.roc(roc_list[[i]], col = colors[i])
        }
      }
      graphics::legend("bottomright",
                      legend = plot_legend_with_auc,
                      col = colors[plot_order],
                      lty = 1,
                      cex = 1.2)
      grDevices::dev.off()
      print(plot_legend_with_auc)

      # Save SVG
      grDevices::svg(svg_file, width = plot_width / plot_res, height = plot_height / plot_res)
      first <- TRUE
      for (i in plot_order) {
        if (first) {
          pROC::plot.roc(roc_list[[i]],
                         col = colors[i],
                         legacy.axes = TRUE,
                         xlab = "1 - Specificity",
                         ylab = "Sensitivity",
                         font = 2, cex = 1.4, font.lab = 2, font.axis = 2,
                         cex.lab = 1.5, cex.axis = 1.5, cex.main = 2)
          first <- FALSE
        } else {
          pROC::lines.roc(roc_list[[i]], col = colors[i])
        }
      }
      graphics::legend("bottomright",
                      legend = plot_legend_with_auc,
                      col = colors[plot_order],
                      lty = 1,
                      cex = 1.2)
      grDevices::dev.off()

      saved_files <- c(saved_files, tiff_file, svg_file)
    }
  }

  # ---- Print optimal cutoff information ----
  if (nrow(auc_summary) > 0) {
    message("Optimal Cutoff Points:")
    for (i in 1:nrow(auc_summary)) {
      message(sprintf("  %s: cutoff = %.3f, sensitivity = %.3f, specificity = %.3f",
                     auc_summary$predictor[i],
                     auc_summary$optimal_cutoff[i],
                     auc_summary$sensitivity[i],
                     auc_summary$specificity[i]))
    }
  }

  # ---- Return structured results ----
  return(list(
    roc_objects = roc_list,
    auc_summary = auc_summary,
    delong_test = delong_results,
    saved_files = if (length(saved_files) > 0) saved_files else NULL,
    call = match.call()
  ))
}


########test
# result <- roc_analysis(
#   data = mtcars,
#   outcome = "vs",
#   predictors = c("mpg", "wt", "disp"),
#   combined_models = list(Combined = c("mpg", "wt")),
#   output_dir = "./test_output",
#   save_format = "all",
#   delong_test = TRUE,
#   direction = "auto"
# )