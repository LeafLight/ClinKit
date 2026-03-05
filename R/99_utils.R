#' Get Global Options for ClinKit
#'
#' Retrieves package-specific global options. If the user has not set a specific
#' option globally (using `options()`), the function returns a provided default value.
#'
#' @param opt_name Character scalar. The name of the option to retrieve (without the "ClinKit." prefix).
#' @param default The default value to return if the option is not found.
#'
#' @return The value of the specified global option, or the default value if it is not set.
#' @keywords internal
get_clinkit_opt <- function(opt_name, default) {
  # Construct the full option name, e.g., "ClinKit.use_timestamp"
  full_name <- paste0("ClinKit.", opt_name)

  # Return user-defined option if set; otherwise, return the default value
  res <- getOption(full_name)
  if (is.null(res)) {
    return(default)
  }

  return(res)
}
#' Generate Unified File Paths
#'
#' An internal utility to generate standardized file paths across the ClinKit package.
#' It automatically handles appending timestamps (based on global package options)
#' and ensures the target output directory exists.
#'
#' @param base_name Character scalar. The core name of the file (e.g., "rcs_summary").
#' @param ext Character scalar. The file extension without the dot (e.g., "csv", "docx").
#' @param output_dir Character scalar. The path to the output directory. Defaults to NULL.
#'
#' @return A character scalar representing the complete file path.
#' @keywords internal
generate_filepath <- function(base_name, ext, output_dir = NULL) {

  # 1. Query global option: append timestamp? (default is TRUE)
  use_timestamp <- get_clinkit_opt("use_timestamp", default = TRUE)

  # 2. Construct the file name
  if (use_timestamp) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    file_name <- sprintf("%s_%s.%s", base_name, timestamp, ext)
  } else {
    file_name <- sprintf("%s.%s", base_name, ext)
  }

  # 3. Append directory path
  if (is.null(output_dir)) {
    return(file_name)
  } else {
    # Ensure the directory exists
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }
    return(file.path(output_dir, file_name))
  }
}
#' Format p-value as plotmath expression string
#'
#' Returns p-values in common journal formats:
#' \itemize{
#'   \item p < 0.001  →  "name < 0.001"
#'   \item 0.001 ≤ p < 0.01  →  scientific notation (with ×10⁻ⁿ)
#'   \item p ≥ 0.01  →  keep 3 decimal places
#' }
#' Outputs as \code{plotmath} expressions that can be directly used in
#' \code{ggplot2::annotate()} or \code{ggpmisc::stat_poly_eq()} label parameters.
#'
#' @param p    Numeric vector, p-values to format (typically 0-1)
#' @param name Character scalar, p-value prefix, e.g., "p", "P interaction", "P trend"
#'
#' @return     Character vector, valid \code{plotmath} expressions, e.g.,
#'             \code{"p < 0.001"}, \code{"p~\"=\"~3.14\\times10^{-4}"},
#'             \code{"p~\"=\"~0.03"}
#'
#' @examples
#' format_p_value(0.0001, "p")
#' @export
format_p_value <- function(p, name) {
  if (p < 0.001) {
    return(paste0(name, ' < 0.001'))
  } else if (p < 0.01) {
    p <-  formatC(p, format = "e", digits = 3)
    p <-  strsplit(p, "e")[[1]]
    return(paste0(name, '~"="~', p[1], '~"\u00d7"~10^', p[3]))
  } else {
    return(paste0(name, '~"="~', round(p, 3)))
  }
}

#' Map variable name to display label
#'
#' Safely map a variable name using an optional named mapping. If the mapping is
#' NULL or the name is not present in the mapping, the original name is returned.
#'
#' @param name Character scalar, the variable name.
#' @param mapping Optional named vector/list, e.g. c("A_B" = "A B").
#'
#' @return A single character string (label).
#' @keywords internal
map_label <- function(name, mapping = NULL) {
  if (is.null(mapping)) {
    return(name)
  }

  val <- mapping[[name]]
  if (is.null(val) || is.na(val)) {
    name
  } else {
    val
  }
}
