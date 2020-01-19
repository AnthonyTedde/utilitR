#' Outlier detection for quantitative data based on standardized Mahalanobis distance
#'
#' @inheritParams outliars
#' @param ... Additional arguments for caret::preProcess.
#'
#' @inherit outliars return
#' @seealso \code{\link[stats]{mahalanobis}}
#' @export
#'
#' @examples
GH_outlier <- function(data,
                       variables = dplyr::everything(),
                       std_err = 3,
                       remove = TRUE,
                       verbose = TRUE,
                       ...){


  ###################################
  # data management and preprocess
  ###################################

  data <- data %>%
    dplyr::select(variables)

  data_pca_preproc <- data %>%
    caret::preProcess(method = c("center", "scale", "nzv", "pca"), ...)

  data_pca <- predict(data_pca_preproc, data) %>%
    tibble::as_tibble()


  ###################################
  # Mahalanobis and GH
  ###################################

  pc_mahalanobis <- data_pca %>%
    mahalanobis(., center = colMeans(.), cov = cov(.))

  ### Standardization
  pc_GH <- pc_mahalanobis / ncol(data_pca)
  row_to_keep <- pc_GH < std_err


  ###################################
  # log Message
  ###################################

  if(verbose){
    line1 = paste0("Outliers: ", sum(!row_to_keep))
    line2 = paste0("Proportion: ", sum(!row_to_keep) / length(row_to_keep))
    message = paste(line1, line2, sep = "\n")
    log_message(message = message)
  }


  ###################################
  # remove -> return the new dataset.
  ###################################

  if(remove)
    data %>%
    dplyr::filter(row_to_keep) %>%
    tibble::as_tibble()
  else row_to_keep

}
