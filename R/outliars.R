#' Outlier detection for quantitative data
#'
#' @param data Data.frame. Candidate data for outliers detection.
#' @param std_err Integer. Which specifies how far from the mean a sample should be for being considered as an outlier.
#' @param remove Boolean. Default TRUE. If TRUE, then the function returns a new dataset minus the assessed outliers. Otherwise, it returns a logical atomic vector specifying -  by the boolean value False - which data is an outlier.
#' @param verbose Boolean. Default TRUE.  If TRUE, some of the intermediate results are output to the console.
#'
#' @return See remove argument.
#' @export
#'
#' @examples
outliars <- function(data,
                     std_err = 3,
                     remove = TRUE,
                     verbose = TRUE){


  outliars_subset <- data %>%
    dplyr::select_if(is.numeric) %>%
    purrr::map(
      ~as.list(
        mean(.x, na.rm = T) + c(-1, 1) * std_err * sd(.x, na.rm = T)) %>%
        setNames(nm = c("lower_bound", "upper_bound")
        )
    ) %>%
    purrr::imap(
      ~data[[.y]] > .x$lower_bound & data[[.y]] < .x$upper_bound
    ) %>%
    purrr::reduce(`*`) %>% as.logical()


  if(verbose){
    message <- c(
      paste0(sum(!outliars_subset), "/", length(outliars_subset)),
      "rows removed"
    )
    utilitR::log_message(message = message, title = F)
  }


  if(remove)
    dplyr::filter(data, outliars_subset)
  else outliars_subset
}

#' Title
#'
#' @param formula depedent var ~ independent ones.
#' @param data Candidate data.
#' @param method List of methods to apply.
#' @param ...
#'
#' @return a logical atomic vector of row outliers based on the combination of
#' all performed t-test outliers detection.
#' @export
#'
#' @examples
t_test_outlier <- function(formula, data, method, verbose = T, ...){


  ###################################
  # Create the formula based on y
  ###################################

  f <- dplyr::enquo(formula) %>%
    dplyr::as_label() %>% formula

  method %>% purrr::map(.f = function(m){

    outliers <- rep(T, nrow(data))
    ctrl <- caret::trainControl(method = "cv")

    if(verbose)
      utilitR::log_message(message = m$method)

    repeat{

      arguments <- c(list(
        form = f,
        data = data[outliers, ],
        preProcess = c("center", "scale", "nzv"),
        trControl = ctrl
      ), m)

      data_model <- do.call(caret::train, arguments)

      data_prediction <- predict(data_model, data[outliers, ])
      residuals <- data_model$trainingData$.outcome - data_prediction

      outliers_current <- utilitR::outliars(data.frame(residuals), remove = F)

      if(all(outliers_current)) break
      else outliers[which(outliers)[which(!outliers_current)]] <- F


    } # End of the repeat loop
    return(outliers)
  }) %>%
    Reduce(f = `*`) %>% as.logical
}

#' Remove row outliers based on the standardized Mahalanobis distance of
#' data points.
#'
#' @inheritParams outliars
#' @param std_err Threshold above which data is considered as outlier.
#' @param ... Argument to pass through caret::preProcess function. e.g.
#' thresh. (see caret::preProcess)
#'
#' @return
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

  pc_GH <- pc_mahalanobis / ncol(data_pca)
  row_to_keep <- pc_GH < std_err


  ###################################
  # log Message
  ###################################

  if(verbose){
    line1 = paste0("Outliers: ", sum(!row_to_keep))
    line2 = paste0("Proportion: ", sum(!row_to_keep) / length(row_to_keep))
    message = paste(line1, line2, sep = "\n")
    utilitR::log_message(message = message)
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
