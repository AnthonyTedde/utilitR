#' Run a t-outlier test (S3)
#'
#' @description
#' The purpose of a t-outlier test is to detect potential outliers from the analysis of residuals based on a first predictive analysis.
#'
#' @aliases t_outlier_test.formula t_outlier_test.recipe
#'
#' @inheritParams outliars
#' @param x Either a recipe or a formula. Depending on x type, the appropriate
#'          fuction is called.
#' @param method A list of lists. Each sublists contain the method name and some related arguments.
#' @param ... Additional arguments for caret::train.
#'
#' @inherit outliars return
#'
#' @author Anthony Tedde
#'
#' @seealso \code{\link[caret]{train}}, \code{\link[recipes]{recipe}}
#'
#' @export t_outlier_test
#'
#'
#' @examples
"t_outlier_test" <- function(x, data, ...){
  UseMethod("t_outlier_test")
}


#' @rdname t_outlier_test
#'
#' @export
#'
t_outlier_test.default <- function(...){
  stop("`x` should be a formula or a recipe", call. = F)
}


#' @inheritParams outliars
#'
#' @rdname t_outlier_test
#'
#' @export
#'
t_outlier_test.formula <- function(x,
                                   data,
                                   method,
                                   std_err = 3,
                                   remove = TRUE,
                                   verbose = TRUE,
                                   ...){

  f <- dplyr::enquo(x) %>%
    dplyr::as_label() %>% formula
  t_outlier_test_internal(x = f,
                          data = data,
                          method = method,
                          std_err = std_err,
                          remove = remove,
                          verbose = verbose,
                          ...)
}


#' @rdname t_outlier_test
#'
#' @export
#'
t_outlier_test.recipe <- function(x,
                                  data,
                                  method,
                                  std_err = 3,
                                  remove = TRUE,
                                  verbose = TRUE,
                                  ...){

  t_outlier_test_internal(x = x,
                          data = data,
                          method = method,
                          std_err = std_err,
                          remove = remove,
                          verbose = verbose,
                          ...)

}


#' @keywords internal
#'
t_outlier_test_internal <- function(x,
                                   data,
                                   method,
                                   std_err = 3,
                                   remove = TRUE,
                                   verbose = TRUE,
                                   ...){

  method %>% purrr::map(.f = function(m){

    outliers <- rep(T, nrow(data))

    if(verbose)
      log_message(message = m$method)

    repeat{

      # `x` is either a formula or a recipe
      arguments <- c(
        list(
          x,
          data = data[outliers, ]
        ), m, list(...)
      )

      ### call caret::train
      model_calibration <- do.call(caret::train, arguments)

      predicted_data <- predict(model_calibration, data[outliers, ])
      Y <- outcome(model_calibration)

      residuals <- Y - predicted_data

      outliers_current <- outliars(data.frame(residuals),
                                   std_err = std_err,
                                   remove = F)

      if(all(outliers_current)) break
      else outliers[which(outliers)[which(!outliers_current)]] <- F


    } # End of the repeat loop
    return(outliers)
  }) %>%
    Reduce(f = `*`) %>% as.logical

}


#' @keywords internal
outcome <- function(x){
  UseMethod("outcome")
}


#' @keywords internal
outcome.train.recipe <- function(x){
  x$recipe$last_term_info %>%
    dplyr::filter(role == "outcome") %>%
    dplyr::pull(variable) %>%
    `[[`(x$trainingData, .)
}


#' @keywords internal
outcome.train.formula <- function(x){
  x$trainingData %>%
    dplyr::pull(.outcome)
}
