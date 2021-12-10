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
