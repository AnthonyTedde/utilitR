#' Title Check if character variables
#'
#' @description
#' If there are at least one character variables into the data.frame `data`,
#' this function will raise an error and force to stop the computation.
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
check_character <- function(data){
  if(any(sapply(data, is.character))){
    stop("Transform character columns to factor")
  }
}
