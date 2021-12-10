#' Title
#'
#' @param message
#' @param title
#'
#' @return
#' @export
#'
#' @examples
log_message <- function(message, title = T){

  if(title){
    cat(paste(rep("*", 50), collapse = ""), fill = T)
    cat(message, fill = T)
    cat(paste(rep("*", 50), collapse = ""), fill = T)
  }else{
    cat(message, fill = T)
  }

  flush.console()
}
