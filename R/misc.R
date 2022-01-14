#' Utility function to log formated messages into the console.
#'
#' @param message A character. Message to be printed.
#' @param title logical. If TRUE, a nice hand-crafted frame is painted around the
#' undoubtedly useful message.
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


#' Create names that could be ordered alphabetically
#'
#' @description
#' Create an ordered list of names using padding of incremental values.
#'
#' @param n An integer. The number of variables
#' @param prefix A character. The prefix added before each incremental value.
#' @param sep A character. The separator used between the prefix and the numerical
#' values.
#'
#' @return The function returns a vector of well-defined alphabetically-ordered
#' names.
#' @export
#'
#' @examples
create_alpha_index <- function(n = 10, prefix, sep = "_"){
  M <- floor(log(n, base = 10))
  z <- M - floor(log(1:n, base = 10))
  idx <- paste0(
    sapply(z, FUN = function(times = z) paste(rep(0, times), collapse = "")),
    1:n
  )
  if(!missing(prefix)) {
    paste(prefix, idx, sep = sep)
  }else idx
}


#' Title Turning factor variables from a data.frame into integer.
#'
#' @param data A data.frame in which there are factor to transform.
#'
#' @return
#' @export
#'
#' @examples
factor_to_integer <- function(data){
  factor_col <- names(data)[sapply(data, is.factor)]
  if(length(factor_col > 0)){
    message("Factor additional columns were turned into integer.")
    data[,factor_col] <- lapply(data[, factor_col, drop = F], as.integer)
  }
  return(data)
}
