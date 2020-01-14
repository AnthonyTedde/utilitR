#' Workout
#'
#' @description A function to perform models training accross with different
#' data. Simultaneously.
#'
#' @param args All the argumnets to pass through the caret::train function
#' @param seed A seed. For reproducibility.
#' @param subset The row subset of data to use for the analysis. All by default.
#' @param features A subset of regressors to use. All by default.
#'
#' @return A list of caret object.
#'
#' @export
#'
#' @examples
workout <- function(args, seed = 1010, subset = T, features = T){
  args %>% purrr::map(.f = function(arg){
    cat(arg$method)
    cat(arg$data)

    ## Get the data from character vector denoting the variable name
    arg$data <- get(arg$data)[subset, ]

    set.seed(seed)
    do.call(caret::train, arg)
  })
}

workout_prepare <- function(data_lst, method_lst, method_parameter_lst){
  method_name <- names(method_lst)
  data_name <- names(data_lst)
  mdls_names <- do.call(paste,
                        c(expand.grid(data_name, method_name), sep = "_"))
  args <- purrr::cross3(data_lst, method_lst, method_parameter_lst)
  # %>%
  #   purrr::map(purrr::flatten)
  return(list(
    args = args,
    mdls_names = mdls_names
  ))
}
