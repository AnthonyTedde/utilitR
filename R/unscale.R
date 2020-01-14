#' Unscaled scaled data according to the former ones.
#'
#' @param scaled_data The data to by unscaled.
#' @param template The template data for unscaling.
#'
#' @return A data.frame of unscaled data.
#' @export
#'
#' @examples
unscale <- function(scaled_data, template){
  purrr::pmap_dfc(list(scaled_data, template), .f = function(s, t){
    if(is.numeric(s))
      s * sd(t, na.rm = T) + mean(t, na.rm = T)
    else s
  })
}
