#' Compute the barycenter distance
#'
#' @param master
#' @param ...
#' @param column
#'
#' @return
#' @export
#'
#' @examples
barycenter_distance <- function(master,
                                ...,
                                column = dplyr::everything())
{

  slave_dfs <- list(...)


  ###############################
  # Define the name of the slave
  ###############################

  # if no name
  if(is.null(names(slave_dfs))){
    names(slave_dfs) <- paste0(
      "sl",
      stringr::str_pad(1:length(slave_dfs),
                       width = ceiling(log10(length(slave_dfs))),
                       pad = '0')
    )
  }


  dfs_lst <- c(list(master), slave_dfs) %>%
    purrr::map(dplyr::select, column) %>%
    setNames(nm=c('master', names(slave_dfs)))

  master_preproc <- dfs_lst$master %>%
    caret::preProcess(method = c("center", "scale", "nzv", "pca"))

  dfs_pca_lst <- dfs_lst %>%
    purrr::map(predict, object = master_preproc)


  master_c.5 <- cov(dfs_pca_lst$master) / 2

  covariance_lst <- dfs_pca_lst[-1] %>%
    purrr::map(function(x){
      cov(x) / 2 + master_c.5
    })

  pc_test_mahalanobis_lst <- dfs_pca_lst[-1] %>%
    purrr::map2(covariance_lst, mahalanobis,
               center = colMeans(dfs_pca_lst$master))

  ### Standardization

  purrr::map2(pc_test_mahalanobis_lst,
              purrr::map(dfs_pca_lst[-1], ncol),
              ~.x / .y)

}
