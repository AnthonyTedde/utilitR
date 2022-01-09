#' Create k-folds partition of a training data.frame.
#'
#' @param X The data.frame to be partitioned.
#' @param k Integer. The number of folds
#' @param strata Character. The name of the column to stratify the data.frame
#' with.
#' @param seed A number. The random seed.
#'
#' @return
#' @export
#'
#' @examples
create_kfold <- function(X, k = 10, strata, seed){
  if(!missing(seed)) set.seed(seed)

  # -- Closure function that create the data partitions -- ####
  create_kfold_closure <- function(x){
    partition_size <- floor(x / k)
    remainder <- x %% k
    c(rep(1:k, partition_size), sample(k, size = remainder))
  }
  # -- Closure function that create the data partitions -- #

  # -- Do the data partitions need to be stratified ? -- ####
  if(missing(strata)){
    K <- create_kfold_closure(nrow(X))
    sample(K, size = length(K))
  }else{
    id <- X[[strata]]
    id_ord <- order(id)
    a <- lapply(table(id), create_kfold_closure)
    K <- Reduce(c, a)[order(id_ord)]
  }
  # -- Do the data partitions need to be stratified ? -- #

  # -- Output kfolds with names -- ####
  structure(split(1:length(K), K),
            names = create_alpha_index(n = k, prefix = "split"))
  # -- Output kfolds with names -- #
}
