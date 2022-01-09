#' Filter sample out using a standardized Mahalanobis distance threshold
#'
#' @description
#' The function uses \code{\link[FactoMineR]{PCA}} to compute the principal
#' components. Then the Global H distance is computed from the projection
#' of the sample to the new coordinates. Each sample farther from the `threshold`
#' value from the barycenter will be noticed.
#' A vector of logical values with the same length as the number of rows of
#' the data.frame provided would be returned. FALSE value in the output
#' vector points out the corresponding records as beyond the threshold.
#'
#' @param X A data.frame of samples.
#' @param Y Another data.frame to which the subsetting is performed.
#' If `Y` is missing, the subsetting is made on `X`.
#' @param variables A vector of selection of variables on which PCA is performed.
#' Not mandatory. Could be logical(same size as the number of variables in the
#' data.frame) or indexes.
#' @param threshold A numeric. Distance below which the sample is accepted.
#' @param ncp An integer. Number of components to keep while performing PCA.
#' @param variance A numeric in [0, 1]
#' Total cumulative percentage of variance to be explained by
#' the new latent variables. Not mandatory.
#' @param verbose If TRUE, the function would talk to you.
#' @param ... Not currently used.
#'
#' @details If the argument `variance` is missing, the function uses the
#' first `ncp` principal components to compute the GH distance. Otherwise,
#' if the `variance` is provided, the function keeps as many components needed
#' to explain the value passed through the argument `variance` of the cumulated
#' variance of the initial variables.
#' @details If `variable` argument is missing, no subsetting is performed.
#'
#'
#' @return A vector of logical values (GH < threshold).
#' @seealso \code{\link[FactoMineR]{PCA}}
#' @seealso \code{\link[stats]{mahalanobis}}
#' @export
#'
#' @examples

GH_filter <- function(X,
                      Y,
                      variables,
                      threshold = ifelse(missing(Y), 5, 3),
                      ncp = 5,
                      variance,
                      verbose = TRUE,
                      ...){

  if(!missing(variables)){
    X <- X[, variables]
  }

  # -- PCA computation -- ####
  pca_std <- FactoMineR::PCA(X, ncp = ncp, graph = F)
  if(!missing(variance)){
    if(variance > 1){
      warning("Variance cannot be greater than one.")
    }else{
      cumvar <- (pca_std$eig[, 3]/100)
      ncp <- (1:length(X))[cumvar >= variance][1]
      pca_std <- FactoMineR::PCA(X, ncp = ncp, graph = F)
    }
  }
  # -- PCA computation -- #

  # -- Mahalanobis computation -- ####
  if(!missing(Y)) X <- Y
  X_coord <- predict(pca_std, newdata = X)[["coord"]]
  X_mahalanobis <- mahalanobis(
    X_coord,
    center = colMeans(X_coord),
    cov = cov(X_coord)
  )

  GH <- X_mahalanobis / pca_std$call$ncp
  filter <- GH < threshold
  # -- Mahalanobis computation -- #

  # -- Logging -- ####
  # * Number of removed sample
  # * Proportion of removed
  # * Number of latent variables kept
  # * Percentage of cumulative variance explained by the new orthogonal axes
  if(verbose){
    line1 <- paste0("# of removed sample: ", sum(!filter))
    line2 <- paste0("Proportion of removed: ",
                    sum(!filter) / length(filter))
    line3 = paste0("# of components: ", pca_std$call$ncp)
    line4 = paste0("% of explained variance: ",
                   round(pca_std$eig[pca_std$call$ncp, 3, drop = T], 2))
    message = paste(line1, line2, line3, line4, sep = "\n")
    log_message(message = message)
  }
  # -- Logging -- #

  return(filter)
}
