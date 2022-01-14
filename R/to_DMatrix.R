#' Turning data.frame to xgboost::xgb.DMatrix format.
#'
#' @param plsmod PLS model computed using \code{\link[pls]{mvr}}.
#' @param data Data.frame
#' @param additional_columns Vector of characters. The list of variable to be
#' kept from the `data`.
#' @param threshold Integer. Minimum cumulative variance that must be explained
#' by the PLS model components.
#' @param y_name Character. Response variable name.
#'
#' @return Matrix of the form \code{\link[xgboost]{xgb.DMatrix}}
#' @export
#'
#' @examples
df_to_DMatrix <- function(plsmod,
                          data,
                          additional_columns,
                          threshold = .99,
                          y_name){
  if(!missing(plsmod)){
    cumvar <- cumsum(plsmod$Xvar / plsmod$Xtotvar)
    Mcomp <- which(cumvar>.99)[1]
    X <- structure(
      data.frame(predict(plsmod,
                         newdata = data,
                         ncomp = 1:Mcomp,
                         type = "scores")),
      names = create_alpha_index(Mcomp, prefix = "pls", sep = "")
    )
    if(!missing(additional_columns)){
      X <- cbind(data[, additional_columns], X)
    }
  }else{
    if(missing(additional_columns)){
      stop("at least plsmod or additional_columns argument should be provided")
    }
    X <- data[, additional_columns]
  }
  check_character(X)
  X <- factor_to_integer(X)
  X <- as.matrix(X)
  xgboost::xgb.DMatrix(X, label = data[, y_name, drop = T])
}
