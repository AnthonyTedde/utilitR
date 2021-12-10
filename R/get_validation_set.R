#' Get validation set
#'
#' Creation of validation sets based on some criterions, such as the proportion
#' of data going to the validation set `bound` ans
#'
#' @param data Data.frame. Candidate data.frame.
#' @param variables Character. What variables to consider in PCA.
#' @param bound Integer. Lower and upper boundary of data proportion to
#' be covered by the validation set.
#' @param group Character. Pivot variable which discriminate each group.
#' @param predicate Function. Function to use for subset selection
#'
#' @return
#' @export
#'
#' @examples
get_validation_set <- function(data,
                               variables = dplyr::everything(),
                               bound = c(0.10, 0.20),
                               group){

  ##############################################################################
  # Create and check boundary
  # TODO:
  #       * Check if boundary are between 0 and 1
  #       * Not only consider a proportion but also a number of data
  ##############################################################################

  lower <- min(bound)
  upper <- max(bound)
  lower <- ifelse(lower == upper, 0, lower)



  ##############################################################################
  #
  ##############################################################################

  data_prop <- data %>%
    dplyr::count(!!!dplyr::syms(group)) %>%
    dplyr::mutate(prop = n/sum(n))

  groupset_combination_lst <- seq_along(data_prop[[group]]) %>%
    purrr::map(combn, x = data_prop[[group]], simplify = F) %>%
    purrr::reduce(c)

  groupset_combination_fnl_lst <- groupset_combination_lst

  sub_data_lst <- groupset_combination_fnl_lst %>%
    purrr::map(~dplyr::filter(data_prop, data_prop[[group]] %in% .x))


  subset_lgl <- sub_data_lst %>%
    purrr::map(dplyr::pull, prop) %>%
    purrr::map(sum) %>%
    purrr::map_lgl(~ (.x > lower & .x < upper))

  subset_final <- sub_data_lst[subset_lgl] %>%
    purrr::map(dplyr::pull, group)

  scores_lst <- subset_final %>%
    purrr::map(.f = function(x){

      ###################################
      # data split
      ###################################
      data_split_lst <- data %>%
        dplyr::mutate(pivot = data[[group]] %in% x) %>%
        dplyr::group_by(pivot) %>%
        dplyr::group_split() %>%
        purrr::map(dplyr::select, variables) %>%
        setNames(nm = c("train", "test"))

      # Just in case...
      if(length(data_split_lst$train) < length(data_split_lst$test))
        names(data_split_lst) <- rev(names(data_split_lst))

      ###################################
      # PCA on train
      ###################################

      data_train_pca_preproc <- data_split_lst$train %>%
        caret::preProcess(method = c("center", "scale", "nzv", "pca"))

      data_train_pca <- predict(data_train_pca_preproc, data_split_lst$train) %>%
        tibble::as_tibble()

      data_test_pca <- predict(data_train_pca_preproc, data_split_lst$test) %>%
        tibble::as_tibble()

      covariance <- cov(data_test_pca)/2 + cov(data_train_pca)/2

      pc_test_mahalanobis <- data_test_pca %>%
        mahalanobis(
          .,
          center = colMeans(data_train_pca),
          cov = covariance
        )

      ### Standardization
      pc_test_mahalanobis / ncol(data_test_pca)

    })


  return(list(
    subsets = subset_final,
    score = scores_lst
  ))
}
