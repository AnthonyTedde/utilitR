testthat::test_that("GH outlier produce an output message", {
  testthat::expect_output(
    utilitR::GH_outlier(trees, remove = F),
    regexp = "^\\*+\nOutliers:[[:blank:]]{1}[[:digit:]]+"
  )
})


testthat::test_that("Return type is correct",{
  out_data_logical <- utilitR::GH_outlier(trees, remove = F)
  testthat::expect_vector(out_data_logical,
                          ptype = logical(),
                          size = nrow(trees))
})
