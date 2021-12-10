testthat::test_that("get_validation_set return", {

  utilitR::get_validation_set(data = mtcars)

  testthat::expect_output(
    utilitR::GH_outlier(trees, remove = F),
    regexp = "^\\*+\nOutliers:[[:blank:]]{1}[[:digit:]]+"
  )
})
