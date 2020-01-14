library(testthat)
library(utilitR)


testthat::test_that("Return type is correct",{
  out_data_logical <- utilitR::outliars(data = mtcars, remove = F)
  testthat::expect_vector(out_data_logical,
                          ptype = logical(),
                          size = nrow(mtcars))
})

testthat::test_that("Output message is produce", {
    testthat::expect_output(
      utilitR::outliars(data = mtcars),
      regexp = "^[[:digit:]]+/[[:digit:]]+ rows removed$"
    )
})

testthat::test_that("Result is correct", {
  set.seed(1001)
  rand_data <- rnorm(1000)
  bound <- 3 * sd(rand_data)

  expected_data <- rand_data > - bound & rand_data < bound
  obtained_data <- utilitR::outliars(data = data.frame(rand_data), remove = F)

  testthat::expect_identical(obtained_data, expected_data)
})
