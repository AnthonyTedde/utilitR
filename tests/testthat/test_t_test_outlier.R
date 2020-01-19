################################################################################
### dataset mtcars used to perform the following test. Please use the same
### if you want to create new tests. Otherwise update that comment and mention
### which set do you use.
################################################################################


testthat::test_that("recipes are accepted", {
  recipe <- recipes::recipe(hp ~ mpg + cyl, data = mtcars) %>%
    recipes::step_center(recipes::all_predictors()) %>%
    recipes::step_scale(recipes::all_predictors())

  methods <- list(
    list(method = "lm")
  )

  testthat::expect_output(
    subset <- utilitR::t_outlier_test(recipe, data = mtcars, method = methods),
    regexp = "[[:digit:]]+/[[:digit:]]+ rows removed"
  )

  testthat::expect_vector(subset, ptype = logical(), size = nrow(mtcars))
})


testthat::test_that("Formulae are accepted", {
  methods <- list(
    list(method = "lm")
  )

  testthat::expect_output(
    subset <- utilitR::t_outlier_test(hp ~ mpg + cyl,
                                      data = mtcars,
                                      method = methods,
                                      preProcess = c("center", "scale", "nzv")),
    regexp = "[[:digit:]]+/[[:digit:]]+ rows removed"
  )

  testthat::expect_vector(subset, ptype = logical(), size = nrow(mtcars))
})
