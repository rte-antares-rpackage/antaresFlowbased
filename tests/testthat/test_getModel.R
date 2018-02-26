context("getAvailableModel")


test_that("getAvailableModel", {
  models <- getAvailableModel()
  expect_true(is.character(models))
  
})
