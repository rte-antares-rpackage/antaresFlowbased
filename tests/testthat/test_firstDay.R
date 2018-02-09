context("found first day")


test_that("test .giveFstDay", {
  vect <- c(1:7)
  expect_equal(.giveFstDay(vect), 3)
  vect <- c(2,3,4,5,6,7,1)
  expect_equal(.giveFstDay(vect), 2)
  vect <- c(1,3,4,5,6,7,2)
  expect_equal(.giveFstDay(vect), 2)
  
  vect <- c(8,3,4,5,6,1,2)
  expect_equal(.giveFstDay(vect), 1)
  
  vect <- c(8,3,4,5,6,2,1)
  expect_equal(.giveFstDay(vect), 1)
  
  vect <- c(8,3,1,1,6,2,2)
  expect_equal(.giveFstDay(vect), 5)
  
  vect <- c(8,3,1,4,6,2,2)
  expect_error(.giveFstDay(vect))

})


test_that("test identifyFirstDay", {
  expect_error(suppressWarnings(identifyFirstDay(opts, "mynoarea", "de")))
  
})