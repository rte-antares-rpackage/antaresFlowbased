test_that("test graph", {
  fb_opts <- setFlowbasedPath(model = "model2017")
  res <- plotFB(1,1,"FR","NL")
  expect_true("combineWidgets" %in% class(res))
})