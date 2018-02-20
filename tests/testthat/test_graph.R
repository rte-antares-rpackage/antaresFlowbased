context("graphs")

test_that("test graph", {
  fb_opts <- setFlowbasedPath(model = "model2017")
  res <- plotFB(1,1,"FR","NL")
  expect_true("combineWidgets" %in% class(res))
  
  res <- plotFB(1,1,"NL","FR")
  
  expect_true("combineWidgets" %in% class(res))
})


test_that("test positionViz", {


  dta <- antaresRead::readAntares(areas = c("fr", "be", "de", "nl"), 
                                  links = c("be - de","be - fr","be - nl",
                                            "de - fr","de - nl"), mcYears = 2,
                                  select = c("LOLD", "UNSP. ENRG", 
                                             "DTG MRG", "UNSP. ENRG", "BALANCE", "FLOW LIN."),
                                  opts = testSt )
  
  ## plot a domain and the matching output points 
 res <- positionViz(opts = testSt, 
              data = dta,
              dayType = 1, hour =19:20, 
              ctry1 = "BE", ctry2 = "FR")
  
  
  expect_true("AmChart" %in% class(res))
})




 