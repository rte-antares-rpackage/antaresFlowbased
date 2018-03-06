context("function adq with additionnal binding constraints")




test_that("compares test case results", {
  # get Antares' flow-based parametes

  # test results
  area_test <- dataNoStrat_adq$areas
  area_test$area <- as.character(area_test$area)
  area_test <- area_test[with(area_test, order(mcYear, timeId, area)), ]
  links_test <- dataNoStrat_adq$links
  links_test <- links_test[with(links_test, order(mcYear, timeId, link)), ]
  
  # expected results = results with right number of binding constraints
  if(clpAPI::versionCLP() %in% "1.16.9"){
    expect_true(all(data.frame(area_exp_64b) == data.frame(area_test)) | 
                                 all(data.frame(area_exp_32b) == data.frame(area_test)))
    expect_true(all(data.frame(links_exp_64b) == data.frame(links_test))| 
                                 all(data.frame(links_exp_32b) == data.frame(links_test)))
  }
})


test_that("checks message when unused binding constraints", {
  # get Antares' flow-based parametes
  opts <- list()
  opts$studyPath <- system.file("testdata/adq/antaresStudy37/user/flowbased/ts.txt", package = "antaresFlowbased")
  if(opts$studyPath == "") opts$studyPath <- system.file("inst/testdata/adq/antaresStudy37/user/flowbased/ts.txt", package = "antaresFlowbased")
  opts$studyPath <- gsub("/user/flowbased/ts.txt","" , opts)
  
  
  optsTMP2 <- opts
  class(optsTMP2) <- "simOptions"
  # launch adq patch
  tf <- system.file("testdata/adq/General/studyNoStrat_ini.RDS", package = "antaresFlowbased")
  if(tf == "")  tf <- system.file("inst/testdata/adq/General/studyNoStrat_ini.RDS", package = "antaresFlowbased")
  dataNoStrat_ini <- readRDS(tf)
  dataNoStrat_ini3 <- copy(dataNoStrat_ini)
  expect_message(suppressWarnings(.applyAdq(opts = opts, dataNoStrat_ini3, fb_opts = optsTMP2)))
})
