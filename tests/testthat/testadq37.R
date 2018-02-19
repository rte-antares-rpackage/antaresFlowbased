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
  fil <- system.file("testdata/adq/General/studyNoStrat_adq.RDS", package = "antaresFlowbased")
  if(fil == "") fil <- system.file("inst/testdata/adq/General/studyNoStrat_adq.RDS", package = "antaresFlowbased")
  outNoStrat_exp <- readRDS(fil)
  area_exp <- outNoStrat_exp$areas
  area_exp <- area_exp[with(area_exp, order(mcYear, timeId, area)), ]
  links_exp <- outNoStrat_exp$links
  links_exp <- links_exp[with(links_exp, order(mcYear, timeId, link)), ]
  
  
  expect_true(base::all.equal(data.frame(area_exp),data.frame(area_test)))
  expect_true(base::all.equal(data.frame(links_exp),data.frame(links_test)))
})


test_that("checks message when unused binding constraints", {
  # get Antares' flow-based parametes
  opts <- list()
  opts$studyPath <- system.file("testdata/adq/antaresStudy37/user/flowbased/ts.txt", package = "antaresFlowbased")
  if(opts$studyPath == "") opts$studyPath <- system.file("inst/testdata/adq/antaresStudy37/user/flowbased/ts.txt", package = "antaresFlowbased")
  opts$studyPath <- gsub("/user/flowbased/ts.txt","" , opts)
  
  # launch adq patch
  tf <- system.file("testdata/adq/General/studyNoStrat_ini.RDS", package = "antaresFlowbased")
  if(tf == "")  tf <- system.file("inst/testdata/adq/General/studyNoStrat_ini.RDS", package = "antaresFlowbased")
  dataNoStrat_ini <- readRDS(tf)
  dataNoStrat_ini3 <- copy(dataNoStrat_ini)
  expect_message(suppressWarnings(.applyAdq(opts = opts, dataNoStrat_ini3)))
})
