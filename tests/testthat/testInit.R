context("Function initFlowBased")

# #Untar and read study
# testStudy <- system.file("testdata/exemple_test.tgz",package = "antaresFlowbased")
# temp_dir <- tempdir()
# untar(testStudy, exdir = temp_dir)
# testStudy <- paste0(temp_dir, "/exemple_test")
# opts <- antaresRead::setSimulationPath(testStudy)

test_that("test initFlowBased", {
  opts1 <- setSimulationPath(testStudy2)
  
  expect_true(identifyFirstDay(opts1) == 1)
  expect_true(suppressWarnings(identifyFirstDay(opts1, secondArea = NULL) == 1))
  expect_warning(identifyFirstDay(opts1, secondArea = NULL))
  
})


test_that("test initFlowBased", {
  opts1 <- setSimulationPath(testStudy2)
  fb_opts <- system.file("input/model/p2017", package = "antaresFlowbased")
  if(fb_opts == "") fb_opts <- system.file("inst/input/model/p2017", package = "antaresFlowbased")
  init <- try(initFlowBased(controlAntares = FALSE, fb_opts = fb_opts, scenarios = 1:34))
  expect_true(class(init) == "list")
  opts1 <- setSimulationPath(testStudy2)
  
  
  expect_true("model_description_fb" %in% getAreas(opts = opts1))

  
  ctr <- fread(paste0(fb_opts, "/weight.txt"))
  bdc <- names(readBindingConstraints(opts1))
  expect_true(all(paste0(ctr$Name, "_fb") %in% bdc))
  
  clusters <- antaresRead::readClusterDesc(opts1)
  clusters <- clusters[area == "model_description_fb"]
  expect_true(all(clusters$unitcount == 1))
  
  clbdc <- all(clusters$cluster %in% paste0("model_description_fb_", tolower(bdc)))
  expect_true(clbdc)
  
})
