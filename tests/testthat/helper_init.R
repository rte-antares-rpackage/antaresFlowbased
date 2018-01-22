
#Untar and read study
testStudy <- system.file("testdata",package = "antaresFlowbased")
if(testStudy == "")testStudy <- system.file("inst/testdata",package = "antaresFlowbased")

temp_dir <- tempdir()
if (Sys.info()['sysname'] == "Windows") {
  untar(file.path(testStudy, "exemple_test.tgz"), exdir = temp_dir, 
        extras = "--force-local")
} else {
  untar(file.path(testStudy, "exemple_test.tgz"), exdir = temp_dir)
}
testStudy <- paste0(temp_dir, "/exemple_test")
opts <- antaresRead::setSimulationPath(testStudy)


