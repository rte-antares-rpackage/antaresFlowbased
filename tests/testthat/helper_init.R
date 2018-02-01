library(antaresRead)
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
pathArea <- file.path(opts$studyPath, "input", "areas")

# plotFB(1,1,"FR","NL", fb_opts = opts)



#Untar and read study
testStudy2 <- system.file("testdata",package = "antaresFlowbased")
if(testStudy2 == "")testStudy2 <- system.file("inst/testdata",package = "antaresFlowbased")

# temp_dir <- tempdir()
if (Sys.info()['sysname'] == "Windows") {
  untar(file.path(testStudy2, "ex_test.tgz"), exdir = temp_dir, 
        extras = "--force-local")
} else {
  untar(file.path(testStudy2, "ex_test.tgz"), exdir = temp_dir)
}
testStudy2 <- paste0(temp_dir, "/ex_test")


# 
# tar(tarfile = "ex_test.tgz",files = "ex_test",
#     compression = "gzip")
