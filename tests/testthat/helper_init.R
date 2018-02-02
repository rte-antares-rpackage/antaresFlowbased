# library(antaresRead)
# #Untar and read study
# 
# testStudy <- system.file("testdata",package = "antaresFlowbased")
# if(testStudy == "")testStudy <- system.file("inst/testdata",package = "antaresFlowbased")
# 
# temp_dir <- tempdir()
# if (Sys.info()['sysname'] == "Windows") {
#   untar(file.path(testStudy, "exemple_test.tgz"), exdir = temp_dir, 
#         extras = "--force-local")
# } else {
#   untar(file.path(testStudy, "exemple_test.tgz"), exdir = temp_dir)
# }
# testStudy <- file.path(temp_dir, "exemple_test")
# opts <- antaresRead::setSimulationPath(testStudy)

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
testStudy2 <- file.path(temp_dir, "ex_test")
opts2 <- antaresRead::setSimulationPath(testStudy2)
opts <- opts2
testStudy <- testStudy2
assign("opts2", opts2, envir = globalenv())
assign("opts", opts, envir = globalenv())
assign("testStudy2", testStudy2, envir = globalenv())
assign("testStudy", testStudy, envir = globalenv())



# 
# tar(tarfile = "ex_test.tgz",files = "ex_test",
#     compression = "gzip")
