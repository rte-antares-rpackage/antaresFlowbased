context("Function initFlowBased")

# #Untar and read study
# testStudy <- system.file("testdata/exemple_test.tgz",package = "antaresFlowbased")
# temp_dir <- tempdir()
# untar(testStudy, exdir = temp_dir)
# testStudy <- paste0(temp_dir, "/exemple_test")
# opts <- antaresRead::setSimulationPath(testStudy)

test_that("test initFlowBased", {
  #Init study environment
  # initFlowBased()
  # 
  # #Test if folder user is ok
  # expect_true("user"%in%list.files(testStudy))
  # userpatch <- paste0(testStudy, "/user")
  # 
  # #Test if folder flowbased is ok
  # expect_true("flowbased"%in%list.files(userpatch))
  # userpatch <- paste0(userpatch, "/flowbased")
  # 
  # #Test if all files are ok
  # files <- c("scenario.txt", "second_member.txt", "ts.txt", "weight.txt")
  # expect_true(unique(files%in%list.files(userpatch)))
  # 
  # #Test scenario file
  # scenario <- paste0(userpatch, "/scenario.txt")
  # scenario <- data.table::fread(scenario)
  # expect_true(names(scenario) == "simulation")
  # expect_true(class(scenario$simulation) == "integer")
  # 
  # #Test second_member file
  # second_member <- paste0(userpatch, "/second_member.txt")
  # second_member <- data.table::fread(second_member)
  # expect_true(unique(names(second_member) %in% c("Id_day", "Id_hour", "vect_b", "Name")))
  # 
  # #Test ts file
  # ts <- paste0(userpatch, "/ts.txt")
  # ts <- data.table::fread(ts)
  # expect_true(("Date"%in% names(ts) ))
  # 
  # #Test if all scenario$simulation have corresponding ts column
  # ammSc <- as.numeric(unique(names(ts))[!"Date"==names(ts)])
  # expect_true(unique(unique(scenario$simulation %in%  ammSc) ))
  # 
  # #Test weight
  # weight <- paste0(userpatch, "/weight.txt")
  # weight <- data.table::fread(weight)
  # expect_true( "name" %in% names(weight))
  
  # unlink(testStudy, recursive = TRUE, force = TRUE)
})

