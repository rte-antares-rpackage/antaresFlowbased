context("computeFB")


test_that("computeFB",{
  suppressWarnings(allFB <- computeFB(dayType = 7, hour = 1, reports = FALSE, outputName = temp_dir))
  
  expect_true(file.exists(paste0(allFB, "/second_member.txt")))
  expect_true(file.exists(paste0(allFB, "/weight.txt")))
  expect_true(file.exists(paste0(allFB, "/domainesFB.RDS")))
  
  
})


