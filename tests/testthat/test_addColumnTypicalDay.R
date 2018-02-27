context("addTypicalDayId")


test_that("addTypicalDayId", {
  
   data <- readAntares(mcYears = 1:10)
   data <- addTypicalDayId(data, testSt)
   
   expect_true(all(unique(data$typicalDay))%in% 1:12)
   
   data <- readAntares(areas = "all", links = "all" ,mcYears = 1:10)
   data <- addTypicalDayId(data, testSt)
   
   expect_true(all(unique(data$areas$typicalDay))%in% 1:12)
   expect_true(all(unique(data$links$typicalDay))%in% 1:12)
   
   
})