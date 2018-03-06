context("addTypicalDayId")


test_that("addTypicalDayId", {
  
   data <- readAntares(areas = "all", links = "all" , mcYears = 2, opts = testSt, showProgress = FALSE)
   
   
   data <- addTypicalDayId(data, testSt)
   
   expect_true(all(unique(data$typicalDay))%in% 1:12)
   
   data <- readAntares(areas = "all", links = "all" ,mcYears = 2, showProgress = FALSE, opts = testSt)
   data <- addTypicalDayId(data, testSt)
   
   expect_true(all(unique(data$areas$typicalDay))%in% 1:12)
   expect_true(all(unique(data$links$typicalDay))%in% 1:12)
   
   
})