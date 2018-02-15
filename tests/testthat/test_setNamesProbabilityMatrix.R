 context("setNamesProbabilityMatrix")
 
 test_that("test on setNamesProbabilityMatrix", {
   dta <- list(data.table(iris), data.table(iris))
   
   setNamesProbabilityMatrix(dta, names(iris), tolower(names(iris)))
   
   toHave <- tolower(names(iris))
   
   expect_true(identical(toHave, names(dta[[1]])))
   expect_true(identical(toHave, names(dta[[2]])))
   
 })