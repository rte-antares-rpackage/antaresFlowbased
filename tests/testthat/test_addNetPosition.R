context("addNetPosition")

test_that("computeFB",{

  data <- readAntares(area = "all", links = "all", mcYears = 2, opts = testSt)
  data <- addNetPosition(data, opts = testSt, ADQ = FALSE)
  ipn1 <- data$areas[!is.na(Balance_CWE)]
  ipn12 <- melt(giveIpn( data$links), id = 1:2)
  expect_true(identical(ipn1$Balance_CWE,ipn12$value))
  
})
