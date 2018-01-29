context("Function askProblemMat")

test_that("test optim", {
  PTDF <- system.file("testdata/optim/PTDF.csv",package = "antaresFlowbased")
  PTDF <- fread(PTDF)
  
  face <- system.file("testdata/optim/B.csv",package = "antaresFlowbased")
  face <- fread(face)
  
  resultsAMPL <- system.file("testdata/optim/AMPLobjective.csv",package = "antaresFlowbased")
  resultsAMPL <- fread(resultsAMPL)
  
  res <- apply(resultsAMPL, 1, function(Z){
    Z <- data.frame(t(Z))
    PTDFsel <- PTDF[Id_day == Z$day & Period == Z$hour]
    pointX <- getVertices(as.matrix(PTDFsel[,.SD, .SDcols = c("BE","DE","FR","NL")]), PTDFsel$RAM)
    pointX <- data.table(pointX)
    
    res <- giveTuples(face, pointX)
    faceY <- do.call("cbind", apply(res, 2, function(X){
      face[X,]
    }))
    problem <- askProblemMat(pointX, faceY, face)
    alpha <- Z$alpha
    tt <- resolvBmat(face, pointX, faceY, problem, alpha)
    round(tt$objval, 1) == round(Z$objective, 1)
  })
  
  expect_true(all(res))
})