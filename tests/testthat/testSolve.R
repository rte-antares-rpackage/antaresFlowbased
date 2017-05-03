context("Function askProblemeMat")

pointX <- fread(system.file("test/data/optim/pointX.csv",package = "antaresFlowbased"))
face <- fread(system.file("test/data/optim/face.csv",package = "antaresFlowbased"))

B <- fread(system.file("test/data/optim/B.csv",package = "antaresFlowbased"))

res <- giveTuples(face, pointX)
faceY <- do.call("cbind", apply(res, 2, function(X){
  face[X,]
}))
res <- askProblemeMat(pointX, faceY, face)
alpha <- 0.56
tt <- resolvBmat(face, pointX, faceY, res, alpha)

expect_equal(sum(round(tt$solution[1:nrow(face)]-B, 0)), 0)
