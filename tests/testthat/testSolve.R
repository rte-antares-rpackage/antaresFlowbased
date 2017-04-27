context("Function askProblemeMat")

pointX <- fread("test/data/optim/pointX.csv")
face <- fread("test/data/optim/face.csv")

B <- fread("test/data/optim/B.csv")

res <- giveTuples(face, pointX)
faceY <- do.call("cbind", apply(res, 2, function(X){
  face[X,]
}))
res <- askProblemeMat(pointX, faceY, face)
alpha <- 0.56
tt <- resolvBmat(face, pointX, faceY, probleme, alpha)

expect_equal(sum(round(tt$solution[1:nrow(face)]-B, 2)), 0)
