alpha <- 0.5


addCons <- function(mat, long, conc, val){
  cont <- rep(0, long)
  cont[conc] <- val
  mat <- list(mat, cont)
  mat
}

pointX <- EXTREME_X
faceY <- EXTREME_Y
face <- FACE_Y
ID <- 1:nrow(face)

iFY <- 1:nrow(face)
jFY <- 1:ncol(face)

iEX <- 1:nrow(pointX)
jEX <- 1:ncol(pointX)

iEY <- 1:nrow(faceY)
jEY <- 1:ncol(faceY)
Nbvar <- nrow(face) +
                   6 * nrow(pointX)+
                   9 * nrow(faceY) +
                   nrow(pointX) * nrow(faceY)

bounds <- c(rep(-Inf, length(iFY)),
            rep(0, length(iEX) * 6),
            rep(-Inf, length(iEY) * 3),
            rep(0, length(iEY)* length(iEX)),
            rep(0, length(iEY) * 6))
bounds <- V_bound(li=1:length(bounds), lb=bounds)
##Contraints on particular face
be_min <- min(pointX[, .SD, .SDcols = 1])
de_min <- min(pointX[, .SD, .SDcols = 2])
fr_min <- min(pointX[, .SD, .SDcols = 3])
de_max <- max(pointX[, .SD, .SDcols = 2])

pointX <- as.matrix(pointX)
faceY <- as.matrix(faceY)
face <- as.matrix(face)

Beclu <- which(apply(face, 1, function(Y)
{
  !any(apply(faceY,1, function(X){
    all(X[1:3] == Y)
  })|
    apply(faceY,1, function(X){
      all(X[4:6] == Y)
    })|
    apply(faceY,1, function(X){
      all(X[7:9] == Y)
    }))}))

be_min_face <- which(apply(face, 1, function(X){
  X[1]== -1 &  X[2]== 0 &  X[3]== 0
}))

de_min_face <- which(apply(face, 1, function(X){
  X[1]==0  &  X[2]== -1 &  X[3]== 0
}))

fr_min_face <- which(apply(face, 1, function(X){
  X[1]==0  &  X[2]== 0 &  X[3]== -1
}))

de_max_face <- which(apply(face, 1, function(X){
  X[1]==0  &  X[2]== 1 &  X[3]== 0
}))

allconstraint <- NULL
rhs <- NULL
direction <- NULL
allconstraint <- allconstraint %>% addCons(Nbvar, be_min_face, 1) %>%
  addCons(Nbvar, de_min_face, 1) %>%
  addCons(Nbvar, de_min_face, 1) %>%
  addCons(Nbvar, fr_min_face, 1) %>%
  addCons(Nbvar, de_max_face, 1)%>%
  addCons(Nbvar, de_max_face, 1)
rhs <- c(rhs, c(-be_min, -de_min - 500, -de_min, - fr_min, de_max - 500, de_max))
direction <- c(direction, "==",  ">=", "<=", "==", ">=", "<=")
for(i in 1:length(Beclu)){
  allconstraint <- allconstraint %>% addCons(Nbvar, Beclu[i], 1)
  rhs <- c(rhs, 10000)
  direction <- c(direction, "==")
}

actual <- length(iFY)
FACE_Y <- as.matrix(FACE_Y)
for(i in iEX){
  for(j in iFY){
    allconstraint <- allconstraint %>% addCons(Nbvar,c(j, 
                                                       actual + i, 
                                                       actual + length(iEX) + i,
                                                       actual + 2*length(iEX) + i,
                                                       actual + 3*length(iEX) + i,
                                                       actual + 4*length(iEX) + i,
                                                       actual + 5*length(iEX) + i
                                               ), c(-1, -FACE_Y[j,] , FACE_Y[j,]))
    direction <- c(direction, "<=")
    rhs <- c(rhs, -t(FACE_Y[j,])%*%pointX[i,])
  }
}

actual <- actual + 6*length(iEX)
Bbons <- match(data.frame(t(faceY[,1:3, drop = FALSE])), data.frame(t(face)))
for(vv in iEY){
  allconstraint <- allconstraint %>% addCons(Nbvar,
    c(Bbons[vv],
      actual + vv,
      actual + length(iEY) + vv,
      actual + length(iEY) *2  + vv),
    c(-1, faceY[vv,1:3])
  )
  direction <- c(direction, "==")
  rhs <- c(rhs, 0)
  
}

Bbons <- match(data.frame(t(faceY[,4:6, drop = FALSE])), data.frame(t(face)))
for(vv in iEY){
  allconstraint <- allconstraint %>% addCons(Nbvar,
    c(Bbons[vv],
      actual + vv,
      actual + length(iEY) + vv,
      actual + length(iEY) *2  + vv),
    c(-1, faceY[vv,4:6])
  )
  direction <- c(direction, "==")
  rhs <- c(rhs, 0)
}


Bbons <- match(data.frame(t(faceY[,7:9, drop = FALSE])), data.frame(t(face)))
for(vv in iEY){
  allconstraint <- allconstraint %>% addCons(Nbvar,
    c(Bbons[vv],
      actual + vv,
      actual + length(iEY) + vv,
      actual + length(iEY) *2  + vv),
    c(-1, faceY[vv,7:9])
  )
  direction <- c(direction, "==")
  rhs <- c(rhs, 0)
}

for(j in iEY){
  for(i in iFY){
    allconstraint <- allconstraint %>% addCons(Nbvar,
                        c(i, actual + j,
                          actual + length(iEY) + j,
                          actual + length(iEY) * 2 + j),
                        c(-1, face[i, ]))
    direction <- c(direction, "<=")
    rhs <- c(rhs, 1)
  }
}

for(j in iEY){
  allconstraint <- allconstraint %>% addCons(Nbvar,
                                             actual + j + (iEX-1)*length(iEY) + length(iEY)*3,
                                            1)
  direction <- c(direction, "==")
  rhs <- c(rhs, 1)
}
actual <- actual + length(iEY) * 3


for(j in iEY){
  allconstraint <- allconstraint %>% addCons(Nbvar,
                                             c(j + length(iFY) + length(iEX) * 6,
                                               actual + j + ((iEX-1)*length(iEY)), 
                                               actual + length(iEX)*length(iEY)+j,
                                               actual + length(iEX)*length(iEY)+length(iEY)*3+j),
                                              c(1, -pointX[, 1], 1, -1))
  direction <- c(direction, "==")
  rhs <- c(rhs, 0)
}



for(j in iEY){
  allconstraint <- allconstraint %>% addCons(Nbvar,
                                             c(j + length(iEY)+ length(iFY) + length(iEX) * 6, actual + j + ((iEX-1)*length(iEY)),
                                               actual + length(iEX)*length(iEY) + j + length(iEY),
                                               actual + length(iEX)*length(iEY) + length(iEY)*3 + j + length(iEY)),
                                             c(1, -pointX[, 2], 1, -1))
  direction <- c(direction, "==")
  rhs <- c(rhs, 0)
}

for(j in iEY){
  allconstraint <- allconstraint %>% addCons(Nbvar,
                                             c(j+length(iEY)*2 + length(iFY) + length(iEX) * 6, actual + j + ((iEX-1)*length(iEY)),
                                               actual+length(iEX)*length(iEY)+j+length(iEY)*2,
                                               actual+length(iEX)*length(iEY)+length(iEY)*3+j+length(iEY)*2),
                                             c(1,- pointX[, 3], 1, -1))
  direction <- c(direction, "==")
  rhs <- c(rhs, 0)
}

matConstr <- matrix(unlist(allconstraint), ncol = Nbvar, byrow = TRUE)

l_constraint <- L_constraint(L = matConstr,
                             dir = direction,
                             rhs = rhs)




obj <- unlist(addCons(NULL, Nbvar, c((length(iFY) +1): (length(iFY) + length(iEX)*6),
                           (1+length(iFY) + length(iEX)* 6 + length(iEY) * 3 + length(iEY)*length(iEX)):
  (length(iFY) + length(iEX)* 6 + length(iEY) * 3 + length(iEY)*length(iEX) + 6*length(iEY))),
  c(rep((1-alpha)/nrow(EXTREME_X),  length(iEX)*6), rep((alpha)/nrow(EXTREME_Y), 6*length(iEY)))))
LP <- OP(obj, l_constraint, maximum = FALSE,
         bounds = bounds)
y <- ROI_solve(LP, solver = "symphony")















coef <- ompr::extract_constraints(res)
dim(coef$matrix)
tt <- NULL
for(i in 1:nrow(coef$matrix)){
  tt[i] <- identical(table(coef$matrix[i,]), table(matConstr[i,]))
}
which(!tt)

table(coef$matrix[37,])
table(matConstr[37,])


re2 <- apply(coef$matrix, 2, table)
re <- apply(matConstr, 2, table)

all(re%in%re2)
which(!re%in%re2)


reT <- unlist(re)
reT <- sort(as.numeric(names(reT)))

reT2 <- unlist(re2)
reT2 <- sort(as.numeric(names(reT2)))
which(reT!= reT2)

