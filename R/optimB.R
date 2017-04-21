#' Gives tuples of B who check all constraints
#'
#' @param face \code{data.table}, face for 3 country, BE, DE anf FR
#' @param pointX \code{data.table}, extreme points for 3 country, BE, DE anf FR
#'
#' @import pipeR
#'
#' @export
giveTuples <- function(face, pointX){
  b <- apply(face, 1, function(x){
    max(t(as.matrix(x))%*%t(as.matrix(pointX)))
  }
  )
  B <- face
  
  B <- as.matrix(B)
  IDfin <- 1:nrow(B)
  res <- sapply(IDfin, function(X)
  {
    sapply(IDfin, function(Y){
      if(Y>X)
      {
        sapply(IDfin, function(Z){
          if(Z>Y)
          {
            Bijk <- rbind(B[X,], B[Y,], B[Z,])
            bijk <- c(b[X], b[Y], b[Z])
            try({x <- solve(Bijk, bijk)
            d <- b+1e-6
            if(all(B%*%x<=d)){
              return(list(x = X, y = Y, z = Z, y1 = x[1], y2 = x[2], y2 = x[3]))
            }
            },silent = TRUE)
          }
          NULL
        }, simplify = FALSE)
      }
    }, simplify = FALSE)
  }, simplify = FALSE)%>>%
    unlist%>>%
    matrix(ncol = 6, byrow = TRUE)
  DD <- dist(res[,4:6], method = "euclidean", p = 2, upper = FALSE)
  DD <- as.matrix(DD)
  DD[lower.tri(DD, diag = TRUE)] <- 1
  DD
  res[which(apply(DD, 2, min)>1e-6),1:3]
}




#' Write optimisation probleme
#'
#' @param face \code{data.table}, face for 3 country, BE, DE anf FR
#' @param pointX \code{data.table}, extreme points for 3 country, BE, DE anf FR
#' @param faceY \code{data.table}, face for 3 country, BE, DE anf FR for all tuple in face
#' 
#' @import pipeR
#'
#' @export
askProbleme <- function(pointX, faceY, face){
  
  be_min <- min(pointX[, .SD, .SDcols = 1])
  de_min <- min(pointX[, .SD, .SDcols = 2])
  fr_min <- min(pointX[, .SD, .SDcols = 3])
  de_max <- max(pointX[, .SD, .SDcols = 2])
  
  ID <- 1:nrow(face)
  
  pointX <- as.matrix(pointX)
  faceY <- as.matrix(faceY)
  face <- as.matrix(face)
  
  iFY <- 1:nrow(face)
  jFY <- 1:ncol(face)
  
  iEX <- 1:nrow(pointX)
  jEX <- 1:ncol(pointX)
  
  iEY <- 1:nrow(faceY)
  jEY <- 1:ncol(faceY)
  
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
  
  res <- MIPModel() 
  res <- res %>% add_variable(b[i], i = iFY, type = "continuous")
  #Contraintes par pays
  res <- res %>% add_constraint(b[be_min_face]== - be_min) %>%
    add_constraint(b[de_min_face] >= -de_min - 500) %>%
    add_constraint(b[de_min_face] <= -de_min) %>%
    add_constraint(b[fr_min_face]== - fr_min) %>%
    add_constraint(b[de_max_face]>= de_max - 500)%>%
    add_constraint(b[de_max_face]<= de_max)
  
  
  
  
  #Contraintes sur Bexclus
  res <- res %>% add_constraint(b[re] == 10000, re = Beclu)
  
  #Declaraction de y+ et y-
  res <- res %>% add_variable(y1_plus[i], lb = 0,i = iEX)
  res <- res %>% add_variable(y2_plus[i], lb = 0,i = iEX)
  res <- res %>% add_variable(y3_plus[i], lb = 0,i = iEX)
  
  res <- res %>% add_variable(y1_moins[i], lb = 0,i = iEX)
  res <- res %>% add_variable(y2_moins[i], lb = 0,i = iEX)
  res <- res %>% add_variable(y3_moins[i], lb = 0,i = iEX)
  
  
  #x_proj_Y
  res <- res %>% add_constraint((face[i, 1] * (pointX[t, 1] + y1_plus[t] - y1_moins[t]) +
                                   face[i, 2] * (pointX[t, 2] + y2_plus[t] - y2_moins[t]) +
                                   face[i, 3] * (pointX[t, 3] + y3_plus[t] - y3_moins[t]))
                                <= b[i], i = iFY, t = iEX)
  
  
  ###### Déclaration des coordonnées des points extrêmes de Y
  res <- res %>% add_variable(y1[i],i = iEY)
  res <- res %>% add_variable(y2[i],i = iEY)
  res <- res %>% add_variable(y3[i],i = iEY)
  
  #ctr_yi
  Bbons <- match(data.frame(t(faceY[,1:3, drop = FALSE])), data.frame(t(face)))
  
  for(vv in iEY){
    res <- res %>% add_constraint(faceY[j, 1] * y1[j] +
                                    faceY[j, 2] * y2[j] +
                                    faceY[j, 3] * y3[j] == b[i], i = Bbons[vv], j = vv)
  }
  
  #ctr_yj
  Bbons <- match(data.frame(t(faceY[,4:6, drop = FALSE])), data.frame(t(face)))
  for(vv in  iEY){
    res <- res %>% add_constraint(faceY[j, 4] * y1[j] +
                                    faceY[j, 5] * y2[j] +
                                    faceY[j, 6] * y3[j] == b[i], i = Bbons[vv], j = vv)
  }
  #ctr_yk
  Bbons <- match(data.frame(t(faceY[,7:9, drop = FALSE])), data.frame(t(face)))
  for(vv in  iEY){
    res <- res %>% add_constraint(faceY[j, 7] * y1[j] +
                                    faceY[j, 8] * y2[j] +
                                    faceY[j, 9] * y3[j] == b[i], i = Bbons[vv], j = vv)
  }
  
  #in_domain
  res <- res %>% add_constraint(face[i,1] * y1[j] +
                                  face[i,2] * y2[j] +
                                  face[i,3] * y3[j] <=
                                  b[i] + 1, i = iFY, j = iEY)
  
  
  ##### projection sur X
  res <- res %>% add_variable(lambda[i, j], lb = 0, i = iEY, j = iEX)
  
  #convexity
  res <- res %>% add_constraint(sum_expr(lambda[i, j], j = iEX) == 1, i = iEY)
  
  ## déclaration des variables d'erreur x+ et x-
  res <- res %>% add_variable(x1_plus[i], lb = 0,i = iEY)
  res <- res %>% add_variable(x2_plus[i], lb = 0,i = iEY)
  res <- res %>% add_variable(x3_plus[i], lb = 0,i = iEY)
  
  res <- res %>% add_variable(x1_moins[i], lb = 0,i = iEY)
  res <- res %>% add_variable(x2_moins[i], lb = 0,i = iEY)
  res <- res %>% add_variable(x3_moins[i], lb = 0,i = iEY)
  
  ## Application de la condition y_sommet
  #ctr_y1
  res <- res %>% add_constraint(y1[i] == sum_expr(lambda[i, j] * pointX[j, 1],
                                                  j = iEX) + x1_plus[i] - x1_moins[i], i = iEY)
  #ctr_y2
  res <- res %>% add_constraint(y2[i] == sum_expr(lambda[i, j] * pointX[j, 2]  
                                                  , j = iEX) + x2_plus[i] - x2_moins[i], i = iEY)
  #ctr_y3
  res <- res %>% add_constraint(y3[i] == sum_expr(lambda[i, j] * pointX[j, 3] 
                                                  , j = iEX) + x3_plus[i] - x3_moins[i] , i = iEY)
  
  
  res
}

#' Resolve optimisation probleme
#'
#' @param pointX \code{data.table}, extreme points for 3 country, BE, DE anf FR
#' @param faceY \code{data.table}, face for 3 country, BE, DE anf FR for all tuple in face
#' @param probleme \code{optimization_model}, make which askProbleme
#' @param alpha \code{numeric}, between 0 and 1, error ponderation if 0 error of type 1 is ignored
#' if 1 error of type 0 is ignored
#' 
#' @import pipeR
#'
#' @export
resolvB <- function(pointX, faceY, probleme, alpha)
{
  iEX <- 1:nrow(pointX)
  iEY <- 1:nrow(faceY)
  
  probleme <- probleme %>% set_objective((1-alpha)/(nrow(pointX))*
                                 sum_expr(y1_plus[i] + y1_moins[i] +
                                            y2_plus[i] +  y2_moins[i] + 
                                            y3_plus[i] + y3_moins[i] , i = iEX ) + 
                                 (alpha)/(nrow(faceY))*sum_expr(x1_plus[i] + x1_moins[i] +
                                                                      x2_plus[i] +  x2_moins[i] + 
                                                                      x3_plus[i] + x3_moins[i] , i = iEY )
                               , direction = "min")
  
  tt <- probleme %>%  solve_model(with_ROI(solver = "symphony", verbosity = -3))
  tt
}




#' Search alpha for a daytype and a hour
#' 
#' @param face \code{data.table}, face for 3 country, BE, DE anf FR
#' @param pointX \code{data.table}, extreme points for 3 country, BE, DE anf FR
#' @param faceY \code{data.table}, face for 3 country, BE, DE anf FR for all tuple in face
#' @param probleme \code{optimization_model}, make which askProbleme
#' @param constraints \code{data.frame} constraints
#' @param univ \code{matrix} generate which .univ
#' 
#' @import pipeR
#'
#' @export
searchAlpha <- function(face, pointX, faceY, probleme, constraints, univ){
  alpha <- 0.5
  tt <- resolvBmat(face, pointX, faceY, probleme, alpha)
  tt
  # bSol <- get_solution(tt, b[i])$value
  # FY <- cbind(FACE_Y, bSol)
  FY <- cbind(FACE_Y,  bSol = tt$solution[1:nrow(face)])
  
  error <- .giveError(FY, constraints = constraints, univ = univ)
  
  nbrep <- 0
  bsup <- 1
  binf <- 0
  while(abs(error$error1 - error$error2)>0.1 & nbrep < 15){
    if(error$error1 - error$error2>0){
      bsup <- alpha
    }else{
      binf <- alpha
    }
    alpha <- (bsup + binf) / 2
    tt <- resolvBmat(face, pointX, faceY, probleme, alpha)
    tt
    # bSol <- get_solution(tt, b[i])$value
    # FY <- cbind(FACE_Y, bSol)
    FY <- cbind(FACE_Y, bSol = tt$solution[1:nrow(face)])
    error <- .giveError(FY, constraints = constraints, univ = univ)
    nbrep <- nbrep + 1
    print(alpha)
  }
  return(list(alpha = alpha, error = error, face = FY))
}

#' Write optimisation probleme, matrix version
#'
#' @param face \code{data.table}, face for 3 country, BE, DE anf FR
#' @param pointX \code{data.table}, extreme points for 3 country, BE, DE anf FR
#' @param faceY \code{data.table}, face for 3 country, BE, DE anf FR for all tuple in face
#' 
#' @import pipeR
#'
#' @export
askProblemeMat <- function(pointX, faceY, face){
  addCons <- function(mat, long, conc, val){
    cont <- rep(0, long)
    cont[conc] <- val
    mat <- list(mat, cont)
    mat
  }
  
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
  list(l_constraint = l_constraint, bounds = bounds, Nbvar = Nbvar)
}


#' Resolve optimisation probleme matrix probleme
#'
#' @param face \code{data.table}, face for 3 country, BE, DE anf FR
#' @param pointX \code{data.table}, extreme points for 3 country, BE, DE anf FR
#' @param faceY \code{data.table}, face for 3 country, BE, DE anf FR for all tuple in face
#' @param probleme \code{optimization_model}, make which askProbleme
#' @param alpha \code{numeric}, between 0 and 1, error ponderation if 0 error of type 1 is ignored
#' if 1 error of type 0 is ignored
#' 
#' @import pipeR
#'
#' @export
resolvBmat <- function(face, pointX, faceY, probleme, alpha)
{
  Nbvar <- probleme$Nbvar
  iEX <- 1:nrow(pointX)
  iEY <- 1:nrow(faceY)
  iFY <- 1:nrow(face)
  addCons <- function(mat, long, conc, val){
    cont <- rep(0, long)
    cont[conc] <- val
    mat <- list(mat, cont)
    mat
  }
obj <- unlist(addCons(NULL, Nbvar, c((length(iFY) +1): (length(iFY) + length(iEX)*6),
                                     (1+length(iFY) + length(iEX)* 6 + length(iEY) * 3 + length(iEY)*length(iEX)):
                                       (length(iFY) + length(iEX)* 6 + length(iEY) * 3 + length(iEY)*length(iEX) + 6*length(iEY))),
                      c(rep((1-alpha)/nrow(pointX),  length(iEX)*6), rep((alpha)/nrow(faceY), 6*length(iEY)))))
LP <- OP(obj, probleme$l_constraint, maximum = FALSE,
         bounds = probleme$bounds)
y <- ROI_solve(LP, solver = "symphony")
y
}
