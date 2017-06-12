#' Search alpha for a daytype and a hour
#'
#' @param face \code{data.table}, face for 3 country, BE, DE anf FR
#' @param pointX \code{data.table}, extreme points for 3 country, BE, DE anf FR
#' @param faceY \code{data.table}, face for 3 country, BE, DE anf FR for all tuple in face
#' @param problem \code{optimization_model}, make with \link{askProblem}
#' @param PTDF \code{data.frame} PTDF
#' @param univ \code{matrix} generate which .univ
#' @param verbose \code{numeric} show log in console. Defaut to 1
#' \itemize{
#'  \item 0 : No log
#'  \item 1 : Short log
#' }
#'
#' @import pipeR
#'
#' @noRd
searchAlpha <- function(face, pointX, faceY, problem, PTDF, univ, verbose = 0){
  alpha <- 0.5
  tt <- resolvBmat(face, pointX, faceY, problem, alpha)
  stratPoint <- (nrow(face) + nrow(pointX) * 6)


  # bSol <- get_solution(tt, b[i])$value
  # FY <- cbind(FACE_Y, bSol)
  FY <- cbind(face,  bSol = tt$solution[1:nrow(face)])

  error <- .giveError(FY, PTDF = PTDF, univ = univ)

  nbrep <- 0
  bsup <- 1
  binf <- 0
  while(abs(bsup-binf)>0.01 & nbrep < 15){
    if(error$error1 - error$error2>0){
      bsup <- alpha
    }else{
      binf <- alpha
    }
    alpha <- (bsup + binf) / 2
    tt <- resolvBmat(face, pointX, faceY, problem, alpha)
    FY <- cbind(face, bSol = tt$solution[1:nrow(face)])
    error <- .giveError(FY, PTDF = PTDF, univ = univ)
    nbrep <- nbrep + 1
  }
  pointsY <- data.frame(BE = tt$solution[(stratPoint+ 1) :
                                           (stratPoint + nrow(faceY))],
                        DE = tt$solution[(stratPoint+ 1+ nrow(faceY)) :
                                           (stratPoint + nrow(faceY)*2)],
                        FR = tt$solution[(stratPoint + 1 + 2*nrow(faceY)) :
                                           (stratPoint + nrow(faceY)*3)]
  )
  names(FY) <- c("BE", "DE", "FR", "B")
  FY <- data.frame(FY)
  if(verbose>0){
    cat(paste0( "\n", "Objective out :" ,tt$objval, "\n"))
  }
  return(list(alpha = alpha, error = error, face = FY, pointsY = pointsY))
}

#' Write optimisation problem, matrix version
#'
#' @param face \code{data.table}, face for 3 country, BE, DE anf FR
#' @param pointX \code{data.table}, extreme points for 3 country, BE, DE anf FR
#' @param faceY \code{data.table}, face for 3 country, BE, DE anf FR for all tuple in face
#'
#' @import pipeR
#'
#' @noRd
askProblemMat <- function(pointX, faceY, face){


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
    nrow(pointX) * nrow(faceY) + 12

  bounds <- c(rep(-Inf, length(iFY)),
              rep(0, length(iEX) * 6),
              rep(-Inf, length(iEY) * 3),
              rep(0, length(iEY)* length(iEX)),
              rep(0, length(iEY) * 6), rep(-Inf, 12))
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
  allconstraint <- allconstraint %>>% .addCons(Nbvar, be_min_face, 1)  %>>%
    .addCons(Nbvar, de_min_face, 1)  %>>%
   # .addCons(Nbvar, de_min_face, 1)  %>>%
    .addCons(Nbvar, fr_min_face, 1)  %>>%
    #.addCons(Nbvar, de_max_face, 1) %>>%
    .addCons(Nbvar, de_max_face, 1)
   #rhs <- list(rhs, list(-be_min, -de_min - 500, -de_min, - fr_min, de_max - 500, de_max))
   #direction <- list(direction, "==",  ">=", "<=", "==", ">=", "<=")
  #

  rhs <- list(rhs, list(-be_min, -de_min, - fr_min, de_max))
  direction <- list(direction, "==", "==", "==", "==")


  if(length(Beclu)>0)
  {
    sapply(1:length(Beclu), function(i){
      allconstraint <<- allconstraint %>>%  .addCons(Nbvar, Beclu[i], 1)
      rhs <<- list(rhs,max(apply(pointX, 1, function(X){sum(face[Beclu[i],]*X)})))#
      direction <<- list(direction, "==")
    })
  }


  actual <- length(iFY)
  face <- as.matrix(face)

  sapply(iEX, function(i){
    sapply(iFY, function(j){
      allconstraint <<- allconstraint %>>%  .addCons(Nbvar,c(j,
                                                            actual + i,
                                                            actual + length(iEX) + i,
                                                            actual + 2*length(iEX) + i,
                                                            actual + 3*length(iEX) + i,
                                                            actual + 4*length(iEX) + i,
                                                            actual + 5*length(iEX) + i
      ), c(-1, -face[j,] , face[j,]))
      direction <<- list(direction, "<=")
      rhs <<- list(rhs, -t(face[j,])%*%pointX[i,])
      NULL
    })})

  actual <- actual + 6*length(iEX)
  Bbons <- match(data.frame(t(faceY[,1:3, drop = FALSE])), data.frame(t(face)))

  sapply(iEY, function(vv){
    allconstraint <<- allconstraint %>>%  .addCons(Nbvar,
                                                  c(Bbons[vv],
                                                    actual + vv,
                                                    actual + length(iEY) + vv,
                                                    actual + length(iEY) *2  + vv),
                                                  c(-1, faceY[vv,1:3])
    )
    direction <<- list(direction, "==")
    rhs <<- list(rhs, 0)
  })

  Bbons <- match(data.frame(t(faceY[,4:6, drop = FALSE])), data.frame(t(face)))
  sapply(iEY, function(vv){
    allconstraint <<- allconstraint %>>%  .addCons(Nbvar,
                                                  c(Bbons[vv],
                                                    actual + vv,
                                                    actual + length(iEY) + vv,
                                                    actual + length(iEY) *2  + vv),
                                                  c(-1, faceY[vv,4:6])
    )
    direction <<- list(direction, "==")
    rhs <<- list(rhs, 0)
  })
  Bbons <- match(data.frame(t(faceY[,7:9, drop = FALSE])), data.frame(t(face)))
  Bbons <- match(data.frame(t(faceY[,7:9, drop = FALSE])), data.frame(t(face)))

  sapply(iEY, function(vv){
    allconstraint <<- allconstraint %>>%  .addCons(Nbvar,
                                                  c(Bbons[vv],
                                                    actual + vv,
                                                    actual + length(iEY) + vv,
                                                    actual + length(iEY) *2  + vv),
                                                  c(-1, faceY[vv,7:9])
    )
    direction <<- list(direction, "==")
    rhs <<- list(rhs, 0)
  })


  sapply(iEY, function(j){
    sapply(iFY, function(i){
      allconstraint <<- allconstraint %>>%  .addCons(Nbvar,
                                                    c(i, actual + j,
                                                      actual + length(iEY) + j,
                                                      actual + length(iEY) * 2 + j),
                                                    c(-1, face[i, ]))
      direction <<- list(direction, "<=")
      rhs <<- list(rhs, 1)
    })
  })



  sapply(iEY, function(j){
    allconstraint <<- allconstraint %>>%  .addCons(Nbvar,
                                                  actual + j + (iEX-1)*length(iEY) + length(iEY)*3,
                                                  1)
    direction <<- list(direction, "==")
    rhs <<- list(rhs, 1)
  })

  actual <- actual + length(iEY) * 3

  sapply(iEY, function(j){
    allconstraint <<- allconstraint %>>%  .addCons(Nbvar,
                                                  c(j + length(iFY) + length(iEX) * 6,
                                                    actual + j + ((iEX-1)*length(iEY)),
                                                    actual + length(iEX)*length(iEY)+j,
                                                    actual + length(iEX)*length(iEY)+length(iEY)*3+j),
                                                  c(1, -pointX[, 1], 1, -1))
    direction <<- list(direction, "==")
    rhs <<- list(rhs, 0)
  })


  sapply(iEY, function(j){
    allconstraint <<- allconstraint %>>%  .addCons(Nbvar,
                                                  c(j + length(iEY)+ length(iFY) + length(iEX) * 6, actual + j + ((iEX-1)*length(iEY)),
                                                    actual + length(iEX)*length(iEY) + j + length(iEY),
                                                    actual + length(iEX)*length(iEY) + length(iEY)*3 + j + length(iEY)),
                                                  c(1, -pointX[, 2], 1, -1))
    direction <<- list(direction, "==")
    rhs <<- list(rhs, 0)
  })

  sapply(iEY, function(j){
    allconstraint <<- allconstraint %>>%  .addCons(Nbvar,
                                                  c(j+length(iEY)*2 + length(iFY) + length(iEX) * 6, actual + j + ((iEX-1)*length(iEY)),
                                                    actual+length(iEX)*length(iEY)+j+length(iEY)*2,
                                                    actual+length(iEX)*length(iEY)+length(iEY)*3+j+length(iEY)*2),
                                                  c(1,- pointX[, 3], 1, -1))
    direction <<- list(direction, "==")
    rhs <<- list(rhs, 0)
  })


  #Nvx pb
  actual <- Nbvar - 12 + 1
  allconstraint <- allconstraint %>>% .addCons(Nbvar,
                                               actual + 2, 1)
  direction <- list(direction, "==")
  rhs <- list(rhs, fr_min)

  allconstraint <- allconstraint %>>% .addCons(Nbvar,
                                               actual + 3, 1)
  direction <- list(direction, "==")
  rhs <- list(rhs, be_min)

  allconstraint <- allconstraint %>>% .addCons(Nbvar,
                                               actual + 7, 1)
  direction <- list(direction, "==")
  rhs <- list(rhs, de_min)

  allconstraint <- allconstraint %>>% .addCons(Nbvar,
                                               actual + 10, 1)
  direction <- list(direction, "==")
  rhs <- list(rhs, de_max)

  sapply(iFY, function(j){
    allconstraint <<- allconstraint %>>%  .addCons(Nbvar,
                                                  c(j, actual:(actual + 2)),
                                                  c(-1,face[j,1],face[j,2],face[j,3]))
    direction <<- list(direction, "<=")
    rhs <<- list(rhs, 0)
  })


  sapply(iFY, function(j){
    allconstraint <<- allconstraint %>>%  .addCons(Nbvar,
                                                   c(j, (actual + 3):(actual + 5)),
                                                   c(-1,face[j,1],face[j,2],face[j,3]))
    direction <<- list(direction, "<=")
    rhs <<- list(rhs, 0)
  })
  sapply(iFY, function(j){
    allconstraint <<- allconstraint %>>%  .addCons(Nbvar,
                                                   c(j, (actual + 6):(actual + 8)),
                                                   c(-1,face[j,1],face[j,2],face[j,3]))
    direction <<- list(direction, "<=")
    rhs <<- list(rhs, 0)
  })
  sapply(iFY, function(j){
    allconstraint <<- allconstraint %>>%  .addCons(Nbvar,
                                                   c(j, (actual + 9):(actual + 11)),
                                                   c(-1,face[j,1],face[j,2],face[j,3]))
    direction <<- list(direction, "<=")
    rhs <<- list(rhs, 0)
  })


  rhs <- unlist(rhs)
  direction <- unlist(direction)
  matConstr <- matrix(unlist(allconstraint), ncol = Nbvar, byrow = TRUE)

  l_constraint <- L_constraint(L = matConstr,
                               dir = direction,
                               rhs = rhs)
  list(l_constraint = l_constraint, bounds = bounds, Nbvar = Nbvar)
}


#' Resolve optimisation problem matrix
#'
#' @param face \code{data.table}, face for 3 country, BE, DE anf FR
#' @param pointX \code{data.table}, extreme points for 3 country, BE, DE anf FR
#' @param faceY \code{data.table}, face for 3 country, BE, DE anf FR for all tuple in face
#' @param problem \code{optimization_model}, make with \link{askProblem}
#' @param alpha \code{numeric}, between 0 and 1, error ponderation if 0 error of type 1 is ignored
#' if 1 error of type 0 is ignored
#'
#' @import pipeR
#' @noRd
resolvBmat <- function(face, pointX, faceY, problem, alpha)
{
  Nbvar <- problem$Nbvar
  iEX <- 1:nrow(pointX)
  iEY <- 1:nrow(faceY)
  iFY <- 1:nrow(face)

  obj <- unlist(.addCons(NULL, Nbvar, c((length(iFY) +1): (length(iFY) + length(iEX)*6),
                                       (1+length(iFY) + length(iEX)* 6 + length(iEY) * 3 + length(iEY)*length(iEX)):
                                         (length(iFY) + length(iEX)* 6 + length(iEY) * 3 + length(iEY)*length(iEX) + 6*length(iEY))),
                        c(rep((1-alpha)/nrow(pointX),  length(iEX)*6), rep((alpha)/nrow(faceY), 6*length(iEY)))))
  LP <- OP(obj, problem$l_constraint, maximum = FALSE,
           bounds = problem$bounds)
  y <- ROI_solve(LP, solver = "clp", control = list(amount = 0))
  y
}



#' Add contraints
#'
#' @param mat \code{list}, previous constraints
#' @param long \code{numeric}, number of element
#' @param conc \code{conc}, place of no 0 elements
#' @param val \code{val}, value of elmeents
#'
#' @noRd
.addCons <- function(mat, long, conc, val){
  cont <- rep(0, long)
  cont[conc] <- val
  mat <- list(mat, cont)
  mat
}

