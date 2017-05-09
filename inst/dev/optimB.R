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