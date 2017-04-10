# load package
library(clpAPI)
require(data.table)
library(Matrix)
extreme_x <- fread("D:/Users/titorobe/Desktop/AMPL_MARION/FirstAmpl/a.txt")
extreme_x

full_xyz <- fread("D:/Users/titorobe/Desktop/AMPL_MARION/FirstAmpl/xyz_full36.txt")
full_xyz#B

system.time(res <- get_b36(extreme_x = extreme_x, full_xyz = full_xyz))
res




get_b36 <- function(extreme_x, full_xyz, n_block = NULL){

  # all_full_id : vecteur des id uniques
  all_full_id <- full_xyz[, unique(V1)]

  # triplet
  triplet <- t(combn(all_full_id, 3))

  # b
  b <- apply(as.matrix(extreme_x) %*% t(as.matrix(full_xyz[, list(V2, V3, V4)])), 2, max)

  # preparing the model
  lp <- initProbCLP()

  # minimize
  setObjDirCLP(lp, 1)

  # dimension pour un bloc d'optim
  param_ncols <- 9
  param_nrows <- 3+nrow(full_xyz)

  # preparing data
  if(is.null(n_block)){
    n_block <- nrow(triplet)
  }

  # initialisation des vecteurs
  x_block_size <- (5*3+nrow(full_xyz)*3)
  x <- vector(mode="double", length= x_block_size * n_block)

  indi_r <- c(rep(1, 5), rep(2, 5), rep(3, 5), rep(4:(4+(nrow(full_xyz) - 1)), each = 3))
  indi_r_block_size <- length(indi_r)
  i <- vector(mode="double", length= indi_r_block_size * n_block)

  # col indices
  indi_c <- c(c(1, 2, 7, 8, 9),
              c(3, 4, 7, 8, 9),
              c(5, 6, 7, 8, 9),
              rep(c(7, 8, 9), nrow(full_xyz)))

  indi_c_block_size <- length(indi_c)
  j <- vector(mode="double", length= indi_c_block_size * n_block)

  # bornes des contraintes
  constraint_ub <- vector(mode="double", length= param_nrows * n_block)

  constraint_lb <- vector(mode="double", length= param_nrows * n_block)

  ctrl <- lapply(1:n_block, function(ir){
    i <- triplet[ir, 1]
    j <- triplet[ir, 2]
    k <- triplet[ir, 3]
    # values of the matrix
    x[(x_block_size*(ir-1)+1):(x_block_size*(ir-1)+x_block_size)] <<- c(-1, 1, full_xyz[i, V2], full_xyz[i, V3], full_xyz[i, V4],
                                                                        -1, 1, full_xyz[j, V2], full_xyz[j, V3], full_xyz[j, V4],
                                                                        -1, 1, full_xyz[k, V2], full_xyz[k, V3], full_xyz[k, V4],
                                                                        as.vector(t(as.matrix(full_xyz[, list(V2, V3, V4)]))))

    # rows indices
    i[(indi_r_block_size*(ir-1)+1):(indi_r_block_size*(ir-1)+indi_r_block_size)] <<- indi_r + param_nrows * (ir-1)

    # col indices
    j[(indi_c_block_size*(ir-1)+1):(indi_c_block_size*(ir-1)+indi_c_block_size)] <<- indi_c + param_ncols*(ir-1)

    constraint_ub[(param_nrows*(ir-1)+1):(param_nrows*(ir-1)+param_nrows)] <<- c(b[i], b[j], b[k], b)

    constraint_lb[(param_nrows*(ir-1)+1):(param_nrows*(ir-1)+param_nrows)] <<-  c(b[i], b[j], b[k], rep(-Inf, length(b)))

    invisible()

  })

  # constraint matrix as sparse row-oriented
  coef_matrix <- sparseMatrix(i = i, j = j, x = x)
  ia <- coef_matrix@i
  ja <-  coef_matrix@p
  ar <- coef_matrix@x

  # Lower bounds for the variables (columns).
  var_lb <- rep(c(rep(0, 6), rep(-Inf, 3)), n_block)

  # objective function
  obj <- rep(c(rep(1, 6), rep(0, 3)), n_block)

  # load problem data
  loadProblemCLP(lp, ncols = param_ncols*n_block, nrows = param_nrows*n_block, ia, ja, ar,
                 lb = var_lb, ub = NULL, obj_coef = obj,
                 rlb = constraint_lb, rub = constraint_ub)


  # solve lp problem
  solveInitialCLP(lp)

  # retrieve the results
  # getSolStatusCLP(lp)
  # getObjValCLP(lp)
  # getColPrimCLP(lp)

  res <- getColPrimCLP(lp)
  # remove problem object
  delProbCLP(lp)

  
  res <- matrix(res, ncol = 9, byrow = TRUE)
  head(res)
  res <- data.table(res)
  names(res) <- c("ei_plus", "ei_moins", "ej_plus", "ej_moins", "ek_plus", "ek_moins", "y1", "y2", "y3")
  res
  
  re1 <- which(res$ei_plus<1e-6 &
                 res$ei_moins<1e-6  &
                 res$ej_plus<1e-6 &
                 res$ej_moins<1e-6 &
                 res$ek_plus<1e-6 &
                 res$ek_moins<1e-6)
  
  all_full_id <- full_xyz[, unique(V1)]
  triplet <- t(combn(all_full_id, 3))
  faisableTriplet <- triplet[re1,]
  faisableTriplet <- data.table(faisableTriplet)
  DD <- dist(res[re1,.SD, .SDcols = c("y1", "y2", "y3")])
  DD <- as.matrix(DD)
  diag(DD)<- diag(DD)+1
  finalTriplet <- faisableTriplet[which(apply(DD, 1, min)>1e-6),]
  finalTriplet
}

# input
extreme_x <- fread("D:/Users/titorobe/Desktop/AMPL_MARION/FirstAmpl/a.txt")
extreme_x

full_xyz <- fread("D:/Users/titorobe/Desktop/AMPL_MARION/FirstAmpl/xyz_full36.txt")
full_xyz

res_3 <- get_b36(extreme_x, full_xyz, n_block = 3)

system.time(res_all <- get_b36(extreme_x, full_xyz))
