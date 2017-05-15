# library(ROI)
# library(ROI.plugin.clp)
# 
# get_b36_ROI <- function(extreme_x, full_xyz, n_block = NULL){
# 
#   # all_full_id : vecteur des id uniques
#   all_full_id <- full_xyz[, unique(V1)]
# 
#   # triplet
#   triplet <- t(combn(all_full_id, 3))
# 
#   # b
#   b <- apply(as.matrix(extreme_x) %*% t(as.matrix(full_xyz[, list(V2, V3, V4)])), 2, max)
# 
# 
#   # dimension pour un bloc d'optim
#   param_ncols <- 9
#   param_nrows <- 3+nrow(full_xyz)
# 
#   # preparing data
#   if(is.null(n_block)){
#     n_block <- nrow(triplet)
#   }
# 
#   # initialisation des vecteurs
#   x_block_size <- (5*3+nrow(full_xyz)*3)
#   x <- vector(mode="double", length= x_block_size * n_block)
# 
#   indi_r <- c(rep(1, 5), rep(2, 5), rep(3, 5), rep(4:(4+(nrow(full_xyz) - 1)), each = 3))
#   indi_r_block_size <- length(indi_r)
#   i <- vector(mode="double", length= indi_r_block_size * n_block)
# 
#   # col indices
#   indi_c <- c(c(1, 2, 7, 8, 9),
#               c(3, 4, 7, 8, 9),
#               c(5, 6, 7, 8, 9),
#               rep(c(7, 8, 9), nrow(full_xyz)))
# 
#   indi_c_block_size <- length(indi_c)
#   j <- vector(mode="double", length= indi_c_block_size * n_block)
# 
#   # bornes des contraintes
#   constraint_ub <- vector(mode="double", length= param_nrows * n_block)
# 
#   constraint_lb <- vector(mode="double", length= param_nrows * n_block)
# 
#   #
#   v_full_xyz <- as.vector(t(as.matrix(full_xyz[, list(V2, V3, V4)])))
# 
#   ctrl <- lapply(1:n_block, function(ir){
#     i <- triplet[ir, 1]
#     j <- triplet[ir, 2]
#     k <- triplet[ir, 3]
#     # values of the matrix
#     x[(x_block_size*(ir-1)+1):(x_block_size*(ir-1)+x_block_size)] <<- c(-1, 1, full_xyz[i, V2], full_xyz[i, V3], full_xyz[i, V4],
#                                                                         -1, 1, full_xyz[j, V2], full_xyz[j, V3], full_xyz[j, V4],
#                                                                         -1, 1, full_xyz[k, V2], full_xyz[k, V3], full_xyz[k, V4],
#                                                                         v_full_xyz)
# 
#     # rows indices
#     i[(indi_r_block_size*(ir-1)+1):(indi_r_block_size*(ir-1)+indi_r_block_size)] <<- indi_r + param_nrows * (ir-1)
# 
#     # col indices
#     j[(indi_c_block_size*(ir-1)+1):(indi_c_block_size*(ir-1)+indi_c_block_size)] <<- indi_c + param_ncols*(ir-1)
# 
#     constraint_ub[(param_nrows*(ir-1)+1):(param_nrows*(ir-1)+param_nrows)] <<- c(b[i], b[j], b[k], b)
# 
#     constraint_lb[(param_nrows*(ir-1)+1):(param_nrows*(ir-1)+param_nrows)] <<-  c(b[i], b[j], b[k], rep(-Inf, length(b)))
# 
#     invisible()
# 
#   })
# 
#   l_constraint <- L_constraint(L = slam::simple_triplet_matrix(i = i, j = j, v = x),
#                dir = ifelse(constraint_lb == "-Inf", "<=", "=="),
#                rhs = constraint_ub)
# 
#   # Lower bounds for the variables (columns).
#   var_lb <- rep(c(rep(0, 6), rep(-Inf, 3)), n_block)
#   lp_bound <- V_bound(li=1:length(var_lb), lb=var_lb)
# 
#   # objective function
#   obj <- rep(c(rep(1, 6), rep(0, 3)), n_block)
# 
#   LP <- OP(obj, l_constraint, maximum = FALSE,
#            bounds = lp_bound)
# 
#   y <- ROI_solve(LP, solver = "clp")
# 
#   y$solution
# }
# 
# 
# extreme_x <- fread("D:\\Users\\benothie\\Desktop\\codeAMPL\\codeAMPL-Benoit\\a.txt")
# extreme_x
# 
# full_xyz <- fread("D:\\Users\\benothie\\Desktop\\codeAMPL\\codeAMPL-Benoit\\xyz_full36.txt")
# 
# system.time(res <- get_b36_ROI(extreme_x = extreme_x, full_xyz = full_xyz))
# res
