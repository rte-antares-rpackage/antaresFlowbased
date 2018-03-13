#' Get vertices from faces
#' 
#' You have plan equation end distance between plan and origine in input You have vertices who are
#' include on convex polyhedron in output. 
#' 
#' @param face \code{data.table}, face for 3 country, BE, DE anf FR
#' @param b \code{numeric}, extreme points b
#'
#' @import pipeR
#'
#' @noRd
getVertices <- function(face, b){
  B <- as.matrix(face)
  IDfin <- 1:nrow(B)
  res <- sapply(IDfin, function(X)
  {
    # sapply(IDfin, function(Y){
    #   if(Y>=X)
    #   {
        sapply(IDfin, function(Z){
          if(Z>=X)
          {
            sapply(IDfin, function(ZZ){
              if(ZZ>=Z)
              {
                Bijk <- rbind(B[X,], B[X,], B[Z,], B[ZZ,], rep(1, 4))
                bijk <- c(b[X], b[X], b[Z], b[ZZ], 0)
                try({
                x <- qr.solve(Bijk, bijk)
                d <- b+1e-6
                if(all(B%*%x<=d)){
                  return(list(x = X, y = X, z = Z, zz = ZZ, y1 = x[1], y2 = x[2], y3 = x[3], y4 = x[4]))
                }
                }, silent = TRUE)
              }
              NULL
            },simplify = FALSE)
        #   }
        #
        # }, simplify = FALSE)
      }
    }, simplify = FALSE)
  }, simplify = FALSE)%>>%
    unlist%>>%
    matrix(ncol = 8, byrow = TRUE)

  res <- res[round(rowSums(res[,5:8]), 2) == 0,]
  DD <- dist(res[,5:8], method = "euclidean", p = 2, upper = FALSE)
  DD <- as.matrix(DD)
  DD[lower.tri(DD, diag = TRUE)] <- 1
  res <- res[which(apply(DD, 2, min)>1e-6),5:7]

  res
}



#' Gives tuples of B who check all constraints
#'
#' @param face \code{data.table}, face for 3 country, BE, DE anf FR
#' @param pointX \code{data.table}, extreme points for 3 country, BE, DE anf FR
#'
#' @import pipeR
#'
#' @noRd
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
              return(list(x = X, y = Y, z = Z, y1 = x[1], y2 = x[2], y3 = x[3]))
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

#' Transform B to antares format
#'
#' @param B \code{data.table}, face for 3 country, BE, DE anf FR
#'
#' @noRd
.fromBtoAntares <- function(B){
  names(B) <- c("BE", "DE", "FR")
  nam <- as.character(1:nrow(B))
  nam <- ifelse(nchar(nam)==1, paste0(0, nam), nam)
  coefAntares <- data.table(Name = paste0("FB", nam),
                            BE.FR = round(B$BE - B$FR, 2),
                            DE.FR = round(B$DE - B$FR, 2),
                            DE.NL = round(B$DE, 2),
                            BE.NL = round(B$BE, 2),
                            BE.DE = round(B$BE - B$DE, 2) )
  coefAntares
}




