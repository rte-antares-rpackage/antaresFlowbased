#' Give B from PTDF
#' 
#' @param PTDF \code{data.frame}, PTDF
#' @param nbClust \code{numeric}, number of cluster
#' 
#' @export
giveBClassif <- function(PTDF, nbClust = 36)
{
  #PTDF <- fread(PTDFPath)
  PTDF <- PTDF[!(DE == 0 & DE == 0 & FR == 0)]
  PTDFKm <- PTDF[,list(BE-NL, DE-NL, FR - NL)]
 # PTDFKm <- PTDF[,list(BE, DE, FR)]
  
  
  PTDFKmCare <- PTDFKm^2
  PTDFKmCare <- rowSums(PTDFKmCare)
  PTDFKm <- PTDFKm / sqrt(PTDFKmCare)
  
 #  table_ref <- data.frame(V1 = c(1,0,0), V2 = c(0,1,0), V3 = c(0,0,1))
 #  c1 <- c(1,1,1)
 #  c2 <- c(-1,1,1)
 #  c3 <- c(1,-1,1)
 #  c4 <- c(-1,-1,1)
 #  c5 <- c(1,0,1)
 #  c6 <- c(-1,0,1)
 #  c7 <- c(0,1,1)
 #  c8 <- c(0,-1,1)
 #  c9 <- c(1,1,0)
 #  c10 <- c(-1,1,0)
 #  table_ref <- rbind(table_ref, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10)
 #  table_ref <- rbind(table_ref, -table_ref)
 #  
 #  table_refOut <- table_ref
 #  names(table_refOut) <- c("BE", "DE", "FR")
 #  PTDFout <- PTDF[, .SD, .SDcols = c("BE", "DE", "FR")]
 #  PTDFout <- rbind(PTDFout, table_refOut)
 #  table_refCare <- table_ref^2
 #  table_ref <- table_ref / sqrt(rowSums(table_refCare))
 # names(table_ref) <- c("BE", "DE", "FR")
 #  PTDFKm <- rbind(PTDFKm, table_ref)
  # PTDFini <- PTDF[,.(DE, BE, FR)]
  # PTDFiniCare <- PTDFini^2
  # PTDFiniCare <- rowSums(PTDFiniCare)
  # PTDFini <- PTDFini/PTDFiniCare
  # PTDFini <- rbind(PTDFini, - PTDFini)
  # names(PTDFini) <- c("V1", "V2", "V3")
  # PTDFini <- rbind(PTDFini, PTDFKm)
  PTDFini <- PTDFKm
  #res <- cutree(hclust(dist(PTDFKm)), 36)
  res <- NULL
  set.seed(1)
  for(i in 1:250){

  res[[i]] <- kmeans(PTDFini, nbClust)
  
  }
  # PTDF$vect <- res[[which.min(unlist(lapply(res, function(X){X$tot.withinss})))]]$cluster
  # centers <- PTDF[, lapply(.SD, mean), .SDcols = c("BE", "DE", "FR"), by = "vect"]

  centers <- res[[which.min(unlist(lapply(res, function(X){X$tot.withinss})))]]$centers
  centers <- data.table(centers)
  names(centers) <- c("BE", "DE","FR")
  affectRow <- function(centers, valueVect)
  {
    conCernRow <- which.min(colSums((t(as.matrix(centers[, .SD, .SDcols = c("BE", "DE", "FR")]))-c(valueVect))^2))
    centers[conCernRow,c("BE", "DE","FR"):=as.list(valueVect)]
  }
  affectRow(centers, c(-1,0,0))
  affectRow(centers, c(0,-1,0))
  affectRow(centers, c(0,0,-1))
  affectRow(centers, c(0,1,0))
  centers[,c("BE", "DE", "FR")]
}