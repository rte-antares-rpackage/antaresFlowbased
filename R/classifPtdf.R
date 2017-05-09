#' Give B from PTDF
#' 
#' @param PTDF \code{data.frame}, PTDF
#' @param nbClust \code{numeric}, number of cluster
#' 
#' @export
giveBClassif <- function(PTDF, nbClust = 36)
{
  PTDF <- PTDF[!(DE == 0 & DE == 0 & FR == 0)]
  PTDFKm <- PTDF[,list(BE-NL, DE-NL, FR - NL)]
  
  PTDFKmCare <- PTDFKm^2
  PTDFKmCare <- rowSums(PTDFKmCare)
  PTDFKm <- PTDFKm / sqrt(PTDFKmCare)
 
  res <- cutree(hclust(dist(PTDFKm, method = "euclidean"), method = "ward.D"), 36)
  
  # res <- NULL
  # for(i in 1:300){
  #   res[[i]] <- kmeans(PTDFKm, 36)
  # }
  # res <- res[[which.min(unlist(lapply(res, function(X)X$tot.withins)))]]$cluster
  # 
  PTDFKm$V4 <- res
  centers <- PTDFKm[,lapply(.SD, mean), by = "V4"]
  centers <- centers[, .SD, .SDcols = c("V1", "V2", "V3")]
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