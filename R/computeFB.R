#' Compute flowbased approximation
#'
#' @param PTDF \code{character}, path for PTDF file
#' @param dayType \code{character / numeric} default All, can specify dayType to compute
#' @param hour \code{character / numeric} default All, can specify hour to compute
#' @param nbFaces \code{numeric} number of faces to keep, default 36.
#'
#' @import ROI
#' @import ROI.plugin.clp
#'
#' @export
computeFB <- function(PTDF = system.file("/optimWork/PTDF.csv", package
                                         = "antaresFlowbased"),
                      outputName = "antaresInput",
                      reports = TRUE,
                      dayType = "All", hour = "All", nbFaces = 36)
{

  univ <- .univ(nb = 500000, bInf = -10000, bSup = 10000)

  PTDF <- fread(PTDF)
  face <- giveBClassif(PTDF, nbClust = nbFaces)

  if(dayType[1] == "All"){
    dayType <- unique(PTDF$Id_day)
  }

  if(hour[1] == "All"){
    hour <- unique(PTDF$Period)
  }

  flowbased <- data.table(expand.grid(hour, dayType))
  names(flowbased) <- c("hour", "dayType")
  flowbased$outFlowBased <- rep(list(), nrow(flowbased))
  sapply(hour, function(X){
    sapply(dayType, function(Y){
      print(paste0("hour ", X))
      print(paste0("dayType ", Y))
      PTDFsel <- PTDF[Id_day == Y & Period == X]

      pointX <- getVertices(as.matrix(PTDFsel[,.SD, .SDcols = c("BE","DE","FR","NL")]), PTDFsel$RAM_0)
      pointX <- data.table(pointX)

      res <- giveTuples(face, pointX)
      faceY <- do.call("cbind", apply(res, 2, function(X){
        face[X,]
      }))
      probleme <- askProblemeMat(pointX, faceY, face)
      out <- searchAlpha(face = face, pointX = pointX,
                         faceY = faceY,
                         probleme = probleme,
                         PTDF = PTDFsel,
                         univ = univ)
      out$pointX <- data.frame(pointX)
      names(out$pointX) <- c( "BE", "DE", "FR")
      out$param$hour <- X
      out$param$dayType <- Y
      out$param$alpha <- out$alpha
      out$alpha <- NULL
      flowbased[hour == X & dayType == Y]$outFlowBased[[1]] <<- list(out)
      NULL
    })
  })%>>%invisible

  ##From B to antares

 antaresFace <- .fromBtoAntares(face)
 
 ##Output
 allFaces <- rbindlist(apply(flowbased,1, function(X){
   data.table(Id_day = X$dayType, Id_hour = X$hour,
              X$outFlowBased$face,
              Name = paste0("FB", 1:nrow(X$outFlowBased$face)))
 }))
 setnames(allFaces, "B", "vect_b")
 outputName <- paste0(getwd(), "/", outputName)
 dir.create(outputName)
 write.table(antaresFace, paste0(outputName, "/coefficients_Antares.csv"), row.names = FALSE, sep = ";", dec = ",")
 saveRDS(flowbased, paste0(outputName, "/domainesFB.RDS"))
 write.table(allFaces, paste0(outputName, "/fichier_b_final.csv"), row.names = FALSE, sep = ";", dec = ",")
 if(reports){
   outputName <- paste0(outputName, "/reports")
   file.create(outputName)
   sapply(unique(flowbased$dayType), function(X){
     generateRaportFb(flowbased, X, outputName)
   })
  }


  flowbased
}


