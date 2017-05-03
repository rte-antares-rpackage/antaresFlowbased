#' Compute flowbased approximation
#'
#' @param PTDF \code{character}, path for PTDF file
#' @param face \code{character}, path for face file
#' @param dayType \code{character / numeric} default All, can specify dayType to compute
#' @param hour \code{character / numeric} default All, can specify hour to compute
#'
#' @import ROI
#' @import ROI.plugin.clp
#'
#' @export
computeFB <- function(PTDF = system.file("/optimWork/PTDF.csv", package
                                         = "antaresFlowbased"),
                      face = system.file("/optimWork/B.csv", package
                                         = "antaresFlowbased"),
                      dayType = "All", hour = "All")
{

  univ <- .univ(nb = 200000, bInf = -10000, bSup = 10000, seed = 123456789)

  PTDF <- fread(PTDF)
  # face <- fread(face)
  face <- giveBClassif(PTDF, nbClust = 36)

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


  flowbased
}


