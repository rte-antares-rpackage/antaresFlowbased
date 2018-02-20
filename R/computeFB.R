#' Compute flowbased approximation
#'
#' @param PTDF \code{character}, path for PTDF file format : 
#' \itemize{
#'  \item Id_day : column of id day, numeric. In default exemple between 1 and 12. 
#'  \item Period : column of period, numeric. In default exemple between 1 and 24.
#'  \item BE : column of BE, numeric. 
#'  \item DE : column of DE, numeric. 
#'  \item FR : column of FR, numeric. 
#'  \item FR : column of FR, numeric. 
#'  \item RAM : column of RAM, numeric. 
#' }
#' @param outputName \code{character}, name of output directory
#' @param reports \code{boolean}, product html reports by typical day or not
#' @param dayType \code{character / numeric} default All, can specify dayType to compute
#' @param hour \code{character / numeric} default All, can specify hour to compute
#' @param nbFaces \code{numeric} number of faces to keep, default 36.
#' @param verbose \code{numeric} show log in console. Defaut to 0.
#' \itemize{
#'  \item 0 : No log
#'  \item 1 : Short log
#' }
#' @import ROI
#' @import ROI.plugin.clp
#'
#' @export
computeFB <- function(PTDF = system.file("/input/ptdf/PTDF.csv", package
                                         = "antaresFlowbased"),
                      outputName =  paste0(getwd(), "/antaresInput"),
                      reports = TRUE,
                      dayType = "All", hour = "All", nbFaces = 36,
                      verbose = 0)
{
  
  Id_day <- Period <- NULL
  pb <- txtProgressBar(style = 3)
  univ <- .univ(nb = 500000, bInf = -10000, bSup = 10000)

  

  PTDF <- .readPTDF(PTDF)

  face <- giveBClassif(PTDF, nbClust = nbFaces)
  face <- round(face, 2)
  if(dayType[1] == "All"){
    dayType <- unique(PTDF$Id_day)
  }

  if(hour[1] == "All"){
    reports <- FALSE
    hour <- unique(PTDF$Period)
  }

  flowbased <- data.table(expand.grid(hour, dayType))
  names(flowbased) <- c("hour", "dayType")
  flowbased$outFlowBased <- rep(list(), nrow(flowbased))
  Nsim <- nrow(flowbased)
  RowUse <- 0
  sapply(hour, function(X){
    sapply(dayType, function(Y){
      if(verbose>0){
        cat(paste0("\n", "Optim for hour : ", X, " and typical day : ", Y, "\n"))
      }
      RowUse <<- RowUse + 1
      setTxtProgressBar(pb, RowUse/Nsim)
      PTDFsel <- PTDF[Id_day == Y & Period == X]

      pointX <- getVertices(as.matrix(PTDFsel[,.SD, .SDcols = c("BE","DE","FR","NL")]), PTDFsel$RAM)
      pointX <- data.table(pointX)

      res <- giveTuples(face, pointX)
      faceY <- do.call("cbind", apply(res, 2, function(X){
        face[X,]
      }))
      problem <- askProblemMat(pointX, faceY, face)
      out <- searchAlpha(face = face, pointX = pointX,
                         faceY = faceY,
                         problem = problem,
                         PTDF = PTDFsel,
                         univ = univ, verbose = verbose)
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
   nam <- 1:nrow(X$outFlowBased$face)
   nam <- as.character(nam)
   nam <- ifelse(nchar(nam) == 1, paste0("0", nam), nam)
   data.table(Id_day = X$dayType, Id_hour = X$hour,
              vect_b = X$outFlowBased$face$B,
              Name = paste0("FB", nam))
 }))

 allFaces$vect_b <- round(allFaces$vect_b, 0)
 dir.create(outputName)
 write.table(antaresFace, paste0(outputName, "/weight.txt"), row.names = FALSE, sep = "\t", dec = ".")
 saveRDS(flowbased, paste0(outputName, "/domainesFB.RDS"))
 write.table(allFaces, paste0(outputName, "/second_member.txt"), row.names = FALSE, sep = "\t", dec = ".")
 if(reports){
   outputNameReports <- paste0(outputName, "/reports")
   dir.create(outputNameReports)
   sapply(unique(flowbased$dayType), function(X){
     print(outputNameReports)
     generateReportFb(allFB = flowbased, dayType = X, output_file = outputNameReports)
   })
  }

 outputName
}

#' @title read PTDF file
#'
#' @description read PTDF file
#' @param PTDF \code{character} PTDF path.
#'
#' @return \code{data.table}
#'
#' @noRd
#'
.readPTDF <- function(PTDF){
  PTDF <- try(fread(PTDF))
  if("RAM_0" %in% names(PTDF))setnames(PTDF, "RAM_0", "RAM")
  
  if(any(names(PTDF) != c("Id_day", "Period", "BE", "DE", "FR", "NL", "RAM")))stop("Names of PTDF must be : Id_day, Period, BE, DE, FR, NL, RAM in this order")
  

  PTDF
}

#' Time Series file to optimisation output
#'
#' @param outputName \code{character}, name of output directory
#' @export
addTSFile <- function(outputName){
  Chroniques <- system.file("/input/ts.txt", package
                            = "antaresFlowbased")
  Chroniques <- fread(Chroniques)
  write.table(Chroniques,paste0(outputName, "/ts.txt"), row.names = FALSE, sep = "\t")
}
