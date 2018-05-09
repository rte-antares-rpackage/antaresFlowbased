#' @title Projection of flow-based domains into a standardized shape (consistent with ANTARES studies)
#' 
#' @description 
#' This function follows two steps: based on the list of flow-based domains given in input, it will calculate a standard shape
#'  (selection of sides, k-medoid method) and will then project the real domains on this standard shape. The projection is 
#' computed using an optimisation algorithm aiming at minimising the volumetric error between the real domain and its projection.
#'  The function will then write in an output directory the standard shape (weights.txt), the projection result for each domain 
#'  (second_member.txt) and an RDS object including the projected domains. Reports can be writen ploting the real and
#' modelled domains and the volumetric error between them.
#'
#' @param PTDF \code{character}, path leading to the flow-based domains (PTDF description) list. This list be a csv file 
#' containing the following columns (and column names): 
#' \itemize{
#'  \item Id_day : numeric, name of each day. Default in example id_day between 1 and 12. 
#'  \item Period : numeric, hour of the day. Default in example period between 1 and 24 (1 is then between 00:00 and 01:00).
#'  \item BE : numeric, PTDF coefficient of Belgium.
#'  \item DE : numeric, PTDF coefficient of Germany. 
#'  \item FR : numeric, PTDF coefficient of France. 
#'  \item NL : numeric, PTDF coefficient of the Netherlands. 
#'  \item RAM : numeric, remaining margin in the critical branch (MW). 
#' }
#' @param outputName \code{character}, path/name of the output directory
#' @param reports \code{boolean}, if TRUE, the function will write html reports (one per typical day). Default to TRUE.
#' @param dayType \code{numeric}, default to All. (optionnal) Vector of id_days to compute.
#' @param hour \code{numeric}, default to All. (optionnal) vector of hours/periods to compute.
#' @param nbFaces \code{numeric}, standard shape parameters: number of sides to select, default to 36.
#' @param verbose \code{numeric}, shows log in console. Default to 0.
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


