#' Find first day of a study
#'
#' This function give type of first day, 1 is monday, 2 is Thuesday, ..., 7 is Sunday 
#'
#' @param opts \code{list} of simulation parameters returned by the function \link{setSimulationPath}. Defaut to \code{antaresRead::simOptions()}
#' @param firstArea \code{character} first area(s) use to compute first day
#' @param secondArea \code{character} second area(s) use to compute first day, it's a security when you want to
#' be sure of first day compute
#'
#' @examples
#' 
#' \dontrun{
#' 
#' opts <- setSimulationPath("MyPath")
#' identifyFirstDay(opts)
#' 
#' }
#' 
#' @export
identifyFirstDay <- function(opts, firstArea = "FR", secondArea = c("FR", "DE", "BE", "LU", "NL"))
{
  meanFR <- .giveMean7(firstArea, opts)
  
  if(!is.null(secondArea))meanCWE <- .giveMean7(secondArea, opts)
 
  fstFrMo <- .giveFstDay(meanFR)
  
  if(!is.null(secondArea))fstCweMo <- .giveFstDay(meanCWE)
  
  if(!is.null(secondArea))
  {
  if(fstFrMo != fstCweMo){
    stop("Cant find first day automatically estimate ")
  }else{
    fstFrMo <- switch(as.character(fstFrMo),
     "1" = 1,
     "2" = 7,
     "3" = 6,
     "4" = 5,
     "5" = 4,
     "6" = 3,
     "7" = 2
   )
    return(fstFrMo)
  }
  }else{
    warning("Only firstArea use to estimate first day")
    fstFrMo <- switch(as.character(fstFrMo),
                      "1" = 1,
                      "2" = 7,
                      "3" = 6,
                      "4" = 5,
                      "5" = 4,
                      "6" = 3,
                      "7" = 2
    )
    return(fstFrMo)
  }
  
}


.giveFstDay <- function(mean7days){
  min7 <- which.min(mean7days)
  mean7days[min7] <- mean7days[min7] + max(mean7days)
  secMin <- which.min(mean7days)
  areNear <- abs(min7 - secMin)
  if(areNear == 1 | areNear == 6){
    near <- TRUE
  }else{
    near <- FALSE
  }
  if(near){
    posVect <- c(min7, secMin)
    posVect <- posVect[order(posVect)]
    fstD <- NA
    if(identical(as.numeric(posVect), c(6, 7))){
      fstD = 1
    }else if(identical(as.numeric(posVect),c(1, 7))){
      fstD = 2
    }else {
      fstD <- max(posVect) + 1
    }
    return(fstD)
  }else{
    stop("Cant find first day automatically")
  }
}


.giveMean7 <- function(area, opts)
{
  LOAD <- readInputTS(load = area, timeStep = "daily", opts = opts)
  if(nrow(LOAD) == 0)stop(paste0("No data found for ", paste0(area, collapse = ";")))
  outTS <- dcast(LOAD, time~area+tsId, value.var = "load")
  meanByDay <- data.table(date = outTS$time, value =  rowMeans(outTS[, .SD, .SDcols = 2:ncol(outTS)]))
  meanByDay[,mod7 := 1:7]
  
  mean7 <- meanByDay[,mean(value), by = mod7]
  mean7 <- mean7$V1
  mean7
}


