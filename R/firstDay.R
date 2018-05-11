#' @title Identify the type of the first day of a study
#' 
#' @description 
#' This function identifies the nature of the first day of a study, by reading the input load data of the study designated by
#'  \code{opts} (calculation of the mean profile of each day to identify the weekend loads). The type of first day is represented
#'  by a number : 1 = Monday, 2 = Tuesday, ..., 7 = Sunday.
#'
#' @param opts \code{list} of simulation parameters returned by the function \link{setSimulationPath} : path of Antares study. 
#' By default, the value is indicated by \code{antaresRead::simOptions()}.
#' @param firstArea \code{character} name of the area(s) to use to calculate the type of first day. By default, 
#' France (\code{"fr"}) is chosen.
#' @param secondArea \code{character} name of additional area(s) to use to check the first day. By default, the value is 
#' \code{c("fr", "de", "be", "nl")} (France, Germany, Belgium, the Netherlands). It can be \code{NULL} but the first day will be
#' determined with less certainty.
#'
#' @examples
#' \dontrun{
#' # Identification of the first day of an Antares study where areas fr, be, de, nl exist
#' opts <- setSimulationPath("pathToAntaresStudy")
#' identifyFirstDay(opts)
#' }
#' 
#' @export
identifyFirstDay <- function(opts, firstArea = "fr", secondArea = c("fr", "de", "be", "nl"))
{
  meanFR <- .giveMean7(firstArea, opts)
  
  if(!is.null(secondArea))meanCWE <- .giveMean7(secondArea, opts)
 
  fstFrMo <- .giveFstDay(meanFR)
  
  if(!is.null(secondArea))fstCweMo <- .giveFstDay(meanCWE)
  
  if(!is.null(secondArea))
  {
  if(fstFrMo != fstCweMo){
    stop("cannot automatically estimate which day of the week is the first day of the Antares study")
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
  mod7 <- value <- NULL
  LOAD <- readInputTS(load = area, timeStep = "daily", opts = opts, showProgress = FALSE)
  if(nrow(LOAD) == 0)stop(paste0("No data found for ", paste0(area, collapse = ";")))
  outTS <- dcast(LOAD, time~area+tsId, value.var = "load")
  meanByDay <- data.table(date = outTS$time, value =  rowMeans(outTS[, .SD, .SDcols = 2:ncol(outTS)]))
  meanByDay[,mod7 := 1:7]
  
  mean7 <- meanByDay[,mean(value), by = mod7]
  mean7 <- mean7$V1
  mean7
}


