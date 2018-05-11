#' @title Creation of time series of typical days for Antares studies
#' 
#' @description 
#' This function defines for each studied day in an Antares study the most representative typical flow-based day, and finally
#' creates daily time series.
#' To establish this correlation, the inputs of the function must include a probability matrix (calculated for each set of 
#' typical day with the package \code{flowBasedClustering}) and the path to the Antares study to convert. The probability matrix
#' will be used to compute a weighted draw among the possible typical days.
#'
#' @param opts \code{list} of simulation parameters returned by the function \link{setSimulationPath}. Link to the Antares study.
#'  Default to \code{antaresRead::simOptions()}
#' @param probabilityMatrix \code{list}, correlation between climatic factors and flow-based typical days, such as returned by
#'  \code{flowBasedClustering::getProbability}. The columns names must be renamed to match Antares' inputs, use the function
#'  \link{setNamesProbabilityMatrix}. Initial format is : area_variable (Ex: fr_load, de_solar ...)
#' @param multiplier \code{data.frame} enabling to convert load factors or normalised values into production/consumption in MW. 
#' 2 columns:
#' \itemize{
#' \item variable : Name of variable (ex: \code{"fr@wind"})
#' \item coef : mutiplier coefficient, for example the installed capacity.
#' }
#' @param interSeasonBegin \code{character or date}, date or vector of dates, YYYY-MM-DD, begin of interseason
#' @param interSeasonEnd \code{character or date}, date or vector of dates, YYYY-MM-DD, end of interseason
#' @param firstDay \code{numeric} Type of the first day of the study (between 1 and 7). For example, if the first day is a
#' wednesday, you must specify firstDay = 3.
#' @param seed \code{numeric} fixed random seed, used for the weighted draw of the typical days. 
#' @param silent \code{boolean}, non display of a progress bar, default to FALSE.
#' @param outputPath \code{character}, path to thefolder where the time series of typical flow-based output file (ts.txt) will 
#' be written.
#' 
#' @examples
#'
#' \dontrun{
#' library(antaresRead)
#' library(flowBasedClustering)
#' library(data.table)
#'
#' # load climate daily time serires
#' climate <- fread(system.file("dataset/climate_example.txt",package = "flowBasedClustering"))
#' 
#' # load clustering results (or build them with clusteringTypicalDays function())
#' clusterTD <- readRDS(system.file("dataset/cluster_example.RDS",package = "flowBasedClustering"))
#' 
#' levelsProba <- list(summerWd = list(FR_load = c(0.5), DE_wind = c(1/3, 2/3), DE_solar = .5),
#'                     summerWe = list(FR_load = c(0.5, 0.7), DE_wind = c(.5)),
#'                     interSeasonWd = list(FR_load = c(0.5, 0.6),
#'                                          DE_wind = c(1/3, 2/3),
#'                                          DE_solar = 0.3), 
#'                     
#'                     interSeasonWe = list(FR_load = c(0.5, 0.6),
#'                                          DE_wind = c(1/3, 2/3),
#'                                          DE_solar = 0.3))
#' matProb <- getProbability(climate, clusterTD,
#'  levelsProba = levelsProba, extrapolationNA = TRUE)
#' 
#' 
#' opts <- antaresRead::setSimulationPath("D:/Users/titorobe/Desktop/antaresStudy", 1)
#' 
#' 
#' 
#' matProb <- setNamesProbabilityMatrix(matProb, c("FR_load", "DE_wind", "DE_solar"),
#'                                    c("fr@load", "de@wind", "de@solar"))
#' 
#' multiplier <- data.frame(variable = c("fr@load", "de@wind", "de@solar"),
#'                          coef = c(1, 352250, 246403))
#'                          
#' firstDay <- identifyFirstDay(opts)
#' interSeasonBegin <- as.Date(c("2017-09-03", "2018-02-02"))
#' interSeasonEnd <- as.Date(c("2020-10-04", "2018-05-02"))
#' 
#' 
#' ts <- createFBTS(opts = opts, probabilityMatrix = matProb, multiplier = multiplier,
#'                  interSeasonBegin = interSeasonBegin,
#'                  interSeasonEnd = interSeasonEnd, firstDay = firstDay, outputPath = getwd())
#' }
#'                  
#' @import data.table
#' @export
createFBTS <- function(opts, probabilityMatrix, multiplier,
                       interSeasonBegin, interSeasonEnd, firstDay, seed = 04052017, silent = FALSE, outputPath = getwd()){
  
  
  
  ##test params
  #multiplier file
  if(!is.data.frame(multiplier)){
    stop("multiplier must be a data.frame")
  }
  if(!all(names(multiplier)%in%c("variable", "coef"))){
    stop("Error, names of multiplier must be : 'variable' and 'coef'")
  }
  
  ##Test probabilityMatrix
  lapply(probabilityMatrix, function(X){
    if(!all(multiplier$variable %in% names(X))){
      stop("All names contains in multiplier$variable must be present in probabilityMatrix")
    }
  })%>>%invisible()
  
  #interSeasonBegin
  interSeasonBegin <- as.Date(interSeasonBegin)
  interSeasonEnd <- as.Date(interSeasonEnd)
  #firstDay
  if(!firstDay%in%1:7)stop("firstDay must be between 1 and 7")

    
  #Seed
  set.seed(seed)
  #Name of climate variables
  sdC <- multiplier$variable
  sdC <- as.character(sdC)

  
  #Copy proba data
  quantiles <- copy(probabilityMatrix[[2]])
  proba <- copy(probabilityMatrix[[1]])
  
  
  
  
  ##Test data cohérency between multiplier and names of probabilityMatrix
  if(!all(sdC%in%names(quantiles))){
    stop("all multiplier variables must be contain in probabilityMatrix columns names")
  }
  
  if(!all(sdC%in%names(proba))){
    stop("all multiplier variables must be contain in probabilityMatrix columns names")
  }
  
  if(!all(grepl("@", sdC))){
    stop("probability matrix names must be changes using
         the syntax area@variable (e.g. fr@load). See function setNamesProbabilityMatrix().")
  }
  
  
  for(i in sdC){
    quantiles[, c(i) := lapply(.SD, function(X){
      X * multiplier[multiplier$variable == i,]$coef
    }), .SDcols = c(i)]
  }
  
  #Time series format
  .formatTs <- function(TS){
    names(TS)[ncol(TS)] <- paste0(TS$area[1],"@", names(TS)[ncol(TS)])
    TS[,"area" := NULL]
    TS
  }
  
  #Load concern TS
  listTs <- lapply(strsplit(sdC, "@"), function(X){
    reg <- list(X[1])
    
    if(! X[2] %in% c("load", "ror", "wind", "solar")) stop("Only solar, ror, wind and load are available")
    
    names(reg) <- X[2]
    reg$timeStep <- "daily"
    reg$showProgress <- !silent
    TS <- do.call("readInputTS", reg)
    TS <- .formatTs(TS)
    TS
  })
  
  #Merge TS
  reduceMerge <- function(x, y)merge(x, y, by = c("timeId","time","day","month","tsId"))
  outTs <- Reduce(reduceMerge, listTs)
  outTs
  
  ##Creat virtual calendar
  dates <- unique(outTs$time)

  
  calendar <- .getVirtualCalendar(dates, interSeasonBegin, interSeasonEnd, firstDay)
  calendar <- rbindlist(sapply(names(calendar), function(X){
    data.table(time = calendar[[X]], class = X)
  }, simplify = FALSE))
  outTs <- merge(outTs, calendar, by = "time")
  
  
  quantiles$quantiles <- gsub("Q", "", quantiles$quantiles)
  ##Use for test
  #outTs <- outTs[sample(1:nrow(outTs), 3000)]
  outTs <- data.table(outTs)
  #Compute typical day, first version (a bit slow) compute is done row/row
 
  names(quantiles) <- gsub("@", "__", names(quantiles))
  names(proba) <- gsub("@", "__", names(proba))
  
  
  
  
  quantiles <- data.frame(quantiles)
  
  
  if(!silent)cat("Compute typical day\n")
  if(!silent)pb <- txtProgressBar(char = "=", style = 3)
  
  names(outTs) <- gsub("@", "__", names(outTs))
  
  sdC<- gsub("@", "__", sdC)
  
  # sdC
  
  nN <- nrow(outTs)
  outTs$typicalDay <- sapply(1:nrow(outTs), function(R){
    # if(R%%100==0)print(R)
    #Select usefull data
    # outTs1 <- outTs[R]
    
    if(!silent)setTxtProgressBar(pb, R/nN)
    oo <- as.list(outTs[R])
    clAs <- oo$class
    
    quantilesClass <- quantiles[quantiles$class == clAs,]
    
    #For all variables create mini request
    allReq <- sapply(sdC, function(Y){
      
      quantilesClassVar <- quantilesClass[,c("quantiles", Y)]
      
      #If no row
      if(nrow(quantilesClassVar)>0){
        quantilesClassVar <- quantilesClassVar[!is.na(quantilesClassVar[,Y]),]
      }
      
      mynmber <- oo[[Y]]
      #If no row
      if(nrow(quantilesClassVar) == 0){
        littleReq <- paste0(Y, "== '0_1'")
      }
      #If one row
      if(nrow(quantilesClassVar) == 1){
        if(mynmber < quantilesClassVar[[Y]]){
          littleReq <- paste0(Y, "== '0_", quantilesClassVar$quantiles, "'")
        }else{
          littleReq <- paste0(Y, "== '", quantilesClassVar$quantiles, "_1'")
        }
      }
      #If more row
      if(nrow(quantilesClassVar) > 1){
        if(mynmber < quantilesClassVar[[Y]][1]){
          littleReq <- paste0(Y, "== '0_", quantilesClassVar$quantiles[1], "'")
        } else if(mynmber > quantilesClassVar[[Y]][length(quantilesClassVar[[Y]])]){
          littleReq <- paste0(Y, "== '", quantilesClassVar$quantiles[nrow(quantilesClassVar)], "_1'")
        }else{
          firstSup <- which.min(mynmber > quantilesClassVar[[Y]])
          littleReq <- paste0(Y, "== '", quantilesClassVar$quantiles[firstSup-1], "_",  quantilesClassVar$quantiles[firstSup], "'")
          
        }
      }
      return(littleReq)
    }, simplify = FALSE)
    
    #Make final request
    allReq <- unlist(allReq)
    allReq <- paste(allReq, collapse = " & ")
    allReq <- paste0(allReq, " & class == '", clAs ,"'")
    toSample <- proba[eval(parse(text = allReq)), .SD, .SDcols = c("idDayType", "probability")]
    
    #Sample typical day
    if(nrow(toSample) == 1){
      idJt <- toSample$idDayType
    }else{
      idJt <- sample(toSample$idDayType, 1, prob = toSample$probability)
    }
    idJt
  })
  
  #Formating TS output
  tsend <- outTs[, .SD, .SDcols = c("time", "tsId", "typicalDay")]
  tsend <- dcast(tsend, time~tsId, value.var = "typicalDay")
  
  setnames(tsend, "time", "Date")
  tsend$Date <- as.character(tsend$Date)
  write.table(tsend, paste0(outputPath, "/ts.txt"), sep = "\t", row.names = FALSE)
  
  tsend
}

#' @title Rename the probabilityMatrix variables
#' 
#' @description 
#' This function changes the name of the variables in the probabilityMatrix, created by \code{flowBasedClustering::getProbability} and 
#' used by \link{createFBTS}. Aim: rename the variables into a format consistent with Antares inputs ("area@variable").
#' For example, go to \link{createFBTS}.
#'
#' @param data \code{list}, probabilityMatrix whose columns to rename, calculated with \code{flowBasedClustering::getProbability}.
#' @param oldName \code{character} vector of old variables names
#' @param newName \code{character} vector of new variables names
#' 
#' @export
setNamesProbabilityMatrix <- function(data, oldName, newName){
  setnames(data[[1]],oldName, newName)
  setnames(data[[2]],oldName, newName)
  data
}
