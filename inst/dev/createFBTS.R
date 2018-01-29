#' Create time series of typical days for antares
#'
#' @param opts \code{list} of simulation parameters returned by the function \link{setSimulationPath}. Defaut to \code{antaresRead::simOptions()}
#' @param probabilityMatrix \code{list} from \link{flowBasedClustering::getProbability()}. Columns must be rename to corespond to antares names.
#' Format used is : area_variable (Ex, fr_load, de_solar ...)
#' @param multiplier \code{data.frame} contain 2 columns.
#' \itemize{
#' \item variable : Name of variable
#' \item coef : coefficient multiplier.
#' }
#' @param interSeasonBegin \code{character or date}, date or vector of dates, YYYY-MM-DD, begin of interseason
#' @param interSeasonEnd \code{character or date}, date or vector of dates, YYYY-MM-DD, end of interseason
#' @param firstDay \code{numeric} indication of type of first day of study. If the first day of study is a
#' wednesday, you must specify firstDay = 3.
#' @examples
#'
#' \dontrun{
#' library(antaresRead)
#' library(flowBasedClustering)
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
#' matProb <- flowBasedClustering::getProbability(climate, clusterTD, levelsProba = levelsProba, extrapolationNA = TRUE)
#' 
#' 
#' opts <- antaresRead::setSimulationPath("D:/Users/titorobe/Desktop/antaresStudy")
#' 
#' setnames(matProb[[1]],"FR_load", "fr_load" )
#' setnames(matProb[[2]],"FR_load", "fr_load" )
#' 
#' setnames(matProb[[1]],"DE_wind", "de_wind" )
#' setnames(matProb[[2]],"DE_wind", "de_wind" )
#' 
#' setnames(matProb[[1]],"DE_solar", "de_solar" )
#' setnames(matProb[[2]],"DE_solar", "de_solar" )
#' 
#' multiplier <- data.frame(variable = c("fr_load", "de_wind", "de_solar"),
#'                          coef = c(1, 352250, 246403))
#' firstDay <- identifyFirstDay(opts)
#' interSeasonBegin <- as.Date(c("2017-09-03", "2018-02-02"))
#' interSeasonEnd <- as.Date(c("2017-10-04", "2018-05-02"))
#' 
#' 
#' ts <- createFBTS(opts = opts, probabilityMatrix = matProb, multiplier = multiplier,
#'                  interSeasonBegin = interSeasonBegin, interSeasonEnd = interSeasonEnd, firstDay = firstDay)
#'                  
#' @import data.table
#' @export
createFBTS <- function(opts, probabilityMatrix, multiplier,
                       interSeasonBegin, interSeasonEnd, firstDay, seed = 04052017){
  
  
  
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
  })%>%invisible()
  
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
  
  for(i in sdC){
    quantiles[, c(i) := lapply(.SD, function(X){
      X * multiplier[multiplier$variable == i,]$coef
    }), .SDcols = c(i)]
  }
  
  #Time series format
  .formatTs <- function(TS){
    names(TS)[ncol(TS)] <- paste0(TS$area[1],"_", names(TS)[ncol(TS)])
    TS[,"area" := NULL]
    TS
  }
  
  #Load concern TS
  listTs <- lapply(strsplit(sdC, "_"), function(X){
    reg <- list(X[1])
    names(reg) <- X[2]
    reg$timeStep <- "daily"
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
  quantiles <- data.frame(quantiles)
  cat("Compute typical day\n")
  pb <- txtProgressBar(char = "=", style = 3)
  nN <- nrow(outTs)
  outTs$typicalDay <- sapply(1:nrow(outTs), function(R){
    # if(R%%100==0)print(R)
    #Select usefull data
    # outTs1 <- outTs[R]
    
    setTxtProgressBar(pb, R/nN)
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
  tsend
}
