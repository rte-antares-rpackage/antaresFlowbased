#' Add typical day columns
#' @title Add typical day columns
#' 
#' @param data \code{antaresdata} data load by \link{readAntares}
#' @param fb_opts \code{list} of simulation parameters returned by the function \link{setSimulationPath} or fb model localisation obtain with \link{setFlowbasedPath}. Defaut to \code{antaresRead::simOptions()}
#' 
#' 
#' @examples
#'
#' \dontrun{
#' 
#' antaresRead::setSimulationPath("D:/Users/titorobe/Desktop/antaresStudy", 1)
#' data <- readAntares(mcYears = 1:10)
#' data <- addTypicalDayId(data)
#' 
#' 
#' data <- readAntares(areas = "all", links = "all", clusters = "all" ,mcYears = 1:10)
#' data <- addTypicalDayId(data)
#' 
#' }
#' 
#' @export
addTypicalDayId <- function(data, fb_opts = antaresRead::simOptions()){
  
  # .ctrlUserHour(opts)

  
  
  if(!"antaresData" %in%class(data)){
    warning(paste0("Your data are not antaresData object, antaresData objetc are object load by readAntares. If you have 
                   write your data in a csv file and you reload them after this is break the sytem of antaresData class.
                   You can write them in a .rds file, this system conserve antaresData class. For this straitement we will try to merge your object but 
                   this may cause a bug "))
  }
  
  
  if(attributes(data)$synthesis){
    stop("You can merge typical day with mcAll")
  }
  
  foldPath <- .mergeFlowBasedPath(fb_opts)
  
  scenario <- fread(paste0(foldPath, "scenario.txt"))
  ts <- fread(paste0(foldPath, "ts.txt"))

  
  tsTransform <- rbindlist(sapply(2:ncol(ts), function(X){
    oo <- ts[, .SD, .SDcols = c(1, X)]
    oo$simulation = X - 1
    oo
  }, simplify = FALSE))
  
  scenario$mcYear <- 1:nrow(scenario)
  out <- merge(tsTransform, scenario, by = "simulation", allow.cartesian = TRUE)
  
  out[,simulation := NULL]

  if("antaresDataList" %in% class(data))
  {
    key <- unique(data[[1]][, .SD, .SDcols = c("time", "mcYear")])
  }else{
    key <- unique(data[, .SD, .SDcols = c("time", "mcYear")])
    
  }
  
  key[,Date := as.Date(time)]
  
  key
  key$Date <- substr(key$Date, 6, 10)
  out$Date <- substr(out$Date, 6, 10)
  out2 <- merge(key, out, by = c("Date", "mcYear"))
  setnames(out2, "1", "typicalDay")
  out2[,Date := NULL]
  
  if("antaresDataList" %in% class(data)){
    for(i in 1:length(data)){
      data[[i]] <- merge(data[[i]], out2, by = c("time", "mcYear"))
    }
  }else{
    data <- merge(data, out2, by = c("time", "mcYear"))
    
  }
  data
  
}