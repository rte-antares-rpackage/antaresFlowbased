#' Add net position 
#'
#' @param data \code{antaresDataList} read with readAntares and with areas and links tables
#' @param opts \code{list} of simulation parameters returned by the function \link{setSimulationPath}. Defaut to \code{antaresRead::simOptions()}
#' @param inAreas \code{character} areas who will included in IPN compute. All links present in inAreas 
#' (for 2 areas concerned by link) will be included. Others links are excluded. 
#' Be careful if only one area in two is present, link is excluded.
#' @param ADQ \code{boolan} apply straitemùent on ADQ columns
#' @param newName \code{character} end of new columns name. Default "_CWE".
#' 
#' 
#' @examples
#' \dontrun{
#' opts <- antaresRead::setSimulationPath("D:/Users/titorobe/Desktop/antaresStudy", 2)
#' data <- readAntares(area = "all", links = "all", mcYears = 1)
#' 
#' ##Add net position for CWE
#' data <- addNetPosition(data, opts, ADQ = FALSE)
#' 
#' ##Add net position for CWE+AT
#' data <- addNetPosition(data, opts, ADQ = FALSE,
#'  inAreas = c("be", "de", "fr", "nl", "at"), newName = "_CWEAt")
#' 
#' }
#' 
#' @export
addNetPosition <- function(data, opts = antaresRead::simOptions(), inAreas = c("be", "de", "fr", "nl"), ADQ = FALSE, newName = "_CWE"){
  allAreas <- getAreas(opts = opts)
  allLinks <- getLinks(opts = opts)
  allLinksSave <- allLinks
  allLinks <- strsplit(allLinks, " - ")
  
  
  
  ##Test if all nedded links are in data
  linkNedded <- unlist(lapply(allLinks, function(X){
    X[1]%in%inAreas&X[2]%in%inAreas
  }))
  
  if(!all(allLinksSave[linkNedded]%in%unique(data$links$link))){
    lkN <- allLinksSave[linkNedded]
    missLk <- lkN[!lkN%in%unique(data$links$link)]
    missLk <- paste0(missLk, collapse = ";")
    stop(paste0("Somes links are missing in your data : ",missLk))
  }
  
  
  allAreas <- allAreas[allAreas%in%inAreas]
  
  areaLinkTable <- sapply(allAreas, function(X){
    paste0(unlist(lapply(allLinks, function(Y){
      if(X == Y[1] & Y[2]%in% inAreas)
      {
        return(paste0("+`", Y[1], " - ", Y[2], "`"))
      }
      if(X == Y[2]& Y[1]%in% inAreas)
      {
        return(paste0("-`", Y[1], " - ", Y[2], "`"))
      }
      
      NULL
    })), collapse = "")
  }, simplify = FALSE)
  class(areaLinkTable)
  if(!ADQ){
    fl <- "FLOW LIN."
  }else{
    fl <- "FLOW LIN._ADQPatch"
  }
  
  ##Suppress no-compete area
  links <- dcast(data$links, time + mcYear~link, value.var = c(fl))
  areaLinkTable <- areaLinkTable[unlist(lapply(areaLinkTable, function(X){
    spl <- strsplit(X,"`")[[1]]
    aR <- spl[1:length(spl)%%2==0]
    all(aR %in% names(links))
  }))]
  
  
  suppressWarnings(
    for(i in 1:length(areaLinkTable)){
      links[,names(areaLinkTable)[i] := eval(parse(text =areaLinkTable[[i]] ))]
    }
  )
  links <- links[,.SD, .SDcols = c("time", "mcYear", names(areaLinkTable)[names(areaLinkTable)%in%names(links)])]
  links <- melt(links, id = 1:2)
  setnames(links, "variable", "area")
  if(!ADQ){
    newColumnName <- paste0("Balance", newName)
  }else{
    newColumnName <- paste0("Balance_ADQPatch", newName)
  }
  
  

  data$areas <- merge(data$areas, links, by = c("time", "mcYear", "area"), all.x = TRUE)
  
  data$areas$value[is.na(data$areas$value)] <- 0
  setnames(data$areas, "value", newColumnName)
  
  setorderv(data$areas, c("area", "mcYear", "timeId"))
  data
}


