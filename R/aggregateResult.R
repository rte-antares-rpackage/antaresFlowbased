#' Move file after simulations
#'
#' @param opts \code{list} of simulation parameters returned by the function \link{setSimulationPath}
#' @param simName \code{character} name of simulation return by \link{runSimulation}
#' 
#' @examples
#'
#' \dontrun{
#' }
#'
#' @rdname moveFilesAfterStudy
#' @export
moveFilesAfterStudy <- function(opts, simName)
{
  #Found courcern files
  outputs <- paste0(opts$studyPath, "/output")
  allStudy <- list.files(outputs)
  allStudy
  simName <- tolower(simName)
  allStudySel <- allStudy[grepl(simName, allStudy)]
  sel <-  which(allStudySel[1] == allStudy)
  
  #Fount study type
  opts2 <- antaresRead::setSimulationPath(opts$studyPath, sel)
  type <- unlist(strsplit(opts2$simDataPath, "/"))
  opts <- antaresRead::setSimulationPath(opts$studyPath, 0)
  type <- type[length(type)]
  outputs_start <- paste0(outputs, "/", allStudySel)
  
  #Creat output dir
  outData <- paste0(opts$studyPath, "/output")
  dateTim <-  substr(as.character(round(Sys.time(), "mins")),1,16)
  dateTim2 <- dateTim
  dateTim <- gsub("-", "", dateTim)
  dateTim <- gsub(":", "", dateTim)
  dateTim <- gsub(" ", "-", dateTim)
  
  
  outData <- paste0(outData, "/", paste0(dateTim, substr(simName, 1, nchar(simName)-10) ))
  outData
  
  
  #Copy all first study
  file.rename(outputs_start[1], outData)
  
  
  
  #Move others MC years
  if(length(outputs_start>1))
  {
    outputs_mc_ind <- paste0(outputs_start[-1],  "/", type, "/mc-ind")
    allMcYear <- lapply(outputs_mc_ind, function(X){
      paste0(X, "/",list.files(X))})%>>%
      unlist
    
    namesMc <- lapply(outputs_mc_ind, list.files)%>>%
      unlist
    outDataMc <- paste0(outData, "/", type, "/mc-ind")
    
    
    file.rename(allMcYear, paste0(outDataMc, "/", namesMc))
  }
  #Remove olds folders
  unlink(outputs_start,recursive = TRUE )
  
  
  .editOutputInfo(outData = outData, 
                  simName = simName, 
                  dateTim2 = dateTim2)

  outDataMc
}


#' Creation of Mc-all
#'
#' @param opts \code{list} of simulation parameters returned by the function \link{setSimulationPath}
#' @param outDataMc \code{character} link to mc-inf folder return by \link{moveFilesAfterStudy}
#' 
#' @examples
#'
#' \dontrun{
#' }
#'
#' @rdname aggregateResult
#' @export
aggregateResult <- function(opts, outDataMc){
  
  newname <- strsplit(outDataMc, "/")[[1]]
  newname <- newname[length(newname)-2]
  opts <- antaresRead::setSimulationPath(opts$studyPath, newname)
  
  
  # names3row <- fread(allFileToLoad[1], nrows = 2)
  # res <- antaresRead::readAntares(areas = "fr", mcYears = "all", timeStep = "weekly",
  #                                 select = "allAreas", clusters = "fr")
  # 
  
  dtaMc <- paste0(opts$simDataPath, "/mc-ind")
  allMc <- paste0(dtaMc, "/", list.files(dtaMc))
  allDataToTransform <- list.files(allMc[1], recursive = TRUE)
  
  sapply(allDataToTransform, .transformToMaAll, opts = opts, allMc = allMc)
  
  
}

