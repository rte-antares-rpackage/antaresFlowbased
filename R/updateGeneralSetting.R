#' @title Change generaldata.ini file
#'
#' @description Change generaldata.ini file
#'
#' @param opts \code{list} of simulation parameters returned by the function \link{setSimulationPath}. Defaut to \code{antaresRead::simOptions()}
#' @param playList \code{numeric}, playList
#'
#' @examples
#'
#' \dontrun{
#'
#' opts <- antaresRead::setSimulationPath("D:/exemple_test", 0)
#' updateGeneralSettingIni(opts)
#'
#' }
#'
#' @seealso \code{writeGeneralSettingIni}
#'
#' @noRd
updateGeneralSettingIni <- function(opts = antaresRead::simOptions(), playList = NULL){

  # Generate path for generaldata.ini
  generaldataIniPatch <- paste0(opts$studyPath, "/settings/generaldata.ini")


  if(is.null(playList))
  {
  # Modify general setting
  generalSetting <- modifyGeneralSetting(generaldataIniPatch)
  }else{
    generalSetting <- modifyGeneralSettingPlayList(generaldataIniPatch, playList)
  }
  # Write file
  writeGeneralSettingIni(generaldataIniPatch, generalSetting)
}




#' @title update general data
#'
#' @description update general data
#'
#' @param generaldataIniPatch \code{character}, path of generaldataIni
#'
#' @import antaresRead
#'
#' @noRd
modifyGeneralSetting <- function(generaldataIniPatch){
  # read current .ini
  generalSetting <- antaresRead:::readIniFile(generaldataIniPatch)

  # desactivation of mc_all
  generalSetting$output$synthesis <- FALSE

  # activation of mc_ind
  generalSetting$general$`year-by-year` <- TRUE

  # activation of playlist
  generalSetting$general$`user-playlist` <- TRUE
  # if(generalSetting$general$filtering){
  #   stop("Param Output profile : Results filtering is Custom, he must be None for flowbased simulation")
  # }
  generalSetting
}


#' @title Modify playList
#'
#' @description Modify playList
#'
#' @param generaldataIniPatch \code{character}, path of generaldataIni
#' @param playList \code{numeric}, playList
#'
#' @import antaresRead
#'
#' @noRd
modifyGeneralSettingPlayList <- function(generaldataIniPatch, playList){
  # read current .ini
  generalSetting <- antaresRead:::readIniFile(generaldataIniPatch)



  # format playlist
  playList <- sapply(playList, function(X){
    as.character(X)
  }, simplify = FALSE)
  names(playList) <- rep("playlist_year +", length(playList))
  playList <- c(playlist_reset = FALSE, playList)

  # change
  generalSetting$playlist <- NULL
  generalSetting$playlist <- playList

  generalSetting
}

#' @title Write generaldata.ini file
#'
#' @description Write generaldata.ini file
#'
#' @param generaldataIniPatch \code{list}
#' @param generalSetting \code{Character}, Path to ini file
#'
#' @noRd
writeGeneralSettingIni <- function(generaldataIniPatch, generalSetting)
{
  # open new file
  writeIni(generalSetting, generaldataIniPatch)

}

.updateOptimizationIni <- function(file){
  ini <- antaresRead:::readIniFile(file)
  ini$filtering$`filter-year-by-year` <- .giveTotalFilter(ini)
  writeIni(ini, file)
}

.updateProportiesLinksIni <- function(file){
  ini <- antaresRead:::readIniFile(file)
  if(length(ini) > 0)
  {
  for(i in 1:length(ini)){
    if(!is.null(ini[[i]]$`filter-synthesis`)){
      ini[[i]]$`filter-year-by-year` <- .giveTotalFilterlinks(ini[[i]])
    }
  }
  writeIni(ini, file)
  }
}


.updateAllAreasIni <- function(opts){
  cat("filtering option is activated input file will be change to apply flowbased simulation. If you stop process now your filtering files can't be restore")
    ##Update areas
    unlink(paste0(opts$studyPath, "/user/tempfile"), recursive = TRUE)
  
    areasInputs <- paste0(opts$studyPath, "/input/areas/")
    areaS <- list.dirs(areasInputs, full.names = FALSE)
    areaS <- areaS[areaS!=""]
    fileToUpdate <- paste0(areasInputs, areaS, "/optimization.ini")
    
    #Keep old user file(s)
    newNameFile <- gsub("/input/", "/user/tempfile/", fileToUpdate)
    sapply(newNameFile, function(D){ dir.create(gsub("/optimization.ini", "",D), recursive = TRUE)})
    file.copy(fileToUpdate, newNameFile, overwrite = TRUE)
    
    #Change files
    sapply(fileToUpdate, .updateOptimizationIni) %>>% invisible()
    
    ##Update links
    linksInputs <- paste0(opts$studyPath, "/input/links/")
    linkS <- list.dirs(linksInputs, full.names = FALSE)
    linkS <- linkS[linkS!=""]
    fileToUpdate <- paste0(linksInputs, linkS, "/properties.ini")
    #Keep old user file(s)
    newNameFile <- gsub("/input/", "/user/tempfile/", fileToUpdate)
    sapply(newNameFile, function(D){ dir.create(gsub("/properties.ini", "",D), recursive = TRUE)})
    file.copy(fileToUpdate, newNameFile, overwrite = TRUE)
    
    #Change files
    sapply(fileToUpdate, .updateProportiesLinksIni) %>>% invisible()
}

.giveTotalFilter <- function(ini){
  existingFilter <- NULL
  filterToAdd  <- NULL
  
  if(!(is.null(ini$filtering$`filter-year-by-year`) || is.na(ini$filtering$`filter-year-by-year`))){
    existingFilter <- unlist(strsplit(ini$filtering$`filter-year-by-year`, ","))
  }
  if(!(is.null(ini$filtering$`filter-synthesis`) || is.na(ini$filtering$`filter-synthesis`))){
    filterToAdd <-  unlist(strsplit(ini$filtering$`filter-synthesis`, ","))
  }
  .totalFilterStrait(existingFilter, filterToAdd)
}


.giveTotalFilterlinks <- function(ini){
  existingFilter <- NULL
  filterToAdd  <- NULL
  
  if(!(is.null(ini$`filter-year-by-year`) || is.na(ini$`filter-year-by-year`))){
    existingFilter <- unlist(strsplit(ini$`filter-year-by-year`, ","))
  }
  if(!(is.null(ini$`filter-synthesis`) || is.na(ini$`filter-synthesis`))){
    filterToAdd <-  unlist(strsplit(ini$`filter-synthesis`, ","))
  }
  .totalFilterStrait(existingFilter, filterToAdd)
  
}

.totalFilterStrait <- function(existingFilter, filterToAdd){
  totalFilter <- unique(c(existingFilter, filterToAdd))
  totalFilter <- gsub(" ", "", totalFilter)
  totalFilter <- unique(c(totalFilter))

  if(length(totalFilter) > 0)
  {
    
    totalFilter <- totalFilter[c(
      which(totalFilter == "hourly"),
      which(totalFilter == "daily"),
      which(totalFilter == "weekly"),
      which(totalFilter == "monthly"),
      which(totalFilter == "annual")
    )]
    totalFilter <- paste(totalFilter, collapse = ", ")
  }
  totalFilter
}
