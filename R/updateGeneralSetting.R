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
#'
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
#'
modifyGeneralSetting <- function(generaldataIniPatch){
  # read current .ini
  generalSetting <- antaresRead:::readIniFile(generaldataIniPatch)

  # desactivation of mc_all
  generalSetting$output$synthesis <- FALSE

  # activation of mc_ind
  generalSetting$general$`year-by-year` <- TRUE

  # activation of playlist
  generalSetting$general$`user-playlist` <- TRUE
  if(generalSetting$general$filtering){
    stop("Param Output profile : Results filtering is Custom, he must be None for flowbased simulation")
  }
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
#'
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
#'
writeGeneralSettingIni <- function(generaldataIniPatch, generalSetting)
{
  # open new file
  writeIni(generalSetting, generaldataIniPatch)

}
