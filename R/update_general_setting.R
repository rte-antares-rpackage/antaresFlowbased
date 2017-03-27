#' @title Change bindingconstraints.ini file
#'
#' @param opts \code{list} of simulation parameters returned by the function \link{setSimulationPath}
#'
#' @examples
#'
#' \dontrun{
#'
#' opts <- antaresRead::setSimulationPath("D:/Users/titorobe/Desktop/exemple_test",0)
#' updateGeneralSettingIni(opts)
#'
#' }
#'
#'
#' @seealso \code{writeGeneralSettingIni}
#' @export
updateGeneralSettingIni <- function(opts, playList = NULL){

  #Generate path for generaldata.ini
  generaldataIniPatch <- paste0(opts$studyPath, "/settings/generaldata.ini")


  if(is.null(playList))
  {
  #Modify general setting
  generalSetting <- modifyGeneralSetting(generaldataIniPatch)
  }else{
    generalSetting <- modifyGeneralSettingPlayList(generaldataIniPatch, playList)
  }
  #Write file
  writeGeneralSettingIni(generaldataIniPatch, generalSetting)
}




#' @title update general data
#' @import antaresRead
#' @export
modifyGeneralSetting <- function(generaldataIniPatch){
  generalSetting <- antaresRead:::readIniFile(generaldataIniPatch)

  #deactivation of mc_all
  generalSetting$output$synthesis <- FALSE

  #activation of mc_ind
  generalSetting$general$`year-by-year` <- TRUE

  #activation of playlist
  generalSetting$general$`user-playlist` <- TRUE

  generalSetting
}





#' @rdname udpate-generaldata_playList
#' @title Modify play list
#' @import antaresRead
#' @export
modifyGeneralSettingPlayList <- function(generaldataIniPatch, playList){
  generalSetting <- antaresRead:::readIniFile(generaldataIniPatch)


  playList <- sapply(playList, function(X){
    as.character(X)
  }, simplify = FALSE)
  names(playList) <- rep("playlist_year +",5)
  playList <- c(playlist_reset = FALSE, playList)

  generalSetting$playlist <- playList

  generalSetting
}




#' Write generaldata.ini file
#'
#' @param listData \code{list}, generaldata.ini as list R
#' @param generaldataIniPatch \code{character} patch of generaldata.ini file
#'
#' @export
writeGeneralSettingIni <- function(generaldataIniPatch, generalSetting)
{
  # open ew file
  write_data <- writeIni(generalSetting, generaldataIniPatch)

}
