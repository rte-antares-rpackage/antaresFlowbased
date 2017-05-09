#' Write ini file from list obtain by antaresRead:::readIniFile and modify by user
#'
#' @param listData \code{list}, modified list otain by antaresRead:::readIniFile
#' @param pathIni \code{Character}, Path to ini file
#'
#' @examples
#'
#' \dontrun{
#' pathIni <- "D:/exemple_test/settings/generaldata.ini"
#' generalSetting <- antaresRead:::readIniFile(pathIni)
#' generalSetting$output$synthesis <- FALSE
#' writeIni(generalSetting, pathIni)
#' }
#'
#' @import pipeR
#'
#' @export
#'
writeIni <- function(listData, pathIni){
  file.remove(pathIni)
  con <- file(pathIni, "wb")
  lapply(1:length(listData),
         .formatedIniList,
         dtaToTransform = listData,
         namesdtaToTransform = names(listData),
         con = con) %>>%
    invisible()
  close(con)
}

#' Change R format to ini format
#' @param val value to format
#'
#' @return val formated value
#'
#' @noRd
.formatedIni <- function(val)
{
  if(class(val) %in% c("numeric", "integer")){
    format(val, nsmall = 6)
  } else if(class(val) %in% c("logical")){
    if(is.na(val))
    {
      ""
    }else{
      tolower(as.character(val))
    }
  } else {
    val
  }
}

#' write ini (raw by raw)
#'
#' @param dtaToTransform \code{list} data to write
#' @param namesdtaToTransform \code{character} names of data to write
#' @param con file connection where data are write
#'
#' @noRd
.formatedIniList <- function(x, dtaToTransform, namesdtaToTransform, con = con){
  if(!is.null(namesdtaToTransform))
  {
    writeChar( paste0("[", namesdtaToTransform[x], "]\n"), con, eos = NULL)
  }else{
    writeChar(paste0("[", x-1, "]\n"), con, eos = NULL)
  }
  tmp_data <- dtaToTransform[[x]]
  # format values
  values <- sapply(tmp_data, .formatedIni)
  # write
  writeChar(paste(paste0(names(tmp_data), " = ", values), collapse = "\n"), con, eos = NULL)
  writeChar("\n\n", con, eos = NULL)
}
