#' Load weigth file
#'
#' @param weigth \code{character} path
#'
#' @return weigth \code{data.table} weigth file
#'
#' @noRd
.getWeight <- function(weigth){
  weigth <- data.table::fread(weigth, sep = ";", dec = ",")
  names(weigth) <- names(weigth)%>>%
    tolower()

  names(weigth) <- gsub(x=names(weigth), pattern =  "[.]", replacement = "%")
  weigth
}

#' Write weigth file
#'
#' @param path \code{character} path
#' @param weightData \code{data.frame} data
#'
#'
#' @noRd
.setWeight <- function(path, weightData){
  tmpfile <- file(description=path, "w")
  write.table(weightData, tmpfile, sep = "\t", dec = ".", row.names = FALSE)
  close(tmpfile)
}

#' Load second member file
#'
#' @param secondMember \code{character} path
#'
#' @return secondMember \code{data.table} secondMember file
#'
#' @noRd
.getSecondMember <- function(secondMember){
  secondMember <- data.table::fread(secondMember, sep = ";", dec = ",")
  secondMember[,.SD, .SDcols = c("Id_day", "Id_hour", "vect_b", "Name")]
}

#' Write second member file
#'
#' @param path \code{character} path
#' @param secondMemberData \code{data.frame} data
#'
#' @noRd
.setSecondMember <- function(path, secondMemberData){
  tmpfile <- file(description=path, "w")
  write.table(secondMemberData, tmpfile, sep = "\t", dec = ".", row.names = FALSE)
  close(tmpfile)
}

#' Load daytype file
#'
#' @param daytype \code{character} path
#'
#' @return daytype \code{data.table} daytype file
#'
#' @noRd
.getDayType <- function(daytype){
  data.table::fread(daytype, sep = ";", dec = ",", header = TRUE)
}

#' Write daytype file
#'
#' @param path \code{character} path
#' @param dayTypeData \code{data.frame} data
#'
#' @noRd
.setDayType <- function(path, dayTypeData){
  tmpfile <- file(description=path, "w")
  write.table(dayTypeData, tmpfile, sep = "\t", dec = ".", row.names = FALSE)
  close(tmpfile)
}

#' generate scenario
#'
#' @return scenario \code{data.frame} scenario file
#'
#' @noRd
.generateScenario <- function(){
  data.frame(simulation = rep(1:200, 5))
}

#' Write scenario file
#'
#' @param path \code{character} path
#' @param scenario \code{data.frame} data
#'
#' @noRd
.setScenario <- function(path, scenario){
  tmpfile <- file(description=path, "w")
  write.table(scenario, tmpfile, sep = "\t", dec = ".", row.names = FALSE)
  close(tmpfile)
}
