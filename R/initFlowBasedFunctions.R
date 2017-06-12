#' Load weigth file
#'
#' @param weigth \code{character} path
#'
#' @return weigth \code{data.table} weigth file
#'
#' @noRd
.getWeight <- function(weigth){
  weigth <- data.table::fread(weigth, sep = "\t", dec = ".")
  names(weigth) <- names(weigth)%>>%
    tolower()
  if(any(names(weigth) != c("name", "be.fr", "de.fr", "de.nl", "be.nl", "be.de"))){
    stop("Names of weigth.txt must be name, be.fr, de.fr, de.nl, be.nl, be.de")
  }
  recontructName <- 1:nrow(weigth)
  recontructName <- as.character(recontructName)
  recontructName <- ifelse(nchar(recontructName) == 1, paste0(0, recontructName), recontructName)
  recontructName <- paste0("FB", recontructName)

  if(any(recontructName != weigth$name)){
    stop(paste0("name column of weigth.txt must contain in order : ", paste0(recontructName, collapse = ","),
         " ||  actualy : ", paste0( weigth$name, collapse = ",")))
  }

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
  secondMember <- data.table::fread(secondMember, sep = "\t", dec = ".")
  if(!all( c("Id_day", "Id_hour", "vect_b", "Name") %in% names(secondMember))){
    stop("Names of second_member.txt must contains Id_day, Id_hour, vect_b, Name")
  }
  secondMember$vect_b <- round(secondMember$vect_b, 0)
  nameConstraints <- sort(unique(secondMember$Name))
  nam <- 1:length(nameConstraints)
  nam <- ifelse(nchar(nam)==1, paste0(0, nam), nam)
  nam <- paste0("FB", nam)
  if(!(all(nameConstraints == nam))){
    stop("Problem in name of constraints in file second_member.txt")
  }

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
  daytype <- data.table::fread(daytype, sep = "\t", dec = ".", header = TRUE)

  if(dim(daytype)[2] == 1){
    stop("Problem in ts.txt format, sep must be tabulation")
  }
  #Control format date

  if(!"Date" %in% names(daytype)){
    stop('Missing Date column in ts.txt (must have a D uppercase')
  }
  if(!all(names(daytype)[2:ncol(daytype)] == as.character(1:(ncol(daytype)-1)))){
    stop(paste0("Problem in ts.txt chroniques names actual : ",
                paste0(names(daytype)[2:ncol(daytype)], collapse = ","),
                "  ||  Must be",
                paste0(as.character(1:(ncol(daytype)-1)), collapse = ",")))
  }

  dateControl <- strsplit(daytype$Date[1], "-")[[1]]
  if(length(dateControl)!=3){
    stop(paste0("Problem of date format in file ts.txt, must be AAAA-MM-DD, actual : ", daytype$Date[1]))
  }

  daytype
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
