#Get weigth file
.getWeight <- function(weigth){
  weigth <- data.table::fread(weigth, sep = ";", dec = ",")
  names(weigth) <- names(weigth)%>>%
    tolower()%>>%
    gsub(x=.,pattern =  "[.]", replacement = "%")
  weigth
}

#set weigth file
.setWeight <- function(path, weightData){
  tmpfile <- file(description=path, "w")
  write.table(weightData, tmpfile, sep = "\t", dec = ".", row.names = FALSE)
  close(tmpfile) 
}

#get second member file
.getSecondMember <- function(secondMember){
  secondMember <- data.table::fread(secondMember, sep = ";", dec = ",")
  secondMember[,.SD, .SDcols = c("Id_day", "Id_hour", "vect_b", "Name")]
}

#set second member file
.setSecondMember <- function(path, secondMemberData){
  tmpfile <- file(description=path, "w")
  write.table(secondMemberData, tmpfile, sep = "\t", dec = ".", row.names = FALSE)
  close(tmpfile) 
}

#get day type file
.getDayType <- function(daytype){
  data.table::fread(daytype, sep = " ", dec = ",", header = TRUE)
}

#set day type file
.setDayType <- function(path, dayTypeData){
  tmpfile <- file(description=path, "w")
  write.table(dayTypeData, tmpfile, sep = "\t", dec = ".", row.names = FALSE)
  close(tmpfile) 
}

#generate scenario
.generateScenario <- function(){
  data.frame(simulation = rep(1:200, 5))
}

#set scenario
.setScenario <- function(path, scenario){
  tmpfile <- file(description=path, "w")
  write.table(scenario, tmpfile, sep = "\t", dec = ".", row.names = FALSE)
  close(tmpfile) 
}
