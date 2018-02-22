.ctrlUserHour <- function(opts){
  
  inif <- file.path(opts$simPath, "info.antares-output")
  outputHour <- antaresEditObject::readIniFile(inif)
  date <- outputHour$general$date
  outputDate <- as.POSIXlt(date, format = "%Y.%m.%d - %H:%M")
  
  inif <- file.path(opts$studyPath, "user", "flowbased", "infos.ini")
  outputHour <- antaresEditObject::readIniFile(inif)
  date <- outputHour$general$date
  
  userDateDate <- as.POSIXlt(date, format = "%Y-%m-%d %H:%M")
      
  if(userDateDate > outputDate){
    warning("user folder write after output folder. Be carefull the domains data may not match with output data")
  }
  
  
}
