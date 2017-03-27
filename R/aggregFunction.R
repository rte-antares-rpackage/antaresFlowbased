
.returnStats <- function(Y){
  c(colMeans(Y),
    Y[,lapply(.SD, var)]%>>%unlist,
    Y[,lapply(.SD, min)]%>>%unlist,
    Y[,lapply(.SD, max)]%>>%unlist)
}


.editOutputInfo <- function(outData, simulationName, dateTim2)
{
  #Edit infos output simulation
  iniPath <- paste0(outData, "/info.antares-output")
  infosIni <- antaresRead:::readIniFile(iniPath)
  infosIni$general$name <- substr(simulationName, 1, nchar(simulationName)-10)
  dateTim2 <- gsub("-" , ".", dateTim2)
  dateTim2 <- gsub(" " , " - ", dateTim2)
  infosIni$general$date <- dateTim2
  infosIni$general$title <- dateTim2
  writeIni(infosIni, iniPath)
}



.transformToMaAll <- function(filesToAggreg, opts, allMc)
{
  print(filesToAggreg)
  endPatch <- paste0(opts$simDataPath, "/mc-all/", filesToAggreg)
  allFileToLoad <- paste0(allMc,"/", filesToAggreg)

  endDir <- strsplit(endPatch, "/")%>>%unlist
  endDir <- paste(endDir[1:(length(endDir)-1)],collapse = "/")
  dir.create(endDir, recursive = TRUE)
  #recup type of aggreg
  type <- strsplit(filesToAggreg, "-")%>>%unlist
  type <- type[length(type)]
  type <-strsplit(type, "[.]")%>>%unlist
  type <- type[1]


  names3row <- read.csv(allFileToLoad[1], nrows = 6, blank.lines.skip = FALSE)
  write.table(names3row, endPatch, row.names = FALSE, eol = "\n",
              quote = FALSE)
  dtaTp <- sapply(allFileToLoad, function(X){
    fread(X, skip = 7, integer64 = "double")
  }, simplify = FALSE, USE.NAMES = FALSE)%>>%rbindlist


  #Change column if type is annual/mouthly/...
  if(type == "weekly")
  {
    dtaTpMean <- dtaTp[, lapply(.SD, mean), by = c("V2")]

  }

  if(type == "daily")
  {
    dtaTpMean <- dtaTp[, lapply(.SD, mean), by = c("V2", "V3", "V4")]
  }

  if(type == "hourly")
  {
    dtaTpMean <- dtaTp[, lapply(.SD, mean), by = c("V2", "V3", "V4", "V5")]
  }

  if(type == "monthly")
  {
    dtaTpMean <- dtaTp[, lapply(.SD, mean), by = c("V2", "V3")]
  }

  if(type == "annual")
  {
    dtaTpMean <- dtaTp
  }

  setcolorder(dtaTpMean, c("V1", names(dtaTpMean)[names(dtaTpMean)!="V1"]))


  write.table(dtaTpMean, endPatch,
              append = TRUE,
              row.names = FALSE,
              col.names =FALSE,
              quote = FALSE,sep = "\t",
              na = "")


}
