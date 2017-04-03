

.editOutputInfo <- function(outData, simulationName, dateTim2, dtTim)
{
  #Edit infos output simulation
  iniPath <- paste0(outData, "/info.antares-output")
  infosIni <- antaresRead:::readIniFile(iniPath)
  infosIni$general$name <- substr(simulationName, 1, nchar(simulationName)-10)
  dateTim2 <- gsub("-" , ".", dateTim2)
  dateTim2 <- gsub(" " , " - ", dateTim2)
  infosIni$general$date <- dateTim2
  infosIni$general$title <- dateTim2
  infosIni$general$timestamp <- round(as.numeric(difftime(dtTim,
                                                    as.POSIXct("1970-01-01 00:00:00"), units = "sec")), 0)
  writeIni(infosIni, iniPath)
}


.transformToMaAll <- function(filesToAggreg, opts, allMc)
{
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

  headers <- read.csv(allFileToLoad[1], nrows = 6, blank.lines.skip = FALSE)
  write.table(headers, endPatch, row.names = FALSE, eol = "\n",
              quote = FALSE)

  SDcolsStart <- switch(type,
                        daily = 5,
                        hourly = 6,
                        monthly = 4,
                        annual = 3,
                        weekly = 3)
  dtaTp <- NULL
  for(i in 1:length(allFileToLoad))
  {
    if(is.null(dtaTp))
    {
      dtaTp <- fread(allFileToLoad[i], skip = 7, integer64 = "double")
      toOverWrite <- names(dtaTp[,.SD, .SDcols = SDcolsStart:ncol(dtaTp)])
    }else{
      dtaTp[,
            (toOverWrite):=dtaTp[, .SD, .SDcols = SDcolsStart:ncol(dtaTp)] +
              fread(allFileToLoad[i], skip = 7, integer64 = "double")[, .SD, .SDcols = SDcolsStart:ncol(dtaTp)]
            ]
    }
  }

  dtaTp[,(toOverWrite):=dtaTp[, .SD, .SDcols = SDcolsStart:ncol(dtaTp)]/length(allFileToLoad)]

  write.table(dtaTp, endPatch,
              append = TRUE,
              row.names = FALSE,
              col.names =FALSE,
              quote = FALSE,sep = "\t",
              na = "")
  
  
}