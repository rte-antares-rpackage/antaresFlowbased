#' @title Move file after simulations
#'
#' @description Move file after simulations
#'
#' @param opts \code{list} of simulation parameters returned by the function \link{setSimulationPath}
#' @param simulationName \code{character} name of simulation return by \link{runSimulationFB}
#' @param verbose \code{numeric} show log in console. Defaut to 1
#' \itemize{
#'  \item{0}{ : No log}
#'  \item{1}{ : Short log}
#'  \item{2}{ : Long log}
#'}
#' 
#' @import antaresRead
#'
moveFilesAfterStudy <- function(opts, simulationName, verbose = 1)
{
  #Found files
  outputs <- paste0(opts$studyPath, "/output")
  allStudy <- list.files(outputs)
  allStudy
  simulationName <- tolower(simulationName)
  allStudySel <- allStudy[grepl(simulationName, allStudy)]
  sel <-  which(allStudySel[1] == allStudy)

  #Found study type
  oldw <- getOption("warn")
  options(warn = -1)
  opts2 <- antaresRead::setSimulationPath(opts$studyPath, sel)
  options(warn = oldw)

  type <- unlist(strsplit(opts2$simDataPath, "/"))
  oldw <- getOption("warn")
  options(warn = -1)
  opts <- antaresRead::setSimulationPath(opts$studyPath, 0)
  options(warn = oldw)
  type <- type[length(type)]
  outputs_start <- paste0(outputs, "/", allStudySel)

  #Create output dir
  outData <- paste0(opts$studyPath, "/output")
  dtTime <- Sys.time()
  dateTim <-  substr(as.character(round(dtTime, "mins")),1,16)

  dateTim2 <- dateTim
  dateTim <- gsub("-", "", dateTim)
  dateTim <- gsub(":", "", dateTim)
  dateTim <- gsub(" ", "-", dateTim)

  realName <- substr(simulationName, 1, nchar(simulationName)-10)
  realName <- paste0(dateTim, realName)
  outData <- paste0(outData, "/", realName)
  outData

  #Copy initial study
  file.rename(outputs_start[1], outData)

  #Move others MC years
  if(length(outputs_start>1))
  {
    outputs_mc_ind <- paste0(outputs_start[-1],  "/", type, "/mc-ind")
    allMcYear <- lapply(outputs_mc_ind, function(X){
      paste0(X, "/",list.files(X))}) %>>% unlist

    namesMc <- lapply(outputs_mc_ind, list.files) %>>% unlist
    outDataMc <- paste0(outData, "/", type, "/mc-ind")

    file.rename(allMcYear, paste0(outDataMc, "/", namesMc))
  }

  #Remove old folders
  unlink(outputs_start,recursive = TRUE )

  .editOutputInfo(outData = outData,
                  simulationName = simulationName,
                  dateTim2 = dateTim2,
                  dtTim = dtTime)

  realName
}


#' Creation of Mc_all
#'
#' @param opts \code{list} of simulation parameters returned by the function \link{setSimulationPath}
#' @param newname \code{character} name of simulation.
#' @param verbose \code{numeric} show log in console. Defaut to 1
#' \itemize{
#'  \item{0}{ : No log}
#'  \item{1}{ : Short log}
#'  \item{2}{ : Long log}
#'}
#' 
#' @import plyr data.table
#'
aggregateResult <- function(opts, newname, verbose = 1){
  if(verbose > 0)
  {
    try({
      pb <- txtProgressBar(style = 3)
    })
  }

  oldw <- getOption("warn")
  options(warn = -1)
  opts <- antaresRead::setSimulationPath(opts$studyPath, newname)
  options(warn = oldw)

  #Version which readAntares
  linkTable <- try(data.table::fread(system.file("/input/format_output/tableOutput.csv", package = "antaresFlowbased")),
                   silent = TRUE)
  .errorTest(linkTable, verbose, "Load of link table")

  #load link table
  linkTable$progNam <- linkTable$Stats
  linkTable$progNam[which(linkTable$progNam == "values")] <- "EXP"
  dtaMc <- paste0(opts$simDataPath, "/mc-ind")

  numMc <- as.numeric(list.files(dtaMc))

  #sapply on timeStep
  allTyped <- c("annual", "daily", "hourly", "monthly", "weekly")
  sapply(allTyped, function(type, verbose)
  {

    .addMessage(verbose, paste0("------- Mc-all : ", type, " -------"))

    try({
    
    #load first MC-year
    a <- Sys.time()
    dta <- antaresRead::readAntares(area = "all", links = "all", clusters = "all",
                                    timeStep = type, simplify = FALSE, mcYears = numMc[1], showProgress = FALSE)
   
    if(length(dta)>0){  
   dtaLoadAndcalcul <- try({
     
    
      
      aTot <- as.numeric(Sys.time() - a)
      SDcolsStartareas <- switch(type,
                                 daily = 6,
                                 annual = 4,
                                 hourly = 7,
                                 monthly = 5,
                                 weekly = 4

      )
      SDcolsStartClust <-  SDcolsStartareas + 1
      #make structure
      struct <- list(areas = dta$areas[,.SD, .SDcols = 1:SDcolsStartareas],
                     links = dta$links[,.SD, .SDcols = 1:SDcolsStartareas],
                     clusters = dta$clusters[,.SD, .SDcols = 1:SDcolsStartClust])

      if(type == "weekly"){
        struct$areas$timeId <- as.numeric(substr(struct$areas$time, nchar(as.character(struct$areas$time[1]))-1,
                                                 nchar(as.character(struct$areas$time[1]))))
        struct$link$timeId <- as.numeric(substr(struct$link$time, nchar(as.character(struct$link$time[1]))-1,
                                                nchar(as.character(struct$link$time[1]))))
        struct$clusters$timeId <- as.numeric(substr(struct$clusters$time, nchar(as.character(struct$clusters$time[1]))-1,
                                                    nchar(as.character(struct$clusters$time[1]))))
      }

      if(!is.null(struct$areas$day)){
        struct$areas$day <- ifelse(nchar(struct$areas$day) == 1,
                                   paste0("0", struct$areas$day),
                                   as.character(struct$areas$day))
      }
      if(!is.null(struct$links$day)){
        struct$links$day <- ifelse(nchar(struct$links$day) == 1,
                                   paste0("0", struct$links$day),
                                   as.character(struct$links$day))
      }
      if(!is.null(struct$links$day)){
        struct$clusters$day <- ifelse(nchar(struct$clusters$day) == 1,
                                      paste0("0", struct$clusters$day),
                                      as.character(struct$clusters$day))
      }

      b <- Sys.time()
      #value structure
      value <- .giveValue(dta, SDcolsStartareas, SDcolsStartClust)
      N <- length(numMc)
      value <- lapply(value, .creatStats)
      btot <- as.numeric(Sys.time() - b)
      if(verbose>0)
      {
        try({
          .progBar(pb, type, 1, N)
        })
      }
      #sequentially add values
      if(N>1)
      {
        for(i in 2:N){

          a <- Sys.time()
          dtaTP <- antaresRead::readAntares(area = "all", links = "all", clusters = "all",
                                            timeStep = type, simplify = FALSE, mcYears = numMc[i],
                                            showProgress = FALSE)

          aTot <- aTot + as.numeric(Sys.time() - a)
          b <- Sys.time()
          valueTP <- .giveValue(dtaTP, SDcolsStartareas, SDcolsStartClust)
          valueTP <- lapply(valueTP, .creatStats)

          value$areas <- .updateStats(value$areas, valueTP$areas)
          value$links <- .updateStats(value$links, valueTP$links)
          value$clusters <- .updateStats(value$clusters, valueTP$clusters)

          btot <- btot + as.numeric(Sys.time() - b)
          if(verbose>0)
          {
            try({
              .progBar(pb, type, i, N)
            })
          }
        }
      }

      #Calcul of sd
      oldw <- getOption("warn")
      options(warn = -1)
      b <- Sys.time()
      value$areas$std <- sqrt((value$areas$sumC - ((value$areas$sum * value$areas$sum)/N))/(N))
      #nan due to round
      for (i in names(value$areas$std))
        value$areas$std[is.nan(get(i)), (i):=0]

      value$links$std <- sqrt((value$links$sumC - ((value$links$sum * value$links$sum)/N))/(N))
      #nan due to round
      for (i in names(value$links$std))
        value$links$std[is.nan(get(i)), (i):=0]

      value$clusters$std <- sqrt((value$clusters$sumC - ((value$clusters$sum * value$clusters$sum)/N))/(N))
      #nan due to round
      for (i in names(value$clusters$std))
        value$clusters$std[is.nan(get(i)), (i):=0]

      options(warn = oldw)
      value$areas$sumC <- NULL
      value$links$sumC <- NULL
      value$clusters$sumC <- NULL
      value$areas$sum <- value$areas$sum / N
      value$links$sum <- value$links$sum / N
      value$clusters$sum <- value$clusters$sum / N
      btot <- btot + as.numeric(Sys.time() - b)
      .addMessage(verbose, paste0("Time for reading data : ", round(aTot,1), " secondes"))
      .addMessage(verbose, paste0("Time for calculating : ", round(btot,1), " secondes"))
    }, silent = TRUE)

    .errorTest(dtaLoadAndcalcul, verbose, "Load data and calcul")

    #Write area
    allfiles <- c("values")
    areaWrite <- try(sapply(allfiles, function(f)
    {
      #prepare data for all country
      areaSpecialFile <- linkTable[Folder == "area" & Files == f & Mode == tolower(opts$mode)]
      namekeep <- paste(areaSpecialFile$Name, areaSpecialFile$Stats)
      namekeepprog <- paste(areaSpecialFile$Name, areaSpecialFile$progNam)
      areas <- cbind(value$areas$sum,  value$areas$std, value$areas$min, value$areas$max)
      areas <- areas[, .SD, .SDcols = which(names(areas)%in%namekeepprog)]
      areas <- areas[, .SD, .SDcols = match(namekeepprog, names(areas))]
      nbvar <- ncol(areas)
      areas <- cbind(struct$areas, areas)
      ncolFix <- ncol(struct$areas)-3
      areas[, c("mcYear", "time") := NULL]
      allAreas <- unique(areas$area)

      for(i in 1:length(namekeepprog))
      {
        var <- namekeepprog[i]
        dig <- areaSpecialFile[var == paste(Name,progNam )]$digits
        areas[, c(var) := .(do.call(round, args = list(get(var), digits = dig)))]
      }

      sapply(allAreas,  function(areasel){
        #for each country prepare file
        areastowrite <- areas[area == areasel]
        areastowrite[,c("area") := NULL]
        indexMin <- min(areas$timeId)
        indexMax <- max(areas$timeId)
        kepNam <- names(struct$areas)[!names(struct$areas)%in%c("area","mcYear","time")]
        nameIndex <- ifelse(type == "weekly", "week", "index")
        kepNam[which(kepNam == "timeId")] <- nameIndex
        #write txt
        .writeFileOut(dta = areastowrite, timestep = type, fileType = f,
                      ctry = areasel, opts = opts, folderType = "areas", nbvar = nbvar,
                      indexMin = indexMin, indexMax = indexMax, ncolFix = ncolFix,
                      nomcair = areaSpecialFile$Name, unit = areaSpecialFile$Unit,
                      nomStruct = kepNam,Stats = areaSpecialFile$Stats)


      })
    }), silent = TRUE)

    .errorTest(areaWrite, verbose, "Area write")

    allfiles <- c("values")
    linkWrite <- try(sapply(allfiles, function(f)
    {
      #prepare data for all link
      linkSpecialFile <- linkTable[Folder == "link" & Files == f & Mode == tolower(opts$mode)]
      namekeep <- paste(linkSpecialFile$Name, linkSpecialFile$Stats)
      namekeepprog <- paste(linkSpecialFile$Name, linkSpecialFile$progNam)
      links <- cbind(value$links$sum,  value$links$std, value$links$min, value$links$max)
      links <- links[, .SD, .SDcols = which(names(links)%in%namekeepprog)]
      links <- links[, .SD, .SDcols = match(namekeepprog, names(links))]
      nbvar <- ncol(links)
      links <- cbind(struct$links, links)
      ncolFix <- ncol(struct$links)-3
      links[, c("mcYear", "time") := NULL]
      allLink<- unique(links$link)

      for(i in 1:length(namekeepprog))
      {
        var <- namekeepprog[i]
        dig <- linkSpecialFile[var == paste(Name,progNam )]$digits
        links[, c(var) := .(do.call(round, args = list(get(var), digits = dig)))]
      }

      sapply(allLink,  function(linksel){
        #for eatch link prepare file
        linkstowrite <- links[link == linksel]
        linkstowrite[,c("link") := NULL]
        indexMin <- min(links$timeId)
        indexMax <- max(links$timeId)
        kepNam <- names(struct$link)[!names(struct$link)%in%c("link","mcYear","time")]
        nameIndex <- ifelse(type == "weekly", "week", "index")
        kepNam[which(kepNam == "timeId")] <- nameIndex
        #write txt
        .writeFileOut(dta = linkstowrite, timestep = type, fileType = f,
                      ctry = linksel, opts = opts, folderType = "links", nbvar = nbvar,
                      indexMin = indexMin, indexMax = indexMax, ncolFix = ncolFix,
                      nomcair = linkSpecialFile$Name, unit = linkSpecialFile$Unit,
                      nomStruct = kepNam,Stats = linkSpecialFile$Stats)
      })
    }), silent = TRUE)

    .errorTest(linkWrite, verbose, "Link write")

    ##Details
    details <- value$clusters$sum
    endClust <- cbind(struct$clusters, details)
    endClust[, c("mcYear") := NULL]

    detailWrite <- try(sapply(unique(endClust$area),  function(ctry){
      #for each country prepare file
      endClustctry <- endClust[area == ctry]
      orderBeg <- unique(endClustctry$time)
      endClustctry[,c("area") := NULL]

      if(tolower(opts$mode) == "economy")
      {
        nameBy <- c("production EXP", "NP Cost EXP", "NODU EXP")
      }else{
        nameBy <- c("production EXP")
      }
      nomStruct <- names(endClustctry)[!names(endClustctry)%in%
                                         c("cluster", nameBy)]
      tmp_formula <- nomStruct
      tmp_formula <- as.formula(paste0(paste0(tmp_formula, collapse = "+"), "~cluster"))

      if(tolower(opts$mode) == "economy")
      {
        endClustctry[, c(nameBy) := list(round(`production EXP`),
                                         round(`NP Cost EXP`),
                                         round(`NODU EXP`))]
      }else{
        endClustctry[, c(nameBy) := list(round(`production EXP`))]
      }
      endClustctry <- data.table::dcast(endClustctry, tmp_formula,
                                        value.var = c(nameBy))

      endClustctry <- endClustctry[match(orderBeg, endClustctry$time)]
      endClustctry[,c("time") := NULL]
      nomStruct <- nomStruct[-which(nomStruct == "time")]
      nomcair <- names(endClustctry)
      nomcair <- nomcair[!nomcair%in%nomStruct]
      nbvar <- length(nomcair)
      unit <- rep("", length(nomcair))
      unit[grep("production EXP_",nomcair)] <- "MWh"
      unit[grep("NP Cost EXP_",nomcair)] <- "NP Cost - Euro"
      unit[grep("NODU EXP_",nomcair)] <- "NODU"
      nomcair <- gsub("production EXP_","",nomcair)
      nomcair <- gsub("NP Cost EXP_","",nomcair)
      nomcair <- gsub("NODU EXP_","",nomcair)
      Stats <- rep("EXP", length(unit))
      nameIndex <- ifelse(type == "weekly", "week", "index")
      nomStruct[which(nomStruct == "timeId")] <- nameIndex
      indexMin <- min(endClustctry$timeId)
      indexMax <- max(endClustctry$timeId)
      ncolFix <- length(nomStruct)
      #write details txt
      .writeFileOut(dta = endClustctry, timestep = type, fileType = "details",
                    ctry = ctry, opts = opts, folderType = "areas", nbvar = nbvar,
                    indexMin = indexMin, indexMax = indexMax, ncolFix = ncolFix,
                    nomcair = nomcair, unit = unit, nomStruct = nomStruct,Stats = Stats)
    }), silent = TRUE)
    .errorTest(detailWrite, verbose, "Detail write")

    }
    })
    .addMessage(verbose, paste0("------- End Mc-all : ", type, " -------"))

  }, verbose = verbose)

  if(verbose>0)
  {
    try({
      close(pb)
    })
  }

}

#' @title Extract value part of data
#'
#' @description Extract value part of data
#'
#' @param dta \code{data.table} of data load which antaresRead::readAntares
#' @param SDcolsStartareas \code{numeric} first column of data for areas
#' @param SDcolsStartClust \code{numeric} first column of data for details
#'
#' @return value {data.table} value selected
#'
#' @noRd
.giveValue <- function(dta, SDcolsStartareas, SDcolsStartClust)
{
  value <- list(areas = dta$areas[,lapply(.SD, as.numeric), .SDcols = (SDcolsStartareas+1):ncol(dta$areas)],
                links = dta$links[,lapply(.SD, as.numeric), .SDcols = (SDcolsStartareas+1):ncol(dta$links)],
                clusters = dta$clusters[,lapply(.SD, as.numeric), .SDcols = (SDcolsStartClust+1):ncol(dta$clusters)])
  value
}

#' @title Create stat file compute min, max sd and mean
#'
#' @description Create stat file compute min, max sd and mean
#'
#' @param X \code{data.table} data load which
#' antaresRead::readAntares and extract which .giveValue
#'
#' @return res {data.table} stats computed
#'
#' @noRd
#'
.creatStats <- function(X){

  # res <- list(sum = X, min = X, max = X,
  #             sumC = data.table::data.table(sapply(X, function(Z) Z*Z)))

  res <- list(sum = X, min = X, max = X, sumC = X*X)

  names(res$sum) <- paste(names(X), "EXP")
  names(res$min) <- paste(names(X), "min")
  names(res$max) <- paste(names(X), "max")
  names(res$sumC) <- paste(names(X), "std")
  res
}

#' @title Update data min, max sd and mean
#'
#' @description Update data min, max sd and mean
#'
#' @param X \code{data.table} data init which .creatStats
#' @param Y \code{data.table} data load which
#' antaresRead::readAntares and extract which .giveValue
#'
#' @return X {data.table} stats updated
#'
#' @noRd
.updateStats <- function(X, Y){
  X$sum <-  X$sum + Y$sum
  X$min <-  pmin.fast(X$min , Y$min)
  X$max <-  pmax.fast(X$max , Y$max)
  X$sumC <-  X$sumC + Y$sumC
  X
}

# fast pmin and pmax for two elements
pmin.fast <- function(k,x) (x+k - abs(x-k))/2
pmax.fast <- function(k,x) (x+k + abs(x-k))/2



#' @title Write mc-all files
#'
#' @description Write mc-all files
#'
#' @param dta \code{data.table} data
#' @param timestep \code{character} must be annual, monthly, weekly, daily or hourly
#' @param fileType \code{character} must be values or details
#' @param ctry \code{character} country.
#' @param opts \code{list} of simulation parameters returned by the function \link{setSimulationPath}
#' @param folderType \code{character} must be areas or links
#' @param nbvar \code{numeric} for header write, currently ncol(dta)
#' @param indexMin \code{numeric} for header write, depend of number of row write and calandar
#' @param indexMax \code{numeric} for header write, depend of number of row write and calandar
#' @param ncolFix \code{numeric} for header write
#' @param nomcair \code{character} for header write, names of variables
#' @param unit \code{character} for header write, unit of variables
#' @param nomStruct \code{character} for header write, depend of timestep
#' @param Stats \code{character} for header write, stats compute for eatch variables
#'
#'
#' @noRd
#'

.writeFileOut <- function(dta, timestep, fileType, ctry, opts, folderType, nbvar,
                          indexMin, indexMax, ncolFix, nomcair, unit, nomStruct, Stats){


  # threads for fwrite
  data.table::setDTthreads(2)

  folderTypesansS <- substr(folderType, 1, nchar(folderType)-1)
  abrtype <- substr(fileType, 1, 2)

  if(timestep == "annual"){
    nomStruct <- ""
    dta$timeId <- "Annual"
  }

  if(folderType == "links"){
    ctryDecomp <- strsplit(as.character(ctry), " - ")%>>%unlist
    entete <- paste0(ctryDecomp[1], "\t",folderTypesansS,"\t",abrtype,
                     "\t",timestep,"\n",ctryDecomp[2] ,"\tVARIABLES\tBEGIN\tEND\n\t",
                     nbvar, "\t",indexMin, "\t",indexMax, "\n\n",
                     ctryDecomp[1], "\t", timestep, paste0(rep("\t", ncolFix), collapse = ""),
                     paste0(nomcair, collapse = "\t"),"\n",
                     paste0(rep("\t", ncolFix+1), collapse = ""),paste0(unit, collapse = "\t"),"\n",
                     "\t", paste0(nomStruct, collapse = "\t"), "\t", paste0(Stats, collapse = "\t"), "\n")
  }else{
    entete <- paste0(ctry, "\t",folderTypesansS,"\t",abrtype, "\t",timestep,"\n\tVARIABLES\tBEGIN\tEND\n\t",
                     nbvar, "\t",indexMin, "\t",indexMax, "\n\n",
                     ctry, "\t", timestep, paste0(rep("\t", ncolFix), collapse = ""),
                     paste0(nomcair, collapse = "\t"),"\n",
                     paste0(rep("\t", ncolFix+1), collapse = ""),paste0(unit, collapse = "\t"),"\n",
                     "\t", paste0(nomStruct, collapse = "\t"), "\t", paste0(Stats, collapse = "\t"), "\n")
  }

  dir.create(paste0(opts$simDataPath, "/mc-all", "/",folderType,"/", ctry),
             recursive = TRUE, showWarnings = FALSE)

  outputFile <- paste0(opts$simDataPath, "/mc-all", "/",folderType,"/", ctry, "/",
                       fileType, "-",timestep,".txt")

  file <- file(outputFile, "wb")

  write.table(entete,file , row.names = FALSE, eol = "",
                          quote = FALSE, col.names = FALSE)

  # write.table(cbind(NA, dta), file,
  #             append = TRUE,
  #             row.names = FALSE,
  #             col.names =FALSE,
  #             quote = FALSE,sep = "\t",
  #             na = "")

  close(file)

  data.table::fwrite(cbind(NA, dta), outputFile,
              append = TRUE,
              row.names = FALSE,
              quote = FALSE, sep = "\t",
              eol = "\n", na = "")


}


#' @title Edit info on output folder
#'
#' @description Edit info on output folder
#'
#' @param outData \code{character} out folder path
#' @param simulationName \code{character} simulation name
#' @param dateTim2 \code{datetime} simulation begin date time
#' @param dtTim \code{datetime} simulation end date time
#'
#' @noRd
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


#' @title Progress bar
#'
#' @description Progress bar
#'
#' @param pb \code{progressbar} progress bar to update
#' @param timestep \code{character} must be annual, monthly, weekly, daily or hourly
#' @param timestep \code{character} must be annual, monthly, weekly, daily or hourly
#' @param mcALLNum \code{numeric} current mcYears position
#' @param nbmcallTOT \code{numeric} number of  mcYear
#'
#' @return progress bar update
#'
#' @noRd
.progBar <- function(pb, timeStep, mcALLNum, nbmcallTOT)
{

  usalTime <- data.frame(period = c("start","annual", "daily", "hourly", "monthly", "weekly"),
                         value = c(0, 20, 200, 600, 800, 1000))
  per <- which(usalTime$period == timeStep)
  dif <- usalTime$value[per] - usalTime$value[per - 1]
  approxEnd <- mcALLNum/nbmcallTOT
  i = (dif * approxEnd + usalTime$value[per - 1]) / usalTime$value[length( usalTime$value)]
  setTxtProgressBar(pb, i)

}
