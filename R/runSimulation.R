#' Run antares Simulation with several flow-based time series
#' 
#' @description
#' This function runs the simulation of an ANTARES study and changes the second members
#' of the binding constraints for each MC year so as to take into account several flow-based
#' domains time series in the model.
#' 
#' \code{runSimulationFB} function works on an ANTARES study whose input have been detailled with 
#' four new files, located in user/flowbased/ directory :
#' 
#' \itemize{
#'  \item weight.txt : names and weights of the binding constraints which define the FB domains
#'  \item second_member.txt : second members of the binding constraints for each typical day and hour 
#'  \item ts.txt : several time series of typical days to describe several aleas on flow-based domains
#'  \item scenario.txt : equivalent to scenario builder, define which time series should be use for each simulated mc-year
#' }
#' 
#' Those files can be automatically build with the function \link{initFlowBased}.
#'
#' @param simulationName \code{character} name of simulation. Defaut to 'FlowBased'.
#' @param mcAll \code{boolean} give mc_all compress results. Defaut to TRUE.
#' @param mcInd \code{boolean} keep mc_ind. Defaut to TRUE.
#' @param mcYears \code{numeric} include mcYears. Default all (all mcYears are included)
#' @param opts \code{list} of simulation parameters returned by the function \link{setSimulationPath}. Defaut to \code{antaresRead::simOptions()}
#' @param verbose \code{numeric} show log in console. Defaut to 1
#' \itemize{
#'  \item 0 : No log
#'  \item 1 : Short log
#'  \item 2 : Long log
#' }
#'
#' @examples
#'
#' \dontrun{
#' # set study
#' antaresRead::setSimulationPath("D:/exemple_test", 0)
#'
#' # set solver path
#' setSolverAntares(path = "C:\\Program Files\\RTE\\Antares\\5.0.9\\bin\\antares-5.0-solver.exe")
#'
#' # init directory for study
#' initFlowBased()
#'
#' # run flowbased simulation
#' runSimulationFB(simulationName = "R_from", verbose = 1)
#' }
#'
#' @export
#'
#' @import plyr
#'
runSimulationFB <- function(simulationName = "FlowBased", mcAll = TRUE, mcInd = TRUE,
                          mcYears = "all", opts = antaresRead::simOptions(),
                          verbose = 1){

  # mcAll & mcInd control
  if(mcAll == FALSE & mcInd == FALSE){
    stop("mcAll and mcInd are equal to FALSE")
  }

  # set simulation parent / first study
  oldw <- getOption("warn")
  options(warn = -1)
  opts <- antaresRead::setSimulationPath(opts$studyPath ,0)
  options(warn = oldw)

  # control mode
  if(!opts$parameters$general$mode %in%c("Economy" , "Adequacy")){
    stop(paste0("Study must be in 'Economy' or 'Adequacy' mode. Function not available for '", opts$parameters$general$mode, "'"))
  }

  #random name to identify simulation
  aleatNameSime <- sample(letters, 10, replace = TRUE) %>>%
    paste0(collapse = "")
  simNameAlea <- paste0(simulationName, aleatNameSime)
  simNameAlea <- tolower(simNameAlea)

  if(verbose == 2){
    cat(paste("Random name generation :", simNameAlea), "\n")
  }

  #Generate path for generaldata.ini
  generaldataIniPatch <- paste0(opts$studyPath, "/settings/generaldata.ini")
  generaldataIniOld <- paste0(opts$studyPath, "/settings/generaldata_old.ini")

  #copy old settings file
  file.copy(generaldataIniPatch, generaldataIniOld)

  #Update general settings and copy old file
  upGenIni <- try(updateGeneralSettingIni(opts), silent = TRUE)
  .errorTest(upGenIni, verbose, "Write of generaldata")

  #load second member
  second_member <- try(data.table::fread(paste0(opts$studyPath,"/user/flowbased/second_member.txt")), silent = TRUE)
  .errorTest(second_member, verbose, "Load of second_member.txt")

  #load ts
  ts <- try(data.table::fread(paste0(opts$studyPath,"/user/flowbased/ts.txt")), silent = TRUE)
  .errorTest(ts, verbose, "Load of ts.txt")

  #load scenarios
  scenario <- try(data.table::fread(paste0(opts$studyPath,"/user/flowbased/scenario.txt")), silent = TRUE)
  .errorTest(scenario, verbose, "Load of scenario.txt")

  #Exclude scenarios to redefine
  if(mcYears[1] != "all"){
    scenario[-c(mcYears)] <- NA
  }

  ##Prepare CMD to run antares
  AntaresPatch <- getSolverAntares()
  if(is.null(AntaresPatch)){
    stop("Antares solver path is not set. Use setSolverAntares()")
  }
  if(!file.exists(AntaresPatch)){
    stop("Antares solver does no exist. Use setSolverAntares()")
  }

  cmd <- '"%s" "%s" -n "%s"'
  cmd <- sprintf(cmd, AntaresPatch, opts$studyPath, simNameAlea)

  # simulation
  allScenario <- as.numeric(na.omit(unique(scenario$simulation)))
  
  # if(.test){
  #   allScenario <- allScenario[2:3]
  # }

  .addMessage(verbose, "---------- Antares part ---------- ", valAf = 1)
  timBegin <- Sys.time()

  progress <- ifelse(verbose == 0, "none" , "text")

  plyr::l_ply(allScenario, function(X, opts, ts, second_member,
                                    scenario, cmd, verbose, timBegin, allScenario){
    #Prepare files before simulation
    .addMessage(verbose, paste0("\n-Scenario : ",X))
    prepareSimulationFiles(opts = opts,
                           ts = ts,
                           secondMember = second_member,
                           scenarios = scenario,
                           simNumber = X,
                           verbose = verbose)


    cmd <- paste0(cmd, "Sim",X)
    .addMessage(verbose, paste0("Antares launching for ",  paste0("scenario : ",X) ))
    beg <- Sys.time()
    out <- .runAntares(cmd)
    out <- out[length(out)]

    if(grepl("error", out)){
      stop(paste0("The antares simulation ", X," must stop see antares logs for more details"))
    }

    .addMessage(verbose, paste0("Antares end for scenario : ",X))
    .addMessage(verbose, paste0("Compute time for scenario ",X, " : ",
                                as.numeric(round(Sys.time()-beg)), " secondes"))

    pourcentDo <- which(allScenario==X)/length(allScenario)

    timeRun <- as.numeric(difftime( Sys.time(), timBegin , units = "min"))
    timeRunMin <- trunc(timeRun)
    timMinSec <- timeRun %% 1
    timMinSec <- round(timMinSec/100*60, 2) * 100

    timeToDo <- timeRun * (1-pourcentDo)/pourcentDo
    timeToDoMin <- trunc(timeToDo)
    timeToDoSec <- timeToDo %% 1
    timeToDoSec <- round(timeToDoSec/100*60, 2) * 100

    if(verbose>1)
    {
      cat(paste0("\nAntares simulation : ", which(allScenario==X), " / ", length(allScenario),
                 "\n Current runtime : ", timeRunMin , ":", timMinSec,
                 "Sec\n Approximate remaning run time before aggregation : ",
                 timeToDoMin , ":", timeToDoSec, "Sec\n"))
    }

  }, opts = opts,
  ts = ts,
  second_member = second_member,
  scenario = scenario,
  cmd = cmd,
  verbose = verbose,
  timBegin = timBegin,
  allScenario = allScenario,
  .progress = progress)
  .addMessage(verbose, "---------- End of antares part ----------", valAf = 1)


  try({
    #Return old param setting
    file.remove(generaldataIniPatch)
    file.rename(generaldataIniOld, generaldataIniPatch)})

  if(mcAll){

    filesMoves <- try(moveFilesAfterStudy(opts, simNameAlea, verbose = verbose), silent = TRUE)
    .errorTest(filesMoves, verbose, "Creation of a single study with Antares format")

    # Mc-all creation
    .addMessage(verbose, "---------------- Mc-all computation ----------------", valAf = 1)

    aggregateResult(opts = opts, newname = filesMoves, verbose = verbose)

    try({
      dtaMc <- paste0(opts$simDataPath, "/mc-ind")
      if(!mcInd){
        unlink(dtaMc, recursive = TRUE)
      }})
    .addMessage(verbose, "---------------- End of Mc-all computation ----------------", valAf = 1)

    #Write digest
    digetsWrite <- try({
      oldw <- getOption("warn")
      options(warn = -1)
      opts <- antaresRead::setSimulationPath(opts$studyPath, filesMoves)
      diges <- data.table::fread(paste0(path.package("antaresFlowbased"), "/output/digest.csv"))
      options(warn = oldw)
      areas <- antaresRead::readAntares(timeStep = "annual", showProgress = FALSE)
      areas <- areas[, .SD, .SDcols = c(1:3,which(names(areas)%in%diges$Variable))]
      allNam <- names(areas)[-c(1:3)]
      areas[, c("timeId", "time"):= NULL]
      for (col in allNam) set(areas, j = col, value = as.numeric(areas[[col]]))
      allStats <- diges$CalcBuYear
      for(i in 1:length(allNam))
      {
        var <- allNam[i]
        fct <- allStats[i]
        areas[, c(var) := .(do.call(fct, args = list(get(var)))), by = area]
      }
      areas <- unique(areas)
      for (col in allNam) set(areas, j = col, value = as.character(areas[[col]], 0))
      coltoKeep <- match(names(areas)[-1], diges$Variable)
      unitKeep <- diges$Unit[coltoKeep]
      StatsKeep <- diges$Stats[coltoKeep]
      rentam <- names(areas)
      areas <- rbindlist(list(data.table(t(c("", unitKeep))),
                              data.table(t(c("", StatsKeep))),
                              areas), fill = FALSE)
      names(areas) <- rentam
      digets <- paste0(opts$simDataPath, "/mc-all/grid")
      dir.create(digets)
      write.table(areas, paste0(digets, "/digest.csv"), row.names = FALSE, sep = ";", quote = FALSE)
    }, silent = TRUE)
  }
  .errorTest(digetsWrite, verbose, "Digest write")
  .addMessage(verbose, "End of run")
}
