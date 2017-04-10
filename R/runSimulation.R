#' Run all simulations
#'
#' @param simulationName \code{character} name of simulation
#' @param mcAll \code{boolean} give mc_all compress results
#' @param mcInd \code{boolean} keep mc_ind.
#' @param mcYears \code{numeric} include mcYears, default NULL,
#' all mcYears are included.
#' @param opts \code{list} of simulation parameters returned by the function \link{setSimulationPath}. Defaut to \code{antaresRead::simOptions()}
#' @param verbose \code{numeric} show log in console.
#' @param .test \code{boolean} if TRUE, just run 3 scenarios.
#' 
#' 
#' @examples
#'
#' \dontrun{
#' antaresRead::setSimulationPath("D:/exemple_test",0)
#' 
#' initFlowBased()
#'
#' setSolverAntares(path = "C:\\Program Files\\RTE\\Antares\\5.0.9\\bin\\antares-5.0-solver.exe")
#'
#' mysim <- runSimulation(simulationName = "R_from", verbose = 1)
#'}
#'
#' @export
runSimulation <- function(simulationName = "FlowBased", mcAll = TRUE, mcInd = TRUE, 
                          mcYears = NULL, opts = antaresRead::simOptions(),
                          verbose = 1, .test = TRUE){
  if(mcAll == FALSE & mcInd == FALSE){
    stop("mcAll and mcInd are equal to FALSE")
  }
  
  
  oldw <- getOption("warn")
  options(warn = -1)
  opts <- antaresRead::setSimulationPath(opts$studyPath ,0)
  options(warn = oldw)
  
  if(!opts$parameters$general$mode %in%c("Economy" , "Adequacy")){
    stop(paste0("Mode os study must be Economy or Adequacy, actually :", opts$parameters$general$mode))
  }
  
  
  
  #random name to identify simulation
  aleatNameSime <- sample(letters, 10, replace = TRUE)%>>%
    paste0(collapse = "")
  simNameAlea <- paste0(simulationName, aleatNameSime)
  simNameAlea <- tolower(simNameAlea)
  
  if(verbose == 2){
    cat(paste("Generationos random name :", simNameAlea), "\n")
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
  ts <- try(data.table::fread(paste0(opts$studyPath,"/user/flowbased/ts.txt")), silent = TRUE)
  .errorTest(ts, verbose, "Load of ts.txt")
  scenario <- try(data.table::fread(paste0(opts$studyPath,"/user/flowbased/scenario.txt")), silent = TRUE)
  .errorTest(scenario, verbose, "Load of scenario.txt")
  
  
  #Exclude scenarios to redefine
  if(!is.null(mcYears)){
    scenario <- scenario[!mcYears] <- NA
  }
  
  ##Prepare CMD to run antares
  AntaresPatch <- getSolverAntares()
  if(is.null(AntaresPatch)){
    stop("Antares solver path is not set. Use getSolverAntares()")
  }
  if(!file.exists(AntaresPatch)){
    stop("Antares solver does no exist. Use getSolverAntares()")
  }
  
  cmd <- '"%s" "%s" -n "%s"'
  cmd <- sprintf(cmd, AntaresPatch, opts$studyPath, simNameAlea)
  
  #Exemple pour l'année i = 1
  allScenario <- unique(scenario$simulation)
  if(.test){
    allScenario <- allScenario[2:3]
  }
  
  .addMessage(verbose, "---------- Antares part ---------- ", valAf = 1)
  timBegin <- Sys.time()
  
  progress <- ifelse(verbose == 0, "none" , "text")
  
  plyr::l_ply(allScenario, function(X, opts, ts, second_member, scenario, cmd, verbose, timBegin,
                               allScenario){
    #Preparation of files before simulaiton
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
    .errorTest(filesMoves, verbose, "Creation of a sigle study which Antares format")
    
    
    
    # 
    # #Mc-all creation
    .addMessage(verbose, "---------------- Mc-all compute ----------------", valAf = 1)
    
    aggregateResult(opts = opts, newname = filesMoves, verbose = verbose)
    
    
    try({
      dtaMc <- paste0(opts$simDataPath, "/mc-ind")
      if(!mcInd){
        unlink(dtaMc, recursive = TRUE)
      }})
    .addMessage(verbose, "---------------- End of Mc-all compute ----------------", valAf = 1)
    
    #Wite digest
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



