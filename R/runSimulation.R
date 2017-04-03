#' Run all simulations
#'
#' @param opts \code{list} of simulation parameters returned by the function \link{setSimulationPath}
#' @param simulationName \code{character} name of simulation
#' @param mcAll \code{boolean} give mc_all compress results
#' @param mcInd \code{boolean} keep mc_ind.
#' @param indicators \code{character} not use in this version
#' @param .test \code{boolean} if TRUE, just run 3 scenarios.
#' @param mcYears \code{numeric} include mcYears, default NULL,
#' all mcYears are included.
#' @param silent \code{boolean} show log in console.
#' 
#' @examples
#'
#' \dontrun{
#' antaresRead::setSimulationPath("D:/exemple_test",0)
#'
#' weight <- system.file("/test/data/coefficients_Antares.csv", package = "antaresFlowbased")
#' secondMember <- system.file("/test/data/fichier_b_final.csv", package = "antaresFlowbased")
#' dayType <- system.file("/test/data/id_FB.txt", package = "antaresFlowbased")
#'
#' initFlowBased(weight = weight, secondMember = secondMember, dayType = dayType)
#'
#' setSolverAntares(path = "C:\\Program Files\\RTE\\Antares\\5.0.9\\bin\\antares-5.0-solver.exe")
#'
#' mysim <- runSimulation("R_from", silent = FALSE)
#'}
#'
#' @export
runSimulation <- function(simulationName = "FlowBased", mcAll = TRUE, mcInd = TRUE,
                          indicators = c("mean", "min", "max", "sd"), .test = TRUE,
                          mcYears = NULL, silent = TRUE,
                          opts = antaresRead::simOptions()){
  oldw <- getOption("warn")
  options(warn = -1)
  opts <- antaresRead::setSimulationPath(opts$studyPath ,0)
  options(warn = oldw)
  
  #random name to identify simulation
  aleatNameSime <- sample(letters, 10, replace = TRUE)%>>%
    paste0(collapse = "")
  simNameAlea <- paste0(simulationName, aleatNameSime)
  simNameAlea <- tolower(simNameAlea)
  
  if(!silent){
    cat(paste("Generationos random name :", simNameAlea))
  }
  
  #Generate path for generaldata.ini
  generaldataIniPatch <- paste0(opts$studyPath, "/settings/generaldata.ini")
  generaldataIniOld <- paste0(opts$studyPath, "/settings/generaldata_old.ini")
  
  #copy old settings file
  file.copy(generaldataIniPatch, generaldataIniOld)
  
  #Update general settings and copy old file
  
  
  upGenIni <- try(updateGeneralSettingIni(opts), silent = TRUE)
  .errorTest(upGenIni, silent, "Write of generaldata : Ok")
  
  #load second member
  second_member <- try(data.table::fread(paste0(opts$studyPath,"/user/flowbased/second_member.txt")), silent = TRUE)
  .errorTest(second_member, silent, "Load of second_member.txt : Ok")
  ts <- try(data.table::fread(paste0(opts$studyPath,"/user/flowbased/ts.txt")), silent = TRUE)
  .errorTest(ts, silent, "Load of ts.txt : Ok")
  scenario <- try(data.table::fread(paste0(opts$studyPath,"/user/flowbased/scenario.txt")), silent = TRUE)
  .errorTest(scenario, silent, "Load of scenario.txt : Ok")
  
  
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
    allScenario <- allScenario[2:5]
  }
  .addMessage(silent, "---------- Antares part ---------- ")
  
  sapply(allScenario, function(X, opts, ts, second_member, scenario, cmd, silent){
    #Preparation of files before simulaiton
    .addMessage(silent, paste0("-Scenario : ",X))
    prepareSimulationFiles(opts = opts,
                           ts = ts,
                           secondMember = second_member,
                           scenarios = scenario,
                           simNumber = X,
                           silent = silent)
    
    
    cmd <- paste0(cmd, "Sim",X)
    .addMessage(silent, paste0("Antares launching for ",  paste0("scenario : ",X) ))
    beg <- Sys.time()
    .runAntares(cmd)
    .addMessage(silent, paste0("Antares end for scenario : ",X))
    .addMessage(silent, paste0("Compute time for scenario ",X, " : ",
                               as.numeric(round(Sys.time()-beg)), " secondes"))
    
    
  }, opts = opts,
  ts = ts,
  second_member = second_member,
  scenario = scenario,
  cmd = cmd,
  silent = silent)
  
  .addMessage(silent, "---------- End of antares part ----------")
  
  #Return old param setting
  file.remove(generaldataIniPatch)
  file.rename(generaldataIniOld, generaldataIniPatch)
  
  filesMoves <- try(moveFilesAfterStudy(opts, simNameAlea, silent = silent), silent = TRUE)
  .errorTest(filesMoves, silent, "Creation of a sigle study which Antares format : Ok")
  
  
  
  # 
  # #Mc-all creation
  .addMessage(silent, "Mc-all compute")
  
  aggregateResult(opts = opts, newname = filesMoves, silent = silent)
  
  dtaMc <- paste0(opts$simDataPath, "/mc-ind")
  
  if(!mcInd){
    unlink(dtaMc, recursive = TRUE)
  }
  
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
  .errorTest(digetsWrite, silent, "Digest write : Ok")
  .addMessage(silent, "End of run")
}



