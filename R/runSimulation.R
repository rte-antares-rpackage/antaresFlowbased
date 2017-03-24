#' Run all simulations
#'
#' @param opts \code{list} of simulation parameters returned by the function \link{setSimulationPath}
#' @param simName \code{wharacter} name of simulation
#' @param mc_all \code{boolean} give compress results
#' @param mc_ind \code{boolean} give mc_ind
#' @param indic \code{character} not use in this version
#'
#' @examples
#'
#' \dontrun{
#' opts <- antaresRead::setSimulationPath("D:/Users/titorobe/Desktop/exemple_test",0)
#' mysim <- runSimulation(opts, "MystudyTest2")
#' }
#'
#' @rdname prepareSimulatioFiles
#' @export
runSimulation <- function(opts, simName, mc_all = TRUE, mc_ind = TRUE, indic = "all"){
  
  #random name to identify simulation
  aleatNameSime <- sample(letters, 10, replace = TRUE)%>>%
    paste0(collapse = "")
  simNameAlea <- paste0(simName, aleatNameSime)
  simNameAlea <- tolower(simNameAlea)
  #Generate path for generaldata.ini
  generaldataIniPatch <- paste0(opts$studyPath, "/settings/generaldata.ini")
  generaldataIniOld <- paste0(opts$studyPath, "/settings/generaldata_old.ini")
  #copy old settings file
  file.copy(generaldataIniPatch, generaldataIniOld)
  
  #Update general settings and copy old file
  updateGeneralSettingIni(opts)
  
  #load second member
  second_membre <- data.table::fread(paste0(opts$studyPath,"/user/flowbased/second_member.txt"))
  ts <- data.table::fread(paste0(opts$studyPath,"/user/flowbased/ts.txt"))
  scenario <- data.table::fread(paste0(opts$studyPath,"/user/flowbased/scenario.txt"))
  
  ##Prepare CMD to run antares
  setSolverAntares()
  AntaresPatch <- getSolverAntares()
  cmd <- '"%s" "%s" -n "%s"'
  cmd <- sprintf(cmd, AntaresPatch, opts$studyPath,simNameAlea)
  #Exemple pour l'année i = 1
  allScenario <- unique(scenario$simulation)
  allScenario <- allScenario[1:5]
  sapply(allScenario, function(X, opts, ts, second_membre, scenario, cmd){
    #Preparation of files before simulaiton
    prepareSimulatioFiles(opts = opts,
                          ts = ts,
                          second_membre = second_membre,
                          scenario = scenario,
                          simNmber = X)
    cmd <- paste0(cmd, "Sim",X)
    .runAntares(cmd)
  }, opts = opts,
  ts = ts,
  second_membre = second_membre,
  scenario = scenario,
  cmd = cmd)
  
  file.remove(generaldataIniPatch)
  #Return old param setting
  file.rename(generaldataIniOld, generaldataIniPatch)
  
  
  #Move files
  filesMoves <- moveFilesAfterStudy(opts, simNameAlea)
 
  #Mc-all creation
  aggregateResult(opts = opts, outDataMc = filesMoves)
  
  #
  
}



