#' Run all simulations
#'
#' @param opts \code{list} of simulation parameters returned by the function \link{setSimulationPath}
#' @param simulationName \code{character} name of simulation
#' @param mcAll \code{boolean} give mc_all compress results
#' @param mcInd \code{boolean} keep mc_ind.
#' @param indicators \code{character} not use in this version
#' @param .test \code{boolean} if TRUE, just run 3 scenarios.
#'
#' @examples
#'
#' \dontrun{
#' opts <- antaresRead::setSimulationPath("D:/Users/titorobe/Desktop/exemple_test",0)
#'
#' weigth <- system.file("/test/data/coefficients_Antares.csv", package = "antaresFlowbased")
#' secondMember <- system.file("/test/data/fichier_b_final.csv", package = "antaresFlowbased")
#' dayType <- system.file("/test/data/id_FB.txt", package = "antaresFlowbased")
#'
#' initFlowBased(opts = opts, weigth = weigth, secondMember = secondMember, dayType = dayType)
#'
#' setSolverAntares(path = "C:\\Program Files\\RTE\\Antares\\5.0.9\\bin\\antares-5.0-solver.exe")
#'
#' mysim <- runSimulation(opts, "MystudyTest14")
#' }
#'
#' @export
runSimulation <- function(opts, simulationName, mcAll = TRUE, mcInd = TRUE,
                          indicators = c("mean", "min", "max", "sd"), .test = TRUE){

  #random name to identify simulation
  aleatNameSime <- sample(letters, 10, replace = TRUE)%>>%
    paste0(collapse = "")
  simNameAlea <- paste0(simulationName, aleatNameSime)
  simNameAlea <- tolower(simNameAlea)

  #Generate path for generaldata.ini
  generaldataIniPatch <- paste0(opts$studyPath, "/settings/generaldata.ini")
  generaldataIniOld <- paste0(opts$studyPath, "/settings/generaldata_old.ini")

  #copy old settings file
  file.copy(generaldataIniPatch, generaldataIniOld)

  #Update general settings and copy old file
  updateGeneralSettingIni(opts)

  #load second member
  second_member <- data.table::fread(paste0(opts$studyPath,"/user/flowbased/second_member.txt"))
  ts <- data.table::fread(paste0(opts$studyPath,"/user/flowbased/ts.txt"))
  scenario <- data.table::fread(paste0(opts$studyPath,"/user/flowbased/scenario.txt"))

  ##Prepare CMD to run antares
  setSolverAntares()
  AntaresPatch <- getSolverAntares()
  cmd <- '"%s" "%s" -n "%s"'
  cmd <- sprintf(cmd, AntaresPatch, opts$studyPath,simNameAlea)
  #Exemple pour l'année i = 1
  allScenario <- unique(scenario$simulation)
  if(.test){
    allScenario <- allScenario[1:3]
  }
  sapply(allScenario, function(X, opts, ts, second_member, scenario, cmd){
    #Preparation of files before simulaiton
    prepareSimulationFiles(opts = opts,
                          ts = ts,
                          secondMember = second_member,
                          scenarios = scenario,
                          simNumber = X)
    cmd <- paste0(cmd, "Sim",X)
    .runAntares(cmd)
  }, opts = opts,
  ts = ts,
  second_member = second_member,
  scenario = scenario,
  cmd = cmd)

  file.remove(generaldataIniPatch)
  #Return old param setting
  file.rename(generaldataIniOld, generaldataIniPatch)
  
  # #Move files
  filesMoves <- moveFilesAfterStudy(opts, simNameAlea)
  # 
  print(filesMoves)
  # #Mc-all creation
  aggregateResult(opts = opts, newname = filesMoves)
  
  #Wite digest
  opts <- antaresRead::setSimulationPath(opts$studyPath, filesMoves)
  diges <- fread(paste0(path.package("antaresFlowbased"), "/output/digest.csv"))
  
  areas <- readAntares(timeStep = "hourly")
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
  
  for (col in allNam) set(areas, j = col, value = round(areas[[col]], 0))
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
  cat("Finish")
}



