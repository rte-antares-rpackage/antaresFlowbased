#' Preparation of sigle simulation files, write playlist and FB
#'
#' @param opts \code{list} of simulation parameters returned by the function \link{setSimulationPath}
#' @param ts \code{data.frame} of dayType and simulation
#' @param second_membre \code{data.frame} os second members
#' @param scenario \code{data.frame} of scenario
#' @param simNmber \code{numeric} numer of simulation must be in scenario
#'
#' @examples
#'
#' \dontrun{
#' opts <- antaresRead::setSimulationPath("D:/Users/titorobe/Desktop/exemple_test",0)
#'  second_membre <- data.table::fread(paste0(opts$studyPath,"/user/flowbased/second_member.txt"))
#'  ts <- data.table::fread(paste0(opts$studyPath,"/user/flowbased/ts.txt"))
#'  scenario <- data.table::fread(paste0(opts$studyPath,"/user/flowbased/scenario.txt"))
#'  prepareSimulatioFiles(opts = opts, ts = ts, second_membre = second_membre, scenario = scenario,
#'                        simNmber = 54 )
#' }
#'
#' @rdname prepareSimulatioFiles
#' @export
prepareSimulatioFiles <- function(opts, ts, second_membre, scenario, simNmber){
  #Write of FB files
  clim <- data.table::data.table(Id_day = unlist(ts[,.SD, .SDcols = colnames(ts)==simNmber]))
  allFB <- merge(clim, second_membre, by = "Id_day", allow.cartesian = TRUE)
  data.table::setkeyv(allFB, c("Id_day", "Id_hour"))
  sapply(unique(allFB$Name),
         .writeFb,
         patch = paste0(opts$studyPath, "/input/bindingconstraints"),
         data = allFB)%>>%
    invisible()
  
  #Write of playList
  scenarioTP <- which(scenario$simulation==simNmber)-1
  updateGeneralSettingIni(opts, scenarioTP)
}