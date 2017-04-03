#' Preparation of sigle simulation files, write playlist and FB
#'
#' @param opts \code{list} of simulation parameters returned by the function \link{setSimulationPath}
#' @param ts \code{data.frame} of dayType and simulation
#' @param secondMember \code{data.frame} os second members
#' @param scenarios \code{data.frame} of scenarios
#' @param simNumber \code{numeric} number of simulation must be in scenarios
#'
#' @examples
#'
#' \dontrun{
#'
#'  path <- "D:/Users/titorobe/Desktop/exemple_test"
#'  opts <- antaresRead::setSimulationPath(,0)
#'
#'  secondMember <- data.table::fread(paste0(opts$studyPath,"/user/flowbased/second_member.txt"))
#'  ts <- data.table::fread(paste0(opts$studyPath,"/user/flowbased/ts.txt"))
#'  scenarios <- data.table::fread(paste0(opts$studyPath,"/user/flowbased/scenario.txt"))
#'
#'  prepareSimulationFiles(opts = opts, ts = ts, secondMember = secondMember,
#'    scenarios = scenarios, simNumber = 54 )
#' }
#'
#' @export
prepareSimulationFiles <- function(opts, ts, secondMember, scenarios, simNumber, silent = TRUE){
  #Creation of bindingconstraints chroniques
  clim <- data.table::data.table(Id_day = unlist(ts[,.SD, .SDcols = colnames(ts)==simNumber]))
  allFB <- merge(clim, secondMember, by = "Id_day", allow.cartesian = TRUE)
  data.table::setkeyv(allFB, c("Id_day", "Id_hour"))
  
  #Write of bindingconstraints
  upFb <- try(sapply(unique(allFB$Name),
         .writeFb,
         patch = paste0(opts$studyPath, "/input/bindingconstraints"),
         data = allFB)%>>%
    invisible(), silent = TRUE)
  .errorTest(upFb, silent, "Update of bindingconstraints : Ok")
  
  #Write of playList
  scenarioTP <- which(scenarios$simulation==simNumber)-1
  upPl <- try(updateGeneralSettingIni(opts, scenarioTP), silent = TRUE)
  .errorTest(upPl, silent, "Update of playlist : Ok")
  
}
