#' Preparation of single simulation files, write playlist and FB
#'
#' @param ts \code{data.frame} of dayType and simulation
#' @param secondMember \code{data.frame} of second members
#' @param scenarios \code{data.frame} of scenarios
#' @param simNumber \code{numeric} number of simulation must be in scenarios
#' @param opts \code{list} of simulation parameters returned by the function \link{setSimulationPath}. Defaut to \code{antaresRead::simOptions()}
#' @param silent \code{boolean} show log in console.
#' 
#' @examples
#'
#' \dontrun{
#'
#'  path <- "D:/Users/titorobe/Desktop/exemple_test"
#'  antaresRead::setSimulationPath(path, 0)
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
prepareSimulationFiles <- function(ts, secondMember, scenarios, simNumber, 
                                   opts = antaresRead::simOptions(), silent = TRUE){
  
  #Creation of bindingconstraints chroniques
  clim <- data.table::data.table(Id_day = unlist(ts[,.SD, .SDcols = colnames(ts)==simNumber]))
  allFB <- merge(clim, secondMember, by = "Id_day", allow.cartesian = TRUE)
  data.table::setkeyv(allFB, c("Id_day", "Id_hour"))
  
  #Write of bindingconstraints
  upFb <- try(
    sapply(unique(allFB$Name),
           .writeFb,
           patch = paste0(opts$studyPath, "/input/bindingconstraints"),
           data = allFB) %>>%
      invisible(), silent = TRUE)
  .errorTest(upFb, silent, "Update of bindingconstraints")
  
  #Write of playList
  scenarioTP <- which(scenarios$simulation == simNumber)-1
  upPl <- try(updateGeneralSettingIni(opts, scenarioTP), silent = TRUE)
  .errorTest(upPl, silent, "Update of playlist")
  
}

# writeFb files
.writeFb <- function(nameFb, patch, data){
  #adding 24 hours (0 : bisextile)
  data <- c(data[Name == nameFb]$vect_b, rep(0,24))
  data <- cbind(data, 0, 0)
  write.table(data, file = paste0(patch, "/", nameFb, ".txt"), sep = "\t",
              row.names = FALSE, col.names = FALSE)
}
