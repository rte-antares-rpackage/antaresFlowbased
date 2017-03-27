#' @title Generate environnement for a flow-based study
#' @description  Generate environnement for a flow-based study
#'
#' @param opts \code{list} of simulation parameters returned by the function \link{setSimulationPath}
#' @param weigth \code{character}, path of weigth file
#' @param secondMember \code{character}, path of secondMember file
#' @param dayType \code{character}, path of dayType file
#' @param determinants \code{character}, vector of determinants, not use in this version
#'
#' @examples
#' \dontrun{
#'
#' path <- "D:/Users/titorobe/Desktop/exemple_test"
#' opts <- antaresRead::setSimulationPath(path, 0)
#'
#' weigth <- system.file("/test/data/coefficients_Antares.csv", package = "antaresFlowbased")
#' secondMember <- system.file("/test/data/fichier_b_final.csv", package = "antaresFlowbased")
#' dayType <- system.file("/test/data/id_FB.txt", package = "antaresFlowbased")
#'
#' initFlowBased(opts = opts,
#'               weigth = weigth,
#'               secondMember = secondMember,
#'               dayType = dayType)
#' }
#'
#' @import pipeR data.table antaresRead
#'
#' @export
initFlowBased <- function(opts,
                          weigth,
                          secondMember,
                          dayType = NULL,
                          determinants = NULL){


  pathProject <- opts$studyPath

  #Create flowbased
  newDir <- paste0(pathProject, "/user/flowbased")
  dir.create(newDir, showWarnings = FALSE, recursive = TRUE)

  #Get weigth
  weigthData <- .getWeigth(weigth)

  #Write weigth.txt
  .setWeigth(path = paste0(newDir, "/weigth.txt"), weigthData = weigthData)

  #Get second members
  secondMemberData <- .getSecondMember(secondMember)

  #Write second_member.txt
  .setSecondMember(path = paste0(newDir, "/second_member.txt"), secondMemberData = secondMemberData)

  #Create type day matrix, just load, it will change in next version
  if(!is.null(dayType)){
    dayTypeData <- .getDayType(dayType)
    .setDayType(path = paste0(newDir, "/ts.txt"),
                dayTypeData = dayTypeData)
  }

  #Generate scenario
  scenario <- .generateScenario()

  #Write scenario
  .setScenario(paste0(newDir, "/scenario.txt"), scenario = scenario)

  #Init constraint
  updateBindingConstraintsIni(paste0(newDir, "/weigth.txt"), opts = opts)

}
