#' @title Generate environnement for a flow-based study
#' @description  Generate environnement for a flow-based study
#'
#' @param weight \code{character}, path of weight file
#' @param secondMember \code{character}, path of secondMember file
#' @param dayType \code{character}, path of dayType file
#' @param opts \code{list} of simulation parameters returned by the function \link{setSimulationPath}. Defaut to \code{antaresRead::simOptions()}
#' 
#' 
#' @examples
#' \dontrun{
#'
#' path <- "D:/exemple_test"
#' antaresRead::setSimulationPath(path, 0)
#'
#' initFlowBased()
#' }
#'
#' @import pipeR data.table antaresRead
#'
#' @export
initFlowBased <- function(weight = system.file("/data/coefficients_Antares.csv", package = "antaresFlowbased"),
                          secondMember = secondMember <- system.file("/data/fichier_b_final.csv", package = "antaresFlowbased"),
                          dayType =  system.file("/data/id_FB.txt", package = "antaresFlowbased"),
                          opts = antaresRead::simOptions()){


  pathProject <- opts$studyPath

  #Create flowbased
  newDir <- paste0(pathProject, "/user/flowbased")
  dir.create(newDir, showWarnings = FALSE, recursive = TRUE)

  #Get weight
  weightData <- .getWeight(weight)

  #Write weight.txt
  .setWeight(path = paste0(newDir, "/weight.txt"), weightData = weightData)

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
  updateBindingConstraintsIni(paste0(newDir, "/weight.txt"), opts = opts)

}
