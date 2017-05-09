#' @title Generate environment for a flow-based study
#'
#' @description  Generate environment for a flow-based study
#'
#' @param fb_opts \code{list} of flowbased parameters returned by the function \link{setFlowbasedPath}. Defaut to \code{antaresFlowbased::fbOptions()}
#' @param opts \code{list} of simulation parameters returned by the function \link{setSimulationPath}. Defaut to \code{antaresRead::simOptions()}
#'
#' @examples
#'
#' \dontrun{
#' # target antares study
#' path <- "D:/exemple_test"
#' antaresRead::setSimulationPath(path, 0)
#' 
#' setFlowbasedPath(bp = "BP2017")
#' 
#' initFlowBased()
#' }
#'
#' @import pipeR data.table antaresRead
#'
#' @export
#'
initFlowBased <- function(fb_opts = antaresFlowbased::fbOptions(),
                          opts = antaresRead::simOptions()){


  pathProject <- opts$studyPath

  #Create flowbased directory
  newDir <- paste0(pathProject, "/user/flowbased")
  dir.create(newDir, showWarnings = FALSE, recursive = TRUE)

  #Get weight
  weightData <- .getWeight(paste0(fb_opts$path, "/coefficients_Antares.csv"))

  #Write weight.txt
  .setWeight(path = paste0(newDir, "/weight.txt"), weightData = weightData)

  #Get second members
  secondMemberData <- .getSecondMember(paste0(fb_opts$path, "/fichier_b_final.csv"))

  #Write second_member.txt
  .setSecondMember(path = paste0(newDir, "/second_member.txt"), secondMemberData = secondMemberData)

  #Create type day matrix, just load, it will change in next version
  if(paste0(fb_opts$path, "/id_FB.csv") != ""){
    dayTypeData <- .getDayType(paste0(fb_opts$path, "/id_FB.csv"))
    .setDayType(path = paste0(newDir, "/ts.txt"),
                dayTypeData = dayTypeData)
  }

  #Generate scenario
  scenario <- .generateScenario()

  #Write scenario
  .setScenario(paste0(newDir, "/scenario.txt"), scenario = scenario)

  #Init constraint
  updateBindingConstraintsIni(paste0(newDir, "/weight.txt"), opts = opts)

  #Return TRUE
  invisible(TRUE)

}
