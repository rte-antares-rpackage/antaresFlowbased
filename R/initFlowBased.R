#' @title Generate environnement for a flow-based study
#' @description  Generate environnement for a flow-based study
#' 
#' @param pathProject \code{character}, path of antares project
#' @param weigth \code{character}, path of weigth file
#' @param secondMember \code{character}, path of secondMember file
#' @param dayType \code{character}, path of dayType file
#' @param determinants \code{character}, vector of determinants, not use in this version
#' 
#' @examples 
#' \dontrun{
#' opts <- antaresRead::setSimulationPath("D:/Users/titorobe/Desktop/exemple_test",0)
#' weigth <- "D:/Users/titorobe/Desktop/run_antares_FB/Write chroniques/Coefficients_Antares.csv"
#' secondMember <- "D:/Users/titorobe/Desktop/run_antares_FB/Write chroniques/fichier b final.csv"
#' dayType <- "D:/Users/titorobe/Desktop/run_antares_FB/Write chroniques/id_FB.txt"
#' initFlowBased(opts = opts,
#'               weigth = weigth,
#'               secondMember = secondMember,
#'               dayType = dayType)
#' }
#' @import pipeR, data.table, antaresRead
#' 
#' @export
initFlowBased <- function(opts,
                          weigth,
                          secondMember,
                          dayType = NULL,
                          determinants = NULL){
  
  
  pathProject <- opts$studyPath
  print(pathProject)
  #Create folders
  dir.create(paste0(pathProject, "/user"))
  
  #Create flowbased
  dir.create(paste0(pathProject, "/user/flowbased"))
  
  newDir <- paste0(pathProject, "/user/flowbased")
  
  #Get weigth
  weigthData <- .getWeigth(weigth)
  
  #Write weigth.txt
  .setWeigth(path = paste0(newDir, "/weigth.txt"),
             weigthData = weigthData)
  
  #Get second members 
  secondMemberData <- .getSecondMember(secondMember)
  
  #Write second_member.txt
  .setSecondMember(path = paste0(newDir, "/second_member.txt"),
                   secondMemberData = secondMemberData)
  
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
