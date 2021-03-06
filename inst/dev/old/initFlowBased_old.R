#' @title Generate environment for a flow-based study
#'
#' @description  Generate environment for a flow-based study
#' 
#'
#' @param fb_opts \code{list} of flowbased parameters returned by the function \link{setFlowbasedPath}. Defaut to \code{antaresFlowbased::fbOptions()}
#' @param opts \code{list} of simulation parameters returned by the function \link{setSimulationPath}. Defaut to \code{antaresRead::simOptions()}
#' @param scenarios \code{numeric} scenarios use for write scenario.txt.
#' 
#' @note 
#' folder deigned by fb_opts contain files :
#' \itemize{
#'   \item{domainesFB.RDS}{RDS file from \link{computeFB}}
#'   \item{second_member.txt}{txt file of second member wich following columns :
#'   \itemize{
#'     \item{Id_day : numeric from 1 to number of day id}
#'     \item{Id_hour : numeric from 1 to number of hour}
#'     \item{vect_b : numeric}
#'     \item{Name : character, name of constaints}
#'   }}
#'   \item{ts.txt}{txt file of time series matrix wich
#'   \itemize{
#'     \item{In row : dates, format : %YYYY-%MM-%DD
#'     \item{In column : vector of chronics}
#'     \item{in cell, numeric (typival day ID)}
#'   }}}
#'   \item{weigth.txt}{weigth file  wich following columns :
#'   \itemize{
#'     \item{Name : character, name of contraint}
#'     \item{BE.FR : numeric, between -1 and 1}
#'     \item{DE.FR : numeric, between -1 and 1}
#'     \item{DE.NL : numeric, between -1 and 1}
#'     \item{BE.NL : numeric, between -1 and 1}
#'     \item{BE.DE : numeric, between -1 and 1}
#'   }}}
#'  
#' 
#'
#' @examples
#'
#' \dontrun{
#' # target antares study
#' path <- "D:/exemple_test"
#' antaresRead::setSimulationPath(path, 0)
#' 
#' setFlowbasedPath(model = "model2017")
#' 
#' initFlowBased()
#' }
#'
#' @import pipeR data.table antaresRead
#'
#' @export
#'
initFlowBased <- function(fb_opts = antaresFlowbased::fbOptions(),
                          opts = antaresRead::simOptions(), scenarios = rep(1:200, times = 5)){
  
  
  pathProject <- opts$studyPath
  
  #Create flowbased directory
  newDir <- paste0(pathProject, "/user/flowbased")
  dir.create(newDir, showWarnings = FALSE, recursive = TRUE)
  
  #Get weight
  weightData <- .getWeight(paste0(fb_opts$path, "/weight.txt"))
  
  #Write weight.txt
  .setWeight(path = paste0(newDir, "/weight.txt"), weightData = weightData)
  
  #Get second members
  secondMemberData <- .getSecondMember(paste0(fb_opts$path, "/second_member.txt"))
  
  #Write second_member.txt
  .setSecondMember(path = paste0(newDir, "/second_member.txt"), secondMemberData = secondMemberData)
  
  #Create type day matrix, just load, it will change in next version
  if(paste0(fb_opts$path, "/ts.txt") != ""){
    dayTypeData <- .getDayType(paste0(fb_opts$path, "/ts.txt"))
    .setDayType(path = paste0(newDir, "/ts.txt"),
                dayTypeData = dayTypeData)
  }
  
  #Generate scenario
  scenario <- data.frame(simulation = scenarios)
  
  #Write scenario
  .setScenario(paste0(newDir, "/scenario.txt"), scenario = scenario)
  
  #Init constraint
  updateBindingConstraintsIni(paste0(newDir, "/weight.txt"), opts = opts)
  
  #Create temp empty file for antares study if they not exsists
  info_weight <- try(read.table(paste0(newDir, "/weight.txt"), sep = "\t", dec = ".", header = T, check.names = F), silent= T)
  info_weight$name <- paste0(info_weight$name, "_fb")
  
  dirContraints <- paste0(opts$inputPath, "/bindingconstraints/")
  sapply(info_weight$name, function(X){
    fil <- paste0(dirContraints, X, ".txt")
    if(!file.exists(fil)){
      file.create(fil)
    }
  })
  
  #Return TRUE
  invisible(TRUE)
  
}

