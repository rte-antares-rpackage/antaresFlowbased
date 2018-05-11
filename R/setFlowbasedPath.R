#' @title Set the flow-based input folder
#' 
#' @description A flow-based "model" consists of the description of the typical domains (the weights, the second members and 
#' an RDS object containing additional information such as the points coordinates and the errors) and the time series file 
#' establishing the chronology of typical days used in an Antares study. 
#' These functions enable to choose the flow-based input folder: one can either use existing models stored in the package or 
#' use his own. 
#'   \itemize{
#'    \item{The function \code{getAvailableModel} provides a list of the existing models.}
#'    \item{The function \code{setFlowbasedPath} sets the flow-based input folder. It can either be one of the models listed by 
#'    \code{getAvailableModel} or a path to a personal folder. This folder will then be used as default parameter for all
#'     functions of the package}
#'    \item{The function \code{fbOptions} indicates the current default path.}
#'   }
#' 
#'
#' @param path (optional)
#'   If the argument "model" is missing. Path to the flow-based input directory, it can be created by the user with the 
#'   functions \link{computeFB} and \link{createFBTS}. The directory must be composed of:
#'   \itemize{
#'    \item{weight.txt}{the PTDF file of the Antares models}
#'    \item{second_member.txt}{the RAM of the Antares model}
#'    \item{ts.txt}{the flow-based time series}
#'    \item{domainesFB.RDS}{information about the domains}
#'   }
#' @param model (optional) 
#' If "path" is missing. Name of the chosen existing model (Use \code{\link{getAvailableModel}} to get the complete list).
#'
#' @return
#' \itemize{ 
#' \item {}{A vector of available models for \code{getAvailableModel}.}
#' \item {\code{path}}{ path of the current input repository, for \code{setFlowbasedPath} and \code{fbOptions}}
#' }
#'
#'
#' @examples
#'
#' \dontrun{
#' # Defaut path set loading the package
#' fbOptions()
#'
#' # Specify a available model
#' getAvailableModel()
#' setFlowbasedPath(model = "model2017")
#'
#' # Select a repository
#' setFlowbasedPath(model = "C:/PATH/TO/INPUT")
#' }
#'
#' @export
#'
#' @name flowbased-path
#'
setFlowbasedPath <- function(path, model) {

  if (missing(path) & missing(model)) {
    stop("Please specify a path to a flowbased input directory or a existed model name")
  }

  if (!missing(path) & !missing(model)) {
    stop("Please specify a path to a flowbased input directory or a existed model name")
  }

  if (!missing(model)) {
    dir_model <- system.file("input/model", package = "antaresFlowbased")
    available_model <- list.dirs(dir_model, full.names = FALSE, recursive = FALSE)
    if(!model%in%available_model){
      stop("Invalid model name. See availabled model with getAvailableModel()")
    }
    path <- paste(dir_model, model, sep = "/")
  }

  # verify path
  all_files <- list.files(path, full.names = FALSE, recursive = FALSE)

  if(!all(c("weight.txt", "domainesFB.RDS", "second_member.txt", "ts.txt") %in% all_files)){
    stop("Flowbased reportory must have this 4 files : 'weight.txt', 'domainesFB.RDS', 'second_member.txt' and 'ts.txt'")
  }

  res <- list(path = path)
  options(flowbased = res)
  class(res) <- "flowBasedPath"

  res
}


#' @rdname flowbased-path
#' @export
#'
fbOptions <- function() {
  opts <- getOption("flowbased")
  if (is.null(opts)) stop("Default flowbased options are not set. You need to run 'setFlowbasedPath()' to set them.")
  else return(opts)
}

#' @rdname flowbased-path
#' @export
#'
getAvailableModel <- function(){
  return(list.dirs(system.file("input/model", package = "antaresFlowbased"), full.names = FALSE, recursive = FALSE))
}
