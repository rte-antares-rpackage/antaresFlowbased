#' Set of function to control the flowbased input repository
#'
#'
#' @details
#' The repertory choosen with \code{setFlowbasedPath} becomes the default
#' parameters for all functions of the package. The package have some availabled model \code{getAvailableModel}.
#' Current path can be retrieve using \code{fbOptions}
#'
#' @param path (optional)
#'   If "model" is missing. Path to the input repertory. . Must have :
#'   \itemize{
#'    \item{weight.txt}{}
#'    \item{second_member.txt}{}
#'    \item{ts.txt}{}
#'    \item{domainesFB.RDS}{}
#'   }
#' @param model (optional) If "path" is missing. The name of input available inside the package. Linked to \code{\link{getAvailableModel}}
#'
#' @return A vector of available model for \code{getAvailableModel}. For \code{setFlowbasedPath} and \code{fbOptions}, a list containing :
#'   \item{path}{path of the current input repository}
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
