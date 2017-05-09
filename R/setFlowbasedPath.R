#' Set of function to control the flowbased input repository
#'
#'
#' @details
#' The repertory choosen with \code{setFlowbasedPath} becomes the default
#' parameters for all functions of the package. The package have some availabled bp \code{getAvailabledBP}.
#' Current path can be retrieve using \code{fbOptions}
#'
#' @param path (optional)
#'   If "bp" is missing. Path to the input repertory. . Must have :
#'   \itemize{
#'    \item{coefficients_Antares.csv}{}
#'    \item{fichier_b_final.csv}{}
#'    \item{id_FB.csv}{}
#'    \item{domainesFB.RDS}{}
#'   }
#' @param bp (optional) If "path" is missing. The name of input availabled inside the package. Linked to \code{\link{getAvailabledBP}}
#'
#' @return A vector of availabled bp for \code{getAvailabledBP}. For \code{setFlowbasedPath} and \code{fbOptions}, a list containing :
#'   \item{path}{path of the current input repository}
#'
#'
#' @examples
#'
#' \dontrun{
#' # Defaut path set loading the package
#' fbOptions()
#'
#' # Specify a availabled bp
#' getAvailabledBP()
#' setFlowbasedPath(bp = "BP2017")
#'
#' # Select a repository
#' setFlowbasedPath(bp = "C:/PATH/TO/INPUT")
#' }
#'
#' @export
#'
#' @name flowbased-path
#'
setFlowbasedPath <- function(path, bp) {

  if (missing(path) & missing(bp)) {
    stop("Please specify a path to a flowbased input directory or a existed bp name")
  }

  if (!missing(path) & !missing(bp)) {
    stop("Please specify a path to a flowbased input directory or a existed bp name")
  }

  if (!missing(bp)) {
    dir_bp <- system.file("input/BP", package = "antaresFlowbased")
    available_bp <- list.dirs(dir_bp, full.names = FALSE, recursive = FALSE)
    if(!bp%in%available_bp){
      stop("Invalid bp name. See availabled BP with getAvailabledBP()")
    }
    path <- paste(dir_bp, bp, sep = "/")
  }

  # verify path
  all_files <- list.files(path, full.names = FALSE, recursive = FALSE)

  if(!all(c("coefficients_Antares.csv", "domainesFB.RDS", "fichier_b_final.csv", "id_FB.csv") %in% all_files)){
    stop("Flowbased reportory must have this 4 files : 'coefficients_Antares.csv', 'domainesFB.RDS', 'fichier_b_final.csv' and 'id_FB.csv'")
  }

  res <- list(path = path)
  options(flowbased = res)

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
getAvailabledBP <- function(){
  return(list.dirs(system.file("input/BP", package = "antaresFlowbased"), full.names = FALSE, recursive = FALSE))
}
