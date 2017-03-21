#' Set the path of the antares solver executable
#'
#' This function set and init an options "antares_solver" with path to the solver executable
#'
#' @param path \code{Character}, Path to the executable antares solver
#'
#' @examples
#'
#' \dontrun{
#'
#' setSolverAntares(path = "C:\\Program Files\\RTE\\Antares\\5.0.9\\bin\\antares-5.0-solver.exe")
#'
#' getSolverAntares()
#'
#' }
#'
#' @rdname solver-antares
#' @export

setSolverAntares <- function(path = "C:\\Program Files\\RTE\\Antares\\5.0.9\\bin\\antares-5.0-solver.exe") {
  if(!file.exists(path)){
    stop("Invalid solver path")
  }

  info <- file.info(path)
  if(info$exe %in% c("no", "unknown")){
    stop("Not an executable file")
  }

  options(antares_solver = path)
}

#' @rdname solver-antares
#' @export
getSolverAntares <- function() {
  options("antares_solver")$antares_solver
}
