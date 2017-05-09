# init options flowbased on load
.onLoad <- function(libname, pkgname){
  available_bp <- list.dirs(system.file("input/BP", package = "antaresFlowbased"), full.names = TRUE, recursive = FALSE)
  if(length(available_bp) > 0){
    options(flowbased = list(path = available_bp[1]))
  }
}
