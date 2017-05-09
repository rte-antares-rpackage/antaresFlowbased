#' Run antares function
#'
#' @param cmd \code{character} command launch
#'
#' @noRd
.runAntares <- function(cmd){
  system(cmd, show.output.on.console = FALSE, intern = TRUE)
}
