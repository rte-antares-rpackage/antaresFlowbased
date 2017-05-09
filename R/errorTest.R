#' @title error test
#'
#' @description error test
#'
#' @param tryReturn return from try
#' @param verbose \code{numeric} show log in console. Defaut to 1
#' @param msg \code{character} message to display if no error
#'
#' @noRd
.errorTest <- function(tryReturn, verbose, msg)
{
  if("try-error" %in% class(tryReturn)){
    stop(msg, " : ", tryReturn[1])
  }else{
    if(verbose == 2){
      cat(paste0(msg, " : Ok\n"))
    }
  }
}

#' @title Display message on console
#'
#' @description Display message on console
#'
#' @param verbose \code{numeric} show log in console. Defaut to 1
#' @param valAf \code{numeric} show log in console if valAf == verbose.
#' @param msg \code{character} message to display if no error
#'
#' @noRd
.addMessage <- function(verbose, msg, valAf = 2){
  if(verbose == valAf){
    cat(paste0(msg, "\n"))
  }
}
