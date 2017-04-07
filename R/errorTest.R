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

.addMessage <- function(verbose, msg, valAf = 2){
  if(verbose == valAf){
    cat(paste0(msg, "\n"))
  }
}