.errorTest <- function(tryReturn, silent, msg)
{
  if("try-error" %in% class(tryReturn)){
    stop(msg, " : ", tryReturn[1])
  }else{
    if(!silent){
      cat(paste0(msg, " : Ok\n"))
    }
  }
}

.addMessage <- function(silent, msg){
  if(!silent){
    cat(paste0(msg, "\n"))
  }
}