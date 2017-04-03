.errorTest <- function(tryReturn, silent, messageOk)
{
  if("try-error" %in% class(tryReturn)){
    stop("Writing binding constraints : ", tryReturn[1])
  }else{
    if(!silent){
      cat(paste0(messageOk, "\n"))
    }
  }
}

.addMessage <- function(silent, message){
  if(!silent){
    cat(paste0(message, "\n"))
  }
}