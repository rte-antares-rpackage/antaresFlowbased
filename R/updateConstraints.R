#' Change bindingconstraints.ini file
#'
#' @param pathWeight \code{Character}, Path to weight .txt file
#' @param opts \code{list} of simulation parameters returned by the function \link{setSimulationPath}. Defaut to \code{antaresRead::simOptions()}
#'
#' @examples
#'
#' \dontrun{
#'
#' updateBindingConstraintsIni(pathWeight <- "D:\\Users\\benothie\\Documents\\weigth.txt")
#'
#' }
#'
#' @rdname update-BindingConstraints
#'
#' @seealso \code{writeBindingConstraintsIni}
#' 
#'
updateBindingConstraintsIni <- function(pathWeight, opts = antaresRead::simOptions()){
  # udpate data
  new_binding_cstr <- try(changeBindingConstraints(pathWeight = pathWeight, opts = opts), silent = TRUE)
  if("try-error" %in% class(new_binding_cstr)){
    stop("Changing binding constraints : ", new_binding_cstr[1])
  }
  
  # write data
  write_binding_cstr <- try(writeBindingConstraintsIni(listData = new_binding_cstr, opts = opts), silent = TRUE)
  if("try-error" %in% class(write_binding_cstr)){
    stop("Writing binding constraints : ", write_binding_cstr[1])
  }
}

#' @rdname update-BindingConstraints
#'
#' @import antaresRead
#' 
#'
changeBindingConstraints <- function(pathWeight, opts = antaresRead::simOptions()){
  # reading binding constraints
  pathIni <- paste0(opts$inputPath, "/bindingconstraints/bindingconstraints.ini")
  binding_cstr <- try(antaresRead:::readIniFile(pathIni), silent = TRUE)
  if("try-error" %in% class(binding_cstr)){
    stop("Reading binding constraints : ", binding_cstr[1])
  }
  
  # read file with weight
  info_weight <- try(read.table(pathWeight, sep = "\t", dec = ".", header = T, check.names = F), silent= T)
  if("try-error" %in% class(info_weight)){
    stop("Reading weight : ", info_weight[1])
  }
  
  stopifnot("name" %in% colnames(info_weight))
  info_weight$name <- paste0(info_weight$name, "_fb")
  # update binding constraints
  # Q : control if we have 36FB ? linked between data & update ?
  
  binding_cstr <- lapply(binding_cstr, function(X){
    if(!is.null(X)){
      if(!is.null(X$name)){
      
      
      if(nchar(X$name)>3){
        
        
        if(tolower(substr(gsub(" ", "", X$name),nchar(X$name)-2,nchar(X$name))[[1]]) == "_fb"){
          NULL
        }else{
          X
        }
      }else{
        X
      }
    }else{
      X
    }
    }else{X}
  })
  binding_cstr <- binding_cstr[!unlist(lapply(binding_cstr, is.null))]
  names(binding_cstr) <- as.character(1:length(binding_cstr))
  sapply(info_weight$name,function(Nam){
    Nam <- as.character(Nam)
    if(!any(unlist(lapply(binding_cstr, function(X){
      X$name %in% Nam
    })))){
      
      
      newConstraint <- list(
        name = as.character(Nam),
        id = as.character(tolower(Nam)),
        enabled = TRUE,
        type = "hourly",
        operator = "less"
      )
      rowSel <- info_weight[info_weight$name == Nam,]
      if(rowSel$`be%fr` != 0){
        newConstraint$`be%fr` <- rowSel$`be%fr`
      }
      if(rowSel$`de%fr` != 0){
        newConstraint$`de%fr` <- rowSel$`de%fr`
      }
      if(rowSel$`de%nl` != 0){
        newConstraint$`de%nl` <- rowSel$`de%nl`
      }
      if(rowSel$`be%nl` != 0){
        newConstraint$`be%nl` <- rowSel$`be%nl`
      }
      if(rowSel$`be%de` != 0){
        newConstraint$`be%de` <- rowSel$`be%de`
      }
      
      binding_cstr[[as.character(length(binding_cstr) + 1)]] <<- newConstraint
    }
  })
  
  
  # up_binding_cstr <- lapply(binding_cstr, function(x){
  #   if(x$name %in% info_weight$name){
  #     # really have to set this 3 parameters ?
  #     x$enabled = TRUE
  #     x$type = "hourly"
  #     x$operator = "less"
  #     # remove other parameters
  #     x[(which(names(x)%in%"operator")+1) : length(x)] <- NULL
  #     
  #     # add new weight
  #     tmp_weight <- info_weight[info_weight$name %in% x$name, -1]
  #     ctrl_add <- lapply(colnames(tmp_weight), function(x){
  #       value <- tmp_weight[1, x]
  #       # write only if not zero
  #       if(value != 0){
  #         x[[x]] <<- tmp_weight[1, x]
  #       }
  #     })
  #     x
  #   } else {
  #     # just remove values??
  #     x$values <- NULL
  #     x
  #   }
  # })
  
  binding_cstr
}

#' Write bindingconstraints.ini file
#'
#' @param listData \code{list}, bindingconstraints.ini as list R
#' @param opts \code{list} of simulation parameters returned by the function \link{setSimulationPath}. Defaut to \code{antaresRead::simOptions()}
#' 
#'
writeBindingConstraintsIni <- function(listData, opts = antaresRead::simOptions()){
  # open new file
  pathIni <- paste0(opts$inputPath, "/bindingconstraints/bindingconstraints.ini")
  
  write_data <- writeIni(listData, pathIni)
  
}

