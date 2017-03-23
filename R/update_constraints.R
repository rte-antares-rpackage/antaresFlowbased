#' Change bindingconstraints.ini file
#'
#' @param pathWeight \code{Character}, Path to weight .txt file
#' @param opts \code{list} of simulation parameters returned by the function \link{setSimulationPath}
#'
#' @examples
#'
#' \dontrun{
#'
#' updateBindingConstraintsIni(pathWeight <- "D:\\Users\\benothie\\Documents\\weigth.txt")
#'
#' }
#'
#' @rdname udpate-BindingConstraints
#'
#' @seealso \code{writeBindingConstraintsIni}
#' @export
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

#' @rdname udpate-BindingConstraints
#' @import antaresRead
#' @export
changeBindingConstraints <- function(pathWeight, opts = antaresRead::simOptions()){
  # reading binding constraints
  binding_cstr <- try(antaresRead::readBindingConstraints(opts = opts), silent = TRUE)
  if("try-error" %in% class(binding_cstr)){
    stop("Reading binding constraints : ", binding_cstr[1])
  }
  
  # read file with weight
  info_weight <- try(read.table(pathWeight, sep = "\t", dec = ".", header = T, check.names = F), silent= T)
  if("try-error" %in% class(info_weight)){
    stop("Reading weight : ", info_weight[1])
  }
  
  stopifnot("name" %in% colnames(info_weight))
  
  # update binding constraints
  # Q : control if we have 36FB ? linked between data & update ?
  up_binding_cstr <- lapply(binding_cstr, function(x){
    if(x$name %in% info_weight$name){
      # really have to set this 3 parameters ?
      x$enabled = TRUE
      x$type = "hourly"
      x$operator = "less"
      
      # remove other parameters
      x[(which(names(x)%in%"operator")+1) : length(x)] <- NULL
      
      # add new weight
      tmp_weight <- info_weight[info_weight$name %in% x$name, -1]
      ctrl_add <- lapply(colnames(tmp_weight), function(x){
        value <- tmp_weight[1, x]
        # write only if not zero
        if(value != 0){
          x[[x]] <<- tmp_weight[1, x]
        }
      })
      x
    } else {
      # just remove values
      x$values <- NULL
      x
    }
  })
  
  up_binding_cstr
}

#' Write bindingconstraints.ini file
#'
#' @param listData \code{list}, bindingconstraints.ini as list R
#' @param opts \code{list} of simulation parameters returned by the function \link{setSimulationPath}
#'
#' @examples
#'
#'
#' @export
writeBindingConstraintsIni <- function(listData, opts = antaresRead::simOptions()){
  # open ew file
  pathIni <- paste0(opts$inputPath, "/bindingconstraints/bindingconstraints.ini")
                    
  write_data <- writeIni(listData, pathIni)
  
}

