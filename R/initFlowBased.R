#' @title Initialization of the Antares flow-based study
#'
#' @description 
#' This function initializes the environment for a flow-based study. The study must be of Antares version equal or higher to 6.1.3 to enable binding 
#' constraints on thermal clusters.
#' \itemize{
#'  \item{
#' It creates a folder containing the description of the typical 
#' flow-based days (files created with the function \link{computeFB}), the flow-based time-series consistent with the study inputs (calculated by 
#' the function \link{createFBTS}), a scenario playlist to combine the flow-based time-series and Antares scenario builder and information 
#' about the model and time of creation of the folder. This folder will then be used to run the simulation and afterwards by all the functions 
#' acting on the outputs (post-processing, plots ...). Be careful to keep the consistency between the model loaded by \code{initFlowBased} and the one
#'  used for running previous simulations.}
#'  \item{
#'  It writes the 36 binding constraints representing the flow-based domains, with the weight files being the coefficients of the constraints and the
#'   second members converted into availability time series of virtual power plants (= the combination weights x links < thermal availability)
#'  }
#'}
#' @param fb_opts \code{list} of flowbased parameters (flow-based model directory) returned by the function \link{setFlowbasedPath}. 
#' By default, the value is indicated by \code{antaresFlowbased::fbOptions()}
#' @param opts \code{list} of simulation parameters returned by the function \link{setSimulationPath}, Antares study. By default, the value is 
#' indicated by \code{antaresRead::simOptions()}
#' @param scenarios \code{numeric} vector, it represents the flow-based scenarios builder. It will be written in the file scenario.txt and used by 
#' Antares to combine Antares time series (the scenarios builder) and the flow-based time series. By default, the value is \code{rep(1:200, times = 5)}
#'  (1000 mcYears, with 200 time series repeated 5 times)
#' 
#' @note 
#' 
#' The folder designed by by fb_opts is a flow-based model. It must include the following files :
#' 
#' \itemize{
#'   \item{domainesFB.RDS : RDS file created by \link{computeFB}, information on the conversion from real to modelled domain}
#'   \item{second_member.txt : text file created by \link{computeFB}, representing the second members (or margins) in the constraints. It includes
#'   the following columns :
#'   \itemize{
#'     \item{Id_day : numeric, from 1 to the number of typical days}
#'     \item{Id_hour : numeric, from 1 to 24}
#'     \item{vect_b : numeric, second member in MW}
#'     \item{Name : character, name of the constraint, generally "FBnumber"}
#'    }
#'   }
#'   
#'   \item{ts.txt: text file created by \link{createFBTS}, flow-based typical day time series with
#'   \itemize{
#'     \item{First colum : dates, format : YYYY-MM-DD}
#'     \item{First row : names of the time series, "number"}
#'     \item{cells : numeric (typival day ID)}
#'    }
#'   }
#'   
#'   \item{weigth.txt : text file representing the weights of the binding constraints. It includes the following columns :
#'   \itemize{
#'     \item{Name : character, name of the contraints, matching the chosen name in the second member file}
#'     \item{BE.FR : numeric coefficient on the link from Belgium to France, between -1 and 1}
#'     \item{DE.FR : numeric coefficient on the link from Germany to France, between -1 and 1}
#'     \item{DE.NL : numeric coefficient on the link from Germany to The Netherlands, between -1 and 1}
#'     \item{BE.NL : numeric coefficient on the link from Belgium to The Netherlands, between -1 and 1}
#'     \item{BE.DE : numeric coefficient on the link from Belgium to Germany, between -1 and 1}
#'    }
#'   }
#'   }
#'   
#'   These files will be written in the Antares study (directory user\flowbased\), as well as additional files:
#'   \itemize{
#'   \item{scenario.txt, flow-based scenario builder, including only one column entitled "scenarios". The row 2 will then match the MC year 1 in
#'   the Antares scenario builder}
#'   \item{infos.ini Informations on the configured model.
#'   \itemize{
#'     \item{date : Time of initialisation}
#'     \item{model : name of the used model}
#'     }
#'   }
#'   }
#'  }
#'  
#' @examples
#'
#' \dontrun{
#' 
#'  antaresRead::setSimulationPath("D:/Users/titorobe/Desktop/antaresStudy",1)
#'  initFlowBased()
#'  }
#'  
#'  
#' @import data.table antaresRead plyr antaresEditObject
#' 
#' @export
initFlowBased <- function(fb_opts = antaresFlowbased::fbOptions()$path,
                          opts = antaresRead::simOptions(), scenarios = rep(1:200, times = 5)){
  
  suppressWarnings(opts <- setSimulationPath(opts$studyPath, "input"))
  #Control antaresSolver >=6.1
  
  
  #Ctrl study version
  if(opts$antaresVersion < 610)stop("Your studie must be in version 6.1 or more")
  
  
  #.ctrlSolver()
  
  
  
  #test fbModel
  .controlFbMod(fb_opts)
  
  modelName <- strsplit(fb_opts, "/")
  modelName <- modelName[[1]][length(modelName[[1]])]
  
  ###Load fbModel data
  #Load weight.txt
  W <- .getWeight(paste0(fb_opts, "/weight.txt"))
  
  #Load second_member.txt
  seM <- .getSecondMember(paste0(fb_opts, "/second_member.txt"))
  
  #Load ts.txt
  tS <- .getDayType(paste0(fb_opts, "/ts.txt"))
  
  
  #Copy files in flowbased study
  userFolder <- paste0(opts$studyPath, "/user")
  if(!dir.exists(userFolder))dir.create(userFolder)
  
  userFolder <- paste0(userFolder, "/flowbased")
  if(!dir.exists(userFolder))dir.create(userFolder)
  
  file.copy(paste0(fb_opts, "/weight.txt"), paste0(userFolder, "/weight.txt"), overwrite = TRUE)
  file.copy(paste0(fb_opts, "/second_member.txt"), paste0(userFolder, "/second_member.txt"), overwrite = TRUE)
  file.copy(paste0(fb_opts, "/ts.txt"), paste0(userFolder, "/ts.txt"), overwrite = TRUE)
  file.copy(paste0(fb_opts, "/domainesFB.RDS"), paste0(userFolder, "/domainesFB.RDS"), overwrite = TRUE)
  
  
  #Write scenario
  write.table(data.table(simulation = scenarios),  paste0(userFolder, "/scenario.txt"), row.names = FALSE)
  
  
  
  
  #Controle coerancy
  if(length(unique(scenarios)) != ncol(tS) - 1)stop("length(unique(scenarios)) must by equal to number of timeseries")
  
  if(!all(sort(unique(scenarios)) %in% 1:max(scenarios)))stop("scenarios must begin to 1 an all scenarios between 1 and length(unique(scenarios)) must be present")
  
  
  
  
  ##Test ready-made
  rediM <- antaresEditObject::readIniFile(paste0(opts$studyPath, "/settings/generaldata.ini"))$general$generate
  if(!is.na(rediM)){
    if(grepl("thermal", rediM))stop("Flow-based modelling can only be used if thermal time-series are ready-made")
    
  }
  
  #Supress building constains "_fb"
  .supressOldBindingConstraints(opts)
  
  #Delete and re-create model_description_fb area
  .deleteOldAreaAndCreatNew(opts)
  suppressWarnings(opts <- setSimulationPath(opts$studyPath, "input"))
  
  #Create new clusters
  .createCluster(tS, opts, W, seM, scenarios)
  
  suppressWarnings(opts <- setSimulationPath(opts$studyPath, "input"))
  
  #Create building C
  .createBindingConstraint(W, opts)
  
  daT <- substr(as.character(Sys.time()), 1, 16)
  
  paramS <- list(general = list(date = daT, model = modelName))
  
  ##Write param of user folder
  antaresEditObject::writeIni(paramS, paste0(userFolder, "/infos.ini"), overwrite = TRUE)
  
  
  cat("Study ready for flow-based simulations")
  
}

.createCluster <- function(tS, opts, W, seM, scenarios)
{
  
  Name <- NULL
  
  #Prepare second member data
  allTs <- names(tS)
  allTs <- allTs[allTs!="Date"]
  
  
  #For eatch weight, create cluster thrm
  sapply(1:nrow(W), function(X){
    tpR <- W[X]
    clusterName <- paste0(tpR$name, "_fb")
    nomCap <- max(seM[Name==tpR$name]$vect_b)
    modulation <- matrix(1, ncol = 4, nrow = 8760)
    # modulation[,1] <- 0
    tsDta <- sapply(allTs, function(ZZ){
      tsT <- tS[[ZZ]]
      tsT <- data.table(Id_day = tsT)
      seM[Name == tpR$name][tsT, on="Id_day", allow.cartesian=TRUE]$vect_b
    })
    
    createCluster(area = "model_description_fb",
                  cluster_name = clusterName,
                  unitcount = 1L,
                  group = "other",
                  nominalcapacity = nomCap,
                  prepro_modulation = modulation,
                  time_series = tsDta, opts = opts)
  })
  
  #Update general setting
  antaresEditObject::updateGeneralSettings(nbyears = length(scenarios), opts = opts)
  
  
  #Update senario builder
  pathsb <- file.path(opts$studyPath, "settings", "scenariobuilder.dat")
  opts <- setSimulationPath(opts$studyPath, "input")
  
  
  
  oldFile <- read.table(pathsb, sep = "@")
  oldFile <- oldFile[-1,]
  allRes <- as.vector(oldFile)
  splitRes <- strsplit(allRes, ",")
  fl <- lapply(splitRes, function(x){
    x[2]
  })
  fl <- unlist(fl)
  toRm <- which(fl == "model_description_fb")
  if(length(toRm) > 0)allRes <- allRes[-toRm]
  
  firstLetter <- c("t")
  areas <- getAreas(opts = opts)
  clusterD <- readClusterDesc(opts = opts)
  clusterD <- clusterD[clusterD$area == "model_description_fb"]
  prim <- paste0("t,", clusterD$area)
  firstC <- 1:length(scenarios)-1
  allValue <- expand.grid( prim, firstC)
  
  
  endFile <- paste(allValue$Var1, allValue$Var2, sep=",")
  endFile <- paste0(endFile, ",", clusterD$cluster, " = ", rep(scenarios,each = length(clusterD$area)) )
  
  
  
  
  endFile <- c("[Default Ruleset]", allRes, endFile)
  write(endFile, pathsb)
  
  
}

.createBindingConstraint <- function(W, opts)
{
  W <- copy(W)
  operator <- "less"
  timeStep <- "hourly"
  sapply(1:nrow(W), function(X){
    ctrCurrent <- W[X]
    ctName <- paste0(ctrCurrent$name, "_fb")
    ctrCurrent <- unlist(ctrCurrent[, .SD, .SDcols = 2:ncol(ctrCurrent)])
    ctrCurrent <- ctrCurrent[which(ctrCurrent!=0)]
    coefficients <- ctrCurrent
    clUpdate <- paste0("model_description_fb.", "model_description_fb_",ctName)
    ctV <- -1
    names(ctV) <-clUpdate 
    names(coefficients) <- tolower(names(coefficients))
    coefficients <- c(coefficients, ctV)
    createBindingConstraint(name = ctName,
                            values = NULL,
                            timeStep = timeStep,
                            operator = "less",
                            coefficients = coefficients,
                            opts = opts, overwrite = TRUE)
    NULL
  })
  
}


.supressOldBindingConstraints <- function(opts)
{
  bdC <- antaresRead::readBindingConstraints(opts)
  nameBdc <- names(bdC)
  bdcToSupress <- nameBdc[grep("_fb$", nameBdc)]
  
  sapply(bdcToSupress, function(X){
    removeBindingConstraint(X, opts = opts)
    NULL
  })
}


.deleteOldAreaAndCreatNew <- function(opts, area = "model_description_fb")
{
  if("model_description_fb" %in% getAreas(opts = opts))opts <- removeArea(area, opts = opts)
  
  opts <- createArea(area, opts = opts)
  opts
  NULL
}

.controlFbMod <- function(fbModel)
{
  fileInFb <- list.files(fbModel)
  if(!all(c("weight.txt", "second_member.txt", "ts.txt") %in% fileInFb))stop("Flow-based model does not contain all necessary input files, second_member.txt, ts.txt and weight.txt")
  
}
