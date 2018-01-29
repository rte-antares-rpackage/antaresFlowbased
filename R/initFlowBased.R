#' @title Generate environment for a flow-based study
#'
#' @description  Generate environment for a flow-based study
#' 
#'
#' @param fb_opts \code{list} of flowbased parameters returned by the function \link{setFlowbasedPath}. Defaut to \code{antaresFlowbased::fbOptions()}
#' @param opts \code{list} of simulation parameters returned by the function \link{setSimulationPath}. Defaut to \code{antaresRead::simOptions()}
#' @param scenarios \code{numeric} scenarios use for write scenario.txt.
#' 
#' @note 
#' folder deigned by fb_opts contain files :
#' \itemize{
#'   \item{domainesFB.RDS}{RDS file from \link{computeFB}}
#'   \item{second_member.txt}{txt file of second member wich following columns :
#'   \itemize{
#'     \item{Id_day : numeric from 1 to number of day id}
#'     \item{Id_hour : numeric from 1 to number of hour}
#'     \item{vect_b : numeric}
#'     \item{Name : character, name of constaints}
#'   }}
#'   \item{ts.txt}{txt file of time series matrix wich
#'   \itemize{
#'     \item{In row : dates, format : %YYYY-%MM-%DD
#'     \item{In column : vector of chronics}
#'     \item{in cell, numeric (typival day ID)}
#'   }}}
#'   \item{weigth.txt}{weigth file  wich following columns :
#'   \itemize{
#'     \item{Name : character, name of contraint}
#'     \item{BE.FR : numeric, between -1 and 1}
#'     \item{DE.FR : numeric, between -1 and 1}
#'     \item{DE.NL : numeric, between -1 and 1}
#'     \item{BE.NL : numeric, between -1 and 1}
#'     \item{BE.DE : numeric, between -1 and 1}
#'   }}}
#'  
#' @examples
#'
#' \dontrun{
#' 
#'  setSolverAntares("AntaresFolderInstall/bin/antares-6.1-solver.exe")
#'  initFlowBased()
#'  }
#'  
#'  
#' @import data.table antaresRead plyr
#' 
#' @export
initFlowBased <- function(fb_opts = antaresFlowbased::fbOptions(),
                          opts = antaresRead::simOptions(), scenarios = rep(1:200, times = 5)){
  
  #Control antaresSolver >=6.1
  .ctrlSolver()
  
  #test fbModel
  .controlFbMod(fb_opts)
  
  ###Load fbModel data
  #Load weight.txt
  W <- .getWeight(paste0(fb_opts, "/weight.txt"))
  
  #Load second_member.txt
  seM <- .getSecondMember(paste0(fb_opts, "/second_member.txt"))
  
  #Load ts.txt
  tS <- .getDayType(paste0(fb_opts, "/ts.txt"))
  
  #Controle coerancy
  if(length(unique(scenarios)) != ncol(tS) - 1){
    stop("length(unique(scenarios)) must by equal to number of timeseries")
  }
  if(!all(sort(unique(scenarios)) %in% 1:max(scenarios))){
    stop("scenarios must begin to 1 an all scenarios between 1 and length(unique(scenarios)) must be present")
  }
  
  #Supress building constains "_fb"
  .supressOldBindingConstraints(opts)
  
  #Delete and re-create model_description_fb area
  .deleteOldAreaAndCreatNew(opts)
  
  #Create new clusters
  .createCluster(tS, opts, W, seM)
  
  #Create building C
  .createBindingConstraint(W, opts)
  
  
  
}

.createCluster <- function(tS, opts, W, seM)
{
  #Prepare second member data
  allTs <- names(tS)
  allTs <- allTs[allTs!="Date"]
  
  
  #For eatch weight, create cluster thrm
  sapply(1:nrow(W), function(X){
    tpR <- W[X]
    clusterName <- paste0(tpR$name, "_fb")
    nomCap <- max(seM[Name==tpR$name]$vect_b)
    modulation <- matrix(1, ncol = 4, nrow = 8760)
    
    tsDta <- sapply(allTs, function(X){
      tsT <- tS[[X]]
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
  firstLetter <- c("l", "s", "w")
  areas <- getAreas(opts = opts)
  firstC <- 1:length(scenarios)-1
  allValue <- expand.grid( firstC, areas, firstLetter)
  endFile <- paste(allValue$Var3, allValue$Var2, allValue$Var1, sep=",")
  endFile <- paste0(endFile, " = ", scenarios)
  
  endFile <- c("[Default Ruleset]", endFile)
  write(endFile, pathsb)
  
  #Run antares
  runSimulation(name = "toto22", path_solver = getSolverAntares(), parallel = TRUE)
  
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
  if("model_description_fb" %in% getAreas(opts = opts)){
    opts <- removeArea(area, opts = opts)
  }
  opts <- createArea(area, opts = opts)
  opts
  NULL
}

.controlFbMod <- function(fbModel)
{
  fileInFb <- list.files(fbModel)
  if(!all(c("weight.txt", "second_member.txt", "ts.txt") %in% fileInFb)){
    stop("Flow-based model does not contain all necessary input files, second_member.txt, ts.txt and weight.txt")
  }
}

.ctrlSolver <- function()
{
  solver <- getSolverAntares()
  solver <- unlist(gsub("-solver.exe", "", solver))
  solver <- strsplit(solver, "antares-")[[1]]
  solver <- solver[[length(solver)]]
  versionSolver <- as.numeric(solver)
  if(versionSolver<6.1){
    stop("Flow-based modelling can only be used on Antares studies of version 6.1 or above.")
  }
}