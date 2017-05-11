## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- echo = FALSE-------------------------------------------------------
suppressWarnings(require(antaresFlowbased, quietly = T))


## ------------------------------------------------------------------------
require(antaresFlowbased)

# default settings
fbOptions()

# available in package
getAvailableBP()

# Specify a availabled bp
setFlowbasedPath(bp = "BP2017")

# Specify a repository
# setFlowbasedPath(bp = "C:/PATH/TO/INPUT")


## ---- fig.width= 7, fig.height= 7----------------------------------------
# one graphic
plotFB(hour = 5, dayType = 1, country1 = "FR", country2 = "DE")

# multiple countries and hour
plotFB(hour = 5:6, dayType = 1, country1 = c("FR", "DE"), country2 = c("DE", "NL"))


## ---- eval=FALSE---------------------------------------------------------
#  antaresRead::setSimulationPath("D:/exemple_test", 0)
#  
#  # initialisation of flowbased study
#  initFlowBased()

## ---- eval=FALSE---------------------------------------------------------
#  # antares solver
#  setSolverAntares(path = "C:/Program Files/RTE/Antares/5.0.9/bin/antares-5.0-solver.exe")
#  
#  
#  getSolverAntares()

## ---- eval=FALSE---------------------------------------------------------
#  res_fb <- runSimulationFB(simulationName = "flowBased-Tuto")

## ---- eval=FALSE---------------------------------------------------------
#  
#  #Compute FB from package PTDF file, save in directory "model1"
#  computeFB(PTDF = system.file("/input/ptdf/PTDF.csv", package = "antaresFlowbased"),
#            outputName = "D:/Users/titorobe/Desktop/model1", verbose = 0, nbFaces = 36)
#  #Add Chronics file to directory "model1"
#  addChroniquesFile("D:/Users/titorobe/Desktop/model1")
#  
#  #Set setFlowbased directory path
#  setFlowbasedPath(path = "D:/Users/titorobe/Desktop/model1")
#  
#  #Run shiny application for error visualisation
#  runAppError()
#  
#  #Set antares study path
#  antaresRead::setSimulationPath("D:/exemple_test", 0)
#  
#  #Init flowbased study
#  initFlowBased()
#  
#  #Set antares solver path
#  setSolverAntares(path = "C:/Program Files/RTE/Antares/5.0.9/bin/antares-5.0-solver.exe")
#  
#  #Run antares simulation
#  runSimulationFB()

