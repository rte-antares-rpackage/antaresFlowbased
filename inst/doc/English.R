## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- eval=FALSE---------------------------------------------------------
#  
#  #Convert domains from PTDF file, save the output in the directory "model1"
#  computeFB(PTDF = system.file("/input/ptdf/PTDF.csv", package = "antaresFlowbased"),
#            outputName = "D:/Users/titorobe/Desktop/model1", verbose = 0, nbFaces = 36)
#  
#  #Add time series file to directory "model1"
#  addTSFile("D:/Users/titorobe/Desktop/model1")
#  
#  #Set setFlowbased directory path
#  setFlowbasedPath(path = "D:/Users/titorobe/Desktop/model1")
#  
#  #Run shiny application to visualize the results of the convertion
#  runAppError()
#  
#  #Set antares study path
#  antaresRead::setSimulationPath("D:/exemple_test", 0)
#  
#  #Initialize the Antares study
#  initFlowBased()
#  
#  #Set Antares solver path
#  setSolverAntares(path = "C:/Program Files/RTE/Antares/5.0.9/bin/antares-5.0-solver.exe")
#  
#  #Run Antares simulation
#  runSimulationFB()

## ---- eval = FALSE-------------------------------------------------------
#  # Specify a repository
#  setFlowbasedPath(path = "C:/PATH/TO/INPUT")

## ---- echo = FALSE-------------------------------------------------------
suppressWarnings(require(antaresFlowbased, quietly = T))


## ------------------------------------------------------------------------
require(antaresFlowbased)

# available in package
getAvailableModel()

# Specify a model already available in the package
setFlowbasedPath(model = "model2017")


## ---- fig.width= 7, fig.height= 7, warning=FALSE-------------------------
# one graphic
plotFB(hour = 5, dayType = 1, country1 = "FR", country2 = "DE")

# multiple countries and hour
plotFB(hour = 5:6, dayType = 1, country1 = c("FR", "DE"), country2 = c("DE", "NL"))


## ---- eval=FALSE---------------------------------------------------------
#  antaresRead::setSimulationPath("D:/exemple_test", 0)
#  
#  # initialisation of flow-based study
#  initFlowBased()

## ---- eval=FALSE---------------------------------------------------------
#  # antares solver
#  setSolverAntares(path = "C:/Program Files/RTE/Antares/5.0.9/bin/antares-5.0-solver.exe")
#  
#  
#  getSolverAntares()

## ---- eval=FALSE---------------------------------------------------------
#  res_fb <- runSimulationFB(simulationName = "flowBased-Tuto")

