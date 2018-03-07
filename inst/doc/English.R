## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- eval=FALSE---------------------------------------------------------
#  
#  #Convert domains from PTDF file, save the output in the directory "model1"
#  computeFB(PTDF = system.file("/input/ptdf/PTDF.csv", package = "antaresFlowbased"),
#            outputName = "D:/model1", verbose = 0, nbFaces = 36)
#  
#  
#  #Set antares study path
#  antaresRead::setSimulationPath("D:/exemple_test", 0)
#  
#  #Create flow-based time series considering their correlations with other inputs of an Antares Study, save the output in the directory "model1"
#  createFBTS(probabilityMatrix = probabilityMatrix, multiplier = multiplier,
#             interSeasonBegin = interSeasonBegin,
#             interSeasonEnd = interSeasonEnd, firstDay = firstDay, outputPath = "D:/model1")
#  
#  
#  #Set setFlowbased directory path
#  setFlowbasedPath(path = "D:/model1")
#  
#  #Run shiny application to visualize the results of the convertion
#  runAppError()
#  
#  #Initialize the Antares study
#  initFlowBased()
#  

## ---- eval=FALSE---------------------------------------------------------
#  
#  # build probabilityMatrix with the function getProbability() from the package flowBasedClustering
#  # select an antaresStudy with the function setSimulationPath() from the package antaresRead
#  
#  # rename columns of the probability matrix
#  
#  matProb <- setNamesProbabilityMatrix(probabilityMatrix, c("FR_load", "DE_wind", "DE_solar"),
#                                     c("fr@load", "de@wind", "de@solar"))
#  
#  # set multipliers
#  
#  multiplier <- data.frame(variable = c("fr@load", "de@wind", "de@solar"),
#                           coef = c(1, 35000, 40000))
#  
#  # set Calendar
#  firstDay <- identifyFirstDay(opts = antaresStudy)
#  interSeasonBegin <- as.Date(c("2017-09-03", "2018-02-02"))
#  interSeasonEnd <- as.Date(c("2020-10-04", "2018-05-02"))
#  
#  # create ts.txt in D:/model1
#  ts <- createFBTS(opts = antaresStudy, probabilityMatrix = probabilityMatrix, multiplier = multiplier,
#                   interSeasonBegin = interSeasonBegin,
#                   interSeasonEnd = interSeasonEnd, firstDay = firstDay, outputPath = "D:/model1")
#  
#  

## ---- eval = FALSE-------------------------------------------------------
#  # Specify a repository
#  setFlowbasedPath(path = "C:/PATH/TO/INPUT")

## ---- eval = FALSE-------------------------------------------------------
#  runAppError()

## ---- eval = TRUE, echo = FALSE------------------------------------------
library(antaresFlowbased)
setFlowbasedPath(model = "model2017")

## ---- fig.width= 7, fig.height= 7, warning=FALSE-------------------------

# multiple countries and hour
plotFB(hour = 5:6, dayType = 1, country1 = c("FR", "DE"), country2 = c("DE", "NL"))


## ---- eval=FALSE---------------------------------------------------------
#  antaresRead::setSimulationPath("D:/exemple_test", 0)
#  
#  # initialisation of flow-based study
#  initFlowBased(scenario = rep(1:200, times = 5))

