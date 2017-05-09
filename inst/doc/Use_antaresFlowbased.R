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
#  # initialisation de l'étude flowbased
#  initFlowBased()

## ---- eval=FALSE---------------------------------------------------------
#  # chemin du solver antares
#  setSolverAntares(path = "C:/Program Files/RTE/Antares/5.0.9/bin/antares-5.0-solver.exe")
#  
#  # affichage du solver renseigne
#  getSolverAntares()

## ---- eval=FALSE---------------------------------------------------------
#  res_fb <- runSimulationFB(simulationName = "flowBased-Tuto")

