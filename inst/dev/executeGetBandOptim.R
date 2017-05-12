

##Optim et rapport
computeFB(PTDF = system.file("/input/ptdf/PTDF.csv", package = "antaresFlowbased"),
          outputName = "D:/Users/titorobe/Desktop/model1", verbose = 0, nbFaces = 36,dayType = 1)

addChroniquesFile("D:/Users/titorobe/Desktop/model1")
setFlowbasedPath(path = "D:/Users/titorobe/Desktop/model1")
runAppError()

antaresRead::setSimulationPath("D:/exemple_test", 0)
initFlowBased()
setSolverAntares(path = "C:/Program Files/RTE/Antares/5.0.9/bin/antares-5.0-solver.exe")
runSimulationFB(mcYears = c(1,2))


antaresRead::setSimulationPath("D:/exemple_test", 1)
library(antaresFlowbased)
library(antaresRead)
opts <- setSimulationPath('D:/Users/titorobe/Desktop/exemple_test/', '20170403-1622r_from')
dta <- adqPatch(opts) 
dta
