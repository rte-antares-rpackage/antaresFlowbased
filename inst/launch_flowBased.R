library(antaresFlowbased)

path <- "D:/Users/titorobe/Desktop/exemple_test"
path <- "D:/Users/benothie/Documents/exemple_test"

opts <- antaresRead::setSimulationPath(path, 0)

weigth <- system.file("/test/data/coefficients_Antares.csv", package = "antaresFlowbased")
secondMember <- system.file("/test/data/fichier_b_final.csv", package = "antaresFlowbased")
dayType <- system.file("/test/data/id_FB.txt", package = "antaresFlowbased")

initFlowBased(opts = opts,
              weigth = weigth,
              secondMember = secondMember,
              dayType = dayType)


setSolverAntares(path = "C:\\Program Files\\RTE\\Antares\\5.0.9\\bin\\antares-5.0-solver.exe")

mysim <- runSimulationFB(opts, "MystudyTest2")
