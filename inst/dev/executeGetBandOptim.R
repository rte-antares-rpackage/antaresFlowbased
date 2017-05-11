




##Optim et rapport
computeFB(PTDF = system.file("/input/ptdf/PTDF.csv", package = "antaresFlowbased"),
          outputName = "D:/Users/titorobe/Desktop/model1", verbose = 0, nbFaces = 36)

addChroniquesFile("D:/Users/titorobe/Desktop/model1")
setFlowbasedPath(path = "D:/Users/titorobe/Desktop/model1")
runAppError()

antaresRead::setSimulationPath("D:/exemple_test", 0)
initFlowBased()
setSolverAntares(path = "C:/Program Files/RTE/Antares/5.0.9/bin/antares-5.0-solver.exe")
runSimulationFB()





generateRaportFb(allFB, 7)
allFB
runAppError(allFB)

allFB$outFlowBased[[16]]
generateRaportFb(allFB, 7)
setFlowbasedPath(bp = "BP2017")
runAppError()
names(res) <- c("BE", "DE", "FR")
res <- data.frame(res)

res2 <- data.frame(ctry1 = res[,1], 
                   ctry2 = res[,2])
res2 <- res2[chull(res2),]
res2 <- rbind(res2, res2[1,])
res <-  res2
library(rAmCharts)
amPlot(res$ctry1, res$ctry2, type = "l")
##Data
##Plot
pipeR::pipeline(
  amXYChart(dataProvider = res),
  addGraph(balloonText = 'x:<b>[[x]]</b> y:<b>[[y]]</b>',
           bullet = 'circle', xField = 'ctry1',yField = 'ctry2',
           lineAlpha = 1),
  addGraph(balloonText = 'x:<b>[[x]]</b> y:<b>[[y]]</b>',
           bullet = 'circle', xField = 'ctry1',yField = 'ctry2',
           lineAlpha = 1),
  setChartCursor()
  
)

library(ROI)


library(data.table)
library(antaresRead)
library(antaresFlowbased)
opts <- setSimulationPath('D:/Users/titorobe/Desktop/exemple_test/', '20170403-1622r_from')
dta <- adqPatch(opts)
dta2 <- readAntares(areas = c("fr", "be", "de", "nl"), 
                    links = c("be - de","be - fr","be - nl","de - fr","de - nl"), mcYears = "all",
                    select = c("LOLD", "UNSP. ENRG", "DTG MRG", "UNSP. ENRG", "BALANCE", "FLOW LIN."))


which(dta$areas$BALANCE-dta2$areas$BALANCE != 0)
dta$areas$BALANCE - dta2$areas$BALANCE

