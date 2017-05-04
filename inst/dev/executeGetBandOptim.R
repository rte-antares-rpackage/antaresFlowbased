
##Optim et rapport
allFB <- computeFB(dayType = 7:8, hour = 1:24)

runAppError(allFB)


generateRaportFb(allFB, 7)


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

##ADQ Patch
b <- fread("inst/ADQpatch/b.txt")
b36 <- fread("inst/ADQpatch/B36.txt")
lole <- fread("inst/ADQpatch/lole.txt")
lole <- unlist(lole)
D <- as.vector(ifelse(lole == 0, 0, 1))
res <- c(
  1, 1, 1, 1,
  D[1]*D[2]*lole[2], -D[1]*D[2]*lole[1],0,0,
  D[1]*D[3]*lole[3], 0, -D[1]*D[3]*lole[1], 0,
  D[1]*D[4]*lole[4],0,0,-D[1]*D[4]*lole[1],
  0,D[2]*D[3]*lole[3],-D[2]*D[3]*lole[2],0,
  0, D[2]*D[4]*lole[4], 0, -D[2]*D[4]*lole[2],
  0,0,D[3]*D[4]*lole[4],-D[3]*D[4]*lole[3])
res <- matrix(res, ncol = 4, byrow = TRUE)
b36 <- as.matrix(b36)[,2:4]
b36 <- cbind(b36, 0)
allMat <- rbind(res, b36)
rep <- c(rep(0, 7), b$V2)
sens <- c(rep("==", 7), rep("<=", length(b$V2)))
objetiv <- c(lole)
l_constraint <- L_constraint(L = allMat,
                             dir = sens,
                             rhs = rep)
bounds <- V_bound(li=1:4, lb=rep(-Inf, 4))

LP <- OP(objetiv, l_constraint, maximum = FALSE,
         bounds = bounds)
y <- ROI_solve(LP, solver = "clp")
y$solution
