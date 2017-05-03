# #Get B
# library(data.table)
# library(pipeR)
# library(ROI)
#

#GiB
# PTDF <- fread("inst/optimWork/PTDF.csv")
# setwd("../../PresolveFB/")
# out <- sapply(1:12, function(X){
#   sapply(1:24, function(Y){
#     write.table(PTDF[Id_day == X & Period == Y, .SD,
#                      .SDcols = c("BE","DE","FR","NL","RAM_0","Row")],
#                 "D:/Users/titorobe/Desktop/PresolveFB/A.csv", row.names = FALSE, sep = ";")
#     system("D:/Users/titorobe/Desktop/PresolveFB/EXEC.bat")
#     tt <- read.table("FiltrageDe_A.csv", sep = "@")
#     write.table(as.matrix(tt[(which(tt == 'SOMMETS')+1):nrow(tt),]), "tp.csv",
#                 row.names = FALSE, col.names = FALSE, quote = FALSE)
#     data.table(Id_day = X, Period = Y,   fread("tp.csv"))
#   }, simplify = FALSE)
# }, simplify = FALSE)
#
# res <- rbindlist(lapply(out, rbindlist))
# write.table(res,"sommets_which_solver.csv", row.names = FALSE)
#

allFB <- computeFB(dayType = 7)
generateRaportFb(allFB, dayType = 7)



##Optim et rapport
allFB <- computeFB(dayType = 8, hour = 12)

sapply( 1:12, function(X){
  sapply( 1:24, function(Y){
    generateRaportFb(allFB, Y, X)
  })
})

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



#
# #Pareto
# sum(get_solution(tt, y1_plus[i])$value) +
#   sum(get_solution(tt, y2_plus[i])$value) +
#   sum(get_solution(tt, y3_plus[i])$value) +
#   sum(get_solution(tt, y1_moins[i])$value) +
#   sum(get_solution(tt, y2_moins[i])$value) +
#   sum(get_solution(tt, y3_moins[i])$value)
#
# sum(get_solution(tt, x1_plus[i])$value) +
#   sum(get_solution(tt, x2_plus[i])$value) +
#   sum(get_solution(tt, x3_plus[i])$value) +
#   sum(get_solution(tt, x1_moins[i])$value) +
#   sum(get_solution(tt, x2_moins[i])$value) +
#   sum(get_solution(tt, x3_moins[i])$value)
#
#
# #Sommets
# sommets <- data.frame(y1 = get_solution(tt, y1[i])$value,
#                       y2 = get_solution(tt, y2[i])$value,
#                       y3 = get_solution(tt, y3[i])$value)
#
#
#
#
# convexData <- function(data){
#   ch <- chull(data)
#   data[c(ch, ch[1]), ]
#
# }
#
# library(geometry)
# library(rgl)
#
# all <- rbind(as.matrix(EXTREME_X), as.matrix(sommets))
# denom <- convhulln(EXTREME_X,option = "FA")$vol + convhulln(sommets,option = "FA")$vol - convhulln(all,option = "FA")$vol
# 1-denom/convhulln(EXTREME_X,option = "FA")$vol
# 1-denom/convhulln(sommets,option = "FA")$vol
#
#
#
# library(hypervolume)
# vol <- expectation_convex(data.frame(EXTREME_X), check_memory = FALSE)
# vol2 <- expectation_convex(data.frame(sommets), check_memory = FALSE)
# hypervolume_holes(vol, vol2)
#
#
# EXTREME_X <- data.frame(EXTREME_X)
# ts.EXTREME_X<- t(convhulln(EXTREME_X))  # see the qhull documentations for the options
# ## Not run:
# rgl.triangles(EXTREME_X[ts.EXTREME_X,1],EXTREME_X[ts.EXTREME_X,2],EXTREME_X[ts.EXTREME_X,3],col="blue",alpha=1)
#
# sommets <- data.frame(sommets)
# ts.sommets<- t(convhulln(sommets))
# rgl.triangles(sommets[ts.sommets,1],sommets[ts.sommets,2],sommets[ts.sommets,3],col="green",alpha=1)
#
# ashape3d()
#
# polygon3d()
# mesh.diff(mesh.dsphere(cbind(sommets[ts.sommets,1],sommets[ts.sommets,2],sommets[ts.sommets,3])),
#           mesh.dsphere(cbind(EXTREME_X[ts.EXTREME_X,1],EXTREME_X[ts.EXTREME_X,2],EXTREME_X[ts.EXTREME_X,3])))
#
#
# mesh.dsphere(cbind(sommets[ts.sommets,1],sommets[ts.sommets,2],sommets[ts.sommets,3]))
#
#
#
#
#
# convhulln(EXTREME_X)
# convex.sommet <- cbind(sommets[ts.sommets,1],sommets[ts.sommets,2],sommets[ts.sommets,3])
# convex.EXTREME_X <- cbind(EXTREME_X[ts.EXTREME_X,1],EXTREME_X[ts.EXTREME_X,2],EXTREME_X[ts.EXTREME_X,3])
#
#
#
#
# p = Polygon( convexData(sommets[, c(1,2)]))
# ps = Polygons(list(p),1)
# sps = SpatialPolygons(list(ps))
# plot(sps, col = "blue")
#
# p = Polygon( convexData(EXTREME_X[, c(1,2)]))
# ps = Polygons(list(p),1)
# sps2 = SpatialPolygons(list(ps))
# plot(sps2, col = "red", add = TRUE)
# sps3 <- intersect(sps, sps2)
# plot(sps3, col = "green", add = TRUE)
# sps3@polygons[[1]]@Polygons[[1]]@area/sps2@polygons[[1]]@Polygons[[1]]@area
# sps3@polygons[[1]]@Polygons[[1]]@area/sps@polygons[[1]]@Polygons[[1]]@area
#
