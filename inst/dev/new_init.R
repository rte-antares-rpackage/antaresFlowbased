library(data.table)
library(antaresEditObject)
library(antaresRead)
library(dplyr)
fbModel <- "D:/Users/titorobe/Desktop/Antares/antaresFlowbased/inst/input/model/p2017"
scenarios <- rep(1:200, times = 5)
opts <- setSimulationPath("D:/Users/titorobe/Desktop/BP16_2020_conso_median_defav", "input")





W <- fread(paste0(fbModel, "/weight.txt"))
seM <-  fread(paste0(fbModel, "/second_member.txt"))
tS <-  fread(paste0(fbModel, "/ts.txt"))








opts <- setSimulationPath("D:/Users/titorobe/Desktop/BP16_2020_conso_median_defav", "input")

