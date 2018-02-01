# 
# 
# matProb <- readRDS(system.file("testdata/proba.RDS", package = "antaresFlowbased"))
# 
# opts <- antaresRead::setSimulationPath("D:/Users/titorobe/Desktop/antaresStudy - Copie")
# 
# setnames(matProb[[1]],"FR_load", "fr_load" )
# setnames(matProb[[2]],"FR_load", "fr_load" )
# 
# setnames(matProb[[1]],"DE_wind", "de_wind" )
# setnames(matProb[[2]],"DE_wind", "de_wind" )
# 
# setnames(matProb[[1]],"DE_solar", "de_solar" )
# setnames(matProb[[2]],"DE_solar", "de_solar" )
# 
# multiplier <- data.frame(variable = c("fr_load", "de_wind", "de_solar"),
#                          coef = c(1, 352250, 246403))
# firstDay <- identifyFirstDay(opts, firstArea = "FR", secondArea = NULL)
# 
# 
# interSeasonBegin <- as.Date(c("2017-09-03", "2018-02-02"))
# interSeasonEnd <- as.Date(c("2017-10-04", "2018-05-02"))
# 
# 
# 
# 
# opts <- setSimulationPath("D:/Users/titorobe/Desktop/ex_test/", "input")
# firstDay <- identifyFirstDay(opts, firstArea = "FR", secondArea = NULL)
# 
# ts <- createFBTS(opts = opts, probabilityMatrix = matProb, multiplier = multiplier,
#                  interSeasonBegin = interSeasonBegin, interSeasonEnd = interSeasonEnd, firstDay = firstDay)
# 
# 
