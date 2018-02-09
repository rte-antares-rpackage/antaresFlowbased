context("createFBTS")


test_that("make ts", {

  op5 <- antaresRead::setSimulationPath(testStudy2)
  
  matProb <- readRDS(system.file("testdata/proba.RDS", package = "antaresFlowbased"))
  
  setnames(matProb[[1]],"FR_load", "fr_load" )
  setnames(matProb[[2]],"FR_load", "fr_load" )
  
  setnames(matProb[[1]],"DE_wind", "de_wind" )
  setnames(matProb[[2]],"DE_wind", "de_wind" )
  
  setnames(matProb[[1]],"DE_solar", "be_wind" )
  setnames(matProb[[2]],"DE_solar", "be_wind" )
  
  multiplier <- data.frame(variable = c("fr_load", "de_wind", "be_wind"),
                           coef = c(1, 352250, 246403))
  firstDay <- suppressWarnings(identifyFirstDay(op5, firstArea = "fr", secondArea = NULL))
  
  
  interSeasonBegin <- as.Date(c("2017-09-03", "2018-02-02"))
  interSeasonEnd <- as.Date(c("2017-10-04", "2018-05-02"))
  
  firstF <- NULL
  for(k in 1:10)
  {
    ts <- createFBTS(opts = op5, probabilityMatrix = matProb, multiplier = multiplier,
                     interSeasonBegin = interSeasonBegin, interSeasonEnd = interSeasonEnd,
                     firstDay = firstDay, seed = k, silent = TRUE)
    
    
    frLoad <- readInputTS(load = "fr", timeStep = "daily", showProgress = FALSE)
    windbe <- readInputTS(wind = c("be"), timeStep = "daily", showProgress = FALSE)
    windde <- readInputTS(wind = c("de"), timeStep = "daily", showProgress = FALSE)
    allDta <- data.table(frLoad, be = windbe[["wind"]],de = windde[["wind"]])
    allDta <- allDta[tsId == 1]
    
    dates <- allDta$time
    calendar <- .getVirtualCalendar(dates, interSeasonBegin, interSeasonEnd, firstDay)
    
    data1 <- allDta[180]
    whoIs <- lapply(calendar, function(X){
      data1$time%in%X
    })
    saison <- names(which(unlist(whoIs)))
    
    quant <- matProb[[2]][class == saison]
    prb <- matProb[[1]][class == saison]
    prb[fr_load == "0_0.5" & de_wind == "0_0.3333" & be_wind == "0_0.5"]
    prob1 <- 0.33
    prob2 <- 0.66
    
    firstF <- c(firstF, ts[ts$time == data1$time]$`1`)
  }
  expect_true(2%in%firstF)
  expect_true(1%in%firstF)
  expect_true(all(firstF%in%c(1, 2)))
  
})
