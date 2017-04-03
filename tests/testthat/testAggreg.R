context("Function aggregateResult")

testStudy <- system.file("test/data/exemple_test",package = "antaresFlowbased")
opts <- antaresRead::setSimulationPath(testStudy)
aggregateResult(opts = opts, newname = "testStydu")
resAA <- readAntares(areas = "all", links = "all", clusters = "all", timeStep = "annual")
resAD <- readAntares(areas = "all", links = "all", clusters = "all", timeStep = "daily")
resAH <- readAntares(areas = "all", links = "all", clusters = "all", timeStep = "hourly")
resAM <- readAntares(areas = "all", links = "all", clusters = "all", timeStep = "monthly")
resAW <- readAntares(areas = "all", links = "all", clusters = "all", timeStep = "weekly")

unlink(paste0(testStudy, "/output/testStydu/economy/mc-all"), recursive = TRUE)

dir.create( paste0(testStudy, "/output/testStydu/economy/mc-all/"))
file.copy(paste0(paste0(testStudy, "/output/testStydu/economy/mc-all-antares/"), 
                 list.files(paste0(testStudy, "/output/testStydu/economy/mc-all-antares/"))),
            paste0(testStudy, "/output/testStydu/economy/mc-all/"), recursive = TRUE)


resRA <- readAntares(areas = "all", links = "all", clusters = "all", timeStep = "annual")
resRD <- readAntares(areas = "all", links = "all", clusters = "all", timeStep = "daily")
resRH <- readAntares(areas = "all", links = "all", clusters = "all", timeStep = "hourly")
resRM <- readAntares(areas = "all", links = "all", clusters = "all", timeStep = "monthly")
resRW <- readAntares(areas = "all", links = "all", clusters = "all", timeStep = "weekly")

unlink(paste0(testStudy, "/output/testStydu/economy/mc-all"), recursive = TRUE)

#Test Areas
resAM$areas$time <- as.factor(resAM$areas$time )
resRA$areas$time <- as.factor(resRA$areas$time )

resA <- resAA$areas - resRA$areas
resD <- resAD$areas - resRD$areas
resH <- resAH$areas - resRH$areas
resM <- resAM$areas - resRM$areas
resW <- resAW$areas - resRW$areas
expect_equal(sum(resA>2 | resA < -2 , na.rm = TRUE) + 
  sum(resD>2 | resD < -2 , na.rm = TRUE) + 
  sum(resH>2 | resH < -2 , na.rm = TRUE) + 
  sum(resM>2 | resM < -2 , na.rm = TRUE) + 
  sum(resW>2 | resW < -2 , na.rm = TRUE), 0)

#Test links
resAM$links$time <- as.factor(resAM$links$time )
resRA$links$time <- as.factor(resRA$links$time )
resA <- resAA$links - resRA$links
resD <- resAD$links - resRD$links
resH <- resAH$links - resRH$links
resM <- resAM$links - resRM$links
resW <- resAW$links - resRW$links
expect_equal(sum(resA>2 | resA < -2 , na.rm = TRUE) + 
               sum(resD>2 | resD < -2 , na.rm = TRUE) + 
               sum(resH>2 | resH < -2 , na.rm = TRUE) + 
               sum(resM>2 | resM < -2 , na.rm = TRUE) + 
               sum(resW>2 | resW < -2 , na.rm = TRUE), 0)


#Test clusters
resAM$clusters$time <- as.factor(resAM$clusters$time )
resRA$clusters$time <- as.factor(resRA$clusters$time )
resA <- resAA$clusters - resRA$clusters
resD <- resAD$clusters - resRD$clusters
resH <- resAH$clusters - resRH$clusters
resM <- resAM$clusters - resRM$clusters
resW <- resAW$clusters - resRW$clusters
expect_equal(sum(resA>2 | resA < -2 , na.rm = TRUE) + 
               sum(resD>2 | resD < -2 , na.rm = TRUE) + 
               sum(resH>2 | resH < -2 , na.rm = TRUE) + 
               sum(resM>2 | resM < -2 , na.rm = TRUE) + 
               sum(resW>2 | resW < -2 , na.rm = TRUE), 0)







