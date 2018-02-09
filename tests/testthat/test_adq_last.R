context("adq")


test_that("adq strategic reserved", {
  area <- fread(system.file("ADQSTRATMRG/new_area.csv", package = "antaresFlowbased"))
  link <- fread(system.file("ADQSTRATMRG/new_links.csv", package = "antaresFlowbased"))
  dta <- list(areas = area, links = link)
  opts <- list()
  opts$studyPath <- system.file("testdata/adq/antaresStudy/user/flowbased/ts.txt", package = "antaresFlowbased")
  opts$studyPath <- gsub("/user/flowbased/ts.txt","" , opts)
  dta$areas <- as.antaresDataTable(dta$areas, timeStep = "hourly", type = "area", synthesis = FALSE)
  dta$links <- as.antaresDataTable(dta$links, timeStep = "hourly", type = "link", synthesis = FALSE)
  
  adqWhioutStratMrg <- suppressWarnings(.applyAdq(opts = opts, dta))
  
  
  areaADQ <- fread(system.file("ADQSTRATMRG/adqpatch_area.csv", package = "antaresFlowbased"))
  linkADQ <- fread(system.file("ADQSTRATMRG/adqpatch_links.csv", package = "antaresFlowbased"))
  
  
  
  
  
  areaADQ <- as.antaresDataTable(areaADQ, timeStep = "hourly", type = "area", synthesis = FALSE)
  linkADQ <- as.antaresDataTable(linkADQ, timeStep = "hourly", type = "link", synthesis = FALSE)
  
  setkeyv(areaADQ, getIdCols(areaADQ))
  setkeyv(adqWhioutStratMrg$areas, getIdCols(adqWhioutStratMrg$areas))
  
  setkeyv(adqWhioutStratMrg$links, getIdCols(adqWhioutStratMrg$links))
  
  setkeyv(adqWhioutStratMrg$links, getIdCols(adqWhioutStratMrg$links))
  
  
  ##Test if area table is ok
  expect_true(identical(adqWhioutStratMrg$areas$BALANCE, areaADQ$BALANCE))
  expect_true(identical(adqWhioutStratMrg$areas$`UNSP. ENRG`, areaADQ$`UNSP. ENRG`))
  expect_true(identical(adqWhioutStratMrg$areas$LOLD, areaADQ$LOLD))
  expect_true(identical(adqWhioutStratMrg$areas$`DTG MRG`, areaADQ$`DTG MRG`))
})


##Same for link


# 
# library(ROI)
# library(data.table)
# library(antaresRead)
# opts <- list()
# opts$studyPath <- system.file("testdata/antaresInput/user/flowbased/ts.txt", package = "antaresFlowbased")
# opts$studyPath <- gsub("/user/flowbased/ts.txt","" , opts)
# 
# 
# dta <- readRDS(system.file("testdata/adq/adqReserv/adq.RDS", package = "antaresFlowbased"))
# 
# 
# stategicBE <- readRDS(system.file("testdata/adq/adqReserv/stategicBE.RDS", package = "antaresFlowbased"))
# stategicDE <- readRDS(system.file("testdata/adq/adqReserv/stategicDE.RDS", package = "antaresFlowbased"))
# 
# dta <- .applyAdq(opts = opts, dta,
#                  stategicBE = stategicBE,
#                  stategicDE = stategicDE)
# 
# # fwrite(dta$areas, "D:/Users/titorobe/Desktop/Antares/antaresFlowbased/inst/testdata/adq/adqReserv/outputAreas.csv", sep = ";")
# # fwrite(dta$links, "D:/Users/titorobe/Desktop/Antares/antaresFlowbased/inst/testdata/adq/adqReserv/outputLinks.csv", sep = ";")
# 
# 
# outArea <- fread(system.file("testdata/adq/adqReserv/outputAreas.csv", package = "antaresFlowbased"))
# outLink <- fread(system.file("testdata/adq/adqReserv/outputLinks.csv", package = "antaresFlowbased"))
# 
# 
# areaAll <- merge(outArea, dta$areas, by = c("mcYear", "timeId", "area"))
# linkAll <- merge(outLink, dta$links, by = c("mcYear", "timeId", "link"))
#
# expect_equal(all(all(areaAll$BALANCE.x == areaAll$BALANCE.y),
#                  all(areaAll$`UNSP. ENRG.x` == areaAll$`UNSP. ENRG.y`),
#                  all(areaAll$LOLD.x == areaAll$LOLD.y),
#                  all(areaAll$`DTG MRG.x` == areaAll$`DTG MRG.y`),
#                  all(areaAll$strategicMargin.x == areaAll$strategicMargin.y),
#                  all(linkAll$`FLOW LIN..x` == linkAll$`FLOW LIN..y`)), TRUE)

# rm(list=ls())
