context("adq")

library(ROI)
library(data.table)
library(antaresRead)



# opts <- list()
# opts$studyPath <- system.file("testdata/antaresInput/user/flowbased/ts.txt", package = "antaresFlowbased")
# opts$studyPath <- gsub("/user/flowbased/ts.txt","" , opts)
# 
# 
# dta2 <- readRDS(system.file("testdata/adq/adqSimple/adq.RDS", package = "antaresFlowbased"))
# dta <- .applyAdq(opts = opts, dta2)
# 
# outArea <- fread(system.file("testdata/adq/adqSimple/outputAreas.csv", package = "antaresFlowbased"))
# outLink <- fread(system.file("testdata/adq/adqSimple/outputLinks.csv", package = "antaresFlowbased"))
# 
# 
# areaAll <- merge(outArea, dta$areas, by = c("mcYear", "timeId", "area"))
# linkAll <- merge(outLink, dta$links, by = c("mcYear", "timeId", "link"))
# 

# expect_equal(all(all(areaAll$BALANCE.x == areaAll$BALANCE.y),
# all(areaAll$`UNSP. ENRG.x` == areaAll$`UNSP. ENRG.y`),
# all(areaAll$LOLD.x == areaAll$LOLD.y),
# all(areaAll$`DTG MRG.x` == areaAll$`DTG MRG.y`),
# all(linkAll$`FLOW LIN..x` == linkAll$`FLOW LIN..y`)), TRUE)
