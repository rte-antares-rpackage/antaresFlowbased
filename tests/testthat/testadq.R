context("adq")

library(ROI)
library(data.table)
library(antaresRead)

library(testthat)

# get Antares' flow-based parametes
opts <- list()
opts$studyPath <- system.file("testdata/adq/antaresStudy/user/flowbased/ts.txt", package = "antaresFlowbased")
opts$studyPath <- gsub("/user/flowbased/ts.txt","" , opts)
# opts

# launch adq patch
dataNoStrat_ini <- readRDS(system.file("testdata/adq/General/studyNoStrat_ini.RDS", package = "antaresFlowbased"))
dataNoStrat_adq <- .applyAdq(opts = opts, dataNoStrat_ini)

area_test <- dataNoStrat_adq$areas
area_test$area <- as.character(area_test$area)
area_test <- area_test[with(area_test, order(mcYear, timeId, area)), ]
## temporaire!
area_test <- subset(area_test, select = -ipn)

links_test <- dataNoStrat_adq$links
links_test <- links_test[with(links_test, order(mcYear, timeId, link)), ]

# Expected results
outNoStrat_exp <- readRDS(system.file("testdata/adq/General/studyNoStrat_adq.RDS", package = "antaresFlowbased"))
area_exp <- outNoStrat_exp$areas
area_exp <- area_exp[with(area_exp, order(mcYear, timeId, area)), ]
links_exp <- outNoStrat_exp$links
links_exp <- links_exp[with(links_exp, order(mcYear, timeId, link)), ]



describe("adqPatch", {
  it("compares test case results", {
    expect_true(all.equal(area_exp,area_test))
    expect_true(all.equal(links_exp,links_test))
  })
  
  
})









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
