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

# initial outputs
area_ini <- dataNoStrat_ini$areas
area_ini <- area_ini[with(area_ini, order(mcYear, timeId, area))]
links_ini <- dataNoStrat_ini$links
links_ini <- links_ini[with(links_ini, order(mcYear, timeId, link))]

# test results
area_test <- dataNoStrat_adq$areas
area_test$area <- as.character(area_test$area)
area_test <- area_test[with(area_test, order(mcYear, timeId, area)), ]
links_test <- dataNoStrat_adq$links
links_test <- links_test[with(links_test, order(mcYear, timeId, link)), ]
## temporaire!
area_test <- subset(area_test, select = -ipn)

# expected results
outNoStrat_exp <- readRDS(system.file("testdata/adq/General/studyNoStrat_adq.RDS", package = "antaresFlowbased"))
area_exp <- outNoStrat_exp$areas
area_exp <- area_exp[with(area_exp, order(mcYear, timeId, area)), ]
links_exp <- outNoStrat_exp$links
links_exp <- links_exp[with(links_exp, order(mcYear, timeId, link)), ]


# parameters
b_file <- fread(system.file("testdata/adq/antaresStudy/user/flowbased/second_member.txt", package = "antaresFlowbased"))
id_file <- fread(system.file("testdata/adq/antaresStudy/user/flowbased/ts.txt", package = "antaresFlowbased"))
case <- data.table(unique(cbind(mcYear = cwe_area$mcYear, timeId = cwe_area$timeId)))



describe("adqPatch", {
  
  it("compares test case results", {
    expect_true(all.equal(area_exp,area_test))
    expect_true(all.equal(links_exp,links_test))
  })
  
  
  it("checks UNSP ENRG is higher after adequacy patch is applied", {
    ## function
    isUnsuppliedHigher <- function(area, area_adq){
      temp1 <- subset(copy(area), select = c("mcYear", "timeId", "UNSP. ENRG"))
      temp1[,tot_ENS_ini:=sum(`UNSP. ENRG`), by=.(mcYear,timeId)]
      temp1 <- unique(temp1[,-"UNSP. ENRG"])
      
      temp2 <- subset(copy(area_adq), select = c("mcYear", "timeId", "UNSP. ENRG"))
      temp2[,tot_ENS_adq:=sum(`UNSP. ENRG`), by=.(mcYear,timeId)]
      temp2 <- unique(temp2[,-"UNSP. ENRG"])
      
      temp <- merge(temp1, temp2, by=c("mcYear","timeId"),all = TRUE)
      temp[,delta:=(tot_ENS_adq - tot_ENS_ini)]
      all(temp$delta >= 0)
    }
    
    expect_true(isUnsuppliedHigher(area = area_ini, area_adq = area_test))
  })
  
  
  it("checks the balance of power befaore and after adq patch", {
    ## function
    isEquivalentSolution <- function(area, area_ad){
      temp1 <- copy(area)
      temp1[,tot_bal_ini :=`DTG MRG`+BALANCE-`UNSP. ENRG`]
      temp2 <- copy(area_ad)
      temp2[,tot_bal_adq :=`DTG MRG`+BALANCE-`UNSP. ENRG`]
      temp <- merge(temp1,temp2, by=c("mcYear", "timeId", "area"))
      temp[,delta:=tot_bal_ini-tot_bal_adq]
      
      all(temp$delta==0)
    }
    
    expect_true(isEquivalentSolution(area = area_ini, area_ad = area_test))
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
