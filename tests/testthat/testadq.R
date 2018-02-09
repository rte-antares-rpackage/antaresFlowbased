context("function adqPatch")

library(data.table)


test_that("checks warning when strategic reserves in the country", {
  expect_warning(.applyAdq(opts = opts3, dataNoStrat_ini),
                 "mcYear : 589 timeId : 2017-12-07 17:00:00 de has LOLD = 1 but DTG MRG>0, adequacy patch not applied")
  expect_warning(.applyAdq(opts = opts3, dataNoStrat_ini),
                 "mcYear : 589 timeId : 2017-12-11 17:00:00 de has LOLD = 1 but DTG MRG>0, adequacy patch not applied")
})


test_that("compares test case results", {
   expect_true(all.equal(area_exp,area_test))
   expect_true(all.equal(links_exp,links_test))
})


test_that("checks UNSP ENRG is higher after adequacy patch is applied", {

  
  expect_true(isUnsuppliedHigher(area = area_ini, area_adq = area_test))
})


test_that("checks the balance of power before and after adq patch", {

  expect_true(isEquivalentSolution(area = area_ini, area_ad = area_test))
})


test_that("checks DTG MRG higher after adq patch", {

  
  expect_true(isDTGHigher(area = area_ini, area_adq = area_test))
  
  
})


test_that("countries with LOLD do not have DTG MRG left", {
  
  expect_true(all(area_test[LOLD==1 & mcYear!=589]$`DTG MRG`==0))
  
})


test_that("checks the final NP is proportionnal to the initial unsupplied energy", {
  
  #calculation unsupplied energy and net position
  iniENS <- t(sapply(1:nrow(case), function(i){
    get_def(areas = area_ini[mcYear == case$mcYear[i] & timeId == case$timeId[i]],
            links = links_ini[mcYear == case$mcYear[i] & timeId == case$timeId[i]],
            year = case$mcYear[i],
            timeI = case$timeId[i])
  }))
  iniENS <- as.data.table(cbind(case,iniENS))
  
  finalNP <- t(sapply(1:nrow(case), function(i){
    get_PN(links = links_test[mcYear == case$mcYear[i] & timeId == case$timeId[i]])
  }))
  finalNP <- as.data.table(cbind(case,finalNP))
  
  fdata <- merge(iniENS,finalNP)
  
  isProp <- sapply(2:nrow(fdata[mcYear!=589]), function(i){
    isProportionnalENS(i=i, f=fdata[mcYear!=589])
  })
  
  expect_true(all(isProp))
  
})


test_that("checks no export while unsupplied energy in the country", {
  
  #lold per area
  flold <- subset(area_test, select=c("mcYear", "timeId", "area", "LOLD"))
  flold <- dcast(flold, mcYear+timeId ~ area)
  
  #final net position
  finalNP <- t(sapply(1:nrow(case), function(i){
    get_PN(links = links_test[mcYear == case$mcYear[i] & timeId == case$timeId[i]])
  }))
  finalNP <- data.table::as.data.table(cbind(case,finalNP))
  
  #merging of net position and lold
  temp <- merge(flold, finalNP)
  temp <- subset(temp, select = -c(1,2))
  temp <- melt(temp, measure.vars = c("be","de","fr","nl"), variable.name = "area", value.name = "LOLD")
  
  expect_true(all((temp[LOLD == 1 & area == "be"]$PN_be)<=0))
  expect_true(all((temp[LOLD == 1 & area == "de"]$PN_de)<=0))
  expect_true(all((temp[LOLD == 1 & area == "fr"]$PN_fr)<=0))
  expect_true(all((temp[LOLD == 1 & area == "nl"]$PN_nl)<=0))
  
})


test_that("All final points belong to their flow-based domains", {

  expect_true(belongToDomain(links = links_exp, path = opts3$studyPath))
  
})

