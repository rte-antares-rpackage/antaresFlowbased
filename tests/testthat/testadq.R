context("function adqPatch")

library(ROI)
library(data.table)
library(antaresRead)

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
# area_test <- subset(area_test, select = -ipn)

# expected results
outNoStrat_exp <- readRDS(system.file("testdata/adq/General/studyNoStrat_adq.RDS", package = "antaresFlowbased"))
area_exp <- outNoStrat_exp$areas
area_exp <- area_exp[with(area_exp, order(mcYear, timeId, area)), ]
links_exp <- outNoStrat_exp$links
links_exp <- links_exp[with(links_exp, order(mcYear, timeId, link)), ]


# parameters
b_file <- fread(system.file("testdata/adq/antaresStudy/user/flowbased/second_member.txt", package = "antaresFlowbased"))
id_file <- fread(system.file("testdata/adq/antaresStudy/user/flowbased/ts.txt", package = "antaresFlowbased"))
case <- data.table(unique(cbind(mcYear = area_exp$mcYear, timeId = area_exp$timeId)))
caseNoWarning <- case[mcYear!=589]


# functions unsupplied energy and net position
get_def <- function(areas, links, year, timeI){
  # 
  # #select data
  # areas <- areas[mcYear == year & timeId == timeI]
  # links <- links[mcYear == year & timeId == timeI]
  
  #def = max(ENS - MRG - (exp-imp),0)
  def_be <- max(areas[area=="be"]$`UNSP. ENRG` 
                - areas[area=="be"]$`DTG MRG`
                - links[link=="be - de"]$`FLOW LIN.`
                - links[link=="be - fr"]$`FLOW LIN.`
                - links[link=="be - nl"]$`FLOW LIN.`, 0)
  
  def_de <- max(areas[area=="de"]$`UNSP. ENRG` 
                - areas[area=="de"]$`DTG MRG`
                - links[link=="de - fr"]$`FLOW LIN.`
                - links[link=="de - nl"]$`FLOW LIN.`
                - (-links[link=="be - de"]$`FLOW LIN.`), 0)
  
  def_fr <- max(areas[area=="fr"]$`UNSP. ENRG` 
                - areas[area=="fr"]$`DTG MRG`
                - (-links[link=="be - fr"]$`FLOW LIN.`)
                - (-links[link=="de - fr"]$`FLOW LIN.`), 0)
  
  def_nl <- max(areas[area=="nl"]$`UNSP. ENRG` 
                - areas[area=="nl"]$`DTG MRG`
                - (-links[link=="be - nl"]$`FLOW LIN.`)
                - (-links[link=="de - nl"]$`FLOW LIN.`), 0)
  
  defT <- data.table(def_be, def_de, def_fr, def_nl)
  
  
  return(defT)
}
get_PN <- function(links){
  
  PN_be <- (links[link=="be - de"]$`FLOW LIN.` 
            + links[link=="be - fr"]$`FLOW LIN.`
            + links[link=="be - nl"]$`FLOW LIN.`)
  
  PN_de <- (links[link=="de - fr"]$`FLOW LIN.`
            + links[link=="de - nl"]$`FLOW LIN.`
            -links[link=="be - de"]$`FLOW LIN.`)
  
  PN_fr <- (- links[link=="be - fr"]$`FLOW LIN.`
            - links[link=="de - fr"]$`FLOW LIN.`)
  
  PN_nl <- (-links[link=="be - nl"]$`FLOW LIN.`
            -links[link=="de - nl"]$`FLOW LIN.`)
  
  PN <- data.table(PN_be, PN_de, PN_fr, PN_nl)
  
  return(PN)
}



test_that("checks warning when strategic reserves in the country", {
  expect_warning(.applyAdq(opts = opts, dataNoStrat_ini),
                 "mcYear : 589 timeId : 2017-12-07 17:00:00 de has LOLD = 1 but DTG MRG>0, adequacy patch not applied")
  expect_warning(.applyAdq(opts = opts, dataNoStrat_ini),
                 "mcYear : 589 timeId : 2017-12-11 17:00:00 de has LOLD = 1 but DTG MRG>0, adequacy patch not applied")
})


test_that("compares test case results", {
   expect_true(all.equal(area_exp,area_test))
   expect_true(all.equal(links_exp,links_test))
})


test_that("checks UNSP ENRG is higher after adequacy patch is applied", {
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


test_that("checks the balance of power before and after adq patch", {
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


test_that("checks DTG MRG higher after adq patch", {
  
  isDTGHigher <- function(area, area_adq){
    temp1 <- subset(copy(area), select = c("mcYear", "timeId", "DTG MRG"))
    temp1[,tot_MRG_ini:=sum(`DTG MRG`), by=.(mcYear,timeId)]
    temp1 <- unique(temp1[,-"DTG MRG"])
    
    temp2 <- subset(copy(area_adq), select = c("mcYear", "timeId", "DTG MRG"))
    temp2[,tot_MRG_adq:=sum(`DTG MRG`), by=.(mcYear,timeId)]
    temp2 <- unique(temp2[,-"DTG MRG"])
    
    temp <- merge(temp1, temp2, by=c("mcYear","timeId"),all = TRUE)
    temp[,delta:=(tot_MRG_adq - tot_MRG_ini)]
    all(temp$delta >= 0)
  }
  
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
  
  #function proportionnality ENS to final import
  isProportionnalENS <- function(i,f){
    if (f$def_be[[i]]>0){u_be <- as.numeric(abs(f$PN_be[[i]])/f$def_be[[i]])
    }else {u_be <- NA}
    if (f$def_de[[i]]>0){u_de <- abs(f$PN_de[[i]])/f$def_de[[i]]
    } else {u_de <- NA}
    if (f$def_fr[[i]]>0){u_fr <- abs(f$PN_fr[[i]])/f$def_fr[[i]]
    }else {u_fr <- NA}
    if (f$def_nl[[i]]>0){u_nl <- abs(f$PN_nl[[i]])/f$def_nl[[i]]
    }else {u_nl <- NA}
    u <- c(u_be, u_de, u_fr, u_nl)
    all(max(abs(u-mean(u, na.rm = TRUE)), na.rm = TRUE)<0.01, na.rm = TRUE)
  }
  
  
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
  finalNP <- as.data.table(cbind(case,finalNP))
  
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
  
  # function initial net position for function belong to domain
  giveIpn <- function(links){
    links <- dcast(links, time + mcYear~link, value.var = c("FLOW LIN."))
    links[, be :=  `be - de` + `be - fr` + `be - nl`]
    links[, de := - `be - de` + `de - fr` + `de - nl`]
    links[, fr :=  -`be - fr` - `de - fr`]
    links[, nl :=  -`be - nl` - `de - nl`]
    links
    links <- links[, .SD, .SDcols = c("time", "mcYear","be","de" ,"fr","nl")]
    links <- melt(links, id = 1:2)
    setnames(links, c("variable","value"), c("area","ipn"))
    
    ipn <- dcast(links, time + mcYear~area, value.var = c("ipn"))
    ipn
  }
  
  # test belonging to used flow-based domain
  belongToDomain <- function(links, path){
    #calculate Net Position within CWE zone
    ipn <- giveIpn(links = links)
    # flow based files
    scenario <- fread(paste0(path, "/user/flowbased/scenario.txt"))
    b36p <- readRDS(system.file("testdata/adq/antaresStudy/domainesFB.RDS", package = "antaresFlowbased"))$outFlowBased[[1]]$face
    b36 <- as.matrix(b36p)[,1:3]
    dateTS <- id_file$Date
    dateTS <- substr(dateTS, 6,10)
    res <- sapply(1:nrow(ipn), function(X)
    {
      linksTp <- ipn[X]
      senar <- scenario[linksTp$mcYear]$simulation
      mod <- substr(linksTp$time, 6,10)
      
      dayType <- id_file[[as.character(senar)]][dateTS == mod]
      Hour <- hour(linksTp$time) + 1
      b <- data.table(1:length(b_file[Id_day == dayType & Id_hour == Hour]$vect_b),
                      b_file[Id_day == dayType & Id_hour == Hour]$vect_b)
      
      all(as.matrix(linksTp[, .SD, .SDcols = c("be", "de", "fr")])%*%t(b36) < b$V2 + 2)
    })
    
    nrow(ipn[!res])==0
  }
  
  expect_true(belongToDomain(links = links_exp, path = opts$studyPath))
  
})

