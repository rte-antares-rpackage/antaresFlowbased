temp_dir <- tempdir()

library(testthat)
library(antaresFlowbased)
library(antaresRead)
library(data.table)
library(ROI)

#Untar and read study
testStudy2 <- system.file("testdata",package = "antaresFlowbased")
if(testStudy2 == "")testStudy2 <- system.file("inst/testdata",package = "antaresFlowbased")

# temp_dir <- tempdir()
if (Sys.info()['sysname'] == "Windows") {
  untar(file.path(testStudy2, "ex_test.tgz"), exdir = temp_dir, 
        extras = "--force-local")
} else {
  untar(file.path(testStudy2, "ex_test.tgz"), exdir = temp_dir)
}
testStudy2 <- file.path(temp_dir, "ex_test")
opts2 <- antaresRead::setSimulationPath(testStudy2)
opts <- opts2
testStudy <- testStudy2
assign("opts2", opts2, envir = globalenv())
assign("opts", opts, envir = globalenv())
assign("testStudy2", testStudy2, envir = globalenv())
assign("testStudy", testStudy, envir = globalenv())



# 
# tar(tarfile = "ex_test.tgz",files = "ex_test",
#     compression = "gzip")

###Init adq
opts3 <- list()
opts3$studyPath <- system.file("testdata/adq/antaresStudy37", package = "antaresFlowbased")
if(opts3$studyPath== "") opts3$studyPath <- system.file("inst/testdata/adq/antaresStudy37", package = "antaresFlowbased")

# launch adq patch
rdP <- system.file("testdata/adq/General/studyNoStrat_ini.RDS", package = "antaresFlowbased")
if(rdP == "")rdP <- system.file("inst/testdata/adq/General/studyNoStrat_ini.RDS", package = "antaresFlowbased")
dataNoStrat_ini <- readRDS(rdP)
dataNoStrat_ini2 <- data.table::copy(dataNoStrat_ini)
dataNoStrat_adq <- suppressWarnings(.applyAdq(opts = opts3, dataNoStrat_ini2))



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
fid <- system.file("testdata/adq/General/studyNoStrat_adq.RDS", package = "antaresFlowbased")
if(fid == "")fid <-  system.file("inst/testdata/adq/General/studyNoStrat_adq.RDS", package = "antaresFlowbased")
outNoStrat_exp <- readRDS(fid)
area_exp <- outNoStrat_exp$areas
area_exp <- area_exp[with(area_exp, order(mcYear, timeId, area)), ]
links_exp <- outNoStrat_exp$links
links_exp <- links_exp[with(links_exp, order(mcYear, timeId, link)), ]


# parameters

b_file <- system.file("testdata/adq/antaresStudy/user/flowbased/second_member.txt", package = "antaresFlowbased")
if(b_file == "")b_file <- system.file("inst/testdata/adq/antaresStudy/user/flowbased/second_member.txt", package = "antaresFlowbased")
b_file <- data.table::fread(b_file)


id_file <- system.file("testdata/adq/antaresStudy/user/flowbased/ts.txt", package = "antaresFlowbased")
if(id_file == "")id_file <- system.file("inst/testdata/adq/antaresStudy/user/flowbased/ts.txt", package = "antaresFlowbased")


id_file <- data.table::fread(id_file)
case <- data.table::data.table(unique(cbind(mcYear = area_exp$mcYear, timeId = area_exp$timeId)))
caseNoWarning <- case[mcYear!=589]








## Function used in test -> ADQ
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
  
  defT <- data.table::data.table(def_be, def_de, def_fr, def_nl)
  
  
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
  
  PN <- data.table::data.table(PN_be, PN_de, PN_fr, PN_nl)
  
  return(PN)
}



# function initial net position for function belong to domain
giveIpn <- function(links){
  links <- data.table::dcast(links, time + mcYear~link, value.var = c("FLOW LIN."))
  links[, be :=  `be - de` + `be - fr` + `be - nl`]
  links[, de := - `be - de` + `de - fr` + `de - nl`]
  links[, fr :=  -`be - fr` - `de - fr`]
  links[, nl :=  -`be - nl` - `de - nl`]
  links
  links <- links[, .SD, .SDcols = c("time", "mcYear","be","de" ,"fr","nl")]
  links <- data.table::melt(links, id = 1:2)
  data.table::setnames(links, c("variable","value"), c("area","ipn"))
  
  ipn <- data.table::dcast(links, time + mcYear~area, value.var = c("ipn"))
  ipn
}

# test belonging to used flow-based domain
belongToDomain <- function(links, path){
  #calculate Net Position within CWE zone
  ipn <- giveIpn(links = links)
  # flow based files
  scn <- system.file("/testdata/adq/antaresStudy/user/flowbased/scenario.txt",  package = "antaresFlowbased")
  if(scn == "")scn <- system.file("inst/testdata/adq/antaresStudy/user/flowbased/scenario.txt",  package = "antaresFlowbased")
  scenario <- data.table::fread(scn)
  
  dfb <- system.file("testdata/adq/antaresStudy/domainesFB.RDS", package = "antaresFlowbased")
  if(dfb == "")dfb <- system.file("inst/testdata/adq/antaresStudy/domainesFB.RDS", package = "antaresFlowbased")
  
  b36p <- readRDS(dfb)$outFlowBased[[1]]$face
  b36 <- as.matrix(b36p)[,1:3]
  dateTS <- id_file$Date
  dateTS <- substr(dateTS, 6,10)
  res <- sapply(1:nrow(ipn), function(X)
  {
    linksTp <- ipn[X]
    senar <- scenario[linksTp$mcYear]$simulation
    mod <- substr(linksTp$time, 6,10)
    
    dayType <- id_file[[as.character(senar)]][dateTS == mod]
    Hour <- data.table::hour(linksTp$time) + 1
    b <- data.table::data.table(1:length(b_file[Id_day == dayType & Id_hour == Hour]$vect_b),
                                b_file[Id_day == dayType & Id_hour == Hour]$vect_b)
    
    all(as.matrix(linksTp[, .SD, .SDcols = c("be", "de", "fr")])%*%t(b36) < b$V2 + 2)
  })
  
  nrow(ipn[!res])==0
}


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


isDTGHigher <- function(area, area_adq){
  temp1 <- subset(data.table::copy(area), select = c("mcYear", "timeId", "DTG MRG"))
  temp1[,tot_MRG_ini:=sum(`DTG MRG`), by=.(mcYear,timeId)]
  temp1 <- unique(temp1[,-"DTG MRG"])
  
  temp2 <- subset(data.table::copy(area_adq), select = c("mcYear", "timeId", "DTG MRG"))
  temp2[,tot_MRG_adq:=sum(`DTG MRG`), by=.(mcYear,timeId)]
  temp2 <- unique(temp2[,-"DTG MRG"])
  
  temp <- merge(temp1, temp2, by=c("mcYear","timeId"),all = TRUE)
  temp[,delta:=(tot_MRG_adq - tot_MRG_ini)]
  all(temp$delta >= -0.00000000001)
}

## function
isEquivalentSolution <- function(area, area_ad){
  temp1 <- data.table::copy(area)
  temp1[,tot_bal_ini :=`DTG MRG`+BALANCE-`UNSP. ENRG`]
  temp2 <- data.table::copy(area_ad)
  temp2[,tot_bal_adq :=`DTG MRG`+BALANCE-`UNSP. ENRG`]
  temp <-merge(temp1,temp2, by=c("mcYear", "timeId", "area"))
  temp[,delta:=tot_bal_ini-tot_bal_adq]
  
  all(temp$delta==0)
}

## function
isUnsuppliedHigher <- function(area, area_adq){
  temp1 <- subset(data.table::copy(area), select = c("mcYear", "timeId", "UNSP. ENRG"))
  temp1[,tot_ENS_ini:=sum(`UNSP. ENRG`), by=.(mcYear,timeId)]
  temp1 <- unique(temp1[,-"UNSP. ENRG"])
  
  temp2 <- subset(data.table::copy(area_adq), select = c("mcYear", "timeId", "UNSP. ENRG"))
  temp2[,tot_ENS_adq:=sum(`UNSP. ENRG`), by=.(mcYear,timeId)]
  temp2 <- unique(temp2[,-"UNSP. ENRG"])
  
  temp <- merge(temp1, temp2, by=c("mcYear","timeId"),all = TRUE)
  temp[,delta:=(tot_ENS_adq - tot_ENS_ini)]
  all(temp$delta >= 0)
}