# 
# study <- "D:/Users/titorobe/Desktop/antaresStudy"
# 
# opts <- antaresRead::setSimulationPath("D:/Users/titorobe/Desktop/antaresStudy", -1)
# dta <- antaresRead::readAntares(areas = c("fr", "be", "de", "nl"),
#                                 links = c("be - de","be - fr","be - nl","de - fr","de - nl"), mcYears = 1:10,
#                                 select = c("LOLD", "UNSP. ENRG", "DTG MRG", "UNSP. ENRG", "BALANCE", "FLOW LIN."), opts = opts)
# 
# 
# 
# countTryList <- toupper(unique(dta$areas$area))
# dayTyList <- unique(readRDS(paste0(opts$studyPath, "/user/flowbased/domainesFB.RDS"))$dayType)
# rangeDate <- range(dta$areas$time)
# rangeDate <- round(rangeDate, "day")
