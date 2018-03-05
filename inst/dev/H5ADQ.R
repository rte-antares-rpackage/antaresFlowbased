
opts <- antaresRead::setSimulationPath("D:/Users/titorobe/Desktop/Antares/antaresFlowbased",1)
setAlias("adqPatchBefore", "Alias for adqPatch", c("LOLD", "UNSP. ENRG", "DTG MRG", "UNSP. ENRG", "BALANCE", "FLOW LIN.", "areas", "links"))

ADQarea <- c("fr", "be", "de", "nl")
ADQlinks <-  c("be - de","be - fr","be - nl","de - fr","de - nl")
strategic_reserve_be <- NULL
strategic_reserve_de <- NULL
keepOldColumns <- FALSE
areasList <- getAreas(opts = opts)
linksList <- getLinks(opts = opts)
fb_opts = antaresRead::setSimulationPath("D:/Users/titorobe/Desktop/antaresStudy", 2)

sapply(opts$mcYears, function(MC){
  if(MC == opts$mcYears[1]){
    writeStruct <- TRUE
  }else{
    writeStruct <- FALSE
  }
  dta <- suppressWarnings(readAntares(areas = ADQarea, 
                     links =  ADQlinks, mcYears = MC,
                     select = c("adqPatchBefore"), opts = opts))
  
  dta <- suppressMessages(suppressWarnings(.applyAdq(opts = opts, dta = dta,
                   fb_opts = fb_opts, strategic_reserve_be = strategic_reserve_be,
                   strategic_reserve_de = strategic_reserve_de, mcYears = MC, keepOldColumns = keepOldColumns)))
  
  dta2 <- suppressWarnings(readAntares(areas = areasList[!areasList%in%ADQarea],
                      links = linksList[!linksList%in%ADQlinks],
                      select = c("adqPatchBefore"),opts = opts, mcYears = MC))
  
  .giveNewName(dta2, keepOldColumns = FALSE, strategic_reserve_be, strategic_reserve_de)
  areaEnd <- rbindlist(list(dta$areas, dta2$areas))
  areaEnd$area <- as.character(areaEnd$area)
  setorderv(areaEnd, c("area", "timeId"))
  
  linkEnd <- rbindlist(list(dta$links, dta2$links))
  linkEnd$link <- as.character(linkEnd$link)
  setorderv(linkEnd, c("link", "timeId"))
  
  outToWrite <- list()
  outToWrite$areas = as.matrix(areaEnd[, .SD, .SDcols = c("BALANCE_ADQPatch",
                                                          "UNSP. ENRG_ADQPatch",
                                                          "LOLD_ADQPatch",
                                                          "DTG MRG_ADQPatch")])
  outToWrite$links = as.matrix(linkEnd[, .SD, .SDcols = c("FLOW LIN._ADQPatch")])
  
  antaresProcessing:::.writeAllTables(timeStep = "hourly",
                                      mcY = "mcInd",
                                      path = opts$h5path,
                                      outToWrite = outToWrite ,
                                      areas = TRUE,
                                      links = TRUE,
                                      clusters = FALSE,
                                      districts = FALSE,
                                      mcYear = MC, writeStruct = writeStruct)
  NULL
})

setAlias("adqPatch", "Alias for adqPatch", c("LOLD_ADQPatch", "UNSP. ENRG_ADQPatch", "DTG MRG_ADQPatch",
                                             "UNSP. ENRG_ADQPatch", "BALANCE_ADQPatch", "FLOW LIN._ADQPatch",
                                             "areas", "links"))

readAntares(select = "adqPatch", opts = opts, mcYears = 1)







dta
