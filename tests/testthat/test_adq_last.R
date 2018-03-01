context("adq + strategic reserved")


test_that("adq strategic reserved", {
  
  aread <- system.file("testdata/adq/General/new_area.csv", package = "antaresFlowbased")
  if(aread == "")aread <- system.file("inst/testdata/adq/General/new_area.csv", package = "antaresFlowbased")
  linkd <- system.file("testdata/adq/General/new_links.csv", package = "antaresFlowbased")
  if(linkd == "")linkd <- system.file("inst/testdata/adq/General/new_links.csv", package = "antaresFlowbased")
  
  
  
  area <- fread(aread)
  link <- fread(linkd)
  dta <- list(areas = area, links = link)
  
  dta$areas <- as.antaresDataTable(dta$areas, timeStep = "hourly", type = "area", synthesis = FALSE)
  dta$links <- as.antaresDataTable(dta$links, timeStep = "hourly", type = "link", synthesis = FALSE)
  
  adqWhioutStratMrg <- suppressWarnings(.applyAdq(opts = opts3, dta, fb_opts = optsTMP))
  
  
  areaADQ <- fread(system.file("testdata/adq/General/adqpatch_area.csv", package = "antaresFlowbased"))
  linkADQ <- fread(system.file("testdata/adq/General/adqpatch_links.csv", package = "antaresFlowbased"))
  
  areaADQ <- as.antaresDataTable(areaADQ, timeStep = "hourly", type = "area", synthesis = FALSE)
  linkADQ <- as.antaresDataTable(linkADQ, timeStep = "hourly", type = "link", synthesis = FALSE)
  
  setkeyv(areaADQ, getIdCols(areaADQ))
  setkeyv(linkADQ, getIdCols(linkADQ))
  
  setkeyv(adqWhioutStratMrg$areas, getIdCols(adqWhioutStratMrg$areas))
  setkeyv(adqWhioutStratMrg$links, getIdCols(adqWhioutStratMrg$links))

  
  ##Test if area table is ok
  expect_true(identical(round(adqWhioutStratMrg$areas$BALANCE, 4), round(areaADQ$BALANCE, 4)))
  expect_true(identical(adqWhioutStratMrg$areas$`UNSP. ENRG`, areaADQ$`UNSP. ENRG`))
  expect_true(identical(adqWhioutStratMrg$areas$LOLD, areaADQ$LOLD))
  expect_true(identical(adqWhioutStratMrg$areas$`DTG MRG`, areaADQ$`DTG MRG`))
  
  ##Test link table
  expect_true(identical(adqWhioutStratMrg$links$`FLOW LIN.`, linkADQ$`FLOW LIN.`))
  
  
  
  ##Apply strat_reseverd
  
  
  
  
  
  with_mock(readAntares = function(...){
    beS <- system.file("testdata/adq/General/be_strategic.csv", package = "antaresFlowbased")
    if(beS == "")beS <-  system.file("inst/testdata/adq/General/be_strategic.csv", package = "antaresFlowbased")
    
    beStrat <- fread(beS, sep = ";")
    beStrat <- as.antaresDataTable(beStrat, timeStep = "hourly", type = "area", synthesis = FALSE)
    beStrat$time <- gsub("/", "-", beStrat$time)
    beStrat$time <- paste0(
      substr(beStrat$time, 7, 10), "-", 
      substr(beStrat$time, 4, 5), "-", 
      substr(beStrat$time, 1, 2), " ", 
      substr(beStrat$time, 12, 13), ":00:00"
    )
    
    beStrat
  },
  {
    adqStratBe <- suppressWarnings(.applyAdq(opts = opts3, dta, strategic_reserve_be = "toto", fb_opts = optsTMP))
    nostrat <- suppressWarnings(.applyAdq(opts = opts3, dta, fb_opts = optsTMP))
    
    stratMrgBe <- readAntares()
    
    
    
    #All reserved > 0
    expect_true(all(adqStratBe$areas$additionalSR >= 0))
    
    
    rowNotApply <- adqStratBe$areas[LOLD == 0, length(LOLD), by = c("time", "mcYear")][V1 == 4]
    out <- merge(rowNotApply,  adqStratBe$areas, by = c("time", "mcYear"))
    
    #No additional SR when adq not applied
    expect_true(all(out$additionalSR == 0))
    
    ##Area not concern
    rowSel <- adqStratBe$areas$area != "be"
    expect_true(all(adqStratBe$areas[rowSel]$additionalSR == 0))
    
    
    #BALANCE & DTG MRG unchanged
    expect_true(identical(adqStratBe$areas$BALANCE,  nostrat$areas$BALANCE))
    expect_true(identical(adqStratBe$areas$`DTG MRG`,  nostrat$areas$`DTG MRG`))
    
    
    
    RowStratM <- merge(adqStratBe$areas[mcYear != 589], stratMrgBe, by = c("area","time", "mcYear"))
    Notapp <- merge(nostrat$areas[mcYear != 589], stratMrgBe, by = c("area","time", "mcYear"))
    
    
    expect_true(all(ifelse((Notapp$`UNSP. ENRG` - RowStratM$`DTG MRG.y`)<0, 0, (Notapp$`UNSP. ENRG` - RowStratM$`DTG MRG.y`))-
                      RowStratM$`UNSP. ENRG` == 0))
    
    
    ##Area concern
    expect_true(sum(adqStratBe$areas[`UNSP. ENRG` == 0]$LOLD) == 0)
    expect_true(all(adqStratBe$areas[area == "be"]$additionalSR  -(nostrat$areas[area == "be"]$`UNSP. ENRG`-  adqStratBe$areas[area == "be"]$`UNSP. ENRG`) == 0))
    out <- merge(adqStratBe$areas[area == "be"], stratMrgBe, by = c("mcYear", "time"))
    expect_true(all(out$additionalSR <= out$`DTG MRG.y`))
    
    
  })
  
  
  
  
  with_mock(readAntares = function(...){
    beS <- system.file("testdata/adq/General/de_strategic.csv", package = "antaresFlowbased")
    if(beS == "")beS <-  system.file("inst/testdata/adq/General/de_strategic.csv", package = "antaresFlowbased")
    
    beStrat <- fread(beS, sep = ";")
    beStrat <- as.antaresDataTable(beStrat, timeStep = "hourly", type = "area", synthesis = FALSE)
    beStrat$time <- gsub("/", "-", beStrat$time)
    beStrat$time <- paste0(
      substr(beStrat$time, 7, 10), "-", 
      substr(beStrat$time, 4, 5), "-", 
      substr(beStrat$time, 1, 2), " ", 
      substr(beStrat$time, 12, 13), ":00:00"
    )
    
    beStrat
  },
  {
    adqStratBe <- suppressWarnings(.applyAdq(opts = opts3, dta, strategic_reserve_de = "toto", fb_opts = optsTMP))
    nostrat <- suppressWarnings(.applyAdq(opts = opts3, dta, fb_opts = optsTMP))
    
    stratMrgBe <- readAntares()
    
    #All reserved > 0
    expect_true(all(adqStratBe$areas$additionalSR >= 0))
    
    
    rowNotApply <- adqStratBe$areas[LOLD == 0, length(LOLD), by = c("time", "mcYear")][V1 == 4]
    out <- merge(rowNotApply,  adqStratBe$areas, by = c("time", "mcYear"))
    
    #No additional SR when adq not applied
    expect_true(all(out$additionalSR == 0))
    
    ##Area not concern
    rowSel <- adqStratBe$areas$area != "de"
    expect_true(all(adqStratBe$areas[rowSel]$additionalSR == 0))
    
    
    #BALANCE & DTG MRG unchanged
    expect_true(identical(adqStratBe$areas$BALANCE,  nostrat$areas$BALANCE))
    expect_true(identical(adqStratBe$areas$`DTG MRG`,  nostrat$areas$`DTG MRG`))
    
    
    
    RowStratM <- merge(adqStratBe$areas[mcYear != 589], stratMrgBe, by = c("area","time", "mcYear"))
    Notapp <- merge(nostrat$areas[mcYear != 589], stratMrgBe, by = c("area","time", "mcYear"))
    
    
    expect_true(all(ifelse((Notapp$`UNSP. ENRG` - RowStratM$`DTG MRG.y`)<0, 0, (Notapp$`UNSP. ENRG` - RowStratM$`DTG MRG.y`))-
                      RowStratM$`UNSP. ENRG` == 0))
    
    
    ##Area concern
    expect_true(sum(adqStratBe$areas[`UNSP. ENRG` == 0]$LOLD) == 0)
    expect_true(all(adqStratBe$areas[area == "de"]$additionalSR  -(nostrat$areas[area == "de"]$`UNSP. ENRG`-  adqStratBe$areas[area == "de"]$`UNSP. ENRG`) == 0))
    out <- merge(adqStratBe$areas[area == "de"], stratMrgBe, by = c("mcYear", "time"))
    expect_true(all(out$additionalSR <= out$`DTG MRG.y`))
    
    
  }
  
  
  )
  
  
})
