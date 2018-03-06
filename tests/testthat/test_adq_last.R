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
  
  adqWhioutStratMrg <- suppressWarnings(.applyAdq(opts = opts3, dta, fb_opts = optsTMP, keepOldColumns = FALSE))
  adqWhioutStratMrg <- restaureOldName(adqWhioutStratMrg)
  
  areaADQ_64b <- fread(system.file("testdata/adq/General/adqpatch_area_64b.csv", package = "antaresFlowbased"))
  linkADQ_64b <- fread(system.file("testdata/adq/General/adqpatch_links_64b.csv", package = "antaresFlowbased"))
  
  areaADQ_64b <- as.antaresDataTable(areaADQ_64b, timeStep = "hourly", type = "area", synthesis = FALSE)
  linkADQ_64b <- as.antaresDataTable(linkADQ_64b, timeStep = "hourly", type = "link", synthesis = FALSE)
  
  setkeyv(areaADQ_64b, getIdCols(areaADQ_64b))
  setkeyv(linkADQ_64b, getIdCols(linkADQ_64b))
  
  areaADQ_32b <- fread(system.file("testdata/adq/General/adqpatch_area_32b.csv", package = "antaresFlowbased"))
  linkADQ_32b <- fread(system.file("testdata/adq/General/adqpatch_links_32b.csv", package = "antaresFlowbased"))
  
  areaADQ_32b <- as.antaresDataTable(areaADQ_32b, timeStep = "hourly", type = "area", synthesis = FALSE)
  linkADQ_32b <- as.antaresDataTable(linkADQ_32b, timeStep = "hourly", type = "link", synthesis = FALSE)
  
  setkeyv(areaADQ_32b, getIdCols(areaADQ_32b))
  setkeyv(linkADQ_32b, getIdCols(linkADQ_32b))
  
  setkeyv(adqWhioutStratMrg$areas, getIdCols(adqWhioutStratMrg$areas))
  setkeyv(adqWhioutStratMrg$links, getIdCols(adqWhioutStratMrg$links))
  
  ##Test if area table is ok
  if(clpAPI::versionCLP() %in% "1.16.9"){
    expect_true(isTRUE(all.equal(round(adqWhioutStratMrg$areas$BALANCE, 4), round(areaADQ_64b$BALANCE, 4))) |
                  isTRUE(all.equal(round(adqWhioutStratMrg$areas$BALANCE, 4), round(areaADQ_32b$BALANCE, 4))))
    
    expect_true(isTRUE(all.equal(adqWhioutStratMrg$areas$`UNSP. ENRG`, areaADQ_64b$`UNSP. ENRG`)) |
                  isTRUE(all.equal(adqWhioutStratMrg$areas$`UNSP. ENRG`, areaADQ_32b$`UNSP. ENRG`)))
    expect_true(isTRUE(all.equal(adqWhioutStratMrg$areas$LOLD, areaADQ_64b$LOLD)) | 
                  isTRUE(all.equal(adqWhioutStratMrg$areas$LOLD, areaADQ_32b$LOLD)))
    expect_true(isTRUE(all.equal(adqWhioutStratMrg$areas$`DTG MRG`, areaADQ_64b$`DTG MRG`)) |
                  isTRUE(all.equal(adqWhioutStratMrg$areas$`DTG MRG`, areaADQ_32b$`DTG MRG`)))
    
    ##Test link table
    expect_true(isTRUE(all.equal(adqWhioutStratMrg$links$`FLOW LIN.`, linkADQ_64b$`FLOW LIN.`)) |
                  isTRUE(all.equal(adqWhioutStratMrg$links$`FLOW LIN.`, linkADQ_32b$`FLOW LIN.`)))
  }
  
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
    adqStratBe <- suppressWarnings(.applyAdq(opts = opts3, dta, strategic_reserve_be = "toto", fb_opts = optsTMP, keepOldColumns = FALSE))
    nostrat <- suppressWarnings(.applyAdq(opts = opts3, dta, fb_opts = optsTMP, keepOldColumns = FALSE))
    
    
    adqStratBe <- restaureOldName(adqStratBe)
    nostrat <- restaureOldName(nostrat)
    
    
    
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
    adqStratBe <- suppressWarnings(.applyAdq(opts = opts3, dta, strategic_reserve_de = "toto", fb_opts = optsTMP, keepOldColumns = FALSE))
    nostrat <- suppressWarnings(.applyAdq(opts = opts3, dta, fb_opts = optsTMP, keepOldColumns = FALSE))
    
    adqStratBe <- restaureOldName(adqStratBe)
    nostrat <- restaureOldName(nostrat)
    
    
    
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
