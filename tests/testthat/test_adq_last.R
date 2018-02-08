

area <- fread(system.file("ADQSTRATMRG/new_area.csv", package = "antaresFlowbased"))
link <- fread(system.file("ADQSTRATMRG/new_links.csv", package = "antaresFlowbased"))
dta <- list(areas = area, links = link)
opts <- list()
opts$studyPath <- system.file("testdata/adq/antaresStudy/user/flowbased/ts.txt", package = "antaresFlowbased")
opts$studyPath <- gsub("/user/flowbased/ts.txt","" , opts)
dta$areas <- as.antaresDataTable(dta$areas, timeStep = "hourly", type = "area", synthesis = FALSE)
dta$links <- as.antaresDataTable(dta$links, timeStep = "hourly", type = "link", synthesis = FALSE)

adqWhioutStratMrg <- .applyAdq(opts = opts, dta)


areaADQ <- fread(system.file("ADQSTRATMRG/adqpatch_area.csv", package = "antaresFlowbased"))
linkADQ <- fread(system.file("ADQSTRATMRG/adqpatch_links.csv", package = "antaresFlowbased"))





areaADQ <- as.antaresDataTable(areaADQ, timeStep = "hourly", type = "area", synthesis = FALSE)
linkADQ <- as.antaresDataTable(linkADQ, timeStep = "hourly", type = "link", synthesis = FALSE)

setkeyv(areaADQ, getIdCols(areaADQ))
setkeyv(adqWhioutStratMrg$areas, getIdCols(adqWhioutStratMrg$areas))

setkeyv(adqWhioutStratMrg$links, getIdCols(adqWhioutStratMrg$links))

setkeyv(adqWhioutStratMrg$links, getIdCols(adqWhioutStratMrg$links))


##Test if area table is ok
expect_true(identical(adqWhioutStratMrg$areas$BALANCE, areaADQ$BALANCE))
expect_true(identical(adqWhioutStratMrg$areas$`UNSP. ENRG`, areaADQ$`UNSP. ENRG`))
expect_true(identical(adqWhioutStratMrg$areas$LOLD, areaADQ$LOLD))
expect_true(identical(adqWhioutStratMrg$areas$`DTG MRG`, areaADQ$`DTG MRG`))

##Same for link



antaresRead::setSimulationPath("D:/Users/titorobe/Desktop/antaresStudyForADQ/", 1)

#No strategic reserve
res <- adqPatch(mcYears = 3)
LOLD <- res$areas[LOLD != 0]

res <- adqPatch(mcYears = 3, strategic_reserve_be = "nl")
