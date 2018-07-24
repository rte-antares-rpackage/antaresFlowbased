## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- eval=FALSE---------------------------------------------------------
#  
#  # Set Antares study path and read the simulation results
#  study <- "MyStuDy"
#  opts <- antaresRead::setSimulationPath(study, 2)
#  dta <- antaresRead::readAntares(areas = c("fr", "be", "de", "nl"),
#                                  links = c("be - de","be - fr","be - nl",
#                                  "de - fr","de - nl"), mcYears = 1:10,
#                                  select = c("LOLD", "UNSP. ENRG",
#                                  "DTG MRG", "UNSP. ENRG", "BALANCE", "FLOW LIN."),
#                                  opts = opts)
#  
#  
#  # Add to the results the time series of typical flow-based days
#  dta <- addTypicalDayId(dta)
#  
#  
#  # Apply the adequacy patch on the results
#  dta_adq <- adqPatch(mcYears = "all", pre_filter = FALSE, strategic_reserve_be = NULL,
#                      strategic_reserve_de = NULL, opts = antaresRead::simOptions(),
#                      fb_opts = opts, select = NULL, keepOldColumns = TRUE)
#  
#  
#  # Visualise the flow-based exchanges (position in the flow-based domains) before or after the adequacy patch
#      # Plot a domain and the matching output positions
#      plotNetPositionFB(fb_opts = opts,
#                        data = dta_adq, drawPositionsAdqP = FALSE,
#                        dayType = 1, hour = c(0, 19),
#                        country1 = "BE", country2 = "FR")
#  
#      # Run a shiny application to visualise the domains and matching output positions
#      runAppPosition(dta_adq)
#  
#  
#  # Add to the results a calculation of the net position of the countries within an area (by default, position in CWE)
#  dta <- addNetPosition(data, opts = antaresRead::simOptions(),
#                        inAreas = c("be",  "de", "fr", "nl"), adq = FALSE, newName = "_CWE")
#  
#  
#  # h5 file: apply the adequacy patch to an output in h5 format
#  adqH5(opts, fb_opts = setFlowbasedPath(path = "D:/model1"))
#  
#  

## ---- eval=FALSE---------------------------------------------------------
#  
#   antaresRead::setSimulationPath("MyStuDy", 1)
#   dta <- readAntares(areas = "all", links = "all", clusters = "all" ,mcYears = 1:10)
#   dta <- addTypicalDayId(data = dta)

## ---- eval=FALSE---------------------------------------------------------
#   opts <- antaresRead::setSimulationPath("MyStuDy", 2)
#   dta <- adqPatch(mcYears = "all", pre_filter = FALSE, strategic_reserve_be = NULL,
#                      strategic_reserve_de = NULL, opts = antaresRead::simOptions(),
#                      fb_opts = opts, select = NULL, keepOldColumns = TRUE)
#   ## or
#   dta <- adqPatch(fb_opts = opts)

## ---- eval=FALSE---------------------------------------------------------
#   opts <- antaresRead::setSimulationPath("MyStuDy", 2)
#   dta <- antaresRead::readAntares(areas = c("fr", "be", "de", "nl"),
#                                   links = c("be - de","be - fr","be - nl","de - fr","de - nl"),
#                                   mcYears = 1:10,
#                                   select = c("LOLD", "UNSP. ENRG",
#                                   "DTG MRG", "UNSP. ENRG", "BALANCE", "FLOW LIN."))

## ---- eval=FALSE---------------------------------------------------------
#   ## Plot the typical day 1 at hours 0 and 19 and the matching net positions on a Belgium-France graph
#   plotNetPositionFB(fb_opts = opts,
#            data = dta,
#            dayType = 1, hour = c(0, 19),
#            country1 = "BE", country2 = "FR")

## ---- eval=FALSE---------------------------------------------------------
#   ## Plot the three typical days of summer at hour 19 and the matching net positions
#   plotNetPositionFB(fb_opts = opts,
#            data = dta,
#            dayType = c(1,2,3), hour = c(19),
#            country1 = "BE", country2 = "FR")

## ---- eval=FALSE---------------------------------------------------------
#   ## Plot only one time, without knowing the used flow-based domain
#   dta$areas <- dta$areas[timeId == 5659,]
#   plotNetPositionFB(fb_opts = opts,
#            data = dta,
#            dayType = "all", hour = "all",
#            filteringEmptyDomains = TRUE,
#            country1 = "FR", country2 = "DE")

## ---- eval=FALSE---------------------------------------------------------
#  ## An exemple of authorized filter :
#  idC <- c(antaresRead::getIdCols(dta$areas))
#  idC <- idC[idC!="area"]
#  LOLD <- dta$areas[,lapply(.SD, sum), by = idC, .SDcols = "LOLD"]
#  LOLD <- LOLD[LOLD!=0]
#  LOLD[,LOLD := NULL]
#  
#  # Merge to filter data
#  dta$areas <- merge(dta$areas, LOLD, by =  idC)
#  ## End filter
#  
#  
#  ## plot domains
#   plotNetPositionFB(fb_opts = opts,
#            data = dta,
#            dayType = "all", hour = c(19),
#            country1 = "BE", country2 = "FR", filteringEmptyDomains = TRUE)
#  

## ---- eval=FALSE---------------------------------------------------------
#   ## Apply the adequacy patch
#   opts <- antaresRead::setSimulationPath("MyStuDy", 2)
#   data_withAdq <- adqPatch(keepOldColumns = TRUE)
#  
#   ## Plot the hours 0 and 19 of typical day 1
#    plotNetPositionFB(fb_opts = opts,
#            data = data_withAdq,
#            dayType = 1, hour = c(0, 19),
#            country1 = "BE", country2 = "FR",
#            drawPositionsBeforeAdqP = TRUE, drawPositionsAdqP = TRUE)

## ---- eval=FALSE---------------------------------------------------------
#   ## Filter on LOLD :
#   idC <- c(antaresRead::getIdCols(data_withAdq$areas))
#   idC <- idC[idC!="area"]
#   LOLD <- data_withAdq$areas[,lapply(.SD, sum), by = idC, .SDcols = "LOLD"]
#   LOLD <- LOLD[LOLD!=0]
#   LOLD[,LOLD := NULL]
#   data_withAdq$areas <- merge(data_withAdq$areas, LOLD, by =  idC)
#  
#  
#   ## plot
#   plotNetPositionFB(fb_opts = opts,
#            data = data_withAdq,
#            dayType = c(5,6,7), hour = c(19),
#            country1 = "BE", country2 = "FR",
#            drawPositionsBeforeAdqP = TRUE, drawPositionsAdqP = TRUE)

## ---- eval=FALSE---------------------------------------------------------
#  runAppPosition(dta)

## ---- eval=FALSE---------------------------------------------------------
#  opts <- antaresRead::setSimulationPath("MyStuDy", 2)
#  data <- readAntares(area = "all", links = "all", mcYears = 1)
#  
#  ##Add net position for CWE
#  data <- addNetPosition(data, opts, adq = FALSE)
#  
#  ##Add net position for CWE+AT
#  data <- addNetPosition(data, opts, adq = FALSE,
#   inAreas = c("be", "de", "fr", "nl", "at"), newName = "_CWEAt")

## ---- eval=FALSE---------------------------------------------------------
#  ## Select the Antares simulation
#  # Select the output in a classic format
#  opts <- antaresRead::setSimulationPath("MyStuDy",2)
#  # Write the study in h5 format
#  antaresRead::writeAntaresH5()
#  # Select the simulation now in an h5 file
#  opts <- antaresRead::setSimulationPath("MyNewStudy",1)
#  
#  
#  ## Select the flow-based model to use. The function setFlowbasedPath can also be used
#  fb_opts = antaresRead::setSimulationPath("MyOldStudy", 2)
#  
#  
#  ## Apply the adequacy patch
#  adqH5(opts = opts, fb_opts = fb_opts,
#        strategic_reserve_be = NULL, strategic_reserve_de = NULL)
#  
#  
#  ## Set alias for an easy read of the file afterwards
#  setAlias("adqPatch", "Alias for adqPatch", c("LOLD_ADQPatch",
#                                               "UNSP. ENRG_ADQPatch", "DTG MRG_ADQPatch",
#                                               "UNSP. ENRG_ADQPatch", "BALANCE_ADQPatch",
#                                               "FLOW LIN._ADQPatch",
#                                               "areas", "links"))
#  ## Read the results of the patch with alias
#  readAntares(select = "adqPatch", opts = opts, mcYears = 1)
#  

