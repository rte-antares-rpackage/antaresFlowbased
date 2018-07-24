#' @title Run the adequacy patch and save the result in an h5 format file
#' 
#' @description This function runs the adequacy patch (\link{adqPatch}) on an Antares output and saves the result in an h5 file.
#' 
#' @param opts \code{list} of simulation parameters returned by the function \link{setSimulationPath}. By default, the value is given by \code{antaresRead::simOptions()},
#'  it must refer to an h5 file.
#' @param fb_opts \code{list} of simulation parameters returned by the function \link{setSimulationPath} or flow-based model path obtained 
#' with the function \link{setFlowbasedPath}. By default, the value is returned by \code{antaresRead::simOptions()}.
#' @param strategic_reserve_be \code{character}, name of the virtual area representing the strategic reserve of Belgium
#' @param strategic_reserve_de \code{character}, name of the virtual area representing the strategic reserve of Germany
#' 
#' 
#' @examples
#'
#' \dontrun{
#' 
#' 
#' ##Opts defind on txt files
#' opts <- antaresRead::setSimulationPath("D:/Users/titorobe/Desktop/antaresStudy",2)
#' 
#' ##Write h5 stud
#' antaresRead::writeAntaresH5()
#' 
#' ##Do a setSimulationPath on h5 file
#' opts <- antaresRead::setSimulationPath("D:/Users/titorobe/Desktop/Antares/antaresFlowbased",1)
#' 
#' ##Use for user folder, you cal also use setFlowbasedPath
#' fb_opts = antaresRead::setSimulationPath("D:/Users/titorobe/Desktop/antaresStudy", 2)
#' 
#' ##Compute adq on H5 file
#' adqH5(opts, fb_opts)
#' 
#' ##Set alias for an easy read
#' setAlias("adqPatch", "Alias for adqPatch", c("LOLD_ADQPatch",
#'                                              "UNSP. ENRG_ADQPatch", "DTG MRG_ADQPatch",
#'                                              "UNSP. ENRG_ADQPatch", "BALANCE_ADQPatch",
#'                                              "FLOW LIN._ADQPatch",
#'                                              "areas", "links"))
#' 
#' ##Read with alias
#' readAntares(select = "adqPatch", opts = opts, mcYears = 1)
#' 
#' 
#' }
#' 
#' @import antaresProcessing
#' 
#' @export
adqH5 <- function(opts, fb_opts, strategic_reserve_be = NULL, strategic_reserve_de = NULL){
  
  setAlias("adqPatchBefore", "Alias for adqPatch", c("LOLD", "UNSP. ENRG", "DTG MRG", "UNSP. ENRG", "BALANCE", "FLOW LIN.", "areas", "links"))
  
  ADQarea <- c("fr", "be", "de", "nl")
  ADQlinks <-  c("be - de","be - fr","be - nl","de - fr","de - nl")
  keepOldColumns <- FALSE
  areasList <- getAreas(opts = opts)
  linksList <- getLinks(opts = opts)
  
  
  plyr::l_ply(opts$mcYears, function(MC){
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
    
    .writeAllTables(timeStep = "hourly",
                                        mcY = "mcInd",
                                        path = opts$h5path,
                                        outToWrite = outToWrite ,
                                        areas = TRUE,
                                        links = TRUE,
                                        clusters = FALSE,
                                        districts = FALSE,
                                        mcYear = MC, writeStruct = writeStruct)
    NULL
  }, .progress = "text")
  
}
