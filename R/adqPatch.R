#' Compute adqPatch for antares study
#' @title Run the adequacy patch
#' 
#' @param opts \code{list} of simulation parameters returned by the function \link{setSimulationPath}. Defaut to \code{antaresRead::simOptions()}
#' @param mcYears \code{numeric} include mcYears. Default all (all mcYears are load)
#' @param pre_filter \code{boolean} filter mcYears before adqPatch apply, load annual data and if LOLD>0 on annual data dont load this mcYears
#' @param strategic_reserve_be \code{character} area use to compute new margin for BE
#' @param strategic_reserve_de \code{character} area use to compute new margin for DE
#' @param select \code{character}, columns to select (columns need for adqPatch are automaticaly add)
#' 
#' @examples
#'
#' \dontrun{
#' antaresRead::setSimulationPath("D:/Users/titorobe/Desktop/exemple_test_BP", 2)
#' 
#' #No strategic reserve
#' res <- adqPatch()
#' 
#' #Strategic reserve
#' res <- adqPatch(strategic_reserve_de = "lu_de", strategic_reserve_be = "lu_be")
#' 
#' #Add a new column
#' res <- adqPatch(strategic_reserve_de = "lu_de", strategic_reserve_be = "lu_be", select = "COAL")
#' 
#' }
#' 
#' @importFrom stats as.formula cutree dist hclust na.omit runif
#' @importFrom utils read.table setTxtProgressBar txtProgressBar write.table
#' 
#' @export
adqPatch <- function(mcYears = "all",
                     pre_filter = FALSE,
                     strategic_reserve_be = NULL,
                     strategic_reserve_de = NULL,
                     opts = antaresRead::simOptions(),
                     select = NULL)
{
  
  
  
  ##Add alias
  setAlias("adqPatch", "Alias for adqPatch", c("LOLD", "UNSP. ENRG", "DTG MRG", "UNSP. ENRG", "BALANCE", "FLOW LIN.", "areas", "links"))
  
  if(pre_filter){
    #Load useful data
    dta <- readAntares(areas = c("fr", "be", "de", "nl"), mcYears = mcYears,
                       select = c(select, "adqPatch"),
                       timeStep = "annual")
    mcYears <- unique(dta[dta$LOLD>0]$mcYear)
  }
  
  
  #Load useful data
  dta <- readAntares(areas = c("fr", "be", "de", "nl"), 
                     links = c("be - de","be - fr","be - nl","de - fr","de - nl"), mcYears = mcYears,
                     select = c(select, "adqPatch"))
  
  
  if(!all(strategic_reserve_be %in% getAreas())){
    strategicNotIn <- strategic_reserve_be[!strategic_reserve_be %in% getAreas()]
    strategicNotIn <- paste(strategicNotIn, sep = "", collapse = ",")
    stop(paste0("area(s) : '", strategicNotIn, "' does not exist"))
    
  }
  
  if(!all(strategic_reserve_de %in% getAreas())){
    strategicNotIn <- strategic_reserve_de[!strategic_reserve_de %in% getAreas()]
    strategicNotIn <- paste(strategicNotIn, sep = "", collapse = ",")
    stop(paste0("area(s) : '", strategicNotIn, "' does not exist"))
  }
  
  
  .applyAdq(opts = opts, dta, strategic_reserve_be = strategic_reserve_be, strategic_reserve_de = strategic_reserve_de, mcYears = mcYears)
  
}

#' Compute adqPatch for antares study
#' 
#' @param opts \code{list} of simulation parameters returned by the function \link{setSimulationPath}. Defaut to \code{antaresRead::simOptions()}
#' @param dta \code{list} data load with readAntares
#' @param strategic_reserve_be \code{character} area use to compute new margin for BE
#' @param strategic_reserve_de \code{character} area use to compute new margin for DE
#' @param mcYears \code{numeric} include mcYears. Default all (all mcYears are load)
#' @param ... use for test
#' 
#' @noRd
.applyAdq <- function(opts, dta, strategic_reserve_be = NULL, strategic_reserve_de = NULL, mcYears = "all", ...){
  oldw <- getOption("warn")
  options(warn = -1)
  
  #Suppress note
  `be - de` <- `de - fr` <- `de - nl` <- `de - nl` <- `be - fr` <- lole <- `UNSP. ENRG` <- `DTG MRG` <- value <- NULL
  Name <- name <-  `be%nl`<- `de%nl` <- `be%fr`<- V4 <- V2 <- LOLD_fr <- LOLD_be <- LOLD_de <- LOLD_nl <- NULL
  lole_fr <- lole_be <- lole_de <- lole_nl <- `UNSP. ENRG_fr` <- `UNSP. ENRG_be`<- `UNSP. ENRG_de`<- `UNSP. ENRG_nl` <- NULL
  Id_day <- Id_hour <- BALANCEN <- BALANCE <- PN <- area <- UNSPN <- strategicMargin <- LOLDN <- NULL
  `DTG MRGN` <- `FLOW LIN.` <- tocop <- stratReserve <- additionalSRN <- strategicMarginN <- LOLD <- additionalSR <- NULL
  `be - nl` <- ipn <- NULL
  
  #Compute Net position from links
  dta <- data.table::copy(dta)
  links <- dcast(dta$links, time + mcYear~link, value.var = c("FLOW LIN."))
  links[, "be" := `be - de` + `be - fr` + `be - nl`]
  links[, "de" := - `be - de` + `de - fr` + `de - nl`]
  links[, "fr" := - `be - fr` - `de - fr`]
  links[, "nl" := - `be - nl` - `de - nl`]
  
  #Merge with data
  links <- links[, .SD, .SDcols = c("time", "mcYear","be","de" ,"fr","nl")]
  links <- melt(links, id = 1:2)
  setnames(links, "variable", "area")
  dta$areas <- merge(dta$areas, links, by = c("time", "mcYear", "area"))
  
  #Compute lole
  dta$areas[, lole :=`UNSP. ENRG` - `DTG MRG` - value]
  dta$areas[, ipn := value]
  dta$areas[, lole:=(ifelse(lole<=0, 0, lole))]
  
  #Keep only useful data
  out <- dta$areas[, .SD, .SDcols = c("area", "mcYear", "time", "lole", "LOLD", "DTG MRG", "ipn", "UNSP. ENRG")]
  out <- dcast(out, time + mcYear~area, value.var = c("lole", "LOLD", "DTG MRG", "ipn", "UNSP. ENRG"))
  
  #Load from study
  secondM <- fread(paste0(opts$studyPath, "/user/flowbased/second_member.txt"))
  scenario <- fread(paste0(opts$studyPath, "/user/flowbased/scenario.txt"))
  ts <- fread(paste0(opts$studyPath, "/user/flowbased/ts.txt"))
  b36p <-  fread(paste0(opts$studyPath, "/user/flowbased/weight.txt"))
  
  
  contraintsExcludes <- setdiff(unique(secondM$Name),b36p$name)
  if(length(contraintsExcludes) > 0){
    message(paste0("Ignored constraint(s): ", paste(contraintsExcludes , collapse = ", ")))
    message("They are not described in the two flow-based files second_member.txt and weight.txt.")
    
    secondM <- secondM[!Name%in%contraintsExcludes]
    b36p <- b36p[!name%in%contraintsExcludes]
  }
  
  #Compute b36
  b36 <- b36p[, list(V1 = 1:.N, V2 = `be%nl`, V3 = `de%nl`, V4 = `be%fr`)]
  b36[,V4:= V2-V4]
  b36Prim <- as.matrix(b36)[,2:4]
  b36Prim <- cbind(b36Prim, 0)
  
  #Filtered unconcorded line
  out <- out[LOLD_fr!=0|LOLD_be!=0|LOLD_de!=0|LOLD_nl!=0]
  
  if(nrow(out) == 0){
    dta <- .preReterunData(dta)
    cat("No row concern by adq patch")
    return(dta)
  }
  
  new <- rbindlist(sapply(1:nrow(out), function(X){
    #Filtered unconcorded line
    outR <- out[X]
    
    ret = 0
    
    if(!is.null(strategic_reserve_be) | !is.null(strategic_reserve_de))
    {
      if(outR$`DTG MRG_be` > 0 & outR$LOLD_be == 1){
        warning(paste0("mcYear : ",outR$mcYear," timeId : " , outR$time,
                       " be has LOLD = 1 but DTG MRG>0, adequacy patch not applied \n"))
        ret = 1
      }
      if(outR$`DTG MRG_de` > 0 & outR$LOLD_de == 1){
        warning(paste0("mcYear : ",outR$mcYear," timeId : " , outR$time,
                       " de has LOLD = 1 but DTG MRG>0, adequacy patch not applied \n"))
        ret = 1
      }
      if(outR$`DTG MRG_fr` > 0 & outR$LOLD_fr == 1){
        warning(paste0("mcYear : ",outR$mcYear," timeId : " , outR$time,
                       " fr has LOLD = 1 but DTG MRG>0, adequacy patch not applied \n"))
        ret = 1
      }
      if(outR$`DTG MRG_nl` > 0 & outR$LOLD_nl == 1){
        warning(paste0("mcYear : ",outR$mcYear," timeId : " , outR$time,
                       " nl has LOLD = 1 but DTG MRG>0, adequacy patch not applied \n"))
        ret = 1
      }
    }
    if(ret == 0){
      
      if(outR$`DTG MRG_be` > 0 & outR$LOLD_be == 1){
        ret = 1
      }
      if(outR$`DTG MRG_de` > 0 & outR$LOLD_de == 1){
        ret = 1
      }
      if(outR$`DTG MRG_fr` > 0 & outR$LOLD_fr == 1){
        ret = 1
      }
      if(outR$`DTG MRG_nl` > 0 & outR$LOLD_nl == 1){
        ret = 1
      }
      
    }
    
    
    if(ret == 0){
      ## addition Baptiste Seguinot 30/06/2017
      # should adequacy patch be run ?
      run_adq_patch <- FALSE
      
      #    if more than two countries have unsupplied energy : YES
      if(nrow(outR[c(which(lole_fr!=0),
                     which(lole_be!=0),
                     which(lole_de!=0),
                     which(lole_nl!=0))])>1)
      {run_adq_patch <- TRUE}
      
      #   if one country wasn't in shortage in the output of ANTARES while contribution to CWE
      #   unsupplied energy was not null : YES
      if(nrow(outR[c(which(lole_fr!=0 & `UNSP. ENRG_fr`==0 ),
                     which(lole_be!=0 & `UNSP. ENRG_be`==0),
                     which(lole_de!=0 & `UNSP. ENRG_de`==0),
                     which(lole_nl!=0 & `UNSP. ENRG_nl`==0))])>0)
      {run_adq_patch <- TRUE}
      
      
      if(run_adq_patch)  
      {
        #Found rigth scenario
        senar <- scenario[outR$mcYear]$simulation
        
        
        dayType <- ts[[as.character(senar)]][which( substr(outR$time, 6, 10) ==    substr(ts$Date, 6, 10))]
        Hour <- hour(outR$time) + 1
        
        #Found rigth b
        b <- data.table(1:length(secondM[Id_day == dayType & Id_hour == Hour]$vect_b),
                        secondM[Id_day == dayType & Id_hour == Hour]$vect_b)
        
        lole <- outR[, .SD, .SDcols = c("lole_be", "lole_de", "lole_fr", "lole_nl")]
        lole <- unlist(lole)
        ipn <- outR[, .SD, .SDcols = c("ipn_be", "ipn_de", "ipn_fr", "ipn_nl")]
        ipn <- unlist(ipn)
        mrg <- outR[, .SD, .SDcols = c("DTG MRG_be", "DTG MRG_de", "DTG MRG_fr", "DTG MRG_nl")]
        mrg <- unlist(mrg)
        
        UNSP <-  outR[, .SD, .SDcols = c("UNSP. ENRG_be", "UNSP. ENRG_de", "UNSP. ENRG_fr", "UNSP. ENRG_nl")]
        UNSP <- unlist(UNSP)
        #Apply adq patch
        sol <- .resolveAdq(b36 = b36Prim, lole = lole, b = b,margin = mrg , ipn = ipn, UNSP = UNSP)
        sol <- round(sol, 0)
        sol <- data.frame(sol)
        if(sum(sol)>0){
          sol[,which.max(sol)] <-  sol[,which.max(sol)] - sum(sol)
        }
        if(sum(sol)<0){
          sol[,which.min(sol)] <-  sol[,which.min(sol)] - sum(sol)
          
        }
        
        cbind(outR, sol)
      }else{NULL}
    }else{
      NULL
    }
    
    
  }, simplify = FALSE))
  
  if(nrow(new) == 0){
    dta <- .preReterunData(dta)
    cat("No row concern by adq patch")
    return(dta)
  }
  
  #Compute net position
  new$`be - fr` <- new$PN_be
  new$`de - nl` <- - new$PN_nl
  new$`de - fr` <- - new$PN_be - new$PN_fr 
  new$`be - de` <- 0
  new$`be - nl` <- 0
  
  
  re_link <- melt(new[, .SD, .SDcols = c(
    "time", "mcYear",    "be - de","be - fr","be - nl","de - fr","de - nl")], c("time", "mcYear"))
  setnames(re_link, "variable", "link")
  re_PN <- melt(new[, .SD, .SDcols = c(
    "time", "mcYear",    "PN_fr",    "PN_be",   "PN_de",      "PN_nl"
  )], c("time", "mcYear"))
  
  re_PN$variable <- gsub("PN_", "",re_PN$variable  )
  
  re_LOLD <- melt(new[, .SD, .SDcols = c(
    "time", "mcYear", "LOLD_fr",    "LOLD_be",   "LOLD_de",      "LOLD_nl"
  )], c("time", "mcYear"))
  
  re_LOLD$variable <- gsub("LOLD_", "",re_LOLD$variable  )
  re <- merge(re_LOLD, re_PN, by = c( "time", "mcYear","variable" ))
  setnames(re, "value.x", "LOLD")
  setnames(re, "value.y", "PN")
  setnames(re, "variable", "area")
  chang <- merge(dta$areas, re, by = c("time" ,"mcYear", "area"))
  chang[, BALANCEN:=BALANCE - value + PN]
  
  ##strategic reserve
  setkeyv(chang, c("area", "time", "mcYear"))
  
  if(!exists("stategicBE"))
  {
    stategicBE <- data.table()
  }
  if(!exists("stategicDE"))
  {
    stategicDE <- data.table()
  }
  
  BEstrategic <- data.table()
  DEstrategic <- data.table()
  
  ##For BE
  if(!is.null(strategic_reserve_be)){
    stategicBE <- readAntares(opts = opts, areas = c(strategic_reserve_be), 
                              mcYears = mcYears,
                              select = c("DTG MRG"))
  }
  
  if(nrow(stategicBE)>0)
  {
    BEstrategic <- merge(chang[area == "be",.SD, .SDcols = c("area", "mcYear", "time")],
                         stategicBE[, .SD, .SDcols = c("mcYear", "time", "DTG MRG")], by = c("mcYear", "time"))
    setkeyv(BEstrategic, c("area", "time", "mcYear"))
    stategicBE$area <- "be"
    
    setnames(BEstrategic, "DTG MRG", "strategicMargin")
    
  }
  
  
  
  ##For DE
  if(!is.null(strategic_reserve_de)){
    stategicDE <- readAntares(opts = opts, areas = c(strategic_reserve_de), 
                              mcYears = mcYears,
                              select = c("DTG MRG"))
  }
  
  if(nrow(stategicDE)>0)
  {
    DEstrategic <- merge(chang[area == "de",.SD, .SDcols = c("area", "mcYear", "time")],
                         stategicDE[, .SD, .SDcols = c("mcYear", "time", "DTG MRG")], by = c("mcYear", "time"))
    stategicDE$area <- "de"
    setnames(DEstrategic, "DTG MRG", "strategicMargin")
    
  }
  
  strategicallData <- rbindlist(list(stategicBE, stategicDE))
  if(nrow(strategicallData)>0){
    setnames(strategicallData, "DTG MRG", "strategicMargin")
  }
  
  stratMargin <- rbindlist(list(BEstrategic, DEstrategic))
  if(nrow(stratMargin) == 0){
    chang$strategicMargin <- 0
  }else{
    chang <- merge(chang, stratMargin, by = c("area", "mcYear", "time"), all.x = TRUE)
    chang$strategicMargin[is.na(chang$strategicMargin)] <- 0
    
  }
  
  chang[, UNSPN:=ifelse(lole>(abs(PN)+strategicMargin), lole -  abs(PN) - strategicMargin,0)]
  chang[,LOLDN := ifelse(UNSPN==0, 0, 1)]
  if(nrow(stratMargin) > 0){
    chang[ area %in% c("be", "de"),strategicMargin := ifelse(lole==0, 
                                                             strategicMargin,
                                                             ifelse(UNSPN>0, 0, strategicMargin - (lole -  abs(PN)))) ]
  }else{
    chang$strategicMargin <- NULL
  }
  chang[,`DTG MRGN` := ifelse(UNSPN==0,
                              ifelse((`DTG MRG` + value - PN - `UNSP. ENRG`)>0, `DTG MRG` + value - PN - `UNSP. ENRG`,0)
                              , 0)]
  
  
  ##Update links
  chang_link <- merge(dta$links, re_link, by = c("time" ,"mcYear", "link"))
  chang_link$`FLOW LIN.` <- chang_link$value
  chang_link$value <- NULL
  setkeyv(chang_link, c("link", "time", "mcYear"))
  setnames(chang_link, "FLOW LIN.", "tocop")
  setkeyv(dta$links, c("link", "time", "mcYear"))
  dta$links[chang_link, `FLOW LIN.` := as.integer(tocop)] 
  
  
  ##Update areas
  setkeyv(chang, c("area", "time", "mcYear"))
  setkeyv(dta$areas, c("area", "time", "mcYear"))
  
  ## Add strategicMargin column
  if(nrow(strategicallData)>0)
  {
    #   dta$areas <- merge(dta$areas, strategicallData, by = c("area", "mcYear", "timeId", "time", "day", "month", "hour"), all.x = TRUE)
    #   
    #   setkeyv(chang, c("area", "time", "mcYear"))
    #   setkeyv(dta$areas, c("area", "time", "mcYear"))
    #   dta$areas$strategicMargin[is.na(dta$areas$strategicMargin)] <- 0
    setnames(chang, "strategicMargin", "strategicMarginN")
    #   dta$areas[chang, strategicMargin := as.integer(strategicMarginN)] 
    #   
  }
  
  chang$additionalSRN <- 0
  
  
  
  if(nrow(stategicDE) > 0){
    
    setnames(stategicDE, "DTG MRG", "stratReserve")
    chang <- merge(chang, stategicDE, by = c("area", "mcYear", "timeId", "time", "day", "month", "hour"), all.x = TRUE)
    chang[!is.na(stratReserve), additionalSRN := stratReserve - strategicMarginN]
    chang[,stratReserve := NULL]
  }
  
  if(nrow(stategicBE) > 0){
    
    setnames(stategicBE, "DTG MRG", "stratReserve")
    chang <- merge(chang, stategicBE, by = c("area", "mcYear", "timeId", "time", "day", "month", "hour"), all.x = TRUE)
    chang[!is.na(stratReserve), additionalSRN := stratReserve - strategicMarginN]
    chang[,stratReserve := NULL]
  }
  
  setkeyv(dta$areas, getIdCols(dta$areas))
  setkeyv(chang, getIdCols(chang))
  
  
  dta$areas[chang, `BALANCE` := as.integer(BALANCEN)] 
  dta$areas[chang, `UNSP. ENRG` := as.integer(UNSPN)] 
  dta$areas[chang, `LOLD` := as.integer(LOLDN)] 
  dta$areas[chang, `DTG MRG` := as.integer(`DTG MRGN`)] 
  if(nrow(strategicallData)>0)
  {
    dta$areas[,additionalSR:= 0]
    dta$areas[chang, additionalSR := as.integer(additionalSRN)] 
  }
  
  dta <- .preReterunData(dta)
  
  options(warn = oldw)
  dta
}


.preReterunData <- function(dta){
  dta$areas$ipn <- NULL
  dta$areas$value <- NULL
  dta$areas$lole <- NULL
  
  # if(nrow(strategicallData)>0)
  # {
  #   dta$areas[, additionalSR:= `DTG MRG` - strategicMargin]
  #   dta$areas$strategicMargin <- NULL
  # }
  # 
  setkeyv(dta$areas, c( "mcYear", "area", "timeId"))
  setkeyv(dta$links, c( "mcYear", "link", "timeId"))
  
  dta
}







#' Correction of lole
#' 
#' @param b36 \code{matrix}, faces
#' @param lole \code{data.frame}, energy
#' @param b \code{numeric}, b
#' @param margin \code{numeric} margin
#' @param ipn \code{numeric} ipn
#' @param UNSP \code{numeric} UNSP
#' 
#' @noRd
.resolveAdq <- function(b36, lole, b, margin, ipn, UNSP){
  D <- as.vector(ifelse(lole == 0, 0, 1))
  res <- c(
    1, 1, 1, 1,
    D[1]*D[2]*lole[2], -D[1]*D[2]*lole[1],0,0,
    D[1]*D[3]*lole[3], 0, -D[1]*D[3]*lole[1], 0,
    D[1]*D[4]*lole[4],0,0,-D[1]*D[4]*lole[1],
    0,D[2]*D[3]*lole[3],-D[2]*D[3]*lole[2],0,
    0, D[2]*D[4]*lole[4], 0, -D[2]*D[4]*lole[2],
    0,0,D[3]*D[4]*lole[4],-D[3]*D[4]*lole[3],
    1-D[1], 1-D[2], 1-D[3], 1-D[4])
  
  outpt <- NULL
  sign <- NULL
  if(D[1] == 0){
    res <- c(res, c(1, 0, 0, 0))
    outpt <- c(outpt, margin[1] + ipn[1] - UNSP[1])
    sign <- c(sign, "<=")
  }
  
  if(D[2] == 0){
    res <- c(res, c(0, 1, 0, 0))
    outpt <- c(outpt, margin[2] + ipn[2] - UNSP[2])
    sign <- c(sign, "<=")
  }
  
  if(D[3] == 0){
    res <- c(res, c(0, 0, 1, 0))
    outpt <- c(outpt, margin[3] + ipn[3] - UNSP[3])
    sign <- c(sign, "<=")
  }
  
  if(D[4] == 0){
    res <- c(res, c(0, 0, 0, 1))
    outpt <- c(outpt, margin[4] + ipn[4] - UNSP[4])
    sign <- c(sign, "<=")
  }
  
  res <- matrix(res, ncol = 4, byrow = TRUE)
  allMat <- rbind(res, b36)
  rep <- c(rep(0, 7), (1 - D[1])*ipn[1] +
             (1 - D[2])*ipn[2] +
             (1 - D[3])*ipn[3] +
             (1 - D[4])*ipn[4], outpt, b$V2)
  sens <- c(rep("==", 7), "<=", sign, rep("<=", length(b$V2)))
  objetiv <- c(lole)
  l_constraint <- L_constraint(L = allMat,
                               dir = sens,
                               rhs = rep)
  bounds <- V_bound(li=1:4, lb=rep(-Inf, 4))
  LP <- OP(objetiv, l_constraint, maximum = FALSE,
           bounds = bounds)
  y <- ROI_solve(LP, solver = "clp",  control = list(amount = 0))
  data.table(PN_be = y$solution[1],
             PN_de = y$solution[2],
             PN_fr = y$solution[3],
             PN_nl = y$solution[4])
}
