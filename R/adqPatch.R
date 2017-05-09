#' Compute adqPatch for antares study
#' 
#' @param opts \code{list} of simulation parameters returned by the function \link{setSimulationPath}. Defaut to \code{antaresRead::simOptions()}
#' 
#' @export
adqPatch <- function(opts)
{
  
  dta <- readAntares(areas = c("fr", "be", "de", "nl"), 
                     links = c("be - de","be - fr","be - nl","de - fr","de - nl"), mcYears = "all",
                     select = c("LOLD", "UNSP. ENRG", "DTG MRG", "UNSP. ENRG", "BALANCE", "FLOW LIN."))
  
  links <- dcast(dta$links, time + mcYear~link, value.var = c("FLOW LIN."))
  links[, be :=`be - de` + `be - fr` + `be - nl`]
  links[, de := - `be - de` + `de - fr` + `de - nl`]
  links[, fr := - (`be - fr` + `de - fr`)]
  links[, nl := - (`be - nl` + `de - nl`)]
  
  
  links <- links[, .SD, .SDcols = c("time", "mcYear","be","de" ,"fr","nl")]
  links <- melt(links, id = 1:2)
  setnames(links, "variable", "area")
  dta$areas <- merge(dta$areas, links, by = c("time", "mcYear", "area"))
  dta$areas[, lole :=`UNSP. ENRG` - `DTG MRG` - value]
  dta$areas[, lole:=(ifelse(lole<=0, 0, lole))]
  
  out <- dta$areas[, .SD, .SDcols = c("area", "mcYear", "time", "lole", "LOLD")]
  out <- dcast(out, time + mcYear~area, value.var = c("lole", "LOLD"))
  
  secondM <- fread(paste0(opts$studyPath, "/user/flowbased/second_member.txt"))
  scenario <- fread(paste0(opts$studyPath, "/user/flowbased/scenario.txt"))
  ts <- fread(paste0(opts$studyPath, "/user/flowbased/ts.txt"))
  b36p <-  fread(paste0(opts$studyPath, "/user/flowbased/weight.txt"))
  
  b36 <- b36p[, list(V1 = 1:.N, V2 = `be%nl`, V3 = `de%nl`, V4 = `be%fr`)]
  b36[,V4:= V2-V4]
  
  b36Prim <- as.matrix(b36)[,2:4]
  b36Prim <- cbind(b36Prim, 0)
  
  
  new <- rbindlist(sapply(1:nrow(out), function(X){
    outR <- out[X]
    if(nrow(outR[c(which(LOLD_fr!=0),
                   which(LOLD_be!=0),
                   which(LOLD_de!=0),
                   which(LOLD_nl!=0))])>0){
      if(nrow(outR[c(which(lole_fr!=0),
                     which(lole_be!=0),
                     which(lole_de!=0),
                     which(lole_nl!=0))])>1)
      {
        print("ici")
        senar <- scenario[outR$mcYear]$simulation
        dayType <- ts[[as.character(senar)]][yday(outR$time)]
        Hour <- hour(outR$time) + 1
        b <- data.table(1:length(secondM[Id_day == dayType & Id_hour == Hour]$vect_b),
                        secondM[Id_day == dayType & Id_hour == Hour]$vect_b)
        
        ##ADQ Patch
        #b <- fread("inst/ADQpatch/b.txt")
        #b36 <- fread("inst/ADQpatch/B36.txt")
        lole <- outR[, .SD, .SDcols = c("lole_be", "lole_de", "lole_fr", "lole_nl")]
        lole <- unlist(lole)
        D <- as.vector(ifelse(lole == 0, 0, 1))
        res <- c(
          1, 1, 1, 1,
          D[1]*D[2]*lole[2], -D[1]*D[2]*lole[1],0,0,
          D[1]*D[3]*lole[3], 0, -D[1]*D[3]*lole[1], 0,
          D[1]*D[4]*lole[4],0,0,-D[1]*D[4]*lole[1],
          0,D[2]*D[3]*lole[3],-D[2]*D[3]*lole[2],0,
          0, D[2]*D[4]*lole[4], 0, -D[2]*D[4]*lole[2],
          0,0,D[3]*D[4]*lole[4],-D[3]*D[4]*lole[3])
        res <- matrix(res, ncol = 4, byrow = TRUE)
        allMat <- rbind(res, b36Prim)
        rep <- c(rep(0, 7), b$V2)
        sens <- c(rep("==", 7), rep("<=", length(b$V2)))
        objetiv <- c(lole)
        l_constraint <- L_constraint(L = allMat,
                                     dir = sens,
                                     rhs = rep)
        bounds <- V_bound(li=1:4, lb=rep(-Inf, 4))
        LP <- OP(objetiv, l_constraint, maximum = FALSE,
                 bounds = bounds)
        y <- ROI_solve(LP, solver = "clp")
        y$solution
        outR$PN_be <- y$solution[1]
        outR$PN_de <- y$solution[2]
        outR$PN_fr <- y$solution[3]
        outR$PN_nl <- y$solution[4]
        outR
      }
    }
  }))
  
  new$`be - de` <- new$PN_be - new$PN_de 
  new$`be - fr` <- new$PN_be - new$PN_fr
  new$`be - nl` <- new$PN_be - new$PN_nl
  new$`de - fr` <- new$PN_de - new$PN_fr 
  new$`de - nl` <- new$PN_de - new$PN_nl
  
  
  re_link <- melt(new[, .SD, .SDcols = c(
    "time", "mcYear",    "be - de","be - fr","be - nl","de - fr","de - nl")], c("time", "mcYear"))
  setnames(re_link, "variable", "link")
  re_PN <- melt(new[, .SD, .SDcols = c(
    "time", "mcYear",    "PN_fr",    "PN_be",   "PN_de",      "PN_nl"
  )], c("time", "mcYear"))
  
  re_PN$variable <- gsub("PN_", "",re_PN$variable  )
  
  re_LOLD <- melt(new[, .SD, .SDcols = c(
    "time", "mcYear",    "LOLD_fr",    "LOLD_be",   "LOLD_de",      "LOLD_nl"
  )], c("time", "mcYear"))
  
  re_LOLD$variable <- gsub("LOLD_", "",re_LOLD$variable  )
  re <- merge(re_LOLD, re_PN, by = c( "time", "mcYear","variable" ))
  setnames(re, "value.x", "LOLD")
  setnames(re, "value.y", "PN")
  setnames(re, "variable", "area")
  chang <- merge(dta$areas, re, by = c("time" ,"mcYear", "area"))
  chang[, BALANCEN:=BALANCE - value + PN]
  chang[, UNSPN:=ifelse(lole>abs(PN), lole, abs(PN))]
  chang[,LOLDN := ifelse(UNSPN==0, 0, 1)]
  chang[,`DTG MRGN` := ifelse(UNSPN==0, 0, `DTG MRG` + value - PN)]
  
  
  chang_link <- merge(dta$links, re_link, by = c("time" ,"mcYear", "link"))
  chang_link$`FLOW LIN.` <- chang_link$value
  chang_link$value <- NULL
  setkeyv(chang_link, c("link", "time", "mcYear"))
  setnames(chang_link, "FLOW LIN.", "tocop")
  setkeyv(dta$links, c("link", "time", "mcYear"))
  dta$links[chang_link, `FLOW LIN.` := as.integer(tocop)] 
  
  setkeyv(chang, c("area", "time", "mcYear"))
  setkeyv(dta$areas, c("area", "time", "mcYear"))
  dta$area[chang, `BALANCE` := as.integer(BALANCEN)] 
  dta$area[chang, `UNSP. ENRG` := as.integer(UNSPN)] 
  dta$area[chang, `LOLD` := LOLDN] 
  dta$area[chang, `DTG MRG` := as.integer(`DTG MRGN`)] 
  dta$area$value <- NULL
  dta$area$lole <- NULL
  setkeyv(dta$areas, c( "mcYear", "area", "timeId"))
  setkeyv(dta$links, c( "mcYear", "link", "timeId"))
  dta
}