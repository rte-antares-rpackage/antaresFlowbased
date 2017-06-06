#' Compute adqPatch for antares study
#' 
#' @param opts \code{list} of simulation parameters returned by the function \link{setSimulationPath}. Defaut to \code{antaresRead::simOptions()}
#' 
#' @export
adqPatch <- function(opts = antaresRead::simOptions())
{
  
  dta <- readAntares(areas = c("fr", "be", "de", "nl"), 
                     links = c("be - de","be - fr","be - nl","de - fr","de - nl"), mcYears = "all",
                     select = c("LOLD", "UNSP. ENRG", "DTG MRG", "UNSP. ENRG", "BALANCE", "FLOW LIN."))
  
  .applyAdq(opts = opts, dta)

  
}

#' Compute adqPatch for antares study
#' 
#' @param opts \code{list} of simulation parameters returned by the function \link{setSimulationPath}. Defaut to \code{antaresRead::simOptions()}
#' @param dta \code{list} data load with readAntares
#' 
#' @noRd
.applyAdq <- function(opts, dta){
  
  dta <- data.table::copy(dta)
  links <- dcast(dta$links, time + mcYear~link, value.var = c("FLOW LIN."))
  links[, be := `be - de` + `be - fr` + `be - nl`]
  links[, de := - `be - de` + `de - fr` + `de - nl`]
  links[, fr := - `be - fr` - `de - fr`]
  links[, nl := - `be - nl` - `de - nl`]
  
  links <- links[, .SD, .SDcols = c("time", "mcYear","be","de" ,"fr","nl")]
  links <- melt(links, id = 1:2)
  setnames(links, "variable", "area")
  dta$areas <- merge(dta$areas, links, by = c("time", "mcYear", "area"))
  dta$areas[, lole :=`UNSP. ENRG` - `DTG MRG` - value]
  dta$areas[, ipn := value]
  dta$areas[, lole:=(ifelse(lole<=0, 0, lole))]
  
  out <- dta$areas[, .SD, .SDcols = c("area", "mcYear", "time", "lole", "LOLD", "DTG MRG", "ipn")]
  out <- dcast(out, time + mcYear~area, value.var = c("lole", "LOLD", "DTG MRG", "ipn"))
  
  secondM <- fread(paste0(opts$studyPath, "/user/flowbased/second_member.txt"))
  scenario <- fread(paste0(opts$studyPath, "/user/flowbased/scenario.txt"))
  ts <- fread(paste0(opts$studyPath, "/user/flowbased/ts.txt"))
  b36p <-  fread(paste0(opts$studyPath, "/user/flowbased/weight.txt"))
  
  
  ####Pour test
  #b36 <- fread("D:/Users/titorobe/Desktop/ADQ PATCH/B36.txt")
  
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
        ret = 0
        if(outR$lole_be == 0 & outR$LOLD_be == 1){
          cat(paste0("mcYear : ",outR$mcYear," timeId : " , outR$time,
                     " be a has LOLD = 1 but DTG MRG>0, adequacy patch not applied \n"))
          ret = 1
        }
        if(outR$lole_de == 0 & outR$LOLD_de == 1){
          cat(paste0("mcYear : ",outR$mcYear," timeId : " , outR$time,
                     " de a has LOLD = 1 but DTG MRG>0, adequacy patch not applied \n"))
          ret = 1
        }
        if(outR$lole_fr == 0 & outR$LOLD_fr == 1){
          cat(paste0("mcYear : ",outR$mcYear," timeId : " , outR$time,
                     " fr a has LOLD = 1 but DTG MRG>0, adequacy patch not applied \n"))
          ret = 1
        }
        if(outR$lole_nl == 0 & outR$LOLD_nl == 1){
          cat(paste0("mcYear : ",outR$mcYear," timeId : " , outR$time,
                     " nl a has LOLD = 1 but DTG MRG>0, adequacy patch not applied \n"))
          ret = 1
        }
        if(ret == 0){
          

        senar <- scenario[outR$mcYear]$simulation
        
       
        dayType <- ts[[as.character(senar)]][which( substr(outR$time, 6, 10) ==    substr(ts$Date, 6, 10))]
        Hour <- hour(outR$time) + 1
        b <- data.table(1:length(secondM[Id_day == dayType & Id_hour == Hour]$vect_b),
                        secondM[Id_day == dayType & Id_hour == Hour]$vect_b)
        ##ADQ Patch
        #b <- fread("inst/ADQpatch/b.txt")
        #b36 <- fread("inst/ADQpatch/B36.txt")
        
        lole <- outR[, .SD, .SDcols = c("lole_be", "lole_de", "lole_fr", "lole_nl")]
        lole <- unlist(lole)
        ipn <- outR[, .SD, .SDcols = c("ipn_be", "ipn_de", "ipn_fr", "ipn_nl")]
        ipn <- unlist(ipn)
        mrg <- outR[, .SD, .SDcols = c("DTG MRG_be", "DTG MRG_de", "DTG MRG_fr", "DTG MRG_nl")]
        mrg <- unlist(mrg)
        
        
        # write.table(b, "D:/Users/titorobe/Desktop/ADQ PATCH/test2ampl/b.txt", row.names = FALSE, sep = "\t", dec = ".", col.names = FALSE)
        # write.table(b36, "D:/Users/titorobe/Desktop/ADQ PATCH/test2ampl/B36.txt", row.names = FALSE, sep = "\t", dec = ".", col.names = FALSE)
        # write.table(data.table(t(lole)), "D:/Users/titorobe/Desktop/ADQ PATCH/test2ampl/lole.txt", row.names = FALSE, sep = "\t", dec = ".", col.names = FALSE)
        # write.table(data.table(t(ipn)), "D:/Users/titorobe/Desktop/ADQ PATCH/test2ampl/InitialPN.txt", row.names = FALSE, sep = "\t", dec = ".", col.names = FALSE)
        # write.table(data.table(t(mrg)), "D:/Users/titorobe/Desktop/ADQ PATCH/test2ampl/margins.txt", row.names = FALSE, sep = "\t", dec = ".", col.names = FALSE)
        # 
        # 
        sol <- .resolveAdq(b36 = b36Prim, lole = lole, b = b,margin = mrg , ipn = ipn)
        sol <- round(sol, 0)
        sol <- data.frame(sol)
        if(sum(sol)>0){
          sol[,which.max(sol)] <-  sol[,which.max(sol)] - sum(sol)
        }
        if(sum(sol)<0){
          sol[,which.min(sol)] <-  sol[,which.min(sol)] - sum(sol)
          
        }
        
        print(sum(sol))
        cbind(outR, sol)
        }else{
          NULL
        }
      }
    }
  }, simplify = FALSE))
  
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
    "time", "mcYear",    "LOLD_fr",    "LOLD_be",   "LOLD_de",      "LOLD_nl"
  )], c("time", "mcYear"))
  
  re_LOLD$variable <- gsub("LOLD_", "",re_LOLD$variable  )
  re <- merge(re_LOLD, re_PN, by = c( "time", "mcYear","variable" ))
  setnames(re, "value.x", "LOLD")
  setnames(re, "value.y", "PN")
  setnames(re, "variable", "area")
  chang <- merge(dta$areas, re, by = c("time" ,"mcYear", "area"))
  chang[, BALANCEN:=BALANCE - value + PN]
  chang[, UNSPN:=ifelse(lole>abs(PN), lole -  abs(PN),0)]
  chang[,LOLDN := ifelse(UNSPN==0, 0, 1)]
  chang[,`DTG MRGN` := ifelse(UNSPN==0, `DTG MRG` + value - PN, 0)]
  
  
  chang_link <- merge(dta$links, re_link, by = c("time" ,"mcYear", "link"))
  chang_link$`FLOW LIN.` <- chang_link$value
  chang_link$value <- NULL
  setkeyv(chang_link, c("link", "time", "mcYear"))
  setnames(chang_link, "FLOW LIN.", "tocop")
  setkeyv(dta$links, c("link", "time", "mcYear"))
  dta$links[chang_link, `FLOW LIN.` := as.integer(tocop)] 
  
  setkeyv(chang, c("area", "time", "mcYear"))
  setkeyv(dta$areas, c("area", "time", "mcYear"))
  dta$areas[chang, `BALANCE` := as.integer(BALANCEN)] 
  dta$areas[chang, `UNSP. ENRG` := as.integer(UNSPN)] 
  dta$areas[chang, `LOLD` := as.integer(LOLDN)] 
  dta$areas[chang, `DTG MRG` := as.integer(`DTG MRGN`)] 
  dta$areas$value <- NULL
  dta$areas$lole <- NULL
  setkeyv(dta$areas, c( "mcYear", "area", "timeId"))
  setkeyv(dta$links, c( "mcYear", "link", "timeId"))
  dta
}









#' Correction of lole
#' 
#' @param b36 \code{matrix}, faces
#' @param lole \code{data.frame}, energy
#' @param b \code{numeric}, b
#' 
#' @noRd
.resolveAdq <- function(b36, lole, b, margin, ipn){
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
    outpt <- c(outpt, margin[1] + ipn[1])
    sign <- c(sign, "<=")
  }

  if(D[2] == 0){
    res <- c(res, c(0, 1, 0, 0))
    outpt <- c(outpt, margin[2] + ipn[2])
    sign <- c(sign, "<=")
  }

  if(D[3] == 0){
    res <- c(res, c(0, 0, 1, 0))
    outpt <- c(outpt, margin[3] + ipn[3])
    sign <- c(sign, "<=")
  }

  if(D[4] == 0){
    res <- c(res, c(0, 0, 0, 1))
    outpt <- c(outpt, margin[4] + ipn[4])
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
