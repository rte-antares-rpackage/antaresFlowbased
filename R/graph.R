#' Plot 2D for flowbased areas
#'
#' @param flowbased \code{list}, flowbased outout obtain which computeFB function
#' @param ctry1 \code{character}, country in X
#' @param ctry2 \code{character}, country in Y
#' @param hour \code{numeric}, hour
#' @param dayType \code{numeric}, dayType
#' @param min \code{numeric}, minimum of axis
#' @param max \code{numeric}, maximum of axis
#'
#' @import rAmCharts
#' @importFrom grDevices chull
#'
#' @noRd
graphFlowBased2D <- function(flowbased, ctry1, ctry2, hour = NULL, dayType = NULL, xlim = c(-7000, 7000), ylim = c(-7000, 7000))
{

  if(!is.null(hour)){
    hour <- paste0(" Hour ", hour)
  }


  if(!is.null(dayType)){
    dayType <- paste0(" Typical day ", dayType)
  }

  if(ctry1 == "NL"){
    ptctry <- -rowSums(flowbased$pointsY)
    ptctryX <- -rowSums(flowbased$pointX)
  }else{
    ptctry <- flowbased$pointsY[[ctry1]]
    ptctryX <- flowbased$pointX[[ctry1]]
  }


  if(ctry2 == "NL"){
    ptctry2 <- -rowSums(flowbased$pointsY)
    ptctry2X <- -rowSums(flowbased$pointX)
  }else{
    ptctry2 <- flowbased$pointsY[[ctry2]]
    ptctry2X <- flowbased$pointX[[ctry2]]
  }

  res <- data.frame(ctry1 = ptctry,
                    ctry2 = ptctry2)
  res <- res[chull(res),]
  res <- rbind(res, res[1,])
  res2 <- data.frame(ctry1 = ptctryX,
                     ctry2 = ptctry2X)
  res2 <- res2[chull(res2),]
  res2 <- rbind(res2, res2[1,])


  max_r <- max(nrow(res), nrow(res2))
  if(nrow(res)<max_r){
    res <- rbind(res, data.frame(ctry1 = rep(NA, max_r-nrow(res)),
                                 ctry2 = rep(NA, max_r-nrow(res))))
  }
  if(nrow(res2)<max_r){
    res2 <- rbind(res2, data.frame(ctry1 = rep(NA,max_r- nrow(res2)),
                                   ctry2 = rep(NA, max_r-nrow(res2))))
  }

  out <- cbind(res, res2)
  names(out) <- c(paste0("Model", ctry1), paste0("Model", ctry2),  paste0("Real", ctry1),  paste0("Real", ctry2))

  out <- round(out, 2)


  pipeR::pipeline(
    amXYChart(dataProvider = out),
    addTitle(text = paste0("Flow-based ", ctry1, "/", ctry2, hour, dayType)),
    addGraph(title = "Model", balloonText =
               paste0('<b>Model<br>', ctry1, '</b> :[[x]] <br><b>',ctry2, '</b> :[[y]]'),

             bullet = 'circle', xField = names(out)[1],yField = names(out)[2],
             lineAlpha = 1, bullet = "bubble", bulletSize = 4, lineColor = "#FF0000",
             lineThickness = 1),
    addGraph(title = "Real",balloonText =
               paste0('<b>Real<br>', ctry1, '</b> :[[x]] <br><b>',ctry2, '</b> :[[y]]'),
             bullet = 'circle', xField = names(out)[3],yField = names(out)[4],
             lineAlpha = 1, bullet = "bubble", bulletSize = 4, lineColor = "#0000FF",
             lineThickness = 1,  dashLength = 7),
    setChartCursor(),
    addValueAxes(title = paste(ctry1, "(MW)"), position = "bottom", minimum = xlim[1], maximum = xlim[2]),
    addValueAxes(title =  paste(ctry2, "(MW)"), minimum = ylim[1], maximum = ylim[2]),
    setExport(enabled = TRUE),
    setLegend(enabled = TRUE)
  )

}

#' Plot 2D for flowbased areas can combine more than one plot
#'
#' @param hour \code{numeric}, hour
#' @param dayType \code{numeric}, dayType
#' @param country1 \code{character}, country in X
#' @param country2 \code{character}, country in Y
#' @param fb_opts \code{list} of flowbased parameters returned by the function \link{setFlowbasedPath}. Defaut to \code{antaresFlowbased::fbOptions()}
#'
#' @examples
#'
#' \dontrun{
#' plotFB(1,1,"FR","NL")
#' plotFB(1:2,1,"FR","NL")
#' plotFB(1:2,1:2,"FR","NL")
#' plotFB(1,1,c("FR", "DE"),c("NL", "FR"))
#' }
#'
#'
#' @export
plotFB <- function(dayType, hour, country1, country2, fb_opts = antaresFlowbased::fbOptions()){
  hoursel <- hour
  dayTypesel <- dayType

  dta <- readRDS(paste0(fb_opts$path, "/domainesFB.RDS"))

  if(!all(hour%in%(dta$hour - 1)))stop(paste0("Some hour are not in data : ",paste0(hour[!hour%in%(dta$hour - 1)])))
  

  if(!all(dayType%in%dta$dayType))stop(paste0("Some typical day are not in data : ",paste0(dayType[!dayType%in%dta$dayType])))
  

  if(!all(country1 %in% c("DE","BE","FR","NL")))stop("All country1 must be in : DE, BE, FR, NL")
  
  if(!all(country2 %in% c("DE","BE","FR","NL")))stop("All country2 must be in : DE, BE, FR, NL")
  
  if(length(country1) != length(country2))stop("country1 must be same length to country2")
  

  allCtry <- data.frame(country1 = country1, country2 = country2)
  graphList <- sapply(hour, function(hoursel){
    sapply(dayType, function(dayTypesel){
      apply(allCtry, 1, function(countsel){
        ctsel <- data.frame(t(countsel))
        tempData <- dta[hour == (hoursel + 1) & dayType == dayTypesel]$outFlowBased[[1]]
        if(length(tempData)==0)stop(paste0("Not available data for typical day ", dayTypesel, " hour ", hoursel))
        
        graphFlowBased2D(tempData,
                         as.character(ctsel$country1), as.character(ctsel$country2)
                         , dayType = dayTypesel, hour = hoursel) %>>% plot()
      })
    })
  })
  combineWidgets(list = graphList)
}








#' Generate html report
#'
#' @param fb_opts \code{list} of flowbased parameters returned by the function \link{setFlowbasedPath}. Defaut to \code{antaresFlowbased::fbOptions()}
#' @param output_file \code{character}, output directory
#' @param dayType \code{numeric}, dayType
#' @param allFB \code{data.table}, load FB directaly in package
#'
#' @import rmarkdown flexdashboard rAmCharts manipulateWidget
#'
#' @examples
#'
#' \dontrun{
#' allFB <- computeFB(dayType = 7)
#' generateReportFb(allFB, dayType = 7)
#' }
#' @export
generateReportFb <- function(dayType, output_file = NULL,
                             fb_opts = antaresFlowbased::fbOptions(),
                             allFB = NULL){

  if(is.null(allFB))
  {
    allFB <- readRDS(paste0(fb_opts$path, "/domainesFB.RDS"))
  }
  dayType2 <- dayType
  if(is.null(output_file)){
    output_file <- getwd()
  }
  output_Dir <- output_file
  output_file <- paste0(output_file, "/", "FlowBased_TD",dayType, "_", Sys.Date(), ".html")
  e <- environment()
  e$dayType <- dayType
  e$dta <- allFB[dayType == dayType2]

  rmarkdown::render(system.file("/report/resumeFBflex.Rmd", package = "antaresFlowbased"),
                    output_file = output_file,
                    params = list(set_title = paste0("Typical Day ", dayType, " (generated on ", Sys.Date(), ")")),
                    intermediates_dir = output_Dir, envir = e,
                    quiet = TRUE)
}


#' Run shiny visualisation of error
#'
#' @param fb_opts \code{list} of flowbased parameters returned by the function \link{setFlowbasedPath}. Defaut to \code{antaresFlowbased::fbOptions()}
#'
#' @import shiny manipulateWidget
#'
#' @export
runAppError <- function(fb_opts = antaresFlowbased::fbOptions()){

  dta <- readRDS(paste0(fb_opts$path, "/domainesFB.RDS"))
  G <- .GlobalEnv
  stopifnot(all(c("hour", "dayType", "outFlowBased") %in% colnames(dta)))
  assign("dtaUseByShiny", dta, envir = G)
  shiny::runApp(system.file("shinyError", package = "antaresFlowbased"),
                launch.browser = TRUE)
}




#' Run shiny visualisation of position
#'
#' @param dta \code{antaresDataList} load with \code{antaresRead}
#' @param fb_opts \code{list} of simulation parameters returned by the function \link{setSimulationPath} or fb model localisation obtain with \link{setFlowbasedPath}. Defaut to \code{antaresRead::simOptions()}
#'
#' @examples
#'
#' \dontrun{
#' opts <- antaresRead::setSimulationPath("D:/Users/titorobe/Desktop/antaresStudy", -1)
#' dta <- antaresRead::readAntares(areas = c("fr", "be", "de", "nl"),
#'                                 links = c("be - de","be - fr","be - nl","de - fr","de - nl"), mcYears = 1:10,
#'                                 select = c("LOLD", "UNSP. ENRG", "DTG MRG", "UNSP. ENRG", "BALANCE", "FLOW LIN."), opts = opts)
#' runAppPosition(dta)
#' }
#'
#' @import shiny manipulateWidget
#'
#' @export
runAppPosition <- function(dta, fb_opts = antaresRead::simOptions()){
  
  #.ctrlUserHour(opts)
  
  foldPath <- .mergeFlowBasedPath(fb_opts)
  
  countTryList <- toupper(c("fr", "be", "de", "nl"))
  dayTyList <- unique(readRDS(paste0(foldPath,"domainesFB.RDS"))$dayType)
  rangeDate <- range(dta$areas$time)
  rangeDate <- round(rangeDate, "day")
  
  G <- .GlobalEnv
  assign("dta", dta, envir = G)
  assign("countTryList", countTryList, envir = G)
  assign("dayTyList", dayTyList, envir = G)
  assign("rangeDate", rangeDate, envir = G)
  assign("fb_opts", fb_opts, envir = G)

  shiny::runApp(system.file("shinyPosition", package = "antaresFlowbased"),
                launch.browser = TRUE)
}





#' Graph function
#'
#' @param fb_opts \code{list} of simulation parameters returned by the function
#'   \link{setSimulationPath} or fb model localisation obtain with
#'   \link{setFlowbasedPath}. Defaut to \code{antaresRead::simOptions()}
#' @param data \code{antaresDataList} import with \link{readAntares}
#' @param dayType : day type, can be numeric or 'all'
#' @param hour : hour, can be numeric or 'all'
#' @param country1 : first country
#' @param country2 : second country
#' @param filteringEmptyDomains \code{boolean} filtering empty domains
#' @param nbMaxPt \code{numeric} number of point maximum on graph. Default
#'   10000.
#' @param drawNoAdqPoints \code{boolean} draw no-adq points default TRUE.
#' @param drawAdqPoints \code{boolean} draw adq points default TRUE.
#'
#'
#' @examples
#' \dontrun{
#' study <- "D:/Users/titorobe/Desktop/antaresStudy"
#'
#' opts <- antaresRead::setSimulationPath(study, 2)
#' dta <- antaresRead::readAntares(areas = c("fr", "be", "de", "nl"),
#'                                 links = c("be - de","be - fr","be - nl",
#'                                 "de - fr","de - nl"), mcYears = 1:10,
#'                                 select = c("LOLD", "UNSP. ENRG",
#'                                 "DTG MRG", "UNSP. ENRG", "BALANCE", "FLOW LIN."),
#'                                  opts = opts)
#'
#' ## plot a domain and the matching output points
#' plotNetPositionFB(fb_opts = opts,
#'          data = dta,
#'          dayType = 1, hour = c(0, 19),
#'          country1 = "BE", country2 = "FR")
#'
#' dta$areas <- dta$areas[timeId == 1]
#' ## plot a sigle idTime with all domains
#' plotNetPositionFB(fb_opts = opts,
#'          data = dta,
#'          dayType = "all", hour = 0,
#'          country1 = "BE", country2 = "FR")
#'
#' ##Filtering empty domains
#'
#' plotNetPositionFB(fb_opts = opts,
#'          data = dta,
#'          dayType = "all", hour = 0,
#'          country1 = "BE", country2 = "FR", filteringEmptyDomains = TRUE)
#'
#'
#'
#' ##See adq position
#'
#'  dta <- adqPatch(fb_opts = opts)
#'
#'
#'  ##If you want to keep only timeId with LOLD!=0 you can't use : dta$areas <- dta$areas[LOLD!=0]
#'  ##You must keep all areas for a given timestep. When you keep only raw with LOLD!=0, for a given timestep 
#'  ##you can keep only somes areas. 
#'  
#'  ##An exemple of authorized filter :
#'  idC <- c(antaresRead::getIdCols(dta$areas))
#'  idC <- idC[idC!="area"]
#'  LOLD <- dta$areas[,lapply(.SD, sum), by = idC, .SDcols = "LOLD"]
#'  LOLD <- LOLD[LOLD!=0]
#'  LOLD[,LOLD := NULL]
#'  dta$areas <- merge(dta$areas, LOLD, by =  idC)
#'  ##And filter
#'  
#'  
#'  plotNetPositionFB(fb_opts = opts,
#'          data = dta,
#'          dayType = 6, hour = 17,
#'          country1 = "BE", country2 = "FR")
#'
#'  plotNetPositionFB(fb_opts = opts,
#'          data = dta,
#'          dayType = 6, hour = 17,
#'          country1 = "DE", country2 = "FR")
#'
#'  plotNetPositionFB(fb_opts = opts,
#'          data = dta,
#'          dayType = 6, hour = 17,
#'          country1 = "BE", country2 = "FR", drawNoAdqPoints = FALSE)
#'
#'  plotNetPositionFB(fb_opts = opts,
#'          data = dta,
#'          dayType = 6, hour = 17,
#'          country1 = "BE", country2 = "FR", drawAdqPoints = FALSE)
#'
#' dta <- adqPatch(fb_opts = opts, keepOldColumns = FALSE)
#'
#'  plotNetPositionFB(fb_opts = opts,
#'          data = dta,
#'          dayType = 6, hour = 17,
#'          country1 = "BE", country2 = "FR", filteringEmptyDomains = TRUE)
#'
#' }
#'
#' @importFrom grDevices topo.colors
#' @export
plotNetPositionFB <- function( data, dayType,
                         hour, country1, country2,
                         fb_opts = antaresRead::simOptions(),
                         filteringEmptyDomains = FALSE, nbMaxPt = 10000, drawNoAdqPoints = TRUE, drawAdqPoints = TRUE){
  
  
  if(!all(c("areas", "links") %in% names(data))){
    stop("your data object must contain areas and links tables")
  }
  
  if(!"antaresDataList"%in%class(data)){
    warning("data object should be an antaresDataList, the best way to load data it's to use antaresRead. If straitment bug it's probably due to your data object")
  }
  
  ##Controle on drawNoAdqPoints & drawAdqPoints
  if((!drawNoAdqPoints) & ! (drawAdqPoints)){
    stop("You have to select at least one type of positions to visualise")
  }
  
  if(!country1%in%c("DE", "BE", "FR", "NL")){
    stop("country1 must be DE, BE, FR or NL")
  }
  if(!country2%in%c("DE", "BE", "FR", "NL")){
    stop("country2 must be DE, BE, FR or NL")
  }
  
  
  idS <- getIdCols(data$areas)
  ##Test if no-adq are present
  namesToTest <- names(data$areas)[!names(data$areas)%in%idS]
  AdqData <- noAdqData <- FALSE
  if(all(c("BALANCE_ADQPatch", "UNSP. ENRG_ADQPatch", "LOLD_ADQPatch", "DTG MRG_ADQPatch") %in%namesToTest)){
    AdqData <- TRUE
  }
  
  
  if(all(c("BALANCE", "UNSP. ENRG", "LOLD", "DTG MRG") %in%namesToTest)){
    noAdqData <- TRUE
  }
  
  if(noAdqData & drawNoAdqPoints){
    drawNoAdqPoints <- TRUE
  }else{
    drawNoAdqPoints <- FALSE
    }
  
  if(AdqData & drawAdqPoints){
    drawAdqPoints <- TRUE
  }else{
    drawAdqPoints <- FALSE
  }
  
  
  ctry1 = country1
  ctry2 = country2
  #.ctrlUserHour(opts)
  
  foldPath <- .mergeFlowBasedPath(fb_opts)
  secondM <- fread(paste0(foldPath, "second_member.txt"))
  if(!file.exists(paste0(foldPath, "scenario.txt"))){
    stop(paste0("The file scenario.txt is missing. Please either: add it to your flow-based model directory and use setFlowBasedPath(path = 'pathToDirectory') or
                use setFlowBasedPath(path = 'pathToAntaresStudy/user/flowbased')"))
  }
  scenario <- fread(paste0(foldPath, "scenario.txt"))
  ts <- fread(paste0(foldPath, "ts.txt"))
  domaines <- readRDS(paste0(foldPath, "domainesFB.RDS"))
  
  if(dayType[1] == "all")dayType <- unique(domaines$dayType)
  if(hour[1] == "all")hour <- 0:23
  
  if(!all(hour%in%0:23)){
    stop("All hour elements must be between 0 and 23 (included)")
  }
  
  if(!all(dayType %in% unique(domaines$dayType))){
    stop("Somes elements specify in dayType are not included in domainesFB.RDS file")
  }
  
  
  
  mcYears <- unique(data$areas$mcYear)
  out <- out2 <- NULL
  if(drawNoAdqPoints)
  {
  ipn <- .giveIpn(data)
  out <- .constructDataForGraph(hour = hour,
                                dayType = dayType,
                                mcYears = mcYears,
                                ipn = ipn,
                                ctry1 = ctry1,
                                ctry2 = ctry2,
                                ts = ts, 
                                domaines = domaines)
    
  }
  
  if(drawAdqPoints)
  {
  ipnADQ <- .giveIpn(data, ADQ = TRUE)
  out2 <- .constructDataForGraph(hour = hour,
                                dayType = dayType,
                                mcYears = mcYears,
                                ipn = ipnADQ,
                                ctry1 = ctry1,
                                ctry2 = ctry2,
                                ts = ts, 
                                domaines = domaines)
  
  out2 <- lapply(out2, function(WW){
    names(WW) <- gsub("Position", "Position_ADQ", names(WW))
    
    WW
    
  })
  
  }
  
  ##Remove domains on out
 
  if((!is.null(out)) & (!is.null(out2)))
  {
  out <- sapply(1:length(out), function(inc){
    cbind(out[[inc]], out2[[inc]][,3:4])
  }, simplify = FALSE, USE.NAMES = FALSE)
  }else{
    if(is.null(out)){
      out <- out2
    }
  }
  
 if(filteringEmptyDomains){
   out <- lapply(out, function(X){
     if(sum(is.na(X[,3]))==nrow(X)){
       return(NULL)
     }else{
       return(X)
     }
   })
 }
  
 nCurvByTyD <- max(unlist(lapply(out, function(X){ncol(X)})))
 
 out <- Reduce(c, out)
 
 nbpt <- sum(unlist(lapply(out, function(X){length(X[!is.na(X)])})))
 if(nbpt > nbMaxPt){
   stop(paste0("You try to draw ", nbpt, " points but you can't draw more than ", nbMaxPt, " points. You can change this limit with nbMaxPt argument but be carefull, your graph can be impossible to draw if you have soo much points."))
 }
 
 
 Mlength <- max(unlist(lapply(out, length)))
 out <- lapply(out, function(X){
   if(length(X)<Mlength){
     X <- c(X, rep(NA, Mlength-length(X)))
   }
   X
 })
 
 
 cleanNam <- gsub("_ADQ","", names(out))
 cleanNam <- cleanNam[!grepl("mcYear", cleanNam)]
 cleanNam <- cleanNam[!grepl("time", cleanNam)]
 
 stayH <- sapply(cleanNam, function(X){
   strsplit(X, "_")[[1]][3]
 })
 
 stayH <- unique(gsub("H", "", stayH))
 
 stayD <- sapply(cleanNam, function(X){
   strsplit(X, "_")[[1]][4]
 })
 
 stayD <- unique(gsub("D", "", stayD))
 
 outF <- Reduce(cbind.data.frame, out)
 outF <- data.frame(outF)
 names(outF) <- names(out)
 out <- outF
 
 
 oneOnNbC <- which(1:ncol(out)%%nCurvByTyD==1)
 allGraph <- list()
 CC <- 0
 colors <- substr(topo.colors(length(oneOnNbC)), 1,7)
 for(X in oneOnNbC){
   curvInThisLoop <- X:(X+nCurvByTyD-1)
   curvInThisLoopnoModel <- curvInThisLoop[3:length(curvInThisLoop)]
   
   CC <- CC + 1
   titleS <- substr(names(out)[X], nchar(names(out)[X])-4, nchar(names(out)[X]))
   titleS <- gsub( "H", "0",titleS)
   titleS <- paste0("Hour ", titleS)
   titleS <- gsub( "_D", " Day ",titleS)
   allGraph <- c(allGraph,
   amGraph(title = titleS, balloonText =paste0('<b>Model<br>', ctry1, '</b> : [[x]] <br><b>',ctry2, '</b> : [[y]]'),
            bullet = 'circle', xField = names(out)[X],yField = names(out)[X+1],
            lineAlpha = 1, bulletSize = 0, lineColor = colors[CC],
            lineThickness = 1, bulletAlpha = 0) )
   
   nameCurve <- names(out)[curvInThisLoopnoModel]
   adqc <- curvInThisLoopnoModel[grep("Position_ADQ", nameCurve)]
   noadqc <- curvInThisLoopnoModel[!curvInThisLoopnoModel%in%adqc]
   
   
   witchTipe <- nameCurve[grep("time_", nameCurve)]
   witchMc <- nameCurve[grep("mcYear_", nameCurve)]
   
   if(length(noadqc)>0)
   {
     
    
     
   allGraph <- c(allGraph,
   amGraph(balloonText =
             paste0('<b>Position<br>', ctry1, '</b> : [[x]] <br><b>',ctry2, '</b> : [[y]]
                                               <b>time : </b>[[', witchTipe, ']]
                    <b>mcYear : </b>[[',witchMc, ']]
                    '),
           xField = names(out)[noadqc[1]],yField = names(out)[noadqc[2]],
           lineAlpha = 0, bullet = "bubble", bulletSize = 4, lineColor = colors[CC],
           lineThickness = 1, visibleInLegend  = FALSE))
   }
   if(length(adqc)>0)
   {
   allGraph <- c(allGraph,
                 amGraph(balloonText =
                           paste0('<b>Position_ADQ<br>', ctry1, '</b> :[[x]] <br><b>',ctry2, '</b> :[[y]]
                                               <b>time : </b>[[', witchTipe, ']]
                    <b>mcYear : </b>[[',witchMc, ']]
                                  '),
                         bullet = 'triangleDown', xField = names(out)[adqc[1]],yField = names(out)[adqc[2]],
                         lineAlpha = 0, bulletSize = 4, lineColor = colors[CC],
                         lineThickness = 1, visibleInLegend  = FALSE,
                         bulletBorderColor = colors[CC], bulletAlpha = 0, bulletBorderAlpha = 1))
   
   }
   
   
 }
 
 
 g <- pipeR::pipeline(
   amXYChart(dataProvider = out),
   addTitle(text = paste0("Flow-based ", ctry1, "/", ctry2, ', hour : ', paste0(stayH, collapse = ";"), ', typical day : ', paste0(stayD, collapse = ";"))),
   setGraphs(allGraph),
   setChartCursor(),
   addValueAxes(title = paste(ctry1, "(MW)"), position = "bottom", minimum = -7000, maximum = 7000),
   addValueAxes(title =  paste(ctry2, "(MW)"), minimum = -7000, maximum = 7000),
   setExport(enabled = TRUE),
   setLegend(enabled = TRUE, switchable = FALSE),
   plot()
 )
 combineWidgets(list = list(g))
}

.giveIpn <- function(dta, ADQ = FALSE){
  if(!ADQ){
    fl <- "FLOW LIN."
  }else{
    fl <- "FLOW LIN._ADQPatch"
  }
  be <- de <- fr <- nl <- `be - de` <- `be - fr` <- `be - nl` <- `de - fr` <- `de - nl` <- lole <- value <-NULL
  `UNSP. ENRG` <- `DTG MRG` <- NULL
  
  links <- dcast(dta$links, time + mcYear~link, value.var = c(fl))
  links[, be :=  `be - de` + `be - fr` + `be - nl`]
  links[, de := - `be - de` + `de - fr` + `de - nl`]
  links[, fr :=  -`be - fr` - `de - fr`]
  links[, nl :=  -`be - nl` - `de - nl`]
  links
  links <- links[, .SD, .SDcols = c("time", "mcYear","be","de" ,"fr","nl")]
  links <- melt(links, id = 1:2)
  setnames(links, "variable", "area")
  dta$areas <- merge(dta$areas, links, by = c("time", "mcYear", "area"))
  if(!ADQ){
  dta$areas[, lole :=`UNSP. ENRG` - `DTG MRG` - value]
  }else{
    dta$areas[, lole :=`UNSP. ENRG_ADQPatch` - `DTG MRG_ADQPatch` - value]
  }
  dta$areas[, ipn := value]
  
  ipn <- dcast(dta$areas, time + mcYear~area, value.var = c("ipn"))
  
  
  
  
  
  ipn
}

.constructDataForGraph <- function(hour, dayType, mcYears, ipn, ctry1, ctry2, ts, domaines){
  out <-  sapply(hour, function(HH){
    sapply(dayType, function(DD){
      ######
      
      ipnout <- sapply(mcYears, function(mcy){
        
        ipntp <- ipn[which(hour(ipn$time)  == HH  & ipn$mcYear == mcy)]
        
        dateSel <- unlist(ts[,.SD, .SDcols = as.character(mcy)] == DD)
        dateSel <- ts$Date[dateSel]
        daysel <- substr(dateSel, 6, 10)
        
        ipntp <- ipntp[substr(ipntp$time, 6, 10) %in% daysel]
        ipntp
      }, simplify = FALSE)
      ipnO <- rbindlist(ipnout)
      # if(HH == 0)HH <- 24
      dSel <- domaines[which(dayType==DD & hour ==( HH  + 1))]
      points <- dSel$outFlowBased[[1]]$pointsY
      points$NL <-  - points$BE - points$DE - points$FR
      
      res <- data.frame("ctry1" = points[,ctry1],
                        "ctry2" = points[,ctry2])
      
      
      res2 <- data.frame("ctry11" = unlist(ipnO[, .SD, .SDcols = tolower(ctry1)]),
                         "ctry22" = unlist(ipnO[, .SD, .SDcols = tolower(ctry2)]),
                         "time" = c(ipnO[, .SD, .SDcols = "time"]),
                         "mcYear" = c(ipnO[, .SD, .SDcols = "mcYear"]))
      res2$time <- as.character(res2$time +1)
      
      ##Supress year :
      res2$time <- substr(res2$time, 6,nchar(res2$time ) -3)
      
      # primaryKey <-  data.frame("time" = unlist(ipnO[, .SD, .SDcols = "time"]),
      #                           "mcYear" = unlist(ipnO[, .SD, .SDcols = "mcYear"]))
      
      
      res <- res[chull(res),]
      res <- rbind(res, res[1,])
      res <- round(res, 2)
      
      max_r <- max(nrow(res), nrow(res2))
      if(nrow(res)<max_r){
        res <- rbind(res, data.frame(ctry1 = rep(NA, max_r-nrow(res)),
                                     ctry2 = rep(NA, max_r-nrow(res))))
      }
      if(nrow(res2)<max_r){
        res2 <- rbind(res2, data.frame(ctry11 = rep(NA,max_r- nrow(res2)),
                                       ctry22 = rep(NA, max_r-nrow(res2)),
                                       time = rep(NA, max_r-nrow(res2)),
                                       mcYear = rep(NA, max_r-nrow(res2))))
      }
      
      out2 <- cbind(res, res2)
      names(out2) <- c(paste0(c("Model", "Model", "Position", "Position"),"_", c(ctry1, ctry2), "_H", HH, "_D", DD), paste0('time',  "_H", HH, "_D", DD), paste0('mcYear',  "_H", HH, "_D", DD))
      
      data.frame(out2)
      
    }, simplify = FALSE)
  }, simplify = FALSE)
  
  
  out <- unlist(out, recursive = FALSE)
  out
}

.mergeFlowBasedPath <- function(fb_opts){
  
  if("simOptions" %in% class(fb_opts)){
    foldPath <- paste0(fb_opts$studyPath, "/user/flowbased/")
  }else if("flowBasedPath" %in% class(fb_opts)){
    foldPath <- paste0(fb_opts$path, "/")
  }else{
    stop("fb_opts must be obtain with setSimulationPath or setFlowbasedPath function")
  }
  if(!file.exists(paste0(foldPath, "second_member.txt"))){
    stop("Impossible to found second_member.txt file, you can specify fb_opts with setSimulationPath or setFlowbasedPath function")
  }
  
  foldPath
}
