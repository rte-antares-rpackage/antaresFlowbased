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
#' @param opts \code{simOptions} load with \code{setSimulationPath}
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
runAppPosition <- function(dta, opts = antaresRead::simOptions()){
  
  .ctrlUserHour(opts)
  countTryList <- toupper(unique(dta$areas$area))
  dayTyList <- unique(readRDS(paste0(opts$studyPath, "/user/flowbased/domainesFB.RDS"))$dayType)
  rangeDate <- range(dta$areas$time)
  rangeDate <- round(rangeDate, "day")
  
  G <- .GlobalEnv
  assign("dta", dta, envir = G)
  assign("countTryList", countTryList, envir = G)
  assign("dayTyList", dayTyList, envir = G)
  assign("rangeDate", rangeDate, envir = G)
  
  shiny::runApp(system.file("shinyPosition", package = "antaresFlowbased"),
                launch.browser = TRUE)
}





#' Graph function
#' 
#' @param opts \code{list} of simulation parameters returned by the function \link{setSimulationPath}. Defaut to \code{antaresRead::simOptions()}
#' @param data \code{antaresDataList} import with \link{readAntares}
#' @param dayType : day type, can be numeric or 'all'
#' @param hour : hour, can be numeric or 'all' (format : 0:23)
#' @param country1 : first country
#' @param country2 : second country
#' @param filteringEmptyDomains \code{boolean} filtering empty domains
#' @param nbMaxPt \code{numeric} number of point maximum on graph. Default 10000.
#' 
#'
#' @examples
#' \dontrun{
#' study <- "D:/Users/titorobe/Desktop/antaresStudy"
#' 
#' opts <- antaresRead::setSimulationPath(study, -1)
#' dta <- antaresRead::readAntares(areas = c("fr", "be", "de", "nl"), 
#'                                 links = c("be - de","be - fr","be - nl",
#'                                 "de - fr","de - nl"), mcYears = 1:10,
#'                                 select = c("LOLD", "UNSP. ENRG", 
#'                                 "DTG MRG", "UNSP. ENRG", "BALANCE", "FLOW LIN."),
#'                                  opts = opts)
#' 
#' ## plot a domain and the matching output points 
#' positionViz(opts = opts, 
#'          data = dta,
#'          dayType = 1, hour = c(9, 19), 
#'          country1 = "BE", country2 = "FR")
#'          
#' dta$areas <- dta$areas[timeId == 1]
#' ## plot a sigle idTime with all domains 
#' positionViz(opts = opts, 
#'          data = dta,
#'          dayType = "all", hour = 0, 
#'          country1 = "BE", country2 = "FR")
#'          
#' ##Filtering empty domains
#' 
#' positionViz(opts = opts, 
#'          data = dta,
#'          dayType = "all", hour = 0, 
#'          country1 = "BE", country2 = "FR", filteringEmptyDomains = TRUE)
#' }
#'
#' @importFrom grDevices topo.colors
#' @export
positionViz <- function( data, dayType, hour, country1, country2, opts = antaresRead::simOptions() , filteringEmptyDomains = FALSE, nbMaxPt = 10000){
  
  ctry1 = country1
  ctry2 = country2
  .ctrlUserHour(opts)
  
  secondM <- fread(paste0(opts$studyPath, "/user/flowbased/second_member.txt"))
  scenario <- fread(paste0(opts$studyPath, "/user/flowbased/scenario.txt"))
  ts <- fread(paste0(opts$studyPath, "/user/flowbased/ts.txt"))
  domaines <- readRDS(paste0(opts$studyPath, "/user/flowbased/domainesFB.RDS"))
  
  if(dayType[1] == "all")dayType <- unique(domaines$dayType)
  if(hour[1] == "all")hour <- 0:23
  
  
  
  mcYears <- unique(data$areas$mcYear)
  ipn <- .giveIpn(data)
  
  

  
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
                         "ctry22" = unlist(ipnO[, .SD, .SDcols = tolower(ctry2)]))
      
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
                                       ctry22 = rep(NA, max_r-nrow(res2))))
      }
      
      out2 <- cbind(res, res2)
      names(out2) <- paste0(c("Model", "Model", "Position", "Position"),"_", c(ctry1, ctry2), "_H", HH, "_D", DD)
      
      data.frame(out2)
      
    }, simplify = FALSE)
  }, simplify = FALSE)
  

 out <- unlist(out, recursive = FALSE)
 
 if(filteringEmptyDomains){
   out <- lapply(out, function(X){
     if(sum(is.na(X[,3]))==nrow(X)){
       return(NULL)
     }else{
       return(X)
     }
   })
 }
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
 
 
 stayH <- sapply(names(out), function(X){
   strsplit(X, "_")[[1]][3]
 })
 
 stayH <- unique(gsub("H", "", stayH))
 
 
 stayD <- sapply(names(out), function(X){
   strsplit(X, "_")[[1]][4]
 })
 
 stayD <- unique(gsub("D", "", stayD))
 
 
 outF <- Reduce(cbind, out)
 outF <- data.frame(outF)
 names(outF) <- names(out)
 out <- outF
 oneOnFour <- which(1:ncol(out)%%4==1)
 allGraph <- list()
 CC <- 0
 colors <- substr(topo.colors(length(oneOnFour)), 1,7)
 for(X in oneOnFour){
   CC <- CC + 1
   titleS <- substr(names(out)[X], nchar(names(out)[X])-4, nchar(names(out)[X]))
   titleS <- gsub( "H", "0",titleS)
   titleS <- paste0("H", titleS)
   allGraph <- c(allGraph,
   amGraph(title = titleS, balloonText =paste0('<b>Model<br>', ctry1, '</b> :[[x]] <br><b>',ctry2, '</b> :[[y]]'),
            bullet = 'circle', xField = names(out)[X],yField = names(out)[X+1],
            lineAlpha = 1, bulletSize = 0, lineColor = colors[CC],
            lineThickness = 1, bulletAlpha = 0) )
   
   allGraph <- c(allGraph,
   amGraph(balloonText =
             paste0('<b>Position<br>', ctry1, '</b> :[[x]] <br><b>',ctry2, '</b> :[[y]]'),
           bullet = 'circle', xField = names(out)[X+2],yField = names(out)[X+3],
           lineAlpha = 0, bullet = "bubble", bulletSize = 4, lineColor = colors[CC],
           lineThickness = 1, visibleInLegend  = FALSE))
   
 }

  
  
 
 g <- pipeR::pipeline(
   amXYChart(dataProvider = out),
   addTitle(text = paste0("Flow-based ", ctry1, "/", ctry2, ', hour : ', paste0(stayH, collapse = ";"), ', typical day : ', paste0(stayD, collapse = ";"))),
   setGraphs(allGraph),
   setChartCursor(),
   addValueAxes(title = paste(ctry1, "(MW)"), position = "bottom", minimum = -7000, maximum = 7000),
   addValueAxes(title =  paste(ctry2, "(MW)"), minimum = -7000, maximum = 7000),
   setExport(enabled = TRUE, switchable = FALSE),
   setLegend(enabled = TRUE)
 )
 combineWidgets(list = list(g%>%plot()))
}

.giveIpn <- function(dta){
  be <- de <- fr <- nl <- `be - de` <- `be - fr` <- `be - nl` <- `de - fr` <- `de - nl` <- lole <- value <-NULL
  `UNSP. ENRG` <- `DTG MRG` <- NULL
  
  links <- dcast(dta$links, time + mcYear~link, value.var = c("FLOW LIN."))
  links[, be :=  `be - de` + `be - fr` + `be - nl`]
  links[, de := - `be - de` + `de - fr` + `de - nl`]
  links[, fr :=  -`be - fr` - `de - fr`]
  links[, nl :=  -`be - nl` - `de - nl`]
  links
  links <- links[, .SD, .SDcols = c("time", "mcYear","be","de" ,"fr","nl")]
  links <- melt(links, id = 1:2)
  setnames(links, "variable", "area")
  dta$areas <- merge(dta$areas, links, by = c("time", "mcYear", "area"))
  dta$areas[, lole :=`UNSP. ENRG` - `DTG MRG` - value]
  dta$areas[, ipn := value]
  
  ipn <- dcast(dta$areas, time + mcYear~area, value.var = c("ipn"))
  ipn
}
