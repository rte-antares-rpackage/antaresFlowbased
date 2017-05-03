#' Plot 2D for flowbased areas
#'
#' @param flowbased \code{list}, flowbased outout obtain which cumputeFB function
#' @param ctry1 \code{character}, country in X
#' @param ctry2 \code{character}, country in Y
#'
#' @import rAmCharts
#'
#' @export
graphFlowBased2D <- function(flowbased, ctry1, ctry2)
{
  
  if(ctry2 == "NL"){
    ptctry2 <- -rowSums(flowbased$pointsY)
    ptctry2X <- -rowSums(flowbased$pointX)
  }else{
    ptctry2 <- flowbased$pointsY[[ctry2]]
    ptctry2X <- flowbased$pointX[[ctry2]]
  }
  
  res <- data.frame(ctry1 = flowbased$pointsY[[ctry1]], 
                    ctry2 = ptctry2)
  res <- res[chull(res),]
  res <- rbind(res, res[1,])
  res2 <- data.frame(ctry1 = flowbased$pointX[[ctry1]], 
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
  names(out) <- c("Mctry1", "Mctry2", "Rctry1", "Rctry2")
  
  out <- round(out, 2)
  
  
  pipeR::pipeline(
    amXYChart(dataProvider = out),
    addGraph(title = "Modélisé", balloonText = 
               paste0('<b>Modélisé<br>', ctry1, '</b> :[[x]] <br><b>',ctry2, '</b> :[[y]]'),
             
             bullet = 'circle', xField = 'Mctry1',yField = 'Mctry2',
             lineAlpha = 1, bulletAlpha = 0),
    addGraph(title = "Réel",balloonText =    
               paste0('<b>Réel<br>', ctry1, '</b> :[[x]] <br><b>',ctry2, '</b> :[[y]]'),
             bullet = 'circle', xField = 'Rctry1',yField = 'Rctry2',
             lineAlpha = 1, bulletAlpha = 0),
    setChartCursor(),
    addValueAxes(title = ctry1, minimum = -7000, maximum = 7000, unit = " MW"),
    addValueAxes(title = ctry2, position = "bottom", minimum = -7000, maximum = 7000, unit = " MW"),
    setExport(enabled = TRUE),
    setLegend(enabled = TRUE)
    
  )
  
}

#' Generate html report
#'
#' @param allFB \code{list}, object obtain which computeFB function
#' @param hour \code{numeric}, hour
#' @param dayType \code{numeric}, dayType
#'
#' @import rmarkdown
#' @import flexdashboard
#' 
#' @export
generateRaportFb <- function(allFB, hour, dayType){
  hourS <- hour
  dayTypeS <- dayType
  output_file <- getwd()
  output_file <- paste0(output_file, "/", "FlowBased_H", hour, "DT",dayType, "_", Sys.Date(), ".html")
  e <- environment()
  e$hour <- hour
  e$dayType <- dayType
  e$dta <- allFB[hour == hourS & dayType == dayTypeS]$outFlowBased[[1]]
  
 
  
  
  rmarkdown::render(system.file("/dev/resumeFBflex.Rmd", package = "antaresFlowbased"),
                    output_file = output_file,
                    intermediates_dir = getwd(), envir = e)
}
