#' Plot 2D for flowbased areas
#'
#' @param flowbased \code{list}, flowbased outout obtain which computeFB function
#' @param ctry1 \code{character}, country in X
#' @param ctry2 \code{character}, country in Y
#' @param hour \code{numeric}, hour
#' @param dayType \code{numeric}, dayType
#'
#' @import rAmCharts
#'
#' @export
graphFlowBased2D <- function(flowbased, ctry1, ctry2, hour = NULL, dayType = NULL)
{

  
  if(!is.null(hour)){
    hour <- paste0(" Hour ", hour)
  }
  
  
  if(!is.null(dayType)){
    dayType <- paste0(" Typical day ", dayType)
  }
  
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
    addTitle(text = paste0("Flowbased ", ctry1, "/", ctry2, hour, dayType)),
    addGraph(title = "Modélisé", balloonText =
               paste0('<b>Modélisé<br>', ctry1, '</b> :[[x]] <br><b>',ctry2, '</b> :[[y]]'),

             bullet = 'circle', xField = 'Mctry2',yField = 'Mctry1',
             lineAlpha = 1, bullet = "bubble", bulletSize = 4, lineColor = "#0101DF",
             lineThickness = 1),
    addGraph(title = "Réel",balloonText =
               paste0('<b>Réel<br>', ctry1, '</b> :[[x]] <br><b>',ctry2, '</b> :[[y]]'),
             bullet = 'circle', xField = 'Rctry2',yField = 'Rctry1',
             lineAlpha = 1, bullet = "bubble", bulletSize = 4, lineColor = "#FF8000",
             lineThickness = 1,  dashLength = 7),
    setChartCursor(),
    addValueAxes(title = paste(ctry1, "(MW)"), position = "bottom", minimum = -7000, maximum = 7000),
    addValueAxes(title =  paste(ctry2, "(MW)"), minimum = -7000, maximum = 7000),
    setExport(enabled = TRUE),
    setLegend(enabled = TRUE)
  )

}

#' Generate html report
#'
#' @param allFB \code{list}, object obtain which computeFB function
#' @param dayType \code{numeric}, dayType
#'
#' @import rmarkdown flexdashboard rAmCharts manipulateWidget
#'
#' @examples
#'
#' \dontrun{
#' allFB <- computeFB(dayType = 7)
#' generateRaportFb(allFB, dayType = 7)
#' }
#' @export
generateRaportFb <- function(allFB, dayType){

  output_file <- getwd()
  output_file <- paste0(output_file, "/", "FlowBased_DT",dayType, "_", Sys.Date(), ".html")
  e <- environment()
  e$dayType <- dayType
  e$dta <- allFB[dayType == dayType]

  rmarkdown::render(system.file("/dev/resumeFBflex.Rmd", package = "antaresFlowbased"),
                    output_file = output_file,
                    params = list(set_title = paste0("Typical Day ", dayType, " (generated on ", Sys.Date(), ")")),
                    intermediates_dir = getwd(), envir = e)
}


#' Generate html report
#'
#' @param dta \code{list}, object obtain which computeFB function
#' @import DT shiny
#' @export
runAppError <- function(dta){
  G <- .GlobalEnv
  assign("dtaUseByShiny", dta, envir = G)
  shiny::runApp(system.file("shinyError", package = "antaresFlowbased"))
}