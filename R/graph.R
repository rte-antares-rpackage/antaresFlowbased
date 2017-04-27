#' Plot 2D for flowbased areas
#'
#' @param flowbased \code{list}, flowbased outout obtain which cumputeFB function
#' @param ctry1 \code{character}, country in X
#' @param ctry2 \code{character}, country in Y
#'
#' @import pipeR
#'
#' @export
graphFlowBased2D <- function(flowbased, ctry1, ctry2)
{
  
  res <- data.frame(ctry1 = flowbased$pointsY[[ctry1]], 
                    ctry2 = flowbased$pointsY[[ctry2]])
  res <- res[chull(res),]
  res2 <- data.frame(ctry1 = flowbased$pointX[[ctry1]], 
                     ctry2 = flowbased$pointX[[ctry2]])
  res2 <- res2[chull(res2),]
  res$Domaine <- "Modélisé"
  res2$Domaine <- "Réel"
  res <- rbind(res, res2)
  library(ggplot2)
  plot <- ggplot(data = res, aes(x = ctry1, y = ctry2, colour=Domaine, fill = Domaine)) +
    geom_polygon(data = res, alpha = 0.5) +
    labs(x = ctry1, y = ctry2) + 
    ylim(-7000, 7000) +
    xlim(-7000, 7000)
  plot
}

#' Generate html report
#'
#' @param allFB \code{list}, object obtain which computeFB function
#' @param hour \code{numeric}, hour
#' @param dayType \code{numeric}, dayType
#'
#' @import rmarkdown
#' @import flexdashboard
#' @import ggplot2
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
