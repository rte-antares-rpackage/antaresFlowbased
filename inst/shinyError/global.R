
library(shiny)
library(antaresFlowbased)
library(DT)
library(data.table)
library(rAmCharts)
library(manipulateWidget)


#dta <- readRDS("../../outModel.RDS")


giveTableError <- function(dta){
  dta$error1 <- round(unlist(lapply(dta$outFlowBased, function(X)X$error$error1)), 3)
  dta$error2 <- round(unlist(lapply(dta$outFlowBased, function(X)X$error$error2)), 3)
  dta$outFlowBased <- NULL
  dta
}
#   graphFlowBased2D( dta$outFlowBased[[input$tableauError_rows_selected]], "BE", "FR"),
#   graphFlowBased2D( dta$outFlowBased[[input$tableauError_rows_selected]], "DE", "FR"),
#   graphFlowBased2D( dta$outFlowBased[[input$tableauError_rows_selected]], "BE", "NL"),
#   graphFlowBased2D( dta$outFlowBased[[input$tableauError_rows_selected]], "DE", "NL")


get_plot_output_list <- function(dta, input_n) {
  # Insert plot output objects the list
  plot_output_list <- lapply(input_n, function(i) {
    plotname <- paste("plot1", i, sep="")
    plot_output_object <- combineWidgetsOutput(plotname)
    plot_output_object <- renderCombineWidgets({
      combineWidgets(
      plot(graphFlowBased2D(dta$outFlowBased[[i]], "BE", "FR", hour = dta$hour[i], dayType = dta$dayType[i] )),
      plot(graphFlowBased2D( dta$outFlowBased[[i]], "DE", "FR", hour = dta$hour[i], dayType = dta$dayType[i])),
      plot(graphFlowBased2D( dta$outFlowBased[[i]], "BE", "NL", hour = dta$hour[i], dayType = dta$dayType[i])),
      plot(graphFlowBased2D( dta$outFlowBased[[i]], "DE", "NL", hour = dta$hour[i], dayType = dta$dayType[i]))
      )
    })
    attributes(plot_output_object)$outputFunc <- function (outputId, width = "100%", height = "1000px") 
    {
      htmlwidgets::shinyWidgetOutput(outputId, "combineWidgets", 
                                     width, height, package = "manipulateWidget")
    }
   # print(attributes(plot_output_object)$outputFunc)
    
    plot_output_object
  })
  
  
  do.call(tagList, plot_output_list) # needed to display properly.
  
  return(plot_output_list)
}