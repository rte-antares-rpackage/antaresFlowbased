library(shiny)
library(antaresFlowbased)
library(DT)
library(data.table)
library(rAmCharts)
library(manipulateWidget)
shinyUI(fluidPage(

  # Application title
  titlePanel(div("Presentation of position", align = "center"), windowTitle = "Presentation of position "),
  # Show a plot of the generated distribution
  column(12, align="center",
    column(2),
    column(2,selectInput("h", "hours", 0:23, multiple = TRUE, selected = 19)),
    column(2,selectInput("d", "dayType", dayTyList, multiple = TRUE, selected = 1)),
    
    column(3,dateRangeInput("dateR", "Range dates", start = rangeDate[1], end = rangeDate[2],
                   min = rangeDate[1], max = rangeDate[2]))
    

   
  ),
  
  
  column(6,  align="center",
         column(6,selectInput("ctry1G1", "Frist country, graph 1", countTryList)),
         column(6, selectInput("ctry2G1", "Second country, graph 1", countTryList, selected = countTryList[2]))),
  column(6,   align="center",  
         column(6,selectInput("ctry1G2", "Frist country, graph 2", countTryList)),
         column(6,selectInput("ctry2G2", "Second country, graph 2", countTryList, selected = countTryList[2]))),
  
  
  
  mainPanel(
    column(6,amChartsOutput("poVi",  height = "600px")),
    column(6,amChartsOutput("poVi2",  height = "600px")) , width = 12 )
))
