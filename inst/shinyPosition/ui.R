library(shiny)
library(antaresFlowbased)
library(DT)
library(data.table)
library(rAmCharts)
library(manipulateWidget)
library(shinyWidgets)
shinyUI(fluidPage(

  # Application title
  titlePanel(div("Presentation of position", align = "center"), windowTitle = "Presentation of position "),
  # Show a plot of the generated distribution
  column(12, align="center",
    column(1,
           dropdownButton(label ="Hours", status = "info",circle = FALSE,
    checkboxInput("hAll", "All",FALSE),
    conditionalPanel("!input.hAll", {
    selectInput("h", "Somes", 0:23, multiple = TRUE, selected = 19)
    }))
    ),
    
    
    column(1,
           dropdownButton(label ="dayTypes", status = "info",circle = FALSE,
                          checkboxInput("dAll", "All",FALSE),
                          conditionalPanel("!input.dAll", {
                            selectInput("d", "dayType", dayTyList, multiple = TRUE, selected = 1)
                          
                          }))
    ),
    
    
    column(2,checkboxInput("nrm", "See no ADQpatch points",TRUE)),
    column(2,checkboxInput("adq", "See ADQpatch points",TRUE)),
    column(3,dateRangeInput("dateR", "Range dates", start = rangeDate[1], end = rangeDate[2],
                   min = rangeDate[1], max = rangeDate[2])),
    column(2, 
           checkboxInput("filteringEmptyDomains", "filtering Empty Domains", FALSE)
           )
    
    

   
  ),
  
  
  column(6,  align="center",
         column(6,selectInput("ctry1G1", "Frist country, graph 1", countTryList)),
         column(6, selectInput("ctry2G1", "Second country, graph 1", countTryList, selected = countTryList[2]))),
  column(6,   align="center",  
         column(6,selectInput("ctry1G2", "Frist country, graph 2", countTryList)),
         column(6,selectInput("ctry2G2", "Second country, graph 2", countTryList, selected = countTryList[2]))),
  

  column(12, 

         actionButton("go", "Refresh"),style = 'text-align: center'
    ),
  
  mainPanel(
    column(6,combineWidgetsOutput("poVi",  height = "600px")),
    column(6,combineWidgetsOutput("poVi2",  height = "600px")) , width = 12 )
))
