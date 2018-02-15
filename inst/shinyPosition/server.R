library(shiny)
library(antaresFlowbased)
library(DT)
library(data.table)
library(rAmCharts)
library(manipulateWidget)
shinyServer(function(input, output, session) {
  observe({
  updateSelectInput(session, inputId = "ctry2G1", choices = {
    countTryList[!countTryList%in%input$ctry1G1]
  }, selected = ifelse(input$ctry2G1 %in% countTryList[!countTryList%in%input$ctry1G1],
                       input$ctry2G1 , 
                       countTryList[!countTryList%in%input$ctry1G1][1]))
  })
  
  
  observe({
    updateSelectInput(session, inputId = "ctry2G2", choices = {
      countTryList[!countTryList%in%input$ctry1G2]
    }, selected = ifelse(input$ctry2G2 %in% countTryList[!countTryList%in%input$ctry1G2],
                         input$ctry2G2 , 
                         countTryList[!countTryList%in%input$ctry1G2][1]))
  })
  
  
  selectData <- reactive({
 
    if(!is.null(input$dateR)){

    out <- dta
    
    out$areas <- dta$areas[time %between% input$dateR]
    
    out$links <- dta$links[time %between% input$dateR]
    
    }else{
      out <- dta
    }
    
    
    out
  })
  
  convertD <- reactive({
    as.numeric(input$d)
  })
  convertH <- reactive({
    as.numeric(input$h)
  })
  output$poVi <- renderAmCharts({
  positionViz(opts = opts,
              data = selectData(),
              dayType = convertD(), hour = convertH(),
              ctry1 = input$ctry1G1, ctry2 = input$ctry2G1)
  
  })
  
  output$poVi2 <- renderAmCharts({
    positionViz(opts = opts,
                data = selectData(),
                dayType = convertD(), hour = convertH(),
                ctry1 = input$ctry1G2, ctry2 = input$ctry2G2)
    
  })

})