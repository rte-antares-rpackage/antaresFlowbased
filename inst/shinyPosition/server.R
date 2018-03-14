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
    
    dateS <- input$dateR
    dateS[2] <-   dateS[2] + 1
    
    out$areas <- dta$areas[time >= dateS[1] & time < dateS[2]]
    out$links <- dta$links[time >= dateS[1] & time < dateS[2]]
    
    }else{
      out <- dta
    }
    
    
    out
  })
  
  convertD <- reactive({
    
    d <- as.numeric(input$d)
    if(input$dAll){
      d <- "all"
    }
    d
  })
  convertH <- reactive({
    h <- as.numeric(input$h)
    
    if(input$hAll){
      h <- "all"
    }
    h
  })
  output$poVi <- renderCombineWidgets({
    input$go
    isolate({
    plotNetPositionFB(fb_opts = fb_opts,
              data = selectData(),
              dayType = convertD(), hour = convertH(),
              country1 = input$ctry1G1, country2 = input$ctry2G1, filteringEmptyDomains = input$filteringEmptyDomains,drawPositionsBeforeAdqP = input$nrm,drawPositionsAdqP = input$adq, palette = input$col)
    })
  })
  
  output$poVi2 <- renderCombineWidgets({
    input$go
    isolate({
    plotNetPositionFB(fb_opts = fb_opts,
                data = selectData(),
                dayType = convertD(), hour = convertH(),
                country1 = input$ctry1G2, country2 = input$ctry2G2, filteringEmptyDomains = input$filteringEmptyDomains,drawPositionsBeforeAdqP = input$nrm,drawPositionsAdqP = input$adq, palette = input$col)
    })
  })

})
