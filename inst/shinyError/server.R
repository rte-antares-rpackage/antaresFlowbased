
shinyServer(function(input, output) {
  
  output$tableauError <- DT::renderDataTable({
    dta2 <- giveTableError(dtaUseByShiny)
    names(dta2)[3] <- paste0("Inf error", " (", round(mean(dta2$error1), 1), " )")
    names(dta2)[4] <- paste0("Sup error", " (", round(mean(dta2$error2), 1), " )")
    
    DT::datatable(dta2,options = list(pageLength = 30000, dom = 't'), rownames = FALSE)
  })
  
  
    # plotOutoutList <- list(  
    #   
    #   
    # )
    
    observe({
      output$plots <- renderUI({ get_plot_output_list(dtaUseByShiny, input$tableauError_rows_selected) })
    })
  

    
    output$downloadData <- downloadHandler(
      filename = function() { paste("Error.zip", sep='') },
      content = function(file) {
        sapply(input$Reports, function(X){
          generateRaportFb(dtaUseByShiny, X)
        })
        filtFiles <- paste0("Flowbased_TD",input$Reports, "_", Sys.Date(), ".html")
    
        temp <- zip(file,files = filtFiles)
        file.remove(filtFiles)
        temp
      }
    )

  
})
