
shinyServer(function(input, output) {
  
  output$tableauError <- DT::renderDataTable({
    DT::datatable(giveTableError(dtaUseByShiny),options = list(pageLength = 30000, dom = 't'), rownames = FALSE)
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
        filtFiles <- paste0("Flowbased_DT",input$Reports, "_", Sys.Date(), ".html")
    
        temp <- zip(file,files = filtFiles)
        file.remove(filtFiles)
        temp
      }
    )

  
})
