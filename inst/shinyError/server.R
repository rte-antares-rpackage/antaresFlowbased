
shinyServer(function(input, output) {

  data_error <- reactive({
    giveTableError(dtaUseByShiny)
  })

  output$mean_inf_error <- renderText({
    paste0("Mean Inf error : ", round(mean(data_error()$Inf_error), 1))
  })

  output$mean_sup_error <- renderText({
    paste0("Mean Sup error : ", round(mean(data_error()$Sup_error), 1))
  })

  output$tableauError <- DT::renderDataTable({
    data_error <- data_error()

    params_length <- c(10, 25, 50)
    DT::datatable(data_error, filter = 'top', options = list(pageLength = 10,
                                                             lengthMenu = c(params_length[params_length < nrow(data_error)], nrow(data_error))),
                  rownames = FALSE)
  })


  output$downloadData <- downloadHandler(
    filename = function() {
      paste('Errordata-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.table(data_error(), con, sep = ";", dec = ",", row.names = FALSE)
    }
  )

  observe({
    output$plots <- renderUI({ get_plot_output_list(dtaUseByShiny, input$tableauError_rows_selected) })
  })

  output$downloadReport<- downloadHandler(
    filename = function() { paste("Error.zip", sep='') },
    content = function(file) {
      sapply(input$Reports, function(X){
        generateReportFb(allFB = dtaUseByShiny, dayType = X)
      })
      filtFiles <- paste0("Flowbased_TD",input$Reports, "_", Sys.Date(), ".html")

      temp <- zip(file,files = filtFiles)
      file.remove(filtFiles)
      temp
    }
  )


})
