
shinyUI(fluidPage(

  # Application title
  titlePanel("Presentation of error"),
    # Show a plot of the generated distribution
    mainPanel(width = 12,
      column(4, 
             column(6,
                    selectInput("Reports", "Reports to exports",unique(dtaUseByShiny$dayType), multiple = TRUE)),
             column(6, downloadButton("downloadData", "Export")),
             DT::dataTableOutput("tableauError")
            ),
      
      column(8,uiOutput("plots"))
    )
  )
)
