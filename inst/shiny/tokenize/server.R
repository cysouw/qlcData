library(shiny)

# load libraries, source-files and data here
library(qlcData)

shinyServer(function(input, output) {
  
  output$out <- renderText({

    paste("You have selected", input$choice)

  })

})
