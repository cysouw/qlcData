library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("IPA check"),

  # Sidebar
  sidebarLayout(
    sidebarPanel(
      
      helpText("Check you text with the following profile:"),
      
      radioButtons("choice"
          , label = "Choose Profile"
          , choices = list( "strict IPA" = "strict IPA"
                          , "valid IPA" = "valid IPA"
                          )
          , selected = "strict IPA"
      )           
    ),

    # Show result
    mainPanel(
      textOutput("out")
    )
  )
))
