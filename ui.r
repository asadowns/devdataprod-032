library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("Spam Detector"),
  sidebarPanel(
    tags$textarea(id="text", class="form-control", label = "Email Text", value="enter text here")
  ),
  mainPanel(
    textOutput('length'),
    textOutput('mean'),
    textOutput('max'),
    textOutput('sum'),
    verbatimTextOutput('test'),
    tableOutput('raw')
    
  )
))