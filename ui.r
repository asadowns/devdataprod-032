library(shiny)
library(shinyjs)

shinyUI(
  fluidPage(align="center",
    useShinyjs(),
    headerPanel("Spam Detector"),
    mainPanel(class="col-sm-offset-2",
      h3("Please type or paste the body of an email into the the text area below."),
      h5("This application will determine whether your email is a valid EMAIL or SPAM."),
      h6("The application applies a Random Forest algorithm produced from the Email Spam Data in ElemStatLearn R package to your text."),
      tags$textarea(id="text", class="form-control input-lg", label = "Email Text", value="enter text here", rows="4"),
      span('Please wait several seconds for the random forest algorithm to run.', class="help-block text-left"),
      tags$hr(),
      h1(class="jumbotron", textOutput('result', inline = TRUE)),
      h3("Details"),
      p("This data set uses the frequency of key words and characters as a fraction of total words and properties related to capital letters in your email to determine whether your email is SPAM or HAM."),
      span('Number of Words: '),
      textOutput('length',inline= TRUE)
    ),
    tableOutput('table1'),
    tableOutput('table2'),
    tableOutput('table3')
  )
)