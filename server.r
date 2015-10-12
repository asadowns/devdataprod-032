library(shiny)
library(shinyjs)
library(stringr)
library(randomForest)
library(ElemStatLearn)

set.seed(101)
controls <- trainControl(method="cv", 5, allowParallel = TRUE)
model <- randomForest(spam ~ ., trcontrol=controls, data=spam, ntree=500)

shinyServer(
  function(input, output) {

    numWords <- reactive({
      words <- str_match_all(input$text, "\\W?[a-zA-Z0-9]+\\W?")
      length(words[[1]])
    })
    
    emailObj <- reactive({
      words <- c()
      words <- str_match_all(input$text, "\\W?[a-zA-Z0-9]+\\W?")
      capitals <- c()
      
      for (i in 1:length(words[[1]])) {
        capitals <- c(capitals,str_length(str_match(words[[1]][i],"[A-Z]{1,}")))
      }
      
      capitals <- capitals[!is.na(capitals)]
      lowerInput <- tolower(input$text)
      newEmail <- list()
      newEmail$A.1 <- 100*str_count(lowerInput,"\\W?make\\W?")/numWords()
      newEmail$A.2 <- 100*str_count(lowerInput,"\\W?address\\W?")/numWords()
      newEmail$A.3 <- 100*str_count(lowerInput,"\\W?all\\W?")/numWords()
      newEmail$A.4 <- 100*str_count(lowerInput,"\\W?3d\\W?")/numWords()
      newEmail$A.5 <- 100*str_count(lowerInput,"\\W?our\\W?")/numWords()
      newEmail$A.6 <- 100*str_count(lowerInput,"\\W?over\\W?")/numWords()
      newEmail$A.7 <- 100*str_count(lowerInput,"\\W?remove\\W?")/numWords()
      newEmail$A.8 <- 100*str_count(lowerInput,"\\W?internet\\W?")/numWords()
      newEmail$A.9 <- 100*str_count(lowerInput,"\\W?order\\W?")/numWords()
      newEmail$A.10 <- 100*str_count(lowerInput,"\\W?mail\\W?")/numWords()
      newEmail$A.11 <- 100*str_count(lowerInput,"\\W?receive\\W?")/numWords()
      newEmail$A.12 <- 100*str_count(lowerInput,"\\W?will\\W?")/numWords()
      newEmail$A.13 <- 100*str_count(lowerInput,"\\W?people\\W?")/numWords()
      newEmail$A.14 <- 100*str_count(lowerInput,"\\W?report\\W?")/numWords()
      newEmail$A.15 <- 100*str_count(lowerInput,"\\W?addresses\\W?")/numWords()
      newEmail$A.16 <- 100*str_count(lowerInput,"\\W?free\\W?")/numWords()
      newEmail$A.17 <- 100*str_count(lowerInput,"\\W?business\\W?")/numWords()
      newEmail$A.18 <- 100*str_count(lowerInput,"\\W?email\\W?")/numWords()
      newEmail$A.19 <- 100*str_count(lowerInput,"\\W?you\\W?")/numWords()
      newEmail$A.20 <- 100*str_count(lowerInput,"\\W?credit\\W?")/numWords()
      newEmail$A.21 <- 100*str_count(lowerInput,"\\W?your\\W?")/numWords()
      newEmail$A.22 <- 100*str_count(lowerInput,"\\W?font\\W?")/numWords()
      newEmail$A.23 <- 100*str_count(lowerInput,"\\W?000\\W?")/numWords()
      newEmail$A.24 <- 100*str_count(lowerInput,"\\W?money\\W?")/numWords()
      newEmail$A.25 <- 100*str_count(lowerInput,"\\W?hp\\W?")/numWords()
      newEmail$A.26 <- 100*str_count(lowerInput,"\\W?hpl\\W?")/numWords()
      newEmail$A.27 <- 100*str_count(lowerInput,"\\W?george\\W?")/numWords()
      newEmail$A.28 <- 100*str_count(lowerInput,"\\W?650\\W?")/numWords()
      newEmail$A.29 <- 100*str_count(lowerInput,"\\W?lab\\W?")/numWords()
      newEmail$A.30 <- 100*str_count(lowerInput,"\\W?labs\\W?")/numWords()
      newEmail$A.31 <- 100*str_count(lowerInput,"\\W?telnet\\W?")/numWords()
      newEmail$A.32 <- 100*str_count(lowerInput,"\\W?857\\W?")/numWords()
      newEmail$A.33 <- 100*str_count(lowerInput,"\\W?data\\W?")/numWords()
      newEmail$A.34 <- 100*str_count(lowerInput,"\\W?415\\W?")/numWords()
      newEmail$A.35 <- 100*str_count(lowerInput,"\\W?85\\W?")/numWords()
      newEmail$A.36 <- 100*str_count(lowerInput,"\\W?technology\\W?")/numWords()
      newEmail$A.37 <- 100*str_count(lowerInput,"\\W?1999\\W?")/numWords()
      newEmail$A.38 <- 100*str_count(lowerInput,"\\W?parts\\W?")/numWords()
      newEmail$A.39 <- 100*str_count(lowerInput,"\\W?pm\\W?")/numWords()
      newEmail$A.40 <- 100*str_count(lowerInput,"\\W?direct\\W?")/numWords()
      newEmail$A.41 <- 100*str_count(lowerInput,"\\W?cs\\W?")/numWords()
      newEmail$A.42 <- 100*str_count(lowerInput,"\\W?meeting\\W?")/numWords()
      newEmail$A.43 <- 100*str_count(lowerInput,"\\W?original\\W?")/numWords()
      newEmail$A.44 <- 100*str_count(lowerInput,"\\W?project\\W?")/numWords()
      newEmail$A.45 <- 100*str_count(lowerInput,"\\W?re\\W?")/numWords()
      newEmail$A.46 <- 100*str_count(lowerInput,"\\W?edu\\W?")/numWords()
      newEmail$A.47 <- 100*str_count(lowerInput,"\\W?table\\W?")/numWords()
      newEmail$A.48 <- 100*str_count(lowerInput,"\\W?conference\\W?")/numWords()
      newEmail$A.49 <- 100*str_count(input$text,";")/numWords()
      newEmail$A.50 <- 100*str_count(input$text,"\\(")/numWords()
      newEmail$A.51 <- 100*str_count(input$text,"\\[")/numWords()
      newEmail$A.52 <- 100*str_count(input$text,"!")/numWords()
      newEmail$A.53 <- 100*str_count(input$text,"\\$")/numWords()
      newEmail$A.54 <- 100*str_count(input$text,"#")/numWords()
      newEmail$A.55 <- ifelse(length(capitals)>0, mean(capitals), 0)
      newEmail$A.56 <- ifelse(length(capitals)>0, max(capitals), 0)
      newEmail$A.57 <- ifelse(length(capitals)>0, sum(capitals), 0)
      newEmail$spam <- 'PROCESSING'
      
      if (!any(is.na(newEmail))) newEmail$spam <- toupper(toString(predict(model, newdata=newEmail)))
      observe({
        if (newEmail$spam == 'SPAM') {
          shinyjs::addClass("result", "text-danger")
          shinyjs::removeClass("result", "text-success")
        } else if (newEmail$spam == 'EMAIL'){
          shinyjs::addClass("result", "text-success")
          shinyjs::removeClass("result", "text-danger")        
          }
      })
      
      newEmail
    })
    
    outputObj <- reactive({
      output <- as.data.frame(emailObj())
      colnames(output) <- c('make','address','all','3d','our','over','remove','internet','order','mail','receive','will','people','report','addresses','free','business','email','you','credit','your','font','000','money','hp','hpl','george','650','lab','labs','telnet','857','data','415','85','technology','1999','parts','pm','direct','cs','meeting','original','project','re','edu','table','conference',';','(','[','!','$','#','capital_average','capital_longest','capital_total', 'spam')
      rownames(output) <- c('email')
      output
    })

    output$length <- renderText({numWords()})
    output$result <- renderText({emailObj()$spam})
    output$table1 <- renderTable({outputObj()[1:19]})
    output$table2 <- renderTable({outputObj()[20:40]})
    output$table3 <- renderTable({outputObj()[41:57]})
  }
)