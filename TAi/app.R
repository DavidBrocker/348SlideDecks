library(shiny)
library(dplyr)
library(bslib)
library(RKaggle)
library(ggplot2)
library(gt)
library(gtsummary)
library(forcats)
library(ellmer)


ui <- 
  fluidPage(
        titlePanel("Chat Interface"),
        sidebarLayout(
            sidebarPanel(
                textInput("user_input", "Type your 
 message:", ""),
                actionButton("send_button", "Send")
              ),
            mainPanel(
              uiOutput("chat_output")
              )
          )
      )
    
 
server <- function(input, output, session) {
      chat_history <- reactiveVal("")
      
      observeEvent(input$send_button, {
        user_message <- input$user_input
        ai_response <- paste("Echo:", user_message)
        # Placeholder for AI response logic
        chat <- 
          chat_openai(
          system_prompt = 
            "You are an excellent research assistant. 
    You provide detailed responses, but are succint 
    in your answers. You do not need to repeat the 
    questions you are asked or qualify them as 
    good/bad questions.", 
          base_url = "https://api.openai.com/v1", 
          api_key = "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"
,  
          echo = "all")
        
        resp <- chat$chat(input$user_input)
        
        # Update chat history
        chat_history(paste(chat_history(), "You:", 
 user_message, "\nAI:", resp, "\n", sep = 
 "\n"))
        
        # Clear input
        updateTextInput(session, "user_input", value
 = "")
      })
      
      output$chat_output <- renderUI({
        verbatimTextOutput("chat_text")
      })
 
      output$chat_text <- renderText({
        chat_history()
      })
    }
    
shinyApp(ui, server)