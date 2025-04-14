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
  tags$style(
    ".chatbox {
      border: 1px solid #blue;
      border-radius: 5px;
      padding: 10px;
      height: 300px;
      overflow-y: auto;
      background-color: #f9f9f9;
      }"
  ),
  sidebarLayout(
    sidebarPanel(
      textInput("user_input", "Type your message:", ""),
      actionButton("send_button", "Send")
    ),
    mainPanel(uiOutput("chatbox"))
  )
)

server <- function(input, output, session) {
  chat_history <- reactiveVal("")
  
  observeEvent(input$send_button, {
    user_message <- input$user_input
    ai_response <- paste("Echo:", user_message)
    chat <- chat_openai(
      system_prompt =
        "You are an excellent research assistant. You provide detailed responses, but are succinct in your answers. You do not need to repeat the questions you are asked or qualify them as good/bad questions.",
      base_url = "https://api.openai.com/v1",
      api_key = "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",
      echo = "all"
    )
    
    resp <- chat$chat(input$user_input)
    
    chat_history(paste(
      chat_history(),
      "You:",
      user_message,
      "\nAI:",
      resp,
      "\n",
      sep = "\n"
    ))
    
    updateTextInput(session, "user_input", value = "")
  })
  
  output$chatbox <- renderUI({
   verbatimTextOutput("chat_text")
  })
  
  output$chat_text <- renderText({
    chat_history()
  })
}

shinyApp(ui, server)