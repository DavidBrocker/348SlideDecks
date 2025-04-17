# library(shiny)
# library(dplyr)
# library(bslib)
# library(RKaggle)
# library(ggplot2)
# library(gt)
# library(gtsummary)
# library(forcats)
# library(ellmer)
# library(shinychat)
# library(pdftools)
# library(bslib)
# 
# ui <- 
#   page_sidebar(
#     title = 
#       tagList(
#         div(
#           h2("Teaching Assitiant Intelligence (TAi)"),
#           h6(em("Your Smartest Study Partner"))
#         )
#   ),
#   sidebar = 
#     sidebar(
#       width= "400px",
#         chat_ui(
#           "tai_chat",
#           fill = TRUE,
#           placeholder = "How can I help you?"),
#         fileInput("fileUpload", "Upload Document")
#       ),
#   layout_columns(
# 
#   )
# )
# 
# server <- function(input, output, session) {
# 
#   
#   observeEvent(input$tai_chat_user_input, {
#     user_message <- input$chats
#     ai_response <- paste("Echo:", user_message)
#     chat <- chat_openai(
#       system_prompt =
#         "You are an excellent research assistant. You provide detailed responses, but are succinct in your answers. You do not need to repeat the questions you are asked or qualify them as good/bad questions.",
#       base_url = "https://api.openai.com/v1",
#       echo = "all"
#      
#     
#     response <- paste0(
#       "<blockquote>",
#       htmltools::htmlEscape(chat$chat(input$tai_chat_user_input)),
#       "</blockquote>"
#     )
#     chat_append("tai_chat", response)
#   
#   })
# }
# 
# shinyApp(ui, server)