library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)

# Define UI for app that draws a histogram ----
ui <- page_sidebar(
  
  # App title ----
  title = "Correlation Simulator",
  
  # Sidebar panel for inputs ----
  sidebar = sidebar(
    
    numericInput(
      inputId = "num",
      label = "Samples (n)",
      value = 30,
      min = 1,
      max = 10000,
      step = 1,
    ),
    
    numericInput(
      inputId = "meanx",
      label = "Mean(x)",
      value = 5,
      min = 1,
      max = 10000,
      step = 1,
    ),
    
    numericInput(
      inputId = "meany",
      label = "Mean(y)",
      value = 5,
      min = 1,
      max = 10000,
      step = 1,
    ),
    
  ),
  plotOutput(outputId = "corPlot")
)

server <- function(input, output) {
  
  output$corPlot <- renderPlot({
    df <- 
      data.frame(
        x = rnorm(n = input$num,
                  m = input$meanx,
                  sd = 1),
        
        y = rnorm(n = input$num,
                  m = input$meany,
                  sd = 1)
      )
    
    df_cor <- cor(df$x,df$y) |> round(2)
    
    df |> 
      ggplot(aes(x,y)) + 
      geom_point() + 
      theme_minimal(
        base_size = 22,
        paper = "cornsilk",
        ink = "navy"
        ) +
      annotate(
        geom = "text",
        x = mean(df$x),
        y = mean(df$y),
        label = paste0(df_cor)
      )
    
  })
  
}


shinyApp(ui = ui, server = server)
