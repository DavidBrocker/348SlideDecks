library(shiny)
library(bslib)
library(RKaggle)
library(ggplot2)
library(gt)
library(forcats)

ui <- 
  page_sidebar(
    sidebar = 
      sidebar(
        title = "Data Sandbox",
        numericInput(
          "row_n",
          "Rows:",
          min = 1,
          max = 1000,
          value = 20,
          step = 1
        ),
        numericInput(
          "col_n",
          "Columns:",
          min = 1,
          max = 1000,
          value = 1,
          step = 1
        ),
        numericInput(
          inputId = "width",
          label = "Bar Width:",
          min = .1,
          max = 1,
          value = .5,
          step = .01
        ),
        textInput(
          inputId = "fill",
          "Bar Fill:",
          value = "blue"
        ),
        input_switch(
          id = "gridlines",
          label = "Gridlines:",
          value = FALSE
        ),
        input_switch(
          id = "flip",
          label = "Orientation:",
          value = FALSE
        )
      ),
    layout_columns(
        card(
          card_header("Variable Names"),
          card_body(
            gt_output("var_names")
            )
          ),
      card(
        card_header("Plot"),
        card_body(
          plotOutput("gt_tab")
          )
        )
      )
  )

server <- function(input, output, session) {
  
  observe({
    
    dat_df <- 
      data.frame(
        grade = sample(c("A","B","C","D","F"),
                       size = input$row_n,
                       replace = TRUE)
      )
  
  output$var_names <- render_gt({
    set.seed(123)
    dat_df |> 
      tbl_summary() |> 
      as_gt()
  })
    
    output$gt_tab <- renderPlot({
      
      p <- 
        dat_df |> 
        group_by(grade) |> 
        count() |> 
        ggplot(aes(fct_reorder(grade, n),n, fill = input$fill)) + 
        geom_col(
          width = input$width
        ) + 
        theme_minimal() +
        scale_fill_identity()
      
      if(input$gridlines) {
        p + 
          theme(
            panel.grid = element_blank()
          )
      } else {
        p
      }
      
      if(input$flip) {
        p + 
          coord_flip()
      } else {
        p
      }
    })

  })
  
}

shinyApp(ui, server)