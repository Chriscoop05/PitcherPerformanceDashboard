library(shiny)
library(tidyverse)
library(png)
library(grid)
library(zoo)


ui = fluidPage(
  
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f5f5f5;
      }
      .main-title {
        text-align: center;
        font-size: 32px;
        font-weight: bold;
        padding: 15px;
        margin-bottom: 20px;
        background-color: #4a5568;
        color: white;
        border-radius: 5px;
      }
      .plot-container {
        background-color: white;
        padding: 15px;
        margin-bottom: 20px;
        border-radius: 5px;
        border: 1px solid #ddd;
      }
      .top-input {
        background-color: white;
        padding: 20px;
        margin-bottom: 20px;
        border-radius: 5px;
        border: 1px solid #ddd;
        text-align: center;
      }
      .input-row {
        display: flex;
        justify-content: center;
        align-items: center;
        gap: 30px;
      }
    "))
  ),
  
  div(class = "top-input",
      fluidRow(
        column(12,
               div(class = "input-row",
                   selectizeInput(
                     inputId = "pitcher_name",
                     label = h4("Select Pitcher:"),
                     choices = NULL,
                     options = list(
                       placeholder = 'Type pitcher name...',
                       maxOptions = 10)
                   )
               )
        )
      )
  ),
  
  # Dynamic title based on pitcher_name input 
  uiOutput("dashboard_title"),
  
  # Row 1: Movement + Usage
  fluidRow(
    column(6, 
           div(class = "plot-container",
               plotOutput("movement_plot", height = "450px")
           )
    ),
    column(6, 
           div(class = "plot-container",
               plotOutput("usage_plot", height = "450px")
           )
    )
  ),
  
  # Row 2: Radar Chart
  fluidRow(
    column(12,
           div(class = "plot-container",
               plotOutput("radar_plot", height = "400px")
           )
    )
  ),
  
  # Row 3: Velocity Distribution
  fluidRow(
    column(12,
           div(class = "plot-container",
               plotOutput("velo_plot", height = "400px")
           )
    )
  ),
  
  # Row 4: Rolling + Heatmap
  fluidRow(
    column(6,
           div(class = "plot-container",
               plotOutput("rolling_plot", height = "450px")
           )
    ),
    column(6,
           div(class = "plot-container",
               plotOutput("heatmap_plot", height = "450px")
           )
    )
  ),
  
  # Row 5: First Half / Second Half Percentile Chart
  fluidRow(
    column(12,
           div(class = "plot-container",
               plotOutput("firstsecondhalf_plot", height = "500px")
           )
    )
  )
)

# Define server
server = function(input, output, session) {
  
  # Populate pitcher choices
  observe({
    pitcher_choices = mutated_sc_2025 %>%
      filter(!is.na(pitcher_name)) %>%
      distinct(pitcher_name) %>%
      arrange(pitcher_name) %>%
      pull(pitcher_name)
    
    updateSelectizeInput(session, "pitcher_name", 
                         choices = pitcher_choices,
                         server = TRUE)
  })
  
  # Dynamic dashboard title
  output$dashboard_title = renderUI({
    tags$div(
      class = "main-title",
      paste(input$pitcher_name, "2025 Dashboard")
    )
  })
  
  # Movement Plot
  output$movement_plot = renderPlot({
    req(input$pitcher_name)
    plot_movement(input$pitcher_name)
  })
  
  # Heatmap Plot
  output$heatmap_plot = renderPlot({
    req(input$pitcher_name)
    plot_heatmap_by_count(input$pitcher_name)
  })
  
  # Radar Plot
  output$radar_plot = renderPlot({
    req(input$pitcher_name)
    create_pitcher_radar(input$pitcher_name)
  })
  
  # Usage Plot
  output$usage_plot = renderPlot({
    req(input$pitcher_name)
    plot_usage_lhb_rhb(input$pitcher_name)
  })
  
  # Rolling Plot
  output$rolling_plot = renderPlot({
    req(input$pitcher_name)
    plot_pitcher_rolling(input$pitcher_name)
  })
  
  # Velocity Plot
  output$velo_plot = renderPlot({
    req(input$pitcher_name)
    plot_velo_distribution(input$pitcher_name)
  })
  
  # First Half / Second Half Percentile Plot
  output$firstsecondhalf_plot = renderPlot({
    req(input$pitcher_name)
    Pitcher.firstsecondhalf.percentile(input$pitcher_name)
  })
}

# Run the application
shinyApp(ui = ui, server = server)