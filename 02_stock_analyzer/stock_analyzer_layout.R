# Business Analytics with Data Science and Machine Learning ----
# Building Business Data Products ----
# STOCK ANALYZER APP - LAYOUT -----

# APPLICATION DESCRIPTION ----
# - Create a basic layout in shiny showing the stock dropdown, interactive plot and commentary


# LIBRARIES ----
library(shiny)
library(shinyWidgets)
library(shinythemes)

library(plotly)
library(tidyverse)
library(lubridate)

library(quantmod)
library(rvest)
library(glue)

source(file = "00_scripts/stock_analysis_functions.R")

# User Interface (UI) ----
ui <- fluidPage(
  theme = shinytheme("simplex"),
  title = "Stock Market Analyzer",
  div(
    column(
      # 1.0 HEADER ----
      h2("Stock Market Analyzer",br(),h6(p("Created by ",
                                           strong("Roy Ruiz"), " | ", 
                                           strong("February 26, 2021")))),
      # 2.0 APPLICATION UI -----
      width = 4,
      multiple = F, 
      actionsBox = FALSE,
      liveSearch = TRUE,
      size = 10,
      wellPanel(
        h4("Stock Index",br(),h6("Step 1: Select an Index Category")),
        # Stock Index Dropdown
        pickerInput(inputId = "indices",
                    choices = c("DAX", "SP500", "DOW", "NASDAQ"),
                    selected = "NASDAQ"),
        
        h4("Stock List",br(),h6("Step 2: Select One to Analyze")),
        # Stock Symbols Dropdown
        uiOutput("stock_list"),
        # Date Range Input
        dateRangeInput(inputId = "date_range", 
                       label   = h4("Date Range",br(),h6("Step 3: Select Your Time Frame")), 
                       start   = "2019-01-01", 
                       end     = today(),       
                       min     = "2017-01-01", 
                       max     = today(), 
                       startview = "year"),
        # Analyze Button
        actionButton(inputId = "analyze", 
                     label   = "Analyze",
                     icon    = icon("chart-line")),
        # Horizontal Divider
        hr(),
        # Short Moving Average Slider
        sliderInput(inputId = "mavg_short_select", 
                    label   = h4("Short Moving Average"), 
                    min     = 3,
                    max     = 50, 
                    value   = c(20), 
                    step    = 1),
        # Long Moving Average Slider
        sliderInput(inputId = "mavg_long_select", 
                    label   = h4("Long Moving Average"), 
                    min     = 10,
                    max     = 120, 
                    value   = c(50), 
                    step    = 1)
        )
    )
  ),
  div(  
    column(
      width = 8,
      h2("Stock Symbol Trend",br(),h6("Plot of Stock Symbol Selected for Analysis")),
      h4(strong(textOutput(outputId = "plot_header"))),
      plotlyOutput("plot")
    )
  ), 
  
  # 3.0 ANALYST COMMENTARY ----
  div(
    column(
      width = 12,
      h4("Analyst Commentary"),
      textOutput(outputId = "analyst_commentary")
    )
  )
)

# SERVER ----
server <- function(input, output, session) {
  
  # Index Selection  
  output$stock_list <- renderUI({
    stock_list <- get_stock_list(input$indices)
    pickerInput(inputId = "index_select", 
                choices = stock_list$label,
                selected = stock_list$label[12])
    })
  
  #Stock Symbol Selection
  stock_symbol <- eventReactive(input$analyze, {
    input$index_select
    })
  
  #Plot Header
  output$plot_header <- renderText({
    stock_symbol()
    })
  
  # Get Stock Data Reactively ----
  observeEvent(eventExpr = input$analyze, handlerExpr = {
    update_ui_panel()
  })
  
  observeEvent(eventExpr = input$mavg_short_select, handlerExpr = {
    update_ui_panel()
  })
  
  observeEvent(eventExpr = input$mavg_long_select, handlerExpr = {
    update_ui_panel()
  })
  
  observeEvent(eventExpr = input$date_range, handlerExpr = {
    update_ui_panel()
  })
  
  # Function to update UI panel
  update_ui_panel <- function(){
    stock_data <- 
      stock_symbol() %>%
      get_symbol_from_user_input()%>%
      get_stock_data(from       = input$date_range[1], 
                     to         = input$date_range[2],
                     mavg_short = input$mavg_short_select,
                     mavg_long  = input$mavg_long_select)
    
    #Render Plot
    output$plot <- renderPlotly(stock_data %>% plot_stock_data()) 
    
    # Render Commentary
    stock_symbol_value <- renderText(stock_symbol())
    output$analyst_commentary <- renderText(generate_commentary(data = stock_data ,user_input = stock_symbol_value()))
  }
}

# RUN APP ----
shinyApp(ui = ui, server = server)