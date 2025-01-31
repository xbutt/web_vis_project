library(dplyr)
library(dygraphs)
library(ggiraph)
library(ggplot2)
library(leaflet)
library(reshape2)
library(shiny)
library(shinyBS)
library(shinythemes)

source("load_data.r")

variable_labels <- list(
  t2m = "Temperature [°C]", 
  sf = "Snowfall [mm]", 
  tp = "Precipitation [mm]"
)

ui_parts_labels <- list(
  girafe = "Scatter Plot", 
  dygraph = "Time Series"
)

ui <- fluidPage( 
  tags$head(tags$script(src = "get_forecast.js"),
            tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  titlePanel("Interactive Climate Dashboard for Uzbekistan"),
  tabsetPanel(
    #main Tab
    tabPanel("Home",
             h2("Welcome to the Climate Data Dashboard"),
             p("This dashboard provides historical climate data from 2000 to 2024 for four major cities: Tashkent, Samarkand, Bukhara, and Fergana. 
                The data includes key climate variables such as Temperature, Precipitation, and Snowfall. Please select either Historical or Interactive plot options to gain a comprehensive understanding of Uzbekistan's climate."),
             h3("Current Temperatures"),
             verbatimTextOutput("temperatures"),
             leafletOutput("map")
    ),
    
    #interactive plots tab
    tabPanel("Interactive Plots",
             p("Dive deeper into the data by selecting specific cities and variables. The interactive plots allow you to customize your analysis and explore the data dynamically."),
             sidebarLayout(
               sidebarPanel(
                 selectInput("variable", "Select Parameter to Visualize:", choices = setNames(names(variable_labels), variable_labels)),
                 checkboxGroupInput("locations", "Select a city:", levels(data$location), selected = levels(data$location)[4]),
                 checkboxGroupInput("ui_parts", "Select plot type:", choices = setNames(names(ui_parts_labels), ui_parts_labels), selected = NULL),
                 bsCollapsePanel("What is a Scatter Plot?", 
                         "A scatter plot is a type of data visualization that shows the relationship between two variables.",
                         style = "info"),
                 bsCollapsePanel("What is a Time Series?", 
                         "A time series is a sequence of data points collected or recorded at specific time intervals.",
                         style = "info"),
                 ),
               mainPanel(
                 conditionalPanel("input.ui_parts.includes('girafe')", 
                                  girafeOutput("girafe", width = 600), 
                                  tableOutput("stats_table")),
                 conditionalPanel(condition = "input.ui_parts.includes('girafe')",
                                  sliderInput("year_slider", "Select Year Range:", 
                                              min = min(data$year), 
                                              max = max(data$year), 
                                              value = c(min(data$year), max(data$year)), 
                                              step = 1)),
                 conditionalPanel("input.ui_parts.includes('dygraph')", dygraphOutput("dygraph"))
                 )
))))


server <- function(input, output) {
  
  data_to_plot <- reactive({
    filtered_data <- data %>%
      filter(location %in% input$locations & 
               variable == input$variable) %>%
      mutate(year = as.integer(year)) 
    
    if ("girafe" %in% input$ui_parts) {
      filtered_data <- filtered_data %>%
        filter(year >= input$year_slider[1], year <= input$year_slider[2])
    }
    return(filtered_data)
  })
  
  output$girafe <- renderGirafe({
    plot <- ggplot(data_to_plot(), aes(x = time, y = value, colour = location, group = location, 
                                       tooltip = paste0("<b>", location, "</b><br>",
                                                        "Date: ", time, "<br>",
                                                        variable_labels[[input$variable]], ": ", round(value, 2)))) +
      geom_point_interactive() +
      scale_color_brewer(palette = "Set2") +
      labs(title = paste("Trends of", variable_labels[[input$variable]], "in", paste(input$locations, collapse = ", ")),
           x = "Year", y = variable_labels[[input$variable]], color = "City") +
      theme_light()
    girafe(ggobj = plot)
  })
  
  stats_summary <- reactive({
    data_to_plot() %>%
      group_by(location) %>%
      summarise(
        Min = min(value, na.rm = TRUE),
        Mean = mean(value, na.rm = TRUE),
        Max = max(value, na.rm = TRUE)
      ) %>%
      rename(City = location)
  })
  
  output$stats_table <- renderTable({
    stats_summary()
  })
  
  output$dygraph <- renderDygraph({
    dygraph(dcast(data_to_plot(), "time ~ location + variable")) %>% dyRangeSelector()
  })
  
  output$temperatures <- renderText({
    text = ""  
    for (loc in 1:nrow(locations)) {
      if (loc == 1) {
        text = paste0(text, locations$name[loc], ": ", input$weather[loc], " °C")  
      } else {
        text = paste0(text, "\n", locations$name[loc], ": ", input$weather[loc], " °C") 
      }
    }
    text  
  })
  
  output$map <- renderLeaflet({
    leaflet() %>% setView(lng = 66.9237, lat = 41.7683, zoom = 6) %>% addTiles() %>%
      addAwesomeMarkers(lng = locations$longitude,
                        lat = locations$latitude, label = locations$name,
                        icon = awesomeIcons("home", markerColor = "blue", iconColor = "black"))
  })
}

shinyApp(ui = ui, server = server)