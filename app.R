library(shiny)
library(shinyBS)
library(ggiraph)
library(ggplot2)
library(dygraphs)
library(reshape2)
library(leaflet)
library(dplyr)


source("load_data.r")

variable_labels <- list(
  t2m = "Temperature [°C]", 
  sf = "Snowfall [mm]", 
  tp = "Precipitation [mm]"
)

ui_parts_labels <- list(
  girafe = "Scatter plot", 
  dyghraphs = "Dygraph"
)

ui <- fluidPage( 
  column(4, 
         titlePanel("Interactive Climate Dashboard for Uzbekistan"),
         selectInput("variable", "Select Parameter to Visualize:", 
                     choices = setNames(names(variable_labels), variable_labels)),
         checkboxGroupInput("locations", "Select a city:", levels(data$location), selected = levels(data$location)[4]),
         checkboxGroupInput("ui_parts", "Show parts:", choices = setNames(names(ui_parts_labels), ui_parts_labels), selected = "girafe"),
         bsCollapsePanel("What is a Scatter Plot?", 
                         "A scatter plot is a type of data visualization that shows the relationship between two variables. 
  In this dashboard, it helps track climate variables over time for selected cities.",
                         style = "primary")
         
  ),
  column(8,
         conditionalPanel("input.ui_parts.includes('girafe')", girafeOutput("girafe", width = 600), tableOutput("stats_table")),
  )
)

server <- function(input, output) {
  
  data_to_plot <- reactive({
    data[location %in% input$locations & variable == input$variable]
  })
  
  stats_summary <- reactive({
    data_to_plot() %>%
      group_by(location) %>%
      summarise(
        Min = min(value, na.rm = TRUE),
        Mean = mean(value, na.rm = TRUE),
        Max = max(value, na.rm = TRUE)
      ) %>%
      rename(City = location)  # Rename "location" to "City"
  })
  
  output$girafe <- renderGirafe({
    plot <- ggplot(data_to_plot(), aes(x = time, y = value, colour = location, group = location, tooltip = paste0("<b>", location, "</b><br>",
                                                                                                                  "Date: ", time, "<br>",
                                                                                                                  variable_labels[[input$variable]],": ", round(value, 2)))) +
    geom_point_interactive() +
    scale_color_brewer(palette = "Set2") +
    labs(title = paste("Trends of", variable_labels[[input$variable]], "in", paste(input$locations, collapse = ", ")),
         x = "Year", y = variable_labels[[input$variable]],  color = "City") +
    theme_light()
    girafe(ggobj = plot)
  })
  
  output$stats_table <- renderTable({
    stats_summary()
  })
  
  
}


shinyApp(ui = ui, server = server)
