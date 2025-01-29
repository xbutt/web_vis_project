library(shiny)
library(ggiraph)
library(dygraphs)
library(reshape2)

source("load_data.r")

#
variable_labels <- list(
  t2m = "Temperature", 
  sf = "Snowfall", 
  tp = "Precipitation"
)

ui <- fluidPage( 
  column(4, 
         titlePanel("Interactive Climate Dashboard for Uzbekistan"),
         # Map the variable options to full names using the list
         selectInput("variable", "Select Parameter to Visualize:", 
                     choices = setNames(names(variable_labels), variable_labels)),
         checkboxGroupInput("locations", "Select a city:", levels(data$location), selected = levels(data$location)[4]),
         checkboxGroupInput("ui_parts", "Show parts:", c("girafe", "dygraphs", "map"), selected = NULL)
  ),
  column(8,
         conditionalPanel("input.ui_parts.includes('girafe')", girafeOutput("girafe", width = 600)),
         conditionalPanel("input.ui_parts.includes('dygraphs')", dygraphOutput("dygraph")),
         conditionalPanel("input.ui_parts.includes('map')", leafletOutput("map"))
  )
)


server <- function(input, output) {
  data_to_plot <- reactive({
    data[location %in% input$locations & variable == input$variable]
  })
  
  output$girafe <- renderGirafe({
    plot <- ggplot(data_to_plot(), aes(x = time, y = value, colour = location, tooltip = paste(time, location, variable, value)))
    plot <- plot + geom_point_interactive()
    girafe(ggobj = plot)
  })
  
  output$dygraph <- renderDygraph({
    dygraph(dcast(data_to_plot(), "time ~ location + variable")) %>% dyRangeSelector()
  })
  
  output$map <- renderLeaflet({
    leaflet() %>% setView(lng = 66.9237, lat = 41.7683, zoom = 6) %>% addTiles() %>%
      addAwesomeMarkers(lng = locations$longitude,
                        lat = locations$latitude, label = locations$name,
                        icon = awesomeIcons("home", markerColor = "blue", iconColor = "black"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
