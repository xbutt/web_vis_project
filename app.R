#libraries loading
library(dplyr)
library(dygraphs)
library(ggiraph)
library(ggplot2)
library(here)
library(httr2)
library(leaflet)
library(reshape2)
library(shiny)
library(shinyBS)
library(shinythemes)
library(RColorBrewer)

#data sourcing
source(here("load_data.R"))
source(here("plot_functions.R"))

#labeling
variable_labels <- list(
  t2m = "Temperature [°C]", 
  sf = "Snowfall [mm]", 
  tp = "Precipitation [mm]"
)

ui_parts_labels <- list(
  girafe = "Scatter Plot", 
  dygraph = "Time Series"
)

palettes = list(
  Brewer = brewer.pal(4, "Set2"),
  Blues = c("#8ecae6", "#219ebc", "#126782", "#005f73"),
  CMYK = c("cyan", "magenta", "yellow", "black" )
)

#frontend
ui <- fluidPage( 
  
  tags$head(tags$script(src = "get_forecast.js"),
            tags$script(src = "palettes.js"),
            tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  
  titlePanel("Interactive Climate Dashboard for Uzbekistan"),
  
  #tabs
  tabsetPanel(
    
    #main tab
    tabPanel("Home",
             h2("Welcome to the Climate Data Dashboard"),
             p("This dashboard provides historical climate data from 2000 to 2024 for four major cities: Tashkent, Samarkand, Bukhara, and Fergana. 
                The data includes key climate variables such as Temperature, Precipitation, and Snowfall. Please select either Historical or Interactive plot options to gain a comprehensive understanding of Uzbekistan's climate."),
             h3("Current Temperatures"),
             verbatimTextOutput("temperatures"),
             leafletOutput("map")
    ),
    
    #historical plots tab
    tabPanel("Historical Plots",
             p("Explore past climate trends through static visualizations."),
             
             #temperature trends panel
             bsCollapse(id = "collapse_temp",
                        open = NULL,  
                        bsCollapsePanel("Temperature Trends (2000-2024)", 
                                        div(style = "display: flex; justify-content: center;", 
                                          plotOutput("temp_boxplot",  width = "1000px", height = "600px"),
                                          icon("info-circle", id = "temp_info_icon", class = "tooltip-icon")),
                                        bsTooltip(id = "temp_info_icon", 
                                                  title = "This boxplot shows the distribution of temperature data for each year across locations.", 
                                                  placement = "right", 
                                                  trigger = "hover"),
                                        style = "primary")
             ),
             
             #precipitation trends panel
             bsCollapse(id = "collapse_precip",
                        open = NULL,  
                        bsCollapsePanel("Precipitation Trends (2000-2024)",
                                        div(style = "display: flex; justify-content: center;",   
                                          plotOutput("precip_boxplot",  width = "1000px", height = "600px"),
                                          icon("info-circle", id = "precip_info_icon_1", class = "tooltip-icon")),
                                        bsTooltip(id = "precip_info_icon_1", 
                                                  title = "This boxplot shows the distribution of precipitation data for each year across locations.", 
                                                  placement = "right", 
                                                  trigger = "hover"),
                                        
                                        div(style = "display: flex; justify-content: center;", 
                                          plotOutput("precip_barplot",  width = "1000px", height = "600px"),
                                          icon("info-circle", id = "precip_info_icon_2", class = "tooltip-icon")),
                                        bsTooltip(id = "precip_info_icon_2", 
                                                  title = "This bar plot shows the total annual precipitation sums across locations.", 
                                                  placement = "right", 
                                                  trigger = "hover"),
                                        style = "primary")
             ),
             
             #show fall trends panel
             bsCollapse(id = "collapse_snowfall",
                        open = NULL,  
                        bsCollapsePanel("Snowfall Trends (2000-2024)",
                                        div(style = "display: flex; justify-content: center;",
                                            plotOutput("sf_boxplot",  width = "1000px", height = "600px"),
                                            icon("info-circle", id = "sf_info_icon_1", class = "tooltip-icon")),
                                        bsTooltip(id = "sf_info_icon_1", 
                                                  title = "This boxplot shows the distribution of snowfall data for each year across locations.", 
                                                  placement = "right", 
                                                  trigger = "hover"),
                                        
                                        div(style = "display: flex; justify-content: center;",  
                                            plotOutput("sf_barplot",  width = "1000px", height = "600px"),
                                            icon("info-circle", id = "sf_info_icon_2", class = "tooltip-icon")),
                                        bsTooltip(id = "sf_info_icon_2", 
                                                  title = "This bar plot shows the total annual snowfall sums across locations.", 
                                                  placement = "right", 
                                                  trigger = "hover"),
                                        style = "primary")
             )
    ),
             
    #interactive plots tab
    tabPanel("Interactive Plots",
             p("Dive deeper into the data by selecting specific cities and variables. The interactive plots allow you to customize your analysis and explore the data dynamically."),
             sidebarLayout(
               sidebarPanel(
                 selectInput("variable", "Select Parameter to Visualize:", choices = setNames(names(variable_labels), variable_labels)),
                 checkboxGroupInput("locations", "Select a city:", levels(data$location), selected = levels(data$location)[4]),
                 checkboxGroupInput("ui_parts", "Select plot type:", choices = setNames(names(ui_parts_labels), ui_parts_labels), selected = FALSE),
                 bsCollapsePanel("What is a Scatter Plot?", 
                         "A scatter plot is a type of data visualization that shows the relationship between two variables.",
                         style = "info"),
                 bsCollapsePanel("What is a Time Series?", 
                         "A time series is a sequence of data points collected or recorded at specific time intervals.",
                         style = "info"),
                 selectInput("palette", "Choose your colour palette:", names(palettes))
                 ),
               mainPanel(
                 textOutput("no_city_message"),
                 conditionalPanel(
                   "input.ui_parts.includes('girafe') && input.locations.length > 0", 
                   girafeOutput("girafe", width = 600), 
                   tableOutput("stats_table")
                 ),
                 conditionalPanel(
                   "input.ui_parts.includes('girafe') && input.locations.length > 0", 
                   sliderInput("year_slider", "Select Year Range:", 
                               min = min(data$year), 
                               max = max(data$year), 
                               value = c(min(data$year), max(data$year)), 
                               step = 1)
                 ),
                 conditionalPanel("input.ui_parts.includes('dygraph') && input.locations.length > 0", 
                                  dygraphOutput("dygraph"))
                 )
))))

#backend
server <- function(input, output, session) {

  color_palette <- c("#8ecae6", "#219ebc", "#126782", "#005f73")
  
  plot_details <- list(
    temp_boxplot = list(type = "box", variable = "t2m", title = "Yearly Distribution of Temperature Data Across Selected Cities", y_label = "Temperature [°C]"),
    precip_boxplot = list(type = "box", variable = "tp", title = "Yearly Distribution of Precipitation Data Across Selected Cities", y_label = "Precipitation [mm]"),
    precip_barplot = list(type = "bar", variable = "tp", title = "Total Precipitation Per Year (2000-2024)", y_label = "Total Precipitation [mm]"),
    sf_boxplot = list(type = "box", variable = "sf", title = "Yearly Distribution of Snowfall Data Across Selected Cities", y_label = "Snowfall [mm]"),
    sf_barplot = list(type = "bar", variable = "sf", title = "Total Annual Snowfall by Location (2000-2024)", y_label = "Total Snowfall [mm]")
  )
  
  lapply(names(plot_details), function(plot_name) {
    plot_info <- plot_details[[plot_name]]
    
    output[[plot_name]] <- renderPlot({
      if (plot_info$type == "box") {
        create_boxplot(
          data = data, 
          variable = plot_info$variable, 
          title = plot_info$title, 
          y_label = plot_info$y_label, 
          color_palette = color_palette
        )
      } else {
        create_barplot(
          data = data, 
          variable = plot_info$variable, 
          title = plot_info$title, 
          y_label = plot_info$y_label, 
          color_palette = color_palette
        )
      }
    })
  })
  
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
    if ("girafe" %in% input$ui_parts) {
      plot <- ggplot(data_to_plot(), aes(x = time, y = value, colour = location, group = location, 
                                         tooltip = paste0("<b>", location, "</b><br>",
                                                          "Date: ", format(time, "%Y-%m"), "<br>", 
                                                          variable_labels[[input$variable]], ": ", round(value, 2)))) +
        geom_point_interactive() + 
        scale_color_manual(values = palettes[[input$palette]]) +
        labs(title = paste("Trends of", variable_labels[[input$variable]], "in", paste(input$locations, collapse = ", ")),
             x = "Year", y = variable_labels[[input$variable]], color = "City") +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", size = 20, hjust = 0.5, color = "#2C3E50"),
          axis.title.x = element_text(face = "bold", size = 14),  
          axis.title.y = element_text(face = "bold", size = 14), 
          legend.position = "right", 
          strip.text = element_text(face = "bold", size = 14)
        )
      return(girafe(ggobj = plot)) 
    }

    return(NULL)
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
  
  observeEvent(input$palette, {
    session$sendCustomMessage("plotColors", palettes[[input$palette]])
  })
  
  output$dygraph <- renderDygraph({
    if ("dygraph" %in% input$ui_parts) {
      return(dygraph(dcast(data_to_plot(), "time ~ location + variable")) %>% 
               dyRangeSelector() %>%
               dyOptions(colors = palettes[[isolate(input$palette)]]))  
    }
    return(NULL)
  })
  
  output$no_city_message <- renderText({
    if (length(input$locations) > 0 && length(input$ui_parts) > 0) {
      return(NULL)
    }
    
    if (length(input$locations) == 0 && length(input$ui_parts) > 0) {
      return("No city selected.")
    }
    
    if (length(input$locations) > 0 && length(input$ui_parts) == 0) {
      return("No plot type selected.")
    }
    
    return(NULL)
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