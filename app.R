#libraries loading
library(dplyr)
library(dygraphs)
library(ggiraph)
library(ggplot2)
library(leaflet)
library(reshape2)
library(shiny)
library(shinyBS)
library(shinythemes)

#data sourcing
source("load_data.r")

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

#frontend
ui <- fluidPage( 
  
  tags$head(tags$script(src = "get_forecast.js"),
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

  color_palette <- c("#8ecae6", "#219ebc", "#126782", "#005f73")
  
  #temperature boxplot
  output$temp_boxplot <- renderPlot({
    ggplot(data %>% filter(variable == "t2m"), aes(x = factor(year), y = value, fill = location)) + 
      geom_boxplot(outlier.shape = NA, alpha = 0.7) +  
      facet_wrap(~ location) +  
      scale_fill_manual(values = color_palette) +  
      scale_x_discrete(guide = guide_axis(angle = 45)) +
      labs(
        title = "Yearly Distribution of Temperature Data Across Selected Cities",  
        x = "Year", 
        y = "Temperature [°C]"
      ) + 
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 20, hjust = 0.5, color = "#2C3E50"),
        axis.title.x = element_text(face = "bold", size = 14),  
        axis.title.y = element_text(face = "bold", size = 14), 
        legend.position = "none", 
        strip.text = element_text(face = "bold", size = 14)
      )  
  })
  
  #precipitation boxplot
  output$precip_boxplot <- renderPlot({
    ggplot(data %>% filter(variable == "tp"), aes(x = factor(year), y = value, fill = location)) + 
      geom_boxplot(outlier.shape = NA, alpha = 0.7) +  
      facet_wrap(~ location) +  
      scale_fill_manual(values = color_palette) +  
      scale_x_discrete(guide = guide_axis(angle = 45)) +
      labs(
        title = "Yearly Distribution of Precipitation Data Across Selected Cities",
        x = "Year", 
        y = "Precipitation [mm]"
      ) + 
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 20, hjust = 0.5, color = "#2C3E50"),
        axis.title.x = element_text(face = "bold", size = 14),  
        axis.title.y = element_text(face = "bold", size = 14), 
        legend.position = "none", 
        strip.text = element_text(face = "bold", size = 14)
      )
  })
  
  #precipitation barplot 
  output$precip_barplot <- renderPlot({
    ggplot(data %>% filter(variable == "tp") %>%
             group_by(year, location) %>%
             summarise(total_precipitation = sum(value, na.rm = TRUE), .groups = "drop"),
           aes(x = factor(year), y = total_precipitation, fill = location)) +
      geom_bar(stat = "identity", position = "dodge", color = "#023047") +
      scale_fill_manual(values = color_palette, name = "Location") +
      scale_x_discrete(guide = guide_axis(angle = 45)) +  
      labs(
        title = "Total Precipitation Per Year (2000-2024)",
        x = "Year", 
        y = "Total Precipitation [mm]"
      ) +  
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 20, hjust = 0.5, color = "#2C3E50"),  
        axis.title.x = element_text(face = "bold", size = 14),  
        axis.title.y = element_text(face = "bold", size = 14),  
        legend.position = "right", 
        legend.title = element_text(face = "bold", size = 14, color = "#2C3E50"),  
        legend.text = element_text(size = 12),  
        strip.text = element_text(face = "bold", size = 14)
      )
  })
  
  #snowfall boxplot
  output$sf_boxplot <- renderPlot({
    ggplot(data %>% filter(variable == "sf"), aes(x = factor(year), y = value, fill = location)) + 
      geom_boxplot(outlier.shape = NA, alpha = 0.7) +  
      facet_wrap(~ location) +  
      scale_fill_manual(values = color_palette) +  
      scale_x_discrete(guide = guide_axis(angle = 45)) +
      labs(
        title = "Yearly Distribution of Snowfall Data Across Selected Cities",
        x = "Year", 
        y = "Snowfall [mm]"
      ) +  
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 20, hjust = 0.5, color = "#2C3E50"),
        axis.title.x = element_text(face = "bold", size = 14),  
        axis.title.y = element_text(face = "bold", size = 14), 
        legend.position = "none", 
        strip.text = element_text(face = "bold", size = 14)
      )
  })
  
  #snowfall barplot 
  output$sf_barplot <- renderPlot({
    ggplot(data %>% filter(variable == "sf") %>%
             group_by(year, location) %>%
             summarise(total_snowfall = sum(value, na.rm = TRUE), .groups = "drop"),
           aes(x = factor(year), y = total_snowfall, fill = location)) +
      geom_bar(stat = "identity", position = "dodge", color = "#023047") +
      scale_fill_manual(values = color_palette, name = "Location") +  
      scale_x_discrete(guide = guide_axis(angle = 45)) +  
      labs(
        title = "Total Annual Snowfall by Location (2000-2024)",
        x = "Year", 
        y = "Total Snowfall [mm]"
      ) +  
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 20, hjust = 0.5, color = "#2C3E50"),  
        axis.title.x = element_text(face = "bold", size = 14),  
        axis.title.y = element_text(face = "bold", size = 14),  
        legend.position = "right", 
        legend.title = element_text(face = "bold", size = 14, color = "#2C3E50"),  
        legend.text = element_text(size = 12),  
        strip.text = element_text(face = "bold", size = 14)
      )
  })
  
  #data processing for interactive plots
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