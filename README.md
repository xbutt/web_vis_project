# Climate Data Dashboard for Uzbekistan

## Project Overview

This project provides an interactive climate data dashboard for Uzbekistan. It allows users to explore historical climate data from 2000 to 2024 for four major cities: Tashkent, Samarkand, Bukhara, and Fergana. The dashboard presents key climate variables such as Temperature, Precipitation, and Snowfall, enabling users to analyze trends and visualize data dynamically.

## Features

-   **Historical Data Visualizations**: View static plots, including boxplots and barplots, showcasing yearly distributions of temperature, precipitation, and snowfall across different cities in Uzbekistan.
-   **Interactive Plots**: Select cities, variables, and time ranges to interactively visualize climate trends. Users can explore the data through scatter plots and time series.
-   **Dynamic Map**: Displays the locations of the four cities on an interactive leaflet map.

## Running the App

To run the dashboard, install the necessary R packages by running the following commands in your RStudio console:

``` r
install.packages(c("dplyr", "dygraphs", "ggiraph", "ggplot2", "httr2", "leaflet", "reshape2", "shiny", "shinyBS", "shinythemes", "RColorBrewer"))
```

Clone or download the repository to your local machine. Open the project in RStudio. Run the app by executing the following command in the console:

``` r
shiny::runApp()
```

The interactive climate dashboard will open in your default web browser.

## Data Source

The climate data used in this project is sourced from the Copernicus Climate Data Store. You can access the dataset with the following DOI: 10.24381/cds.68d2bb30
