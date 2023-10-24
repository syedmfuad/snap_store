# Load required libraries
library(shiny)
library(dplyr)
library(sf)
library(ggplot2)

# Assuming you have your dataframe as 'group_mean' with a 'geometry' column containing polygons

# Define the UI for the Shiny app
ui <- fluidPage(
  titlePanel("County Store Count Map"),
  
  # Create a slider for selecting the year
  sliderInput("date", "Select Year", min = 1990, max = 2022, value = 1990),
  
  # Create the ggplot map
  plotOutput("map")
)

# Define the server for the Shiny app
server <- function(input, output) {
  # Filter the data for the selected year
  filtered_data <- reactive({
    newdata %>%
      filter(date == input$date)
  })
  
  # Render the ggplot map
  output$map <- renderPlot({
    p <- ggplot() +
      geom_sf(data = filtered_data(), aes(fill = total_stores, color = total_stores), size = 0.1) +
      geom_sf(data = state_overlay, fill = NA, color = "black", size = 0.1)
    
    print(p)
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)