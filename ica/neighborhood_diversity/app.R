#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application
shinyApp(ui = ui, server = server)






library(shiny)
library(tidyverse)
library(sf)
library(plotly)


data_by_dist <- read_rds("data/diverse_data_by_dist.rds")
data_by_year <- read_csv("data/diverse_data_by_year.csv")

metro_names <- data_by_dist |>
  pull(metro_name) |>
  unique() |>
  sort()

ui <- fluidPage(
  titlePanel("Neighborhood Diversity Interactive App"),

  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "city_select",
        label = "Choose a City:",
        choices = metro_names,
        selected = "Minneapolis-St. Paul-Bloomington, MN-WI"
      ),

      sliderInput(
        inputId = "span_input",
        label = "Smoothing Span (Wiggliness):",
        min = 0.1,
        max = 1.0,
        value = 0.5,
        step = 0.1
      )
    ),

    mainPanel(
      plotlyOutput(outputId = "diversity_scatter"),

      plotOutput(outputId = "diversity_map"),

      plotlyOutput(outputId = "race_bar")
    )
  )
)

server <- function(input, output) {

  output$diversity_scatter <- renderPlotly({
    filtered_data <- data_by_dist |>
      filter(metro_name == input$city_select)

    p <- ggplot(filtered_data, aes(x = distmiles, y = entropy)) +
      geom_point(alpha = 0.3, aes(text = paste("Tract:", tract_id))) +
      geom_smooth(method = "loess", span = input$span_input, color = "blue") +
      labs(title = paste("Diversity Profile:", input$city_select),
           x = "Distance to City Hall (miles)",
           y = "Entropy (Diversity) Score") +
      theme_minimal()

    ggplotly(p)
  })

  output$diversity_map <- renderPlot({
    data_by_dist |>
      filter(metro_name == input$city_select) |>
      ggplot() +
      geom_sf(aes(fill = entropy)) +
      scale_fill_viridis_c() +
      theme_void() +
      labs(fill = "Diversity")
  })

  output$race_bar <- renderPlotly({
    race_data <- data_by_dist |>
      filter(metro_name == input$city_select) |>
      st_drop_geometry() |>
      summarise(across(c(aian, asian, black, hispanic, white, two_or_more), sum)) |>
      pivot_longer(everything(), names_to = "Race", values_to = "Count")

    p <- ggplot(race_data, aes(x = reorder(Race, -Count), y = Count, fill = Race)) +
      geom_col() +
      labs(x = "Race Group", y = "Total Population") +
      theme_minimal() +
      guides(fill = "none")

    ggplotly(p)
  })
}

shinyApp(ui = ui, server = server)
