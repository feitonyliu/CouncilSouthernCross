library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)

# Generate sample data
set.seed(123)
years <- 2014:2024
categories <- c("Roads", "Parks", "Waste Management", "Community Services", "Administration", "Public Safety")

data <- expand.grid(Year = years, Category = categories) %>%
  mutate(Spending = runif(n(), 1000000, 10000000)) %>%
  arrange(Year, Category)

ui <- page_sidebar(
  title = "Southern Cross Council Spending Dashboard",
  sidebar = sidebar(
    sliderInput("year_range", "Select Year Range:",
                min = min(years), max = max(years),
                value = c(min(years), max(years)), step = 1, sep = ""),
    selectizeInput("categories", "Select Spending Categories:",
                   choices = categories, multiple = TRUE,
                   selected = categories)  # Pre-select all categories
  ),
  layout_column_wrap(
    width = 1,
    heights_equal = "row",
    card(
      full_screen = TRUE,
      card_header("Total Spending by Category"),
      plotlyOutput("pie_chart")
    ),
    card(
      full_screen = TRUE,
      card_header("Spending Trends by Category"),
      plotlyOutput("line_chart")
    )
  )
)

server <- function(input, output, session) {
  
  # Filter data based on selected year range
  filtered_data <- reactive({
    req(input$year_range)
    data %>%
      filter(Year >= input$year_range[1] & Year <= input$year_range[2])
  })
  
  # Pie chart for total spending by category
  output$pie_chart <- renderPlotly({
    pie_data <- filtered_data() %>%
      group_by(Category) %>%
      summarise(TotalSpending = sum(Spending))
    
    plot_ly(pie_data, labels = ~Category, values = ~TotalSpending, type = 'pie') %>%
      layout(title = "Total Spending by Category",
             showlegend = TRUE,
             height = 400)  # Explicitly set height
  })
  
  # Line chart for selected categories
  output$line_chart <- renderPlotly({
    req(input$categories)
    line_data <- filtered_data() %>%
      filter(Category %in% input$categories)
    
    plot_ly(line_data, x = ~Year, y = ~Spending, color = ~Category, type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "Spending Trends by Selected Categories",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Spending ($)"),
             height = 400)  # Explicitly set height
  })
}

shinyApp(ui, server)
