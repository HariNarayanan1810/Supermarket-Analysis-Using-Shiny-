library(shiny)
library(ggplot2)
library(dplyr)

# Load Data
supermarket_data <- read.csv("C:/PSGTECH/Sem-2/OSS/Supermarket/SupermarketAnalysis/data/supermarket_sales - Sheet1.csv")

# Convert Date column to Date type
supermarket_data$Date <- as.Date(supermarket_data$Date, format="%m/%d/%Y")

#  UI
ui <- fluidPage(
  titlePanel("Supermarket Sales Dashboard"),

  sidebarLayout(
    sidebarPanel(
      selectInput("product", "Select Product Line:",
                  choices = unique(supermarket_data$Product.line),
                  selected = unique(supermarket_data$Product.line)[1]),
      radioButtons("payment", "Select Payment Method:",
                   choices = unique(supermarket_data$Payment))
    ),

    mainPanel(
      plotOutput("salesTrend"),
      plotOutput("productSales"),
      plotOutput("paymentDist")
    )
  )
)

# Server
server <- function(input, output) {

  # Filtered Daily Sales Trend
  output$salesTrend <- renderPlot({
    filtered_data <- supermarket_data %>%
      filter(Product.line == input$product, Payment == input$payment)

    ggplot(filtered_data, aes(x = Date, y = Total)) +
      geom_line(stat = "summary", fun = sum, color = "blue") +
      labs(title = "Daily Sales Trend", x = "Date", y = "Total Sales") +
      theme_minimal()
  })

  # Sales Distribution by Product Line
  output$productSales <- renderPlot({
    filtered_data <- supermarket_data %>% filter(Product.line == input$product)
    ggplot(filtered_data, aes(x = Product.line, y = Total, fill = Product.line)) +
      geom_boxplot() +
      labs(title = "Sales Distribution by Product Line", x = "Product Line", y = "Total Sales") +
      theme_minimal()
  })

  # Payment Methods
  output$paymentDist <- renderPlot({
    filtered_data <- supermarket_data %>% filter(Payment == input$payment)
    ggplot(filtered_data, aes(x = Payment, fill = Payment)) +
      geom_bar() +
      labs(title = "Preferred Payment Methods", x = "Payment Method", y = "Count") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
