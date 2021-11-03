library(shiny)
library(plotly)
data <- read.csv("GA Converted 3.csv")


ui <- fluidPage(
  textOutput("text"),
  
  # plotly output widget
  plotlyOutput("plot")
)

server <- function(input, output){
  output$text <- renderText({
    "Revenue Vs e-commerce conversion rate"
  })
  
  # Render interactive plotly chart
  output$plot <- renderPlotly({
    data %>% 
      plot_ly(x=~Ecommerce.Conversion.Rate) %>%
      add_lines(y=~Revenue, name="Revenue") %>%
      add_lines(y=~Ecommerce.Conversion.Rate, name="e-commerce conversion rate") %>%
      layout(
        Title = " Revenue Vs e-commerce conversion rate",
        xaxis = list(
          rangeslider = list(type = "x")
        )
      ) 
  })
}

shinyApp(ui = ui, server = server)
