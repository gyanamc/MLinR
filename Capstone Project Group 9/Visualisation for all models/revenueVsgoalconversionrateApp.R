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
    "Revenue Vs Goal Conversion Rate"
  })
  
  # Render interactive plotly chart
  output$plot <- renderPlotly({
    data %>% 
      plot_ly(x=~Goal.Conversion.Rate) %>%
      add_lines(y=~Revenue, name="Revenue") %>%
      add_lines(y=~Goal.Conversion.Rate, name="Goal Conversion Rate") %>%
      layout(
        Title = " Revenue Vs Goal Conversion Rate",
        xaxis = list(
          rangeslider = list(type = "x")
        )
      ) 
  })
}

shinyApp(ui = ui, server = server)
