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
    "Revenue Vs Avg Session Duration"
  })
  
  # Render interactive plotly chart
  output$plot <- renderPlotly({
    data %>% 
      plot_ly(x=~Avg..Session.Duration) %>%
      add_lines(y=~Revenue, name="Revenue") %>%
      add_lines(y=~Avg..Session.Duration, name="Avg Session Duration") %>%
      layout(
        Title = " Revenue Vs Avg Session Duration",
        xaxis = list(
          rangeslider = list(type = "x")
        )
      ) 
  })
}

shinyApp(ui = ui, server = server)
