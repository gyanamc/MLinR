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
    "Revenue Vs Goal Completion"
  })
  
  # Render interactive plotly chart
  output$plot <- renderPlotly({
    data %>% 
      plot_ly(x=~Goal.Completions) %>%
      add_lines(y=~Revenue, name="Revenue") %>%
      add_lines(y=~data$Goal.Completions, name="Goal Completion") %>%
      layout(
        Title = " Revenue Vs Goal Completion",
        xaxis = list(
          rangeslider = list(type = "x")
        )
      ) 
  })
}

shinyApp(ui = ui, server = server)
