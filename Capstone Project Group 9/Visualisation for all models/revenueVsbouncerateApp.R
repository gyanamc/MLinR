library(shiny)

data <- read.csv("GA Converted 3.csv")


ui <- fluidPage(
  textOutput("text"),
  
  # plotly output widget
  plotlyOutput("plot")
)

server <- function(input, output){
  output$text <- renderText({
    "Revenue Vs Bounce Rate"
  })
  
  # Render interactive plotly chart
  output$plot <- renderPlotly({
    data %>% 
      plot_ly(x=~Bounce.Rate) %>%
      add_lines(y=~Revenue, name="Revenue") %>%
      add_lines(y=~Bounce.Rate, name="Bounce Rate") %>%
      layout(
        Title = " Revenue Vs Bounce Rate",
        xaxis = list(
          rangeslider = list(type = "x")
        )
      ) 
  })
}

shinyApp(ui = ui, server = server)
