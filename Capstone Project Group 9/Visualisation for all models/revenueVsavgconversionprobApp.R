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
    "Revenue Vs avg conversion probablity"
  })
  
  # Render interactive plotly chart
  output$plot <- renderPlotly({
    data %>% 
      plot_ly(x=~Avg....Conversion.Probability) %>%
      add_lines(y=~Revenue, name="Revenue") %>%
      add_lines(y=~Avg....Conversion.Probability, name="avg. conversion probablity") %>%
      layout(
        Title = " Revenue Vs avg. conversion probablity",
        xaxis = list(
          rangeslider = list(type = "x")
        )
      ) 
  })
}

shinyApp(ui = ui, server = server)
