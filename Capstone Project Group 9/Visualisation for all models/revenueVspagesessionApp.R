library(shiny)

data <- read.csv("GA Converted 3.csv")


ui <- fluidPage(
  textOutput("text"),
  
  # plotly output widget
  plotlyOutput("plot")
)

server <- function(input, output){
  output$text <- renderText({
    "Revenue Vs Page Sessions"
  })
  
  # Render interactive plotly chart
  output$plot <- renderPlotly({
    data %>% 
      plot_ly(x=~Pages...Session) %>%
      add_lines(y=~Revenue, name="Revenue") %>%
      add_lines(y=~Pages...Session, name="Page Sessions") %>%
      layout(
        Title = " Revenue Vs Page Sessions",
        xaxis = list(
          rangeslider = list(type = "x")
        )
      ) 
  })
}

shinyApp(ui = ui, server = server)
