library(shiny)
library(plotly)

df<- read.csv("GAFullData.csv")
ui <- fluidPage(
  textOutput("text"),
  
  # plotly output widget
  plotlyOutput("plot")
)

server <- function(input, output){
  output$text <- renderText({
    "Page Sessions Vs Bounce Rate:"
  })
  
  # Render interactive plotly chart
  output$plot <- renderPlotly({
    df %>% 
      plot_ly(x=~Bounce.Rate) %>%
      add_lines(y=~Pages...Session, name="Page Sessions") %>%
      add_lines(y=~Bounce.Rate, name="Bounce Rate") %>%
      layout(
        Title = " Page Sessions Vs Bounce Rate",
        xaxis = list(
          rangeslider = list(type = "x")
        )
      ) 
  })
}

shinyApp(ui = ui, server = server)
