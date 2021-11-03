library(shiny)
library(plotly)
df=read.csv("GAFullData.csv")

ui <- fluidPage(
  textOutput("text"),
  
  # plotly output widget
  plotlyOutput("plot")
)

server <- function(input, output){
  output$text <- renderText({
    "This is an interative plot within a Shiny app:"
  })
  
  # Render interactive plotly chart
  output$plot <- renderPlotly({
    df %>% 
      plot_ly(x=~Bounce.Rate) %>%
      add_lines(y=~Goal.Conversion.Rate, name="Goal Conversion Rate") %>%
      add_lines(y=~Bounce.Rate, name="Bounce Rate") %>%
      layout(
        Title = " Goal Conversion Rate Vs Bounce Rate",
        xaxis = list(
          rangeslider = list(type = "x")
        )
      ) 
  })
}

shinyApp(ui = ui, server = server)
