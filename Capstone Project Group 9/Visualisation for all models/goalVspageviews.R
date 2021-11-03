library(shiny)
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
      plot_ly(x=~Goal.Completions) %>%
      add_lines(y=~Unique.Pageviews, name="Goal.Completion") %>%
      add_lines(y=~Unique.Pageviews, name="Unique View on page 3") %>%
      layout(
        Title = " Goal Vs Unique views",
        xaxis = list(
          rangeslider = list(type = "x")
        )
      ) 
  })
}

shinyApp(ui = ui, server = server)
