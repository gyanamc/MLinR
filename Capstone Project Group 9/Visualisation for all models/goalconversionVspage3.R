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
      plot_ly(x=~Goal.Conversion.Rate) %>%
      add_lines(y=~Unique.Views.3.Primary.Pages, name="3 Product Pages Views") %>%
      add_lines(y=~Goal.Conversion.Rate, name="Goal Conversion Rate") %>%
      layout(
        Title = " Goal Conversion Rate Vs 3 Product Pages Veiws",
        xaxis = list(
          rangeslider = list(type = "x")
        )
      ) 
  })
}

shinyApp(ui = ui, server = server)
