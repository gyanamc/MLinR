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
      plot_ly(x=~Unique.Views.3.Primary.Pages) %>%
      add_lines(y=~Bounce.Rate, name="Bounce Rate") %>%
      add_lines(y=~Unique.Views.3.Primary.Pages, name="Unique View on 3 product pages") %>%
      layout(
        Title = " Bounce Rate Vs 3 Product page views",
        xaxis = list(
          rangeslider = list(type = "x")
        )
      ) 
  })
}

shinyApp(ui = ui, server = server)
