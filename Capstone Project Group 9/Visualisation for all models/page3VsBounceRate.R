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
    "Unique page view on 3 product pages Vs Bounce Rate"
  })
  
  # Render interactive plotly chart
  output$plot <- renderPlotly({
    df %>% 
      plot_ly(x=~Bounce.Rate) %>%
      add_lines(y=~Unique.Views.3.Primary.Pages, name="Unique view on 3 produt pages") %>%
      add_lines(y=~Bounce.Rate, name="Bounce Rate") %>%
      layout(
        Title = " 3 Product page views Vs Bounce Rate",
        xaxis = list(
          rangeslider = list(type = "x")
        )
      ) 
  })
}

shinyApp(ui = ui, server = server)
