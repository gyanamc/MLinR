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
      plot_ly(x=~Unique.Views.3.Primary.Pages) %>%
      add_lines(y=~Revenue, name="Revenue") %>%
      add_lines(y=~Unique.Views.3.Primary.Pages, name="3 Product Page visits") %>%
      layout(
        Title = " Revenue Vs 3 Product page views",
        xaxis = list(
          rangeslider = list(type = "x")
        )
      ) 
  })
}

shinyApp(ui = ui, server = server)
