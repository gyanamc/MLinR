library(shiny)
library(plotly)

df <- read.csv("GAFullData.csv")
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
    df %>% 
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
