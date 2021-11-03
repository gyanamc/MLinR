library(shiny)

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
      add_lines(y=~Pages...Session, name="Page Sessions") %>%
      add_lines(y=~Unique.Views.3.Primary.Pages, name="Unique Views on 3 product pages") %>%
      layout(
        Title = " Unique Views on 3 Product Pages Vs Page Sessions",
        xaxis = list(
          rangeslider = list(type = "x")
        )
      ) 
  })
}

shinyApp(ui = ui, server = server)
