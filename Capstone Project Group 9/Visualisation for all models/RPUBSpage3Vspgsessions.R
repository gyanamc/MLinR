library(plotly)
library(MASS)
df <- read.csv("GAFullData.csv")

#covmat <- matrix(c(0.8, 0.4, 0.3, 0.8), nrow = 2, byrow = T)
#df <- mvrnorm(n = 10000, c(0,0), Sigma = covmat)
df <- as.data.frame(df)

#colnames(df) <- c("x", "y")
fig <- plot_ly(df, x = ~Unique.Views.3.Primary.Pages, y = ~Pages...Session, alpha = 0.3)
fig <- fig %>% add_markers(marker = list(line = list(color = "black", width = 1)))
fig <- fig %>% layout(
  title = "3 product pages Vs page sessions",
  xaxis = list(domain = c(0.1, 1)),
  yaxis = list(title = "y"),
  updatemenus = list(
    list(
      y = 0.8,
      buttons = list(
        
        list(method = "restyle",
             args = list("type", "scatter"),
             label = "Scatter"),
        
        list(method = "restyle",
             args = list("type", "histogram2d"),
             label = "2D Histogram")))
  ))

fig
