library(shiny)
library(ggplot2)
library(dplyr)

carmodel <- c("Škoda Octavia")
caryear <- unique(df$year)

ui <- fluidPage(
  selectInput("carmodel", "Model", choices = carmodel),
  # textInput("inputyear", "Year",  placeholder = "2007"),
  selectInput("inputyear", "Year", unique(df$year) %>% sort(), multiple = TRUE),
  # sliderInput("pricerange", "Median price", min(df$price_EUR), max(df$price_EUR), value=df$price_EUR[df$year == input$inputyear,]),
  tableOutput("table")
)

server <- function(input, output, session) {
  output$table <- renderTable({
    df[df$year %in% input$inputyear,] %>% arrange(price_EUR)
  })
}
  

shinyApp(ui, server) 

