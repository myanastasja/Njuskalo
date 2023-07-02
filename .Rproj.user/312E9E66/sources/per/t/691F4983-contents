library(shiny)
library(ggplot2)

carmodel <- c("Škoda Octavia")
caryear <- unique(df$year)

ui <- fluidPage(
  selectInput("carmodel", "Model", choices = carmodel),
  textInput("year", "Year", placeholder = "2017"),
  tableOutput("table")
)

server <- function(input, output, session) {
  output$table <- renderTable({
    df[df$year == input$year,]
  })
}
  

shinyApp(ui, server) 