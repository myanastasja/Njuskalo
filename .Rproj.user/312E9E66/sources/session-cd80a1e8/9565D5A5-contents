#install.packages("shiny")
#library(shiny)

##### BASIC EXAMPLE #####

# ui <- fluidPage(
#   "Hello, world!"
# )

# server <- function(input, output, session) {
# }


##### 2ND EXAMPLE #####
ui <- fluidPage(
  selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
  verbatimTextOutput("summary"),
  tableOutput("table")
)

# server <- function(input, output, session) {
#   output$summary <- renderPrint({
#     dataset <- get(input$dataset, "package:datasets")
#     summary(dataset)
#   })
#   
#   output$table <- renderTable({
#     dataset <- get(input$dataset, "package:datasets")
#     dataset
#   })
# }


##### 2.B EXAMPLE - REMOVING DUPLICATE CODE "get(input$dataset, "package:datasets")" #####
server <- function(input, output, session) {
  # Create a reactive expression
  dataset <- reactive({
    get(input$dataset, "package:datasets")
  })
  
  output$summary <- renderPrint({
    # Use a reactive expression by calling it like a function
    summary(dataset())
  })
  
  output$table <- renderTable({
    dataset()
  })
}

shinyApp(ui, server)

