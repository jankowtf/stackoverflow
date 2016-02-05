
# UI ---------------------------------------------------------------------

ui <- fluidPage(
  p(),
  textInput("title", "Title"),
  textInput("description", "Description"),
  tags$hr(),
  h3("Database state"),
  DT::dataTableOutput("datatable")
)

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  output$datatable <- DT::renderDataTable(
    data.frame(
      Title = input$title,
      Description = input$description,
      stringsAsFactors = FALSE
    ), server = FALSE, selection = list(target = "row+column")
  )
}

shinyApp(ui, server)

