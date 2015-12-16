library(shiny)

ui <- fluidPage(
  actionButton("action_trigger", "Trigger 1"),
  h3("Database state"),
  textOutput("result"),
  p(),
  uiOutput("ui_conditional")
)

server <- function(input, output, session) {
  db <- reactiveValues(
    title = "",
    description = ""
  )
  
  ui_control <- reactiveValues(show = FALSE)
  
  output$ui_conditional <- renderUI({
    if (!ui_control$show) return()
    
    tagList(
      textInput("title", "Title"),
      textInput("description", "Description"),
      div(style="display:inline-block",
        actionButton("action_create", "Create"),
        actionButton("action_cancel", "Cancel")
      )
    )
  })
  
  observeEvent(input$action_trigger, {
    ui_control$show <- TRUE
  })
  observeEvent(input$action_create, {
    writeToDb(input)
    ui_control$show <- FALSE
  })
  observeEvent(input$action_cancel, {
    ui_control$show <- FALSE
  })
  
  writeToDb <- function(input) {
    db$title <- input$title
    db$description <- input$description
  }
  
  output$result <- renderText({
    c(
      paste0("Title: ", db$title),
      paste0("Description: ", db$description)
    )
  })
}

shinyApp(ui, server)