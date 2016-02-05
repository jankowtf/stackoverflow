
# Functions ---------------------------------------------------------------

createRecord <- function(input, db) {
  db$data <- rbind(
    db$data, 
    data.frame(
      title = input$title,
      description =input$description,
      stringsAsFactors = FALSE
    )
  )
}
updateRecord <- function(input, db, selection) {
  db$data[selection, ] <- data.frame(
    title = input$title,
    description =input$description,
    stringsAsFactors = FALSE
  )
}
deleteRecord <- function(db, selection) {
  db$data <- db$data[-selection, ]
}

# UI ---------------------------------------------------------------------

ui <- fluidPage(
  tabsetPanel(
    id = "tabs",
    tabPanel(
      "A",
      p(),
      actionButton("action_trigger", "Create"),
      h3("Database state"),
      DT::dataTableOutput("datatable"),
      p(),
      uiOutput("ui_input")
    )
  )
)

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  db <- reactiveValues(
    title = "",
    description = ""
  )
  
  ## Initialize DB //
  db <- reactiveValues(data = data.frame(title = NA, description = NA)[-1,])
  
  ## UI control //
  ui_control <- reactiveValues(
    case = c("hide", "create", "update")[1],
    selection = NULL,
    render_table = TRUE
  )
  
  ## Observe //
  observeEvent(input$action_trigger, {
    ui_control$case <- "create"
  })
  observeEvent(input$action_create, {
    createRecord(input, db = db)
    ui_control$case <- "hide"
  })
  observeEvent(input$action_update, {
    updateRecord(input, db = db, selection = ui_control$selection)
    ui_control$case <- "hide"
  })
  observeEvent(input$action_delete, {
    deleteRecord(db = db, selection = ui_control$selection)
    ui_control$case <- "hide"
  })
  observeEvent(input$action_cancel, {
    ui_control$case <- "hide"
  })
  
  observe({
    idx <- input$datatable_rows_selected
    if (!is.null(idx)) {
      ui_control$case <- "update"
    } else {
      ui_control$case <- "hide"
    }
    ui_control$selection <- idx
  })
  
  ## Render UI //
  output$ui_input <- renderUI({
    case <- ui_control$case
    if (case == "hide") return()
    
    ## Case dependent input //
    if (case == "create") {
      title <- ""
      description <- ""
      buttons <- div(style="display:inline-block",
        actionButton("action_create", "Create"),
        actionButton("action_cancel", "Cancel")
      )
    } else if (case == "update") {
      title <- db$data[ui_control$selection, "title"]
      description <- db$data[ui_control$selection, "description"]
      buttons <- div(style="display:inline-block",
        actionButton("action_update", "Update"),
        actionButton("action_cancel", "Cancel"),
        p(),
        actionButton("action_delete", "Delete",
          icon = icon("exclamation-triangle"))
      )
    } else {
      stop(sprintf("Invalid case: %s", case))
    }
    
    tagList(
      textInput("title", "Title", title),
      textInput("description", "Description", description),
      buttons
    )
  })
  
  ## Database //
  output$datatable <- DT::renderDataTable(
    db$data, server = FALSE, selection = list(target = "row+column")
  )
  
  ## Observe action links //
  observeEvent(input$link_to_tab_b, {
    newvalue <- "B"
    updateTabsetPanel(session, "tabs", newvalue)
  })
  observeEvent(input$link_to_tab_a, {
    newvalue <- "A"
    updateTabsetPanel(session, "tabs", newvalue)
  })
}

shinyApp(ui, server)

