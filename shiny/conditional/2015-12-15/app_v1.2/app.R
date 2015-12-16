library(shiny)

# Functions ---------------------------------------------------------------

createRecord <- function(input, db) {
  db$data <- rbind(
    db$data, 
    data.frame(
      firstname = input$firstname,
      lastname =input$lastname,
      stringsAsFactors = FALSE
    )
  )
}
updateRecord <- function(input, db, selection) {
  db$data[selection, ] <- data.frame(
    firstname = input$firstname,
    lastname =input$lastname,
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
      actionButton("action_trigger", "Trigger"),
      h3("Database state"),
      DT::dataTableOutput("datatable"),
      p(),
      uiOutput("ui_input"),
      p(),
      actionLink("link_to_tab_b", "Link to tab B")
    ),
    tabPanel(
      "B",
      h3("Some information"),
      tags$li("Item 1"),
      tags$li("Item 2"),
      p(),
      actionLink("link_to_tab_a", "Link to tab A")
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
  db <- reactiveValues(data = data.frame(firstname = NA, lastname = NA)[-1,])
  
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
      firstname <- ""
      lastname <- ""
      buttons <- div(style="display:inline-block",
        actionButton("action_create", "Create"),
        actionButton("action_cancel", "Cancel")
      )
    } else if (case == "update") {
      firstname <- db$data[ui_control$selection, "firstname"]
      lastname <- db$data[ui_control$selection, "lastname"]
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
      textInput("firstname", "First name", firstname),
      textInput("lastname", "Last name", lastname),
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