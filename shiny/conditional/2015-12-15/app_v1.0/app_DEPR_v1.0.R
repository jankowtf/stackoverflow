library(shiny)

GLOBALS <- list()
GLOBALS$debug$enabled <- TRUE

# Auxiliary functions -----------------------------------------------------

createDynamicUi_conditional <- function(
  input,
  output,
  ui_decision,
  debug = GLOBALS$debug$enabled
) {
  if (debug) {
    message("Dynamic UI: conditional ----------")
    print(Sys.time())
  }
  
  ## Form components //
  container <- list()
  
  field <- "title"
  name <- "Title"
  value <- ""
  container[[field]] <- textInput(field, name, value)
  
  field <- "description"
  name <- "Description"
  value <- ""
  container[[field]] <- textInput(field, name, value)
  
  ## Bundle in box //
  value <- if (ui_decision == "hide") {
    div()
  } else if (ui_decision == "show" || ui_decision == "create") {
    container$buttons <- div(style="display:inline-block",
      actionButton("action_create", "Create"),
      actionButton("action_cancel", "Cancel")
    )
    do.call(div, args = list(container, title = "conditional dynamic UI"))
  } else {
    "Not implemented yet"
  }
  # print(value)
  value
}

# UI ----------------------------------------------------------------------

ui <- fluidPage(
  actionButton("action_trigger", "Trigger 1"),
  h3("Database state"),
  textOutput("result"),
  p(),
  uiOutput("ui_conditional")
)

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  #####################
  ## REACTIVE VALUES ##
  #####################
  
  db <- reactiveValues(
    title = "",
    description = ""
  )
  
  ui_control <- reactiveValues(
    action_trigger = 0,
    action_trigger__last = 0,
    action_create = 0,
    action_create__last = 0,
    action_cancel = 0,
    action_cancel__last = 0
  )
  
  #################
  ## UI DECISION ##
  #################
  
  ui_decision <- reactive({
    ## Dependencies //
    ## Trigger button:
    value <- input$action_trigger
    if (ui_control$action_trigger != value) ui_control$action_trigger <- value
    
    ## Create button:
    ## Dynamically created within `createDynamicUi_conditional`
    value <- input$action_create
    if (is.null(value)) {
      value <- 0
    }
    if (ui_control$action_create != value) {
      ui_control$action_create <- value
    }
    
    ## Cancel button:
    ## Dynamically created within `createDynamicUi_conditional`
    value <- input$action_cancel
    if (is.null(value)) {
      value <- 0
    }
    if (ui_control$action_cancel != value) {
      ui_control$action_cancel <- value
    }
    
    if (GLOBALS$debug$enabled) {
      message("Dependency clearance -----")
      message("action_trigger:")
      print(ui_control$action_trigger)
      print(ui_control$action_trigger__last)
      message("action_create:")
      print(ui_control$action_create)
      print(ui_control$action_create__last)
      message("action_cancel:")
      print(ui_control$action_cancel)
      print(ui_control$action_cancel__last)
    }
    ui_decision <- if (
      c (ui_control$action_trigger == 0 && ui_control$action_trigger == 0) ||
        c(
          ui_control$action_trigger > 0 &&
            ui_control$action_trigger <= ui_control$action_trigger__last &&
            
            ui_control$action_cancel > 0 &&
            ui_control$action_cancel > ui_control$action_cancel__last
        ) ||
        c(
          ui_control$action_create == 0 &&
            ui_control$action_create__last > 0
        )
    ) {
      "hide"
    } else if (
      ui_control$action_trigger >= ui_control$action_trigger__last &&
        ui_control$action_create == ui_control$action_create__last
    ) {
      ## Synchronize //
      ui_control$action_cancel__last <- ui_control$action_cancel
      "show"
    } else if (
      ui_control$action_create > ui_control$action_create__last
    ) {
      "create"
    } else {
      "Not implemented yet"
    }
    if (GLOBALS$debug$enabled) {
      print(ui_decision)
    }
    ## Synchronize //
    ui_control$action_trigger__last <- ui_control$action_trigger
    ui_control$action_create__last <- ui_control$action_create
    
    ui_decision
  })
  
  output$ui_conditional <- renderUI({
    createDynamicUi_conditional(input, output, ui_decision = ui_decision())
  })
  
  #################
  ## WRITE TO DB ##
  #################
  
  writeToDb <- reactive({
    ui_decision <- ui_decision()
    if (ui_decision == "create") {
      db$title <- input$title
      db$description <- input$description
    }
  })
  
  ###################
  ## RENDER RESULT ##
  ###################
  
  output$result <- renderText({
    writeToDb()
    c(
      paste0("Title: ", db$title),
      paste0("Description: ", db$description)
    )
  })
}

shinyApp(ui, server)