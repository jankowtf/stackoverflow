
# Meta --------------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(DT)
library(RSQLite)
library(digest)
library(timetrackr)

## App name //
app_name <- "Time tracking"

enable_times <- FALSE
sqlite <- TRUE

source("global.R")

# Fields ------------------------------------------------------------------

fields <- c(
  "summary",
  "project",
  "week", 
  "status", 
  "time_estimated",
  "time_spent",
  "time_until_done",
  "description",
  "adhoc",
  "interruption",
  "meeting",
  "_uid",
  "_time_created",
  "_time_modified"
)

# Functions ---------------------------------------------------------------

if (!sqlite) {
  createDynamicUi_issueDetails <- function(
    input,
    output,
    ui_control,
    debug = GLOBALS$debug$enabled
  ) {
    ## Dependencies //
    action_selected_row <- ui_control$idx
    
    ## Aux function //
    getFormValue <- function(field, idx, default = "") {
      if (!is.null(idx)) {
        dat <- db$data
        dat[idx, field]
      } else {
        value <- isolate(input[[field]])
        if (is.null(value)) {
          default
        } else {
          value
        }
      }
    }
    
    ## Form components //
    container <- list()
    
    field <- "summary"
    name <- "Summary"
    print(name)
    value <- getFormValue(field = field, idx = action_selected_row)
    container[[field]] <- textInput(field, name, value)
    
    field <- "description"
    name <- "Description"
    print(name)
    value <- getFormValue(field = field, idx = action_selected_row)
    container[[field]] <- textInput(field, name, value)
    
    field <- "project"
    name <- "Project"
    print(name)
    value <- getFormValue(field = field, idx = action_selected_row)
    container[[field]] <- textInput(field, name, value)
    
    field <- "week"
    name <- "Week"
    print(name)
    value <- getFormValue(field = field, idx = action_selected_row)
    if (is.na(value)) {
      value <- as.numeric(format(Sys.Date(), "%V"))
    }
    container[[field]] <- numericInput(field, name, value,
      min = 1, max = 53, step = 1)
    
    field <- "status"
    name <- "Status"
    print(name)
    value <- getFormValue(field = field, idx = action_selected_row,
      default = validStatuses()["todo"])
    container[[field]] <- selectInput(field, name, unname(validStatuses()),
      selected = value)
    
    field <- "time_estimated"
    name <- "Time estimated"
    print(name)
    value <- getFormValue(field = field, idx = action_selected_row)
    container[[field]] <- textInput(field, name, value)
    
    field <- "adhoc"
    name <- "Ad hoc"
    print(name)
    value <- getFormValue(field = field, idx = action_selected_row, default = FALSE)
    if (is.null(value) || is.na(value)) {
      value <- FALSE
    }
    container[[field]] <- checkboxInput(field, name, value = value)
    
    field <- "interruption"
    name <- "Interruption"
    print(name)
    value <- getFormValue(field = field, idx = action_selected_row, default = FALSE)
    if (is.null(value) || is.na(value)) {
      value <- FALSE
    }
    container[[field]] <- checkboxInput(field, name, value = value)
    
    field <- "meeting"
    name <- "Meeting"
    print(name)
    value <- getFormValue(field = field, idx = action_selected_row, default = FALSE)
    if (is.null(value) || is.na(value)) {
      value <- FALSE
    }
    container[[field]] <- checkboxInput(field, name, value = value)
    
    ## Bundle in box //
    value <- if (ui_control$case == "create") {
      container$buttons <- div(style="display:inline-block",
        actionButton("action_task_create_2", "Create"),
        actionButton("action_task_create_cancel", "Cancel")
      )
      do.call(box, args = list(container, title = "Create task",
        status = "primary", width = NULL))
    } else if (ui_control$case == "update") {
      container$buttons <- div(style="display:inline-block",
        actionButton("action_task_update", "Update"),
        actionButton("action_task_update_cancel", "Cancel"),
        p(),
        actionButton("action_task_delete", "Delete",
          icon = icon("exclamation-triangle"))
      )
      do.call(box, args = list(container, title = "Update task",
        status = "danger", width = NULL))
    } else {
      stop("Not implemented")
    }
    value
  }
}

# UI ----------------------------------------------------------------------

ui <- dashboardPage(
  ## Header //
  dashboardHeader(title = app_name),
  
  ## Sidebar content //
  dashboardSidebar(
    sidebarMenu(
      menuItem("Tasks", tabName = "tasks", icon = icon("database"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "tasks",
        fluidRow(
          column(width = 7,
            div()
          ),
          column(width = 3,
            actionButton("action_task_create", "Create task")
          )
        ),
        p(),
        fluidRow(
          column(width = 7,
            box(
              title = "Task list",
              DT::dataTableOutput("dt_issues"),
              width = NULL
            )
          ),
          column(width = 3,
            # uiOutput('ui_times'),
            uiOutput("ui_issues")
          )
        )
      )
    )
  )
)

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  ## Initialize DB //
  if (!sqlite) {
    db <- reactiveValues(
      data = as.data.frame(matrix(NA, ncol = length(fields), 
        dimnames = list(NULL, fields)))
    )
  } else {
    app$prepare(
      public_fields_compact = GLOBALS$db$tables$issues$public_fields_compact,
      public_fields_details = GLOBALS$db$tables$issues$public_fields_details,
      private_fields = GLOBALS$db$tables$issues$private_fields,
      times_public_fields_compact = GLOBALS$db$tables$times$public_fields_compact,
      times_public_fields_details = GLOBALS$db$tables$times$public_fields_details,
      times_private_fields = GLOBALS$db$tables$times$private_fields
    )
  }
  
  ui_control_issues <- reactiveValues(
    show = FALSE,
    case = c("create", "update")[1],
    idx = NULL
  )
  
  observe({
    idx <- input$dt_issues_rows_selected
    if (!is.null(idx)) {
      ui_control_issues$show <- TRUE
      ui_control_issues$case <- "update"
    } else {
      ui_control_issues$show <- FALSE
    }
    ui_control_issues$idx <- idx
  })
  
  ## Assemble dynamic UI: issue details //
  output$ui_issues <- renderUI({
    if (!ui_control_issues$show) {
      return()
    }
    createDynamicUi_issueDetails(input, output,
      ui_control = ui_control_issues)
  })
  
  ## Database //
  #   createRecord <- function(input, db) {
  #     db$data <- rbind(
  #       db$data, 
  #       data.frame(
  #         firstname = input$firstname,
  #         lastname =input$lastname,
  #         stringsAsFactors = FALSE
  #       )
  #     )
  #   }
  #   updateRecord <- function(input, db, ui_control) {
  #     db$data[ui_control$selection, ] <- data.frame(
  #       firstname = input$firstname,
  #       lastname =input$lastname,
  #       stringsAsFactors = FALSE
  #     )
  #   }
  
  ## Render //
  if (!sqlite) {
    output$dt_issues <- DT::renderDataTable(
      db$data, server = FALSE,
      filter = "top",
      width = "100%", class = "cell-border stripe",
      selection = "single",
      options = list(
        dom = "ltipr",
        autoWidth = TRUE,
        columnDefs = list(list(width = '300px', targets = "_all"))
        
      )
    )
  } else {
    output$dt_issues <- DT::renderDataTable({
      renderResults_dbTableIssues(input)
    }, filter = "top",
      width = "100%", class = "cell-border stripe",
      selection = "single",
      options = list(
        dom = "ltipr",
        autoWidth = TRUE,
        columnDefs = list(list(width = '300px', targets = "_all"))
        
      )
    )
  }
}

# Launch  ---------------------------------------------------------------

shinyApp(ui, server)
