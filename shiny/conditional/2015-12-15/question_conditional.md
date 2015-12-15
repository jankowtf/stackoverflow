Actual question
----
  
  How do I design(*) a shiny app where certain UI elements depend on *multiple* conditions that need to be systematically handle (as opposed to just one condition)?


(*) in a maintainable way that won't drive you mad ;-)

------

Details
----

I've read [Build a dynamic UI that reacts to user input](http://shiny.rstudio.com/articles/dynamic-ui.html) and like `conditionalPanel()`, but I have the feeling it's too "one-dimensional" for the app I have in mind (see bottom).

**What I want to be able to do:**

1. Have one (or more) UI element(s) that can trigger conditional UI parts:

*State 1*

[![State 1][1]][1]

2. Those conditional UI parts usually have some input fields and at least two action buttons: `Create` and `Cancel`:

*State 2*

[![State 2][2]][2]

3. If `Create` is clicked, the input should be appropriately processed (e.g. writing stuff to a DB) and then the conditional UI part should "disappear" again as its condition "expired":

*State 3*

[![State 3][3]][3]


*State 4*

[![State 4][4]][4]

4. If `Cancel` is clicked, the UI part should "disappear" again as its condition "expired":

*State 4*

[![State 4][4]][4]

5. A subsequent click on `Trigger` should "start the cycle" again

**Problem with multiple dependencies and dynamic dependency states:**

AFAIU, I can't simply put the dependencies (see `action_trigger`, `action_create` and `action_cancel` below) into the reactive context that builds the conditional UI as some dependency are dynamically created (see `output$ui_conditional <- renderUI({})` below). 

This results in a lot of invalidation/adjustment steps until a "stable state" is reached.

That's why I came up with the idea of introducing sort of a "dependency state clearance" layer (see `ui_decision <- reactive({})` below)

Current solution
-----

My current solution feels very wrong, very fragile and very high maintenance. You can also find it at [GitHub]()

**Globals:**

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

**UI part:**

# UI ----------------------------------------------------------------------

ui <- fluidPage(
actionButton("action_trigger", "Trigger 1"),
h3("Database state"),
textOutput("result"),
p(),
uiOutput("ui_conditional")
)

**Server part:**

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

**Running the app:**

shinyApp(ui, server)

Big picture
-----

This is the app that I'm actually having in mind: [`timetrackr`]()

It has been build without introducing a clearance layer as drafted above. While it does provide the desired functionality, very often, you need to click UI elements more than once until a stable dependency state is reached which is really irritating.


  [1]: http://i.stack.imgur.com/J0V57.png
  [2]: http://i.stack.imgur.com/cu8ex.png
  [3]: http://i.stack.imgur.com/pbxOF.png
  [4]: http://i.stack.imgur.com/4YD7K.png