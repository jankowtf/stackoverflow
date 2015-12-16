library(shiny)
library(shinydashboard)

# UI ---------------------------------------------------------------------

ui <- fluidPage(
  tabsetPanel(
    id = "panels",
    tabPanel(
      "A",
      p(),
      actionLink("link_to_tabpanel_b", "Link to panel B")
    ),
    tabPanel(
      "B",
      h3("Some information"),
      tags$li("Item 1"),
      tags$li("Item 2"),
      actionLink("link_to_tabpanel_a", "Link to panel A")
    )
  )
)

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  #   observeEvent(input$link_to_tabpanel_b, {
  #     tags$a(href = "#tab-4527-2")
  #   })
  observeEvent(input$link_to_tabpanel_b, {
    newvalue <- "B"
    updateTabItems(session, "panels", newvalue)
  })
  observeEvent(input$link_to_tabpanel_a, {
    newvalue <- "A"
    updateTabsetPanel(session, "panels", newvalue)
  })
}

shinyApp(ui, server)
