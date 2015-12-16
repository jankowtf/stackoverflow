library(shiny)

# UI ---------------------------------------------------------------------

ui <- fluidPage(
  tabsetPanel(
    tabPanel(
      "A",
      p(),
      actionLink("link_to_tabpanel_b", "Link to panel B")
    ),
    tabPanel(
      "B",
      h3("Some information"),
      tags$li("Item 1"),
      tags$li("Item 2")
    )
  )
)

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  observeEvent(input$link_to_tabpanel_b, {
    tags$a(href = "#tab-4527-2")
  })
}

shinyApp(ui, server)