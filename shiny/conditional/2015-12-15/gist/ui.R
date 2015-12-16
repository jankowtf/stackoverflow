library(shiny)

# Define UI for random distribution application 
shinyUI(pageWithSidebar(
  
	# Application title
	headerPanel("Tabsets"),
	
	# Sidebar with controls to select the random distribution type
	# and number of observations to generate. Note the use of the br()
	# element to introduce extra vertical spacing
	sidebarPanel(
		radioButtons("dist", "Distribution type:",
					 list("Normal" = "norm",
					 	 "Uniform" = "unif",
					 	 "Log-normal" = "lnorm",
					 	 "Exponential" = "exp")),
		br(),
		
		sliderInput("n", 
					"Number of observations:", 
					value = 500,
					min = 1, 
					max = 1000)
	),
	
	# Show a tabset that includes a plot, summary, and table view
	# of the generated distribution
	mainPanel(
		tabsetPanel(
			tabPanel("Plot", plotOutput("plot"),div(id="linkToSummary",tags$a("This is a link to Summary Tab")) ), 
			tabPanel("Summary", verbatimTextOutput("summary")), 
			tabPanel("Table", tableOutput("table"),HTML("<script>$('#linkToSummary').click(function() {
						 tabs = $('.tabbable .nav.nav-tabs li')
					 	 tabs.each(function() {
							$(this).removeClass('active')
					 	 })
						 $(tabs[1]).addClass('active')
						
						 tabsContents = $('.tabbable .tab-content .tab-pane')
					 	 tabsContents.each(function() {
							$(this).removeClass('active')
					 	 })
						 $(tabsContents[1]).addClass('active')

						$('#summary').trigger('change').trigger('shown');
						 
					 })</script>
			")	)
		)
	)
))