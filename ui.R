
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
				
				# Application title
				titlePanel("Single Observer Distance Sampling"),
				
				# Sidebar with a slider input for number of bins
				sidebarLayout(
						sidebarPanel(
								fileInput("datafile","Data file"),
								numericInput("width","Width",value=0,min=0),
								selectInput("keyfct","Key Function",choices=c("Unif","Half-normal","Hazard rate","Gamma"),selected="Half-normal"),
								selectInput("adjfct","Adjustment Function",choices=c("None","Cosine","Simple Polynomial","Hermite polynomial"),selected="None"),
								actionButton("goButton", "Fit model"),
								hr(),
								sliderInput("bins",
										"Number of bins:",
										min = 1,
										max = 50,
										value = 4),
								htmlOutput("P")
						),
						
						# Show a plot of the generated distribution
						mainPanel(		
								tabsetPanel(type = "tabs", 
										tabPanel("Data",dataTableOutput("datatable")),
										tabPanel("Plot", plotOutput("distPlot")),  
										tabPanel("GOF", dataTableOutput("gof"))
								)
						)
				)
		))
