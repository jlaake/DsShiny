shinyUI(fluidPage(
				titlePanel("Single Observer Distance Sampling"),
				sidebarLayout(
						sidebarPanel(
								fileInput("datafile","Data file"),
								checkboxInput("binned","Binned Distances",value=FALSE),
								numericInput("width","Width",value=0,min=0),
								conditionalPanel(
										condition = "input.binned",
										textInput("cutpoints", "Bin Cut Points",value="")),
								conditionalPanel(
										condition = "input.binned",
										actionButton("dobins", "Create bins")),
								hr(),
								radioButtons("type","Sample type",choices=c("Line","Point"),selected="Line"),
								hr(),
								selectInput("keyfct","Key Function",choices=c("Uniform","Half-normal","Hazard rate","Gamma"),selected="Half-normal"),
								selectInput("adjfct","Adjustment Function",choices=c("None","Cosine","Simple Polynomial","Hermite polynomial"),selected="None"),
								conditionalPanel(
										condition = "input.adjfct != 'None'",
										textInput("order", "Order")),
								conditionalPanel(
										condition = "input.adjfct == 'None' & input.keyfct!='Uniform'",
										textInput("scale.formula", "Scale Formula",value="~1")),
								actionButton("goButton", "Fit model"),
								hr(),
								conditionalPanel(
										condition = "!input.binned",
										sliderInput("bins",
												"Number of bins:",
												min = 1,
												max = 50,
												value = 4))
						),
						# Show a plot of the generated distribution
						mainPanel(		
								tabsetPanel(type = "tabs", 
										tabPanel("Data",dataTableOutput("datatable")),
										tabPanel("Fitted Model",verbatimTextOutput("fit")),
										tabPanel("Plot", plotOutput("distPlot")),  
										tabPanel("GOF", 
												htmlOutput("P"),
												hr(),
												dataTableOutput("gof"))
								)
						)
				)
		))
