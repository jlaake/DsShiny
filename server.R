library(shiny)
library(mrds)

chitable <- function(observed, expected) {
	x = rbind(observed, expected, (observed - expected)^2/expected)
	x = cbind(x, apply(x, 1, sum))
	colnames(x)[ncol(x)]="Total"
	x=rbind(bin=colnames(x),x)
	rownames(x) = c("Bin","Observed", "Expected", "Chisquare")
	return(x)
}

shinyServer(function(input, output,session) {
# Load data from file; update width with max distance
			data <- reactive({
						if (is.null(input$datafile)) {
							# User has not uploaded a file yet
							return(NULL)
						}
						else
						{
							obs<-read.delim(input$datafile$datapath)
							if(input$width==0)
								width<-max(obs$distance)
							else
								width<-input$width
						}
						updateNumericInput(session,"width",value=width)
						return(list(obs=obs,width=width))
					})
# Render to data table
			output$datatable <- renderDataTable(data()$obs)
# Once go pressed, fit model
			results <- reactive({
						if (input$goButton == 0)
							return(NULL)
						else
						{
							input$goButton
							isolate({
										if(is.null(data()))return(NULL)
										df=data()$obs
										key<-switch(input$keyfct,
												"Half-normal"="hn",
												"Hazard rate"="hr",
												"Uniform"="unif",
												"Gamma"="gamma")
										adj<-switch(input$adjfct,
												"Cosine"="cos",
												"Simple Polynomial"="poly",
												"Hermite Polynomial"="herm",
												"None"=NULL)
										if(is.null(adj))
										{
											eval(parse(text=paste('ddf(dsmodel = ~mcds(key = "',key,'",formula = ~size), data = df[df$observer==1, ],method = "ds", meta.data = list(width = data()$width))',sep="")))  
										} else
										{
											eval(parse(text=paste('ddf(dsmodel = ~mcds(key = "',key,'",adj.series="',adj,'",formula = ~size), data = df[df$observer==1, ],method = "ds", meta.data = list(width = data()$width))',sep="")))
										}})
						}
					})
# Reactive to number of bins, compute gof
			gof <- reactive({
						if(is.null(data()))return(NULL)
						bins<-seq(0, data()$width, length.out = input$bins + 1)
						gof <- ddf.gof(results(),qq=FALSE,breaks=bins)
						list(bins=bins,gof=gof)
					})
# Reactive to number of bins, produce and output plot
			output$distPlot <- renderPlot({
						if (input$goButton == 0) return()
						else
						{
							if(is.null(data()))return(NULL)
							plot(results(),breaks=gof()$bins)
						}
					})
# Once go button pushed output gof chi-square table
			output$gof <- renderDataTable(
					{
						if (input$goButton == 0)
							return()
						else
						{
							if(is.null(data()))return(NULL)
							t(chitable(gof()$gof$chisquare$chi1$observed, gof()$gof$chisquare$chi1$expected))                
						}
					})
# Once go button pushed output gof chi-square P-value
			output$P <- renderText( 
					{
						if (input$goButton == 0)
							return()
						else
						{
							if(is.null(data()))return(NULL)
							if(!is.na(gof()$gof$chisquare$chi1$p)){
								paste("\nP =",format(gof()$gof$chisquare$chi1$p,digits=5),
										" with ",gof()$gof$chisquare$chi1$df," degrees of freedom\n",sep="")
							}else{
								"\nNo degrees of freedom for test\n"
							}
						}})
		})
