library(shiny)
library(mrds)
source("helper.r")
shinyServer(function(input, output,session) {
			# Load data from file; update width with max distance
			data <- reactive({
						maxcut <- 0
						if (is.null(input$datafile)) {
							# User has not uploaded a file yet
							return(NULL)
						}
						else
						{
							obs<-read.delim(input$datafile$datapath)
							left <- 0
							if (input$dobins == 0)
							{
								if(!is.null(obs$distbegin)&!is.null(obs$distend))
								{
									cutpoints=sort(unique(c(obs$distbegin,obs$distend)))
									updateCheckboxInput(session,"binned",value=TRUE)
									updateTextInput(session,"cutpoints",value=paste("c(",paste(cutpoints,collapse=","),")",sep=""))
									left <- cutpoints[1]
									maxcut <- cutpoints[length(cutpoints)]
								}        
							} else
							{
								if(input$binned)
								{
									input$dobins
									isolate({
												if(input$cutpoints!="")
												{
													cutpoints=eval(parse(text=input$cutpoints))
													cutpoints=cutpoints[order(cutpoints)]
													if(any(obs$distance>cutpoints[length(cutpoints)]))
													{
														width <- cutpoints[length(cutpoints)]
														obs<- obs[obs$distance<=cutpoints[length(cutpoints)],]
													}
													if(any(obs$distance<cutpoints[1]))
													{
														left <- cutpoints[1]
														obs<- obs[obs$distance>=left,]
													} 
													int=cut(obs$distance,cutpoints,right=FALSE)
													obs$distbegin=cutpoints[as.numeric(int)]
													obs$distend=cutpoints[as.numeric(int)+1]
													maxcut <- cutpoints[length(cutpoints)]
												 }
											})
								}        
							}
							if(input$width==0)
								width<-max(obs$distance)
							else
							{
								width<-input$width
								width <- max(width, maxcut)
							}
							obs <- obs[obs$distance<width,]          
							updateNumericInput(session,"width",value=width)        
						}
						return(list(obs=obs,width=width,left=left))
					})
			# Render to data table
			output$datatable <- renderDataTable(data()$obs)
			# Once go has been pressed, fit model
			results <- reactive({
						if (input$goButton == 0)
							invisible()
						else
						{
							input$goButton
							isolate({
										if(is.null(data()))invisible()
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
										if(input$type=="Point")
											meta.data=list(point=TRUE)
										else
											meta.data=list(point=FALSE)
										meta.data$width <- data()$width
										meta.data$left <- data()$left
										if(input$binned)meta.data$binned <- TRUE
										if(input$binned)meta.data$breaks <- input$cutpoints
										if(is.null(adj))
										{
											if(key!="unif")
												if(input$scale.formula!="~1" & input$scale.formula!="")
													eval(parse(text=paste('ddf(dsmodel = ~mcds(key = "',key,'",formula = ',input$scale.formula,'), data = df[df$observer==1, ],method = "ds", meta.data = meta.data)',sep="")))  
												else
													eval(parse(text=paste('ddf(dsmodel = ~cds(key = "',key,'"), data = df[df$observer==1, ],method = "ds", meta.data = meta.data)',sep="")))  
											else
												eval(parse(text=paste('ddf(dsmodel = ~cds(key = "unif"), data = df[df$observer==1, ],method = "ds", meta.data = meta.data',sep="")))  
										} else
											eval(parse(text=paste('ddf(dsmodel = ~cds(key = "',key,'",adj.series="',adj,'",adj.order=',input$order,'), data = df[df$observer==1, ],method = "ds", meta.data = meta.data)',sep="")))
									})
						}
					})
			# Reactive to number of bins, compute gof
			gof <- reactive({
						if(is.null(data()))invisible()
						isolate({width<-data()$width})
						if(!input$binned)
							bins<-seq(0, width, length.out = input$bins + 1)
						else
							bins <- eval(parse(text=input$cutpoints))
						gof <- ddf.gof(results(),qq=FALSE,breaks=bins)
						list(bins=bins,gof=gof)
					})
			# Reactive to number of bins, produce and output plot
			output$distPlot <- renderPlot({
						if (input$goButton == 0) return()
						else
						{
							if(is.null(data()))invisible()
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
							if(is.null(data()))invisible()
							t(chitable(gof()$gof$chisquare$chi1$observed, gof()$gof$chisquare$chi1$expected))                
						}
					})
# Once go button pushed output gof chi-square P-value
			output$P <- renderText( 
					{
						if (input$goButton == 0)
							invisible()
						else
						{
							if(is.null(data()))invisible()
							if(!is.na(gof()$gof$chisquare$chi1$p)){
								paste("\nP =",format(gof()$gof$chisquare$chi1$p,digits=5),
										" with ",gof()$gof$chisquare$chi1$df," degrees of freedom\n",sep="")
							}else{
								"\nNo degrees of freedom for test\n"
							}
						}})
			output$fit <- reactive( 
					{
						if (input$goButton == 0)
							invisible()
						else
						{
							input$goButton
							isolate({
										if(is.null(data()))invisible()
										paste(capture.output(print(results())),collapse="\n")
									})
						}
					})
			
		})
