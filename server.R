
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(mrds)
data(book.tee.data)
tees=book.tee.data$book.tee.dataframe
result=ddf(dsmodel = ~mcds(key = "hn", formula = ~1), data = tees[tees$observer ==1, ], 
           method = "ds", meta.data = list(width = 4))

chitable <- function(observed, expected) {
  x = rbind(observed, expected, (observed - expected)^2/expected)
  x = cbind(x, apply(x, 1, sum))
  colnames(x)[ncol(x)]="Total"
  x=rbind(bin=colnames(x),x)
  rownames(x) = c("Bin","Observed", "Expected", "Chisquare")
  return(x)
}

shinyServer(function(input, output) {
    bins = reactive(seq(0, 4, length.out = input$bins + 1))
    gof = reactive(ddf.gof(result,qq=FALSE,breaks=bins()))
    
    output$distPlot <- renderPlot({
      # plot the data with those bins
      plot(result,breaks=bins())
     })
   
    output$gof <- renderDataTable(
      {
        # compute gof table
        t(chitable(gof()$chisquare$chi1$observed, gof()$chisquare$chi1$expected))
      })
    output$P <- renderText( 
      {
        if(!is.na(gof()$chisquare$chi1$p)){
        paste("\nP =",format(gof()$chisquare$chi1$p,digits=5),
                " with ",gof()$chisquare$chi1$df," degrees of freedom\n",sep="")
        }else{
         "\nNo degrees of freedom for test\n"
      }})
})
