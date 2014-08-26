#' Create Rmarkdown document using knitr
#' 
#' @usage create_document(type="html")
#'        create_knitr(output,type="html")
#' @param output result list from shinyApp
#' @param type "html","pdf" or "word"
#' @return filename of result from render in markdown
#' @export create_document
#' @import shiny rmarkdown knitr
#' @aliases create_document create_knitr
#' @author Jeff Laake
create_document <- function(type="html")
{
#	library(shiny)
#	library(knitr)
#	library(rmarkdown)
	create_knitr(runApp(file.path(system.file(package="DsShiny"),"shiny")),type=type)
}
create_knitr <- function(output,type="html")
{
	shinypath=file.path(system.file(package="DsShiny"),"shiny")
	con <- file("data.csv",open="wt")
	writeLines(output$data,con)
	close(con)
	con <- file(file.path(shinypath,"distance_knitr_template.rmd"),open="rt")
	knitr_string <- readLines(con)
	idx <- grep("ddf_string",knitr_string)
	knitr_string[idx] <- gsub("ddf_string",output$ddfstring,knitr_string[idx])
	idx <- grep("bin_string",knitr_string)
	knitr_string[idx] <- gsub("bin_string",output$binstring,knitr_string[idx])
	close(con)
	con <- file("distance_knitr.rmd",open="wt")
	writeLines(knitr_string,con)
	close(con)
	if(type=="html")
	{
		render("distance_knitr.rmd",html_document())
		file="distance_knitr.html"
	}
	else
	if(type=="pdf")
	{
		render("distance_knitr.rmd",pdf_document())
		file="distance_knitr.pdf"
	}
	else
	{
		render("distance_knitr.rmd",word_document())
		file="distance_knitr.docx"
	}
	# code from Yihui's animate package
	if (.Platform$OS.type == 'windows') {
		try(shell.exec(file))
	} else if (Sys.info()['sysname'] == 'Darwin') {
		system(paste('open ', shQuote(file)))
	} else system(paste('xdg-open ', shQuote(file)))
}


