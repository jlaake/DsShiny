chitable <- function(observed, expected) {
	x = rbind(observed, expected, (observed - expected)^2/expected)
	x = cbind(x, apply(x, 1, sum))
	colnames(x)[ncol(x)]="Total"
	x=rbind(bin=colnames(x),x)
	rownames(x) = c("Bin","Observed", "Expected", "Chisquare")
	return(x)
}


