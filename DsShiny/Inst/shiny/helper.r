chitable <- function(observed, expected) {
	x = rbind(observed, expected, (observed - expected)^2/expected)
	x = cbind(x, apply(x, 1, sum))
  x=rbind(x[1,,drop=FALSE],t(apply(x[2:3,,drop=FALSE],1,function(x) sprintf("%0.3f",x))))
	colnames(x)[ncol(x)]="Total"
	x=rbind(bin=colnames(x),x)
	rownames(x) = c("Bin","Observed", "Expected", "Chisquare")
	return(x)
}

ddf_string <- function(key=NULL,adj=NULL,adj.order=NULL,scale.formula="",meta.data,method="ds",mr.formula=~distance)
{
if(is.null(key))
{
  str <- paste("ddf(mrmodel=~glm(formula=",mr.formula,"), data = df,method = '",method,"', meta.data = list(name",meta.data,"))",sep="")   
}else
  if(is.null(mr.formula))
  {
    if(is.null(adj))
    {
      if(key!="unif")
      {
        if(scale.formula!="~1" & scale.formula!="")
          str <- paste("ddf(dsmodel = ~mcds(key = '",key,"',formula =",scale.formula,"), data = df,method = '",method,"', meta.data = list(",meta.data,"))",sep="")
        else
          str <- paste("ddf(dsmodel = ~cds(key = '",key,"'), data = df,method = '",method,"', meta.data = list(",meta.data,"))",sep="")  
      } else
        str <- paste("ddf(dsmodel = ~cds(key = 'unif'), data = df,method ='",method,"', meta.data=list(",meta.data,"))",sep="")
    } else
      str <- paste("ddf(dsmodel = ~cds(key = '",key,"',adj.series='",adj,"',adj.order=",adj.order,"), data = df,method = '",method,"', meta.data = list(name",meta.data,"))",sep="")    
  } else {
    if(is.null(adj))
    {
      if(key!="unif")
      {
        if(scale.formula!="~1" & scale.formula!="")
          str <- paste("ddf(mrmodel=~glm(formula=",mr.formula,"),dsmodel=~mcds(key = '",key,"',formula =",scale.formula,"), data = df,method = '",method,"', meta.data = list(",meta.data,"))",sep="")
        else
          str <- paste("ddf(mrmodel=~glm(formula=",mr.formula,"),dsmodel = ~cds(key = '",key,"'), data = df,method = '",method,"', meta.data = list(",meta.data,"))",sep="")  
      } else
        str <- paste("ddf(mrmodel=~glm(formula=",mr.formula,"),dsmodel = ~cds(key = 'unif'), data = df,method = '",method,"', meta.data = list(",meta.data,"))",sep="")
    } else
      str <- paste("ddf(mrmodel=~glm(formula=",mr.formula,"),dsmodel = ~cds(key = '",key,"',adj.series='",adj,"',adj.order=",adj.order,"), data = df,method = '",method,"', meta.data = list(name",meta.data,"))",sep="")   
  }   
return(str)
}

