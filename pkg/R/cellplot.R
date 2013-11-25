cellplot <- function(x,y, vp=NULL, e){
	name <- paste("(", deparse(substitute(x)),",",deparse(substitute(y)), ")", sep="")
	pushViewport(viewport( name=name, layout.pos.row=x, layout.pos.col=y))
	n <- 1
	if (!is.null(vp)){ 
		pushViewport(vp)
		n <- n + 1
	}
	e
	popViewport(n=n)
}