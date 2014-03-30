cellplot <- function(x,y, vp=NULL, e){
	pushViewport(viewport(layout.pos.row=x, layout.pos.col=y))
	n <- 1
	if (!is.null(vp)){ 
		pushViewport(vp)
		n <- n + 1
	}
	e
	upViewport(n=n)
}