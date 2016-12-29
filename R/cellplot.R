cellplot <- function(x, y, name=NULL, vp=NULL, e) {
	pushViewport(viewport(layout.pos.row=x, layout.pos.col=y, name=name, clip=TRUE))
	n <- 1
	if (!is.null(vp)){ 
		pushViewport(vp)
		n <- n + 1
	}
	v <- current.viewport()
	x <- e
	upViewport(n=n)
	gTree(children=gList(x), vp=v, name=name)
}
