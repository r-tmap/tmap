tm_add_legend <- function(type = c("fill", "symbol", "text", "line"), 
						  labels, 
						  col=NULL, 
						  size=NULL, 
						  shape=NULL,
						  lwd=NULL,
						  text=NULL, 
						  alpha=NA,
						  border.col="black",
						  border.lwd=1,
						  border.alpha=NA,
						  title="", 
						  is.portrait=TRUE, 
						  z=NA) {
	g <- list(tm_add_legend=c(as.list(environment()), list(are.dots=FALSE, call=names(match.call(expand.dots = TRUE)[-1]))))
	class(g) <- "tmap"
	g
}
