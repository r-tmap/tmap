\dontrun{
data(NLD_muni, NLD_prov)

tm_shape(NLD_muni) +
	tm_polygons("origin_native", border.alpha=0.5, style="cont", title="Native Dutch (%)") +
	tm_logo("http://statline.cbs.nl/Statweb/Images/cbs_logo.png", 
        position=c("left", "bottom"), height = 2) +
	tm_layout(bg.color="gray98")

data(World)

tm_shape(World) +
	tm_polygons("HPI", palette="RdYlGn", auto.palette.mapping=FALSE) +
	tm_logo(c("https://www.r-project.org/logo/Rlogo.png", 
        system.file("img/tmap.png", package="tmap"))) +
	tm_logo("http://blog.kulikulifoods.com/wp-content/uploads/2014/10/logo.png", 
        height=5, position = c("left", "top")) +
	tm_format("World")
}
