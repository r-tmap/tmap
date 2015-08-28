# download Dutch neighborhood shape file
dir <- tempdir()
temp <- tempfile()

download.file("http://www.cbs.nl/nl-NL/menu/themas/dossiers/nederland-regionaal/links/2014-buurtkaart-shape-versie-1-el.htm", temp, mode="wb")
unzip(temp, exdir = dir)

#NLD_nbhd <- read_shape("../../shape_files/buurt_2014.shp")
NLD_nbhd <- read_shape(file.path(dir, "buurt_2014.shp"))

# fix self-intersection but in shape object
NLD_nbhd <- rgeos::gBuffer(NLD_nbhd, byid=TRUE, width=0)


NLD_Horst <- NLD_nbhd[NLD_nbhd$GM_NAAM=="Horst aan de Maas", ]
qtm(NLD_Horst) + tm_text("BU_NAAM")

NLD_Sevenum <- NLD_Horst[NLD_Horst$BU_NAAM %in% c("Sevenum", "Verspreide huizen Sevenum"), ]

qtm(NLD_Sevenum[2,]) + tm_layout(bg.color="blue")



# from http://journal.r-project.org/archive/2012-2/RJournal_2012-2_Murrell2.pdf
x <- c(.1, .5, .9,
	   .5, .9, .1,
	   .4, .5, .6,
	   .4, .6, .5)
y <- c(.1, .8, .1,
	   .8, .1, .8,
		.5, .4, .5,
		 .3, .3, .2)
id <- rep(1:4, each=3)

grid.newpage()
grid.path(x, y, id=id,
		  gp=gpar(fill=c("red", "blue", "grey", "green")))
grid.export("path.svg")


grid.polygon(x, y, id=id,
		  gp=gpar(fill=c("red", "blue", "grey")))
grid.export("polygon.svg")



grid.newpage()
grid.path(x, y, id=id,
		  rule="evenodd",
		  gp=gpar(fill=rep("grey", 3)))

qtm(NLD_Sevenum[2,]) + tm_layout(bg.color="blue")
qtm(NLD_Sevenum[1,]) + tm_layout(bg.color="blue")
qtm(NLD_Sevenum[1:2,]) + tm_layout(bg.color="blue")
qtm(NLD_Sevenum[2:1,]) + tm_layout(bg.color="blue")


data(World)
tm_shape(World) + tm_polygons("pop_est")
tmap_svg <- grid.export(name = NULL)$svg


require(gridSVG)

example(grid.path)

grid.export("path.svg")





