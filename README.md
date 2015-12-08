tmap
===

R package for thematic maps

[![Travis-CI Build Status](https://travis-ci.org/mtennekes/tmap.svg?branch=master)](https://travis-ci.org/mtennekes/tmap)

![Classic world map](http://www.von-tijn.nl/tijn/research/tmap_classic.png)

Installation
------------

`tmap` is available on [CRAN](http://cran.r-project.org/package=tmap)!

The latest development version can be installed using `devtools`.

```r
library(devtools)
install_github("mtennekes/tmap", subdir = "pkg")
```

Next CRAN release, version 1.2, expected mid December. From now on, odd numbered versions will be development versions, and even numbered versions stable CRAN releases (as with data.table).


Usage
-----

```r
library(tmap)
```

Example
-----

The example shown above works with the latest development version.

```r

# load elevation raster data, and country polygons
data(land, World)

# convert to Eckert IV projection
land_eck4 <- set_projection(land, "eck4")

# plot
tm_shape(land_eck4) +
	tm_raster("elevation", 
		breaks=c(-Inf, 250, 500, 1000, 1500, 2000, 2500, 3000, 4000, Inf),  
		palette = terrain.colors(9), 
		title="Elevation") +
tm_shape(World) +
	tm_borders("grey20") +
	tm_grid(projection="longlat", labels.size = .5) +
	tm_text("name", size="AREA") +
	tm_compass(position = c(.65, .15), color.light = "grey90") +
	tm_credits("Eckert IV projection", position = c("RIGHT", "BOTTOM")) +
	tm_layout(inner.margins=c(.04,.03, .02, .01), 
		legend.position = c("left", "bottom"), 
		legend.frame = TRUE, 
		bg.color="lightblue", 
		legend.bg.color="lightblue", 
		earth.boundary = TRUE, 
		space.color="grey90") + 
	tm_style_classic()
```



-----

New features (already implemented):

* Open Street Map layers support, see ```read_osm```
* Handy ```bb``` function for setting and changing bounding boxes
* Small multiples can take argument for each small multiple
* Interactive SVG output (function ```itmap```)
* Map attributes and styling (see example above)

Features to be included in later versions:

* Flow map
* Cartogram


