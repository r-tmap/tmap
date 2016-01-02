tmap
===


R package for thematic maps

![Classic world map](http://www.von-tijn.nl/tijn/research/tmap_classic.png)

Installation
------------

`tmap` is available on [CRAN](http://cran.r-project.org/package=tmap). The latest version is 1.2 (December 2015).


Development
------------

Odd numbered versions will be development versions, and even numbered versions stable CRAN releases (as with data.table).

The latest development version can be installed using `devtools`.

```r
library(devtools)
install_github("mtennekes/tmap", subdir = "pkg")
```

See [NEWS](https://github.com/mtennekes/tmap/blob/master/pkg/NEWS) for the latest features and improvements.

Do you want to help with the development of the package? Please let me know! I could use some help with the following issues:

* Implementation of [cartograms](https://github.com/mtennekes/tmap/issues/10)
* Improvement of the [interactive tool](https://github.com/mtennekes/tmap/issues/20) `itmap`.
* Finally, any feedback, requests, tips, or bug reports are welcome!


Usage
-----

```r
library(tmap)
```

Example
-----

The example shown above works with the latest CRAN version.

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


[`tmap`][1] is an actively maintained open-source [R][2]-library for drawing thematic maps, written by [Martijn Tennekes][3]. The API is based on [*A Layered Grammar of Graphics*][4] by Hadley Wickham and resembles the syntax of [tag:ggplot2], a popular R-library for drawing charts.



Vignette
-----
[tmap in a nutshell][6]

Presentation
-----
[tmap: creating thematic maps in a flexible way][10]

Other resources
-----

* [Tutorial Creating maps in R][9]
* [Blog post StatialControl][7]
* [Blog post TWIAV][8]


  [1]: http://cran.r-project.org/web/packages/tmap/index.html
  [2]: http://stackoverflow.com/tags/r/info
  [3]: http://stackoverflow.com/users/1393348/martijn-tennekes
  [4]: http://vita.had.co.nz/papers/layered-grammar.pdf
  [5]: https://github.com/mtennekes/tmap
  [6]: https://cran.r-project.org/web/packages/tmap/vignettes/tmap-nutshell.html
  [7]: http://spatcontrol.net/SpatialControl/2015/11/06/tmap-r-package/
  [8]: http://www.twiav.nl/en/blog0002en.php
  [9]: https://github.com/Robinlovelace/Creating-maps-in-R/raw/master/intro-spatial-rl.pdf
  [10]: http://von-tijn.nl/tijn/research/presentations/tmap_user2015.pdf



