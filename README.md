tmap: thematic maps in R
===


![Classic world map](http://www.von-tijn.nl/tijn/research/tmap_classic.png)

[`tmap`][1] is an actively maintained open-source [R][2]-library for drawing thematic maps, written by [Martijn Tennekes][3]. The API is based on [*A Layered Grammar of Graphics*][4] by Hadley Wickham and resembles the syntax of `ggplot2`, a popular R-library for drawing charts.


Installation
------------

`tmap` is available on [CRAN](http://cran.r-project.org/package=tmap). The latest version is 1.6 (2016-10-21).


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

* Interactive small multiples (i.e. small multiples in view mode)
* Flow map
* Finally, any feedback, requests, tips, or bug reports are welcome!


Usage
-----

```r
library(tmap)
```

Examples
-----

See package documentation for many more examples.

### 1. Classic World map

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

Result: see above

### 2. US Choropleth

[R code][13]

![US Choropleth](http://www.von-tijn.nl/tijn/research/tmap/USchoro.png)

### 3. Crimes in Greater London

[R code][14]

![Dotmap of crimes in Greater London](http://www.von-tijn.nl/tijn/research/tmap/dotmap.png)

![Dasymetric map of crimes in Greater London](http://www.von-tijn.nl/tijn/research/tmap/dasy.png)

![Crimes in London city by type](http://www.von-tijn.nl/tijn/research/tmap/crimes_facets.png)

![Screenshot interactive map](http://www.von-tijn.nl/tijn/research/tmap/view_crimes.jpg)



Vignettes
-----
[tmap in a nutshell][6]

[tmap modes: plot and interactive view][11]

Presentation
-----
[tmap: creating thematic maps in a flexible way][10]

Other resources
-----

* [Tutorial Creating maps in R][9]
* [Blog post StatialControl][7]
* [Blog post TWIAV][8]
* [Computer World: Create maps in R in 10 (fairly) easy steps][12]
* [Tutorial Visualising spatial data: from base to shiny - workshop][15]

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
  [11]: https://cran.r-project.org/web/packages/tmap/vignettes/tmap-modes.html
  [12]: http://cwrld.us/Rmaps10
  [13]: https://github.com/mtennekes/tmap/blob/master/demo/US_choropleth.R
  [14]: https://github.com/mtennekes/tmap/blob/master/demo/crimes_in_Greater_London.R
  [15]: https://github.com/Robinlovelace/Creating-maps-in-R/blob/master/vignettes/vspd-base-shiny.Rmd
