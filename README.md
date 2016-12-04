tmap: thematic maps in R
===

[`tmap`][1] is an actively maintained open-source [R][2]-library for drawing thematic maps, written by [Martijn Tennekes][3]. The API is based on [*A Layered Grammar of Graphics*][4] by Hadley Wickham and resembles the syntax of `ggplot2`, a popular R-library for drawing charts.

<span>
<a href = "https://github.com/mtennekes/tmap/tree/master/demo/ClassicMap"><img src="http://www.von-tijn.nl/tijn/research/tmap/icons/classic.jpg" alt="Classic World Map" height="125px"/></a>
<a href = "https://github.com/mtennekes/tmap/tree/master/demo/BubbleMap"><img src="http://www.von-tijn.nl/tijn/research/tmap/icons/bubble.jpg" alt="Bubble Map" height="125px"/></a>
<a href = "https://github.com/mtennekes/tmap/tree/master/demo/BubbleMap"><img src="http://www.von-tijn.nl/tijn/research/tmap/icons/view_metro2.jpg" alt="Bubble Map" height="125px"/></a>
<a href = "https://github.com/mtennekes/tmap/tree/master/demo/WorldFacets"><img src="http://www.von-tijn.nl/tijn/research/tmap/icons/world_facets2.jpg" alt="World facets" height="125px"/></a>
<a href = "https://github.com/mtennekes/tmap/tree/master/demo/USChoropleth"><img src="http://www.von-tijn.nl/tijn/research/tmap/icons/USchoro.jpg" alt="US Choropleth" height="125px"/></a>
<a href = "https://github.com/mtennekes/tmap/tree/master/demo/USChoropleth"><img src="http://www.von-tijn.nl/tijn/research/tmap/icons/US_PR.jpg" alt="US Choropleth" height="125px"/></a>
<a href = "https://github.com/mtennekes/tmap/tree/master/demo/LondonCrimes">
<img src="http://www.von-tijn.nl/tijn/research/tmap/icons/crimes3.jpg" alt="London Crimes" height="125px"/></a>
<a href = "https://github.com/mtennekes/tmap/tree/master/demo/LondonCrimes">
<img src="http://www.von-tijn.nl/tijn/research/tmap/icons/crimes4.jpg" alt="London Crimes" height="125px"/></a>
<a href = "https://github.com/mtennekes/tmap/tree/master/demo/LondonCrimes">
<img src="http://www.von-tijn.nl/tijn/research/tmap/icons/crimes5.jpg" alt="London Crimes" height="125px"/></a>
<a href = "https://github.com/mtennekes/tmap/tree/master/demo/LondonCrimes">
<img src="http://www.von-tijn.nl/tijn/research/tmap/icons/view_crimes2.jpg" alt="London Crimes" height="125px"/></a>
</span>


Installation
------------

`tmap` is available on [CRAN](http://cran.r-project.org/package=tmap). The latest version is 1.6-1 (2016-10-29).


The `tmap` packages relies on the R packages `rgdal` and `rgeos`, which depend on the external libraries `gdal`, `proj.4` and `geos`. On Windows, these are embedded in `rgdal` and `rgeos`. On Linux (Ubuntu), these libraries can be installed as follows:

```bash
sudo apt-get install libgdal-dev
sudo apt-get install libproj-dev
sudo apt-get install libgeos-dev
```

See source pages for [gdal](http://trac.osgeo.org/gdal), [proj](http://trac.osgeo.org/proj), and [geos](http://trac.osgeo.org/geos). For Mac OS users, see http://www.kyngchaos.com.

Java is required for obtaining OpenStreetMap image (with the function`read_osm`). It can be installed in Linux (Ubuntu) with:

```bash
sudo-apt install openjdk-9-jre
sudo R CMD javareconf
```

Development
------------

Odd numbered versions will be development versions and even numbered versions stable CRAN releases (as with data.table).

The latest development version can be installed using `devtools`.

```r
library(devtools)
install_github("mtennekes/tmap", subdir = "pkg")
```

See [NEWS](https://github.com/mtennekes/tmap/blob/master/pkg/NEWS) for the latest features and improvements and the [issue list](https://github.com/mtennekes/tmap/issues) for discussions of enhancements and bugs.

Do you want to help with the development of the package? Please let me know! Any feedback, requests, tips, or bug reports are welcome!


Usage
-----

```r
library(tmap)
```


Vignettes, Demos, and Examples
-----
[tmap in a nutshell][6]

[tmap modes: plot and interactive view][11]

[demo pages](https://github.com/mtennekes/tmap/tree/master/demo)

[examples](https://github.com/mtennekes/tmap/tree/master/examples)


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
* [Stack Overflow questions (#tmap)][16]


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
  [16]: http://stackoverflow.com/questions/tagged/tmap
  