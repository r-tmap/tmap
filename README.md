tmap: thematic maps in R
===

<!--[![Build Status](https://travis-ci.org/mtennekes/tmap.png?branch=master)](https://travis-ci.org/mtennekes/tmap)-->
<!--[![Coverage Status](https://img.shields.io/codecov/c/github/mtennekes/tmap/master.svg)](https://codecov.io/github/mtennekes/tmap?branch=master)-->
<!--[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/mtennekes/tmap?branch=master&svg=true)](https://ci.appveyor.com/project/mtennekes/tmap)-->
[![License](https://img.shields.io/badge/License-GPL%20v3-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-3.0.html) 
[![CRAN](http://www.r-pkg.org/badges/version/tmap)](https://cran.r-project.org/package=tmap) 
[![cran checks](https://badges.cranchecks.info/worst/tmap.svg)](https://cran.r-project.org/web/checks/check_results_tmap.html)
[![Downloads](http://cranlogs.r-pkg.org/badges/tmap?color=brightgreen)](http://www.r-pkg.org/pkg/tmap)

[`tmap`][1] is an actively maintained open-source [R][2]-library for drawing thematic maps. The API is based on [*A Layered Grammar of Graphics*][4] and resembles the syntax of [`ggplot2`][3], a popular R-library for drawing charts.


<span>
<img src="https://mtennekes.github.io/downloads/images/classic.png" alt="Classic World Map" height="125px"/>
<img src="https://mtennekes.github.io/downloads/images/bubble.png" alt="Bubble Map" height="125px"/>
<img src="https://mtennekes.github.io/downloads/images/view_metro4.jpg" alt="Bubble Map" height="125px"/>
<img src="https://mtennekes.github.io/downloads/images/world_facets2.png" alt="World facets" height="125px"/>
<img src="https://mtennekes.github.io/downloads/images/USchoro.png" alt="US Choropleth" height="125px"/>
<img src="https://mtennekes.github.io/downloads/images/US_PR.jpg" alt="US Choropleth" height="125px"/>
<img src="https://mtennekes.github.io/downloads/images/crimes3b.png" alt="London Crimes" height="125px"/>
<img src="https://mtennekes.github.io/downloads/images/crimes4.png" alt="London Crimes" height="125px"/>
<img src="https://mtennekes.github.io/downloads/images/crimes5.png" alt="London Crimes" height="125px"/>
<img src="https://mtennekes.github.io/downloads/images/view_crimes2.JPG" alt="London Crimes" height="125px"/>
</span>

See [below](https://github.com/r-tmap/tmap/#reference) the source code for these images.


Installation
------------

`tmap` is available on [![CRAN](http://www.r-pkg.org/badges/version/tmap)](https://cran.r-project.org/package=tmap). Installation is straightforward:

```r
install.packages("tmap")
```

For Linux and macOS users who are new to working with spatial data in R, this may fail since additional (non-R) libraries are required (which are automatically installed for Windows users).

**Windows**
No additional installation required.

**Linux (Ubuntu)**
See https://r-spatial.github.io/sf/#linux

**macOS**
See https://r-spatial.github.io/sf/#macos


Development (major update!)
------------

We are working on a major update (version4). The development of the current CRAN release (3.3) has been stopped, but if there are critical bugs, let us know. Version 4 will be submitted to CRAN this year, when it is mature enough and when it is backwards compatible with version 3.

Are you curious about version 4? You can install the development version as follows:

```r
library(remotes)
install_github("r-tmap/tmap@v4")
```

Any feedback, requests, tips, or bug reports are welcome!

Usage
-----

```r
library(tmap)
```

Book 
-----
Tennekes, M., Nowosad, J., Elegant and informative maps with tmap. Draft version, https://r-tmap.github.io/

Reference
----
[Tennekes, M., 2018, tmap: Thematic Maps in R, Journal of Statistical Software, 84(6), 1-39](https://doi.org/10.18637/jss.v084.i06)

This paper has been written for `tmap` 1.11-2. See the [reproducible code](https://cran.r-project.org/package=tmap/vignettes/tmap-JSS-code.html) using `tmap` 2.x and 3.x. When version 4 is published on CRAN, we will publish the book, which will then be the official reference for tmap.

Vignettes 
-----

[tmap: get started!](https://cran.r-project.org/package=tmap/vignettes/tmap-getstarted.html)

[tmap: what is changed in recent versions?](https://cran.r-project.org/package=tmap/vignettes/tmap-changes.html)

[tmap v4: a sneak peak](https://mtennekes.github.io/tmap4/index.html)


Tutorials
-----

[tmap in RMarkdown](https://github.com/r-tmap/tmap/blob/master/demo/tutorials/rmarkdown_tmap.Rmd)


Presentations
-----

* [Creating thematic maps in R][23] (OpenGeoHUB Summer School 2019)
* [Plotting spatial data with R](https://github.com/mtennekes/tmap-workshop) (eRum 2018) 
* [Exploring and presenting maps with tmap][21] (useR!2017)
* [tmap: creating thematic maps in a flexible way][10] (useR!2015)]


Other resources
-----

* [Geocomputation with R; Making maps with R, Robin Lovelace, Jakub Nowosad, Jannes Muenchow][20]
* [R tip: Create maps in R][24]
* [Working with Spatial Data and using tmap, Samantha A. Alger][22]
* [Computer World: Mapping in R just got a whole lot easier][18]
* [National Socio-Environmental Synthesis Center: Maps in R][19]
* [Introduction to visualising spatial data in R][9]
* [Blog post StatialControl][7]
* [Blog post TWIAV][8]
* [Computer World: Create maps in R in 10 (fairly) easy steps][12]
* [Computer World: Great R packages for data import, wrangling and visualization][17]
* [Tutorial Visualising spatial data: from base to shiny - workshop][15]
* [Stack Overflow questions (#tmap)][16]


Getting help
-----

There are two main places to get help with `tmap`:

1.  [stackoverflow](http://stackoverflow.com/tags/tmap) is a great source of answers to common tmap questions. 
It is also a great place to get help, once you have created a reproducible example that illustrates your problem. 
Please tag your questions with *tmap*.
2.  If you have a request or if think your problem is caused by a bug, please open an [issue](https://github.com/mtennekes/tmap/issues), preferably with a reproducible example.


  [1]: http://cran.r-project.org/package=tmap
  [2]: http://stackoverflow.com/tags/r/info
  [3]: http://cran.r-project.org/package=ggplot2
  [4]: http://vita.had.co.nz/papers/layered-grammar.pdf
  [5]: https://github.com/mtennekes/tmap
  [6]: https://cran.r-project.org/web/packages/tmap/vignettes/tmap-nutshell.html
  [7]: http://spatcontrol.net/SpatialControl/2015/11/06/tmap-r-package/
  [8]: http://www.twiav.nl/en/blog0002en.php
  [9]: https://cran.r-project.org/doc/contrib/intro-spatial-rl.pdf
  [10]: https://mtennekes.github.io/downloads/presentations/tmap_user2015.pdf
  [11]: https://cran.r-project.org/web/packages/tmap/vignettes/tmap-modes.html
  [12]: http://cwrld.us/Rmaps10
  [13]: https://github.com/mtennekes/tmap/blob/master/demo/US_choropleth.R
  [14]: https://github.com/mtennekes/tmap/blob/master/demo/crimes_in_Greater_London.R
  [15]: https://github.com/Robinlovelace/Creating-maps-in-R/blob/master/vignettes/vspd-base-shiny.Rmd
  [16]: http://stackoverflow.com/questions/tagged/tmap
  [17]: http://www.computerworld.com/article/2921176/business-intelligence/great-r-packages-for-data-import-wrangling-visualization.html
  [18]:	http://www.computerworld.com/article/3175623/data-analytics/mapping-in-r-just-got-a-whole-lot-easier.html
  [19]: https://sesync-ci.github.io/maps-in-R-lesson/
  [20]: https://r.geocompx.org/adv-map
  [21]: https://mtennekes.github.io/downloads/presentations/tmap_user2017.pdf
  [22]: https://gotellilab.github.io/Bio381/StudentPresentations/SpatialDataTutorial.html
  [23]: https://mtennekes.github.io/downloads/presentations/tmap_opengeo_muenster.pdf
  [24]: https://www.youtube.com/watch?v=wgFVmzSbaQc#t=3m20s
  
