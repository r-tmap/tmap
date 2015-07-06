tmap
===

R package for thematic maps

Installation
------------

`tmap` is available on [CRAN](http://cran.r-project.org/package=tmap)!
The latest development version can be installed using `devtools`.

```r
library(devtools)
install_github("tmap", username="mtennekes", subdir="pkg")
```

Usage
-----

```r
library(tmap)
```


Next CRAN release (version 1.1)
-----

The next version of ```tmap``` will be probably be published on CRAN within a few weeks.

New features (already implemented):

* Open Street Map layers support
* Handy ```bb``` function for setting and changing bounding boxes
* Small multiples can take argument for each small multiple

New features (still working on):

* Minor tweaks on the features listed above
* Interactive SVG output (via ```gridSVG``` package)
* Show interactive SVG in RStudio console
* Flow map (```tm_flow```)

Features to be included in later versions:

* Cartogram

