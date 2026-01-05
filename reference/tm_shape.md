# Shape (spatial object) specification

Specify a shape, which is a spatial object from one of these spatial
object class packages:
[`sf`](https://r-spatial.github.io/sf/reference/sf.html),
[`stars`](https://r-spatial.github.io/stars/reference/st_as_stars.html),
or `terra`.

## Usage

``` r
tm_shape(
  shp = NULL,
  bbox = NULL,
  crs = NULL,
  is.main = NA,
  name = NULL,
  unit = NULL,
  filter = NULL,
  ...
)
```

## Arguments

- shp:

  Spatial object

- bbox:

  Bounding box of the map (only used if `shp` is the main shape (see
  `is.main`). Three options: a
  [`sf::st_bbox()`](https://r-spatial.github.io/sf/reference/st_bbox.html)
  object, an Open Street Map query (passed on to
  [`tmaptools::geocode_OSM()`](https://r-tmap.github.io/tmaptools/reference/geocode_OSM.html)),
  or `"FULL"`, which means the whole earth (this also guarantees that
  transformations to another CRS keep the whole earth, unlike
  [`sf::st_bbox()`](https://r-spatial.github.io/sf/reference/st_bbox.html)).

- crs:

  Map projection (CRS). Can be set to an `crs` object (see
  [`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html)),
  a proj4string, an EPSG number, the value `"auto"` (automatic crs
  recommendation), or one the the following generic projections:
  `c("laea", "aeqd", "utm", "pconic", "eqdc", "stere")`. See details.

- is.main:

  Is `shp` the main shape, which determines the crs and bounding box of
  the map? By default, `TRUE` if it is the first `tm_shape` call

- name:

  of the spatial object

- unit:

  Unit of the coordinates

- filter:

  Filter features

- ...:

  passed on to
  [`bb`](https://r-tmap.github.io/tmaptools/reference/bb.html) (e.g.
  `ext` can be used to enlarge or shrink a bounding box)

## Details

The map projection (`crs`) determines in which coordinate system the
spatial object is processed and plotted. See [vignette about
CRS](https://r-tmap.github.io/tmap/articles/foundations_crs). The `crs`
can be specified in two places: 1) `tm_shape()` and
[`tm_crs()`](https://r-tmap.github.io/tmap/reference/tm_crs.md). In both
cases, the map is plotted into the specified `crs`. The difference is
that in the first option, the `crs` is also taken into account in
spatial transformation functions, such as the calculation of centroids
and cartograms. In the second option, the `crs` is only used in the
plotting phase.

The automatic crs recommendation (which is still work-in-progress) is
the following:

|                           |                                                                                                                                                                                  |
|---------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **Property**              | **Recommendation**                                                                                                                                                               |
| `global` (for world maps) | A pseudocylindrical projection tmap option `crs_global`, by default `"eqearth` (Equal Earth). See <https://r-tmap.github.io/tmap/articles/41_advanced_crs.html> for more options |
| `area` (equal area)       | Lambert Azimuthal Equal Area (`laea`)                                                                                                                                            |
| `distance` (equidistant)  | Azimuthal Equidistant (`aeqd`)                                                                                                                                                   |
| `shape` (conformal)       | Stereographic (`stere`)                                                                                                                                                          |

For further info about the available "generic" projects see: for utm:
<https://proj.org/en/9.4/operations/projections/utm.html> for laea:
<https://proj.org/en/9.4/operations/projections/laea.html> for aeqd:
<https://proj.org/en/9.4/operations/projections/aeqd.html> for pconic:
<https://proj.org/en/9.4/operations/projections/pconic.html> for eqdc:
<https://proj.org/en/9.4/operations/projections/eqdc.html>

## Note

as of tmap 4.0, simplify has been removed. Please use
[`tmaptools::simplify_shape()`](https://r-tmap.github.io/tmaptools/reference/simplify_shape.html)
instead

## See also

[vignette about
CRS](https://r-tmap.github.io/tmap/articles/foundations_crs)

## Examples

``` r
tm_shape(World, crs = "auto") +
  tm_polygons()


tm_shape(World, crs = 3035, bb = "Europe") +
  tm_polygons()


tm_shape(World, crs = "+proj=robin", filter = World$continent=="Africa") +
  tm_polygons()
```
