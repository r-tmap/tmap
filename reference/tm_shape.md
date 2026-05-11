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
  layer = NULL,
  name = NULL,
  unit = NULL,
  filter = NULL,
  ...
)
```

## Arguments

- shp:

  Spatial data object. Typically an object from sf, terra, or stars.
  Additional spatial data types can be supported via extension packages,
  such as tmap.networks and tmap.sources (experimental). These may
  include, for example, remote or streaming data sources.

- bbox:

  Bounding box of the map. Only used when `shp` is the main shape (see
  `is.main`). Three options are supported:

  - a
    [`sf::st_bbox()`](https://r-spatial.github.io/sf/reference/st_bbox.html)
    object,

  - a character string specifying a location, passed to
    [`tmaptools::geocode_OSM()`](https://r-tmap.github.io/tmaptools/reference/geocode_OSM.html),

  - `"FULL"`, which represents the whole earth. This option ensures that
    reprojection retains the full global extent, unlike a regular
    bounding box.

- crs:

  Map projection (CRS). Can be set to an `crs` object (see
  [`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html)),
  a proj4string, an EPSG number, the value `"auto"` (automatic crs
  recommendation), or one the the following generic projections:
  `c("laea", "aeqd", "utm", "pconic", "eqdc", "stere")`. See details.

- is.main:

  Is `shp` the main shape, which determines the crs and bounding box of
  the map? By default, `TRUE` if it is the first `tm_shape` call

- layer:

  Name of the layer to use. This is primarily relevant for multi-layer
  or remote data sources (e.g. PMTiles or vector tiles), where multiple
  layers may be available.

- name:

  of the spatial object

- unit:

  Unit of distance measurement, used by
  [`tm_scalebar()`](https://r-tmap.github.io/tmap/reference/tm_scalebar.md).
  Either a specific unit string such as `"km"`, `"m"`, `"mi"`, `"yd"`,
  `"ft"`, or `"in"` (see
  [`units::valid_udunits()`](https://r-quantities.github.io/units/reference/valid_udunits.html)
  for all options), or one of two automatic options: `"metric"`
  (default) selects the most readable unit from km, m, and mm;
  `"imperial"` selects from mi, yd, ft, and in. In both cases the unit
  is chosen as the largest one for which the map width expressed in that
  unit is at least 10.

- filter:

  Optional filter expression used to subset features. The exact syntax
  depends on the data source. For in-memory objects (e.g. sf), this is
  typically evaluated in R, whereas for remote sources it may be
  translated to a query and executed on the server side.

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

|  |  |
|----|----|
| **Property** | **Recommendation** |
| `global` (for world maps) | A pseudocylindrical projection tmap option `crs_global`, by default `"eqearth` (Equal Earth). See <https://r-tmap.github.io/tmap/articles/41_advanced_crs.html> for more options |
| `area` (equal area) | Lambert Azimuthal Equal Area (`laea`) |
| `distance` (equidistant) | Azimuthal Equidistant (`aeqd`) |
| `shape` (conformal) | Stereographic (`stere`) |

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
