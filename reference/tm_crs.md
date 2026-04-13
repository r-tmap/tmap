# Set the map projection (CRS)

This function sets the map projection. It can also be set via
[`tm_shape()`](https://r-tmap.github.io/tmap/reference/tm_shape.md), but
`tm_crs` is generally recommended for most cases. It can also be
determined automatically (see details); however, this is still
work-in-progress.

## Usage

``` r
tm_crs(crs = NA, property = NA, bbox = NULL)
```

## Arguments

- crs:

  Map projection (CRS). Can be set to an `crs` object (see
  [`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html)),
  a proj4string, an EPSG number, the value `"auto"` (automatic crs
  recommendation), or one the the following generic projections:
  `c("laea", "aeqd", "utm", "pconic", "eqdc", "stere")`. See details.

- property:

  Which property should the projection have? One of: `"global"`,
  `"area"` (equal-area), `"distance"` (equidistant), `"shape"`
  (conformal). Only applicable if `crs = "auto"`. See details.

- bbox:

  bounding box. Three options: a
  [`sf::st_bbox()`](https://r-spatial.github.io/sf/reference/st_bbox.html)
  object, an Open Street Map query (passed on to
  [`tmaptools::geocode_OSM()`](https://r-tmap.github.io/tmaptools/reference/geocode_OSM.html)),
  or `"FULL"`, which means the whole earth , which means the whole earth
  (this also guarantees that transformations to another CRS keep the
  whole earth, unlike
  [`sf::st_bbox()`](https://r-spatial.github.io/sf/reference/st_bbox.html)).

## Details

The map projection (`crs`) determines in which coordinate system the
spatial object is processed and plotted. See [vignette about
CRS](https://r-tmap.github.io/tmap/articles/foundations_crs). The `crs`
can be specified in two places: 1)
[`tm_shape()`](https://r-tmap.github.io/tmap/reference/tm_shape.md) and
`tm_crs()`. In both cases, the map is plotted into the specified `crs`.
The difference is that in the first option, the `crs` is also taken into
account in spatial transformation functions, such as the calculation of
centroids and cartograms. In the second option, the `crs` is only used
in the plotting phase.

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

Plans are to migrate the functionality regarding generic crs and
automatic crs recommendation to a separate package.

## See also

[vignette about
CRS](https://r-tmap.github.io/tmap/articles/foundations_crs)

## Examples

``` r
SA = World[World$continent == "South America", ]

# latlon coordinates (WGS84)
tm_shape(SA) +
  tm_polygons() +
  tm_graticules() +
  tm_crs(4326)


tm_list = lapply(c("global", "area", "distance", "shape"), FUN = function(property) {
  tm_shape(SA) +
    tm_polygons() +
    tm_graticules() +
    tm_crs(property = property)  +
  tm_title(property)
})

tmap_arrange(tm_list, nrow = 1)
```
