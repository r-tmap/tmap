# Popup specification for interactive maps

`tm_popup()` specifies the popups that are shown in interactive
(`"view"`) mode when a feature is clicked. It is passed to the `popup`
argument of the layer functions
([`tm_polygons()`](https://r-tmap.github.io/tmap/reference/tm_polygons.md),
[`tm_symbols()`](https://r-tmap.github.io/tmap/reference/tm_symbols.md),
[`tm_lines()`](https://r-tmap.github.io/tmap/reference/tm_lines.md),
etc.). It replaces the (now deprecated) layer arguments `popup.vars` and
`popup.format`.

## Usage

``` r
tm_popup(
  vars = NA,
  title = NA,
  format = tm_label_format(),
  width = "auto",
  max.height = "25em",
  title.align = "left",
  title.color = NULL,
  label.align = "left",
  label.color = "#888888",
  value.align = "right",
  value.color = NULL,
  css = NULL
)
```

## Arguments

- vars:

  Names of the data variables that are shown in the popup table. A
  (possibly named) character vector; when named, the names are used as
  labels in the popup table. Besides a character vector, the following
  special values are supported (identical to the former `popup.vars`
  argument):

  `TRUE`

  :   show all variables of the shape object;

  `FALSE`

  :   disable popups;

  `NA` (default)

  :   automatic: if visual variables (e.g. `fill`) are used, only those
      are shown, otherwise all variables of the shape object are shown.

- title:

  Name of the data variable used as the popup title (the bold header
  shown above the popup table). This overrules the layer argument `id`,
  analogous to how `hover` overrules `id` for hover labels. The default
  (`NA`) means that the popup title is derived from `id` (the former,
  and still default, behaviour). A length-one character vector is
  expected; a named value is allowed and reserved for future use.

- format:

  A list of formatting options for the popup values, the output of
  [`tm_label_format()`](https://r-tmap.github.io/tmap/reference/tm_label_format.md).
  Only applicable to numeric data variables. If one list of formatting
  options is provided, it is applied to all numeric variables of `vars`.
  A (named) list of lists can also be provided; in that case, each list
  of formatting options is applied to the named variable.

- width:

  Width of the popup content (view mode). A bare number is interpreted
  as pixels (e.g. `300` means `"300px"`); a character string is used
  as-is, so any CSS length is accepted (`"300px"`, `"20em"`, `"50%"`).
  The default `"auto"` lets the popup size to its content.

- max.height:

  Maximum height of the popup table before it becomes vertically
  scrollable (view mode). A bare number is interpreted as `em` (e.g. `5`
  means `"5em"`, roughly "show 5 lines"); a character string is used
  as-is. Default `"25em"`. Use `max.height = "none"` (or `NA`/`Inf`) to
  remove the cap, so the popup grows to fit its content and never
  scrolls.

- title.align, label.align, value.align:

  Horizontal alignment of the popup title (bold header), the
  variable-name (label) column, and the value column respectively. Each
  one of `"left"`, `"center"`, or `"right"`. Defaults: title `"left"`,
  label `"left"`, value `"right"`.

- title.color, label.color, value.color:

  Text color of the popup title, the label column, and the value column
  respectively. `NULL` (the default for title and value) inherits the
  browser/popup default; `label.color` defaults to `"#888888"` (grey).

- css:

  Optional free-form CSS, injected verbatim as a `<style>` block in each
  popup, for full restyling beyond the arguments above. Target the
  semantic classes `.tmap-popup` (container), `.tmap-popup-table`,
  `.tmap-popup-title`, `.tmap-popup-label`, and `.tmap-popup-value`. To
  size the popup box itself, target the backend's own popup element
  (`.leaflet-popup-content` in view mode, `.maplibregl-popup-content` in
  maplibre mode). Default `NULL`.

## Value

A `tm_popup` object.

## See also

[`tm_polygons()`](https://r-tmap.github.io/tmap/reference/tm_polygons.md),
[`tm_symbols()`](https://r-tmap.github.io/tmap/reference/tm_symbols.md),
[`tm_lines()`](https://r-tmap.github.io/tmap/reference/tm_lines.md)
