# Internal methods for map components in plot and view mode

Internal methods for map components

## Usage

``` r
tmapGridAuxPrepare(a, bs, id, o)

# Default S3 method
tmapGridAuxPrepare(a, bs, id, o)

tmapGridAuxPlot(
  a,
  bi,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o
)

# Default S3 method
tmapGridAuxPlot(
  a,
  bi,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o
)

# S3 method for class 'tm_aux_graticules'
tmapGridAuxPrepare(a, bs, id, o)

# S3 method for class 'tm_aux_grid'
tmapGridAuxPrepare(a, bs, id, o)

# S3 method for class 'tm_aux_basemap'
tmapGridAuxPlot(
  a,
  bi,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o
)

# S3 method for class 'tm_aux_tiles'
tmapGridAuxPlot(
  a,
  bi,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o
)

# S3 method for class 'tm_aux_graticules'
tmapGridAuxPlot(
  a,
  bi,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o
)

# S3 method for class 'tm_aux_grid'
tmapGridAuxPlot(
  a,
  bi,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o
)

# S3 method for class 'tm_aux_basemap'
tmapGridAuxPrepare(a, bs, id, o)

# S3 method for class 'tm_aux_tiles'
tmapGridAuxPrepare(a, bs, id, o)

tmapGridCompPrepare(comp, o)

# Default S3 method
tmapGridCompPrepare(comp, o)

tmapGridDataPlot(
  a,
  shpTM,
  dt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o,
  ...
)

# Default S3 method
tmapGridDataPlot(
  a,
  shpTM,
  dt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o,
  ...
)

# S3 method for class 'tm_data_lines'
tmapGridDataPlot(
  a,
  shpTM,
  dt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o,
  ...
)

# S3 method for class 'tm_data_iso'
tmapGridDataPlot(
  a,
  shpTM,
  dt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o,
  ...
)

# S3 method for class 'tm_data_polygons'
tmapGridDataPlot(
  a,
  shpTM,
  dt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o,
  ...
)

# S3 method for class 'tm_data_fill'
tmapGridDataPlot(
  a,
  shpTM,
  dt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o,
  ...
)

# S3 method for class 'tm_data_borders'
tmapGridDataPlot(
  a,
  shpTM,
  dt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o,
  ...
)

# S3 method for class 'tm_data_raster'
tmapGridDataPlot(
  a,
  shpTM,
  dt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o,
  ...
)

# S3 method for class 'tm_data_symbols'
tmapGridDataPlot(
  a,
  shpTM,
  dt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o,
  ...
)

# S3 method for class 'tm_data_dots'
tmapGridDataPlot(
  a,
  shpTM,
  dt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o,
  ...
)

# S3 method for class 'tm_data_bubbles'
tmapGridDataPlot(
  a,
  shpTM,
  dt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o,
  ...
)

# S3 method for class 'tm_data_squares'
tmapGridDataPlot(
  a,
  shpTM,
  dt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o,
  ...
)

# S3 method for class 'tm_data_markers'
tmapGridDataPlot(
  a,
  shpTM,
  dt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o,
  ...
)

# S3 method for class 'tm_data_text'
tmapGridDataPlot(
  a,
  shpTM,
  dt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o,
  ...
)

# S3 method for class 'tm_data_labels'
tmapGridDataPlot(
  a,
  shpTM,
  dt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o,
  ...
)

# S3 method for class 'tm_data_labels_highlighted'
tmapGridDataPlot(
  a,
  shpTM,
  dt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o,
  ...
)

tmapLeafletAuxPrepare(a, bs, id, o)

tmapLeafletAuxPlot(
  a,
  bi,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o
)

# S3 method for class 'tm_aux_basemap'
tmapLeafletAuxPrepare(a, bs, id, o)

# S3 method for class 'tm_aux_tiles'
tmapLeafletAuxPrepare(a, bs, id, o)

# S3 method for class 'tm_aux_basemap'
tmapLeafletAuxPlot(
  a,
  bi,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o
)

# S3 method for class 'tm_aux_tiles'
tmapLeafletAuxPlot(
  a,
  bi,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o
)

# S3 method for class 'tm_aux_graticules'
tmapLeafletAuxPrepare(a, bs, id, o)

# S3 method for class 'tm_aux_grid'
tmapLeafletAuxPrepare(a, bs, id, o)

# S3 method for class 'tm_aux_graticules'
tmapLeafletAuxPlot(
  a,
  bi,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o
)

# S3 method for class 'tm_aux_grid'
tmapLeafletAuxPlot(
  a,
  bi,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o
)

tmapLeafletCompPrepare(comp, o)

# S3 method for class 'tm_chart_none'
tmapLeafletCompPrepare(comp, o)

# Default S3 method
tmapLeafletCompPrepare(comp, o)

# S3 method for class 'tm_data_lines'
tmapLeafletDataPlot(
  a,
  shpTM,
  dt,
  pdt,
  popup.format,
  hdt,
  idt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o,
  ...
)

# S3 method for class 'tm_data_iso'
tmapLeafletDataPlot(
  a,
  shpTM,
  dt,
  pdt,
  popup.format,
  hdt,
  idt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o,
  ...
)

# S3 method for class 'tm_data_polygons'
tmapLeafletDataPlot(
  a,
  shpTM,
  dt,
  pdt,
  popup.format,
  hdt,
  idt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o,
  ...
)

# S3 method for class 'tm_data_fill'
tmapLeafletDataPlot(
  a,
  shpTM,
  dt,
  pdt,
  popup.format,
  hdt,
  idt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o,
  ...
)

# S3 method for class 'tm_data_borders'
tmapLeafletDataPlot(
  a,
  shpTM,
  dt,
  pdt,
  popup.format,
  hdt,
  idt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o,
  ...
)

# S3 method for class 'tm_data_raster'
tmapLeafletDataPlot(
  a,
  shpTM,
  dt,
  pdt,
  popup.format,
  hdt,
  idt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o,
  ...
)

# S3 method for class 'tm_data_symbols'
tmapLeafletDataPlot(
  a,
  shpTM,
  dt,
  pdt,
  popup.format,
  hdt,
  idt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o,
  ...
)

# S3 method for class 'tm_data_dots'
tmapLeafletDataPlot(
  a,
  shpTM,
  dt,
  pdt,
  popup.format,
  hdt,
  idt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o,
  ...
)

# S3 method for class 'tm_data_bubbles'
tmapLeafletDataPlot(
  a,
  shpTM,
  dt,
  pdt,
  popup.format,
  hdt,
  idt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o,
  ...
)

# S3 method for class 'tm_data_squares'
tmapLeafletDataPlot(
  a,
  shpTM,
  dt,
  pdt,
  popup.format,
  hdt,
  idt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o,
  ...
)

# S3 method for class 'tm_data_markers'
tmapLeafletDataPlot(
  a,
  shpTM,
  dt,
  pdt,
  popup.format,
  hdt,
  idt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o,
  ...
)

# S3 method for class 'tm_data_text'
tmapLeafletDataPlot(
  a,
  shpTM,
  dt,
  pdt,
  popup.format,
  hdt,
  idt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o,
  ...
)

# S3 method for class 'tm_data_labels'
tmapLeafletDataPlot(
  a,
  shpTM,
  dt,
  pdt,
  popup.format,
  hdt,
  idt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o,
  ...
)

# S3 method for class 'tm_data_labels_highlighted'
tmapLeafletDataPlot(
  a,
  shpTM,
  dt,
  pdt,
  popup.format,
  hdt,
  idt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o,
  ...
)

tmapLeafletDataPlot(
  a,
  shpTM,
  dt,
  pdt,
  popup.format,
  hdt,
  idt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o,
  ...
)

# Default S3 method
tmapLeafletDataPlot(
  a,
  shpTM,
  dt,
  pdt,
  popup.format,
  hdt,
  idt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o,
  ...
)
```

## Arguments

- o:

  options

- comp:

  component
