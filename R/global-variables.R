# Will need investigating, maybe some are true flags
# i.e. a function that needs to be prefixed, a variable that is unused, or misnamed
# although most seem to come from data.table variables.
# But this will reduce clutter in R CMD CHECK
utils::globalVariables(c(
  ".", "..cols", "X__", "Y__", "asp", "attr.color", "bbox",
  "between.margin", "bi", "blue", "by1", "by1__", "by2", "by2__", "by2b__",
  "by3", "by3__", "ca", "cell.h", "cell.v", "columns", "comp",
  "crs", "crs_parameters", "dasp", "devsize", "facet_col", "facet_row", "fl",
  "fn", "gp", "green", "grid.labels.format",
  "grid.labels.inside.frame", "grid.labels.margin.x", "grid.labels.margin.y",
  "grid.labels.pos", "grid.labels.rot", "grid.labels.size",
  "grid.labels.space.x", "grid.labels.space.y", "hover", "id",
  "ids", "label.show", "labels.cardinal", "labels.inside.frame",
  "labels.pos", "labels.rot", "legH", "legW",
  "legend.present.auto", "legend.present.fix", "lfmv", "lineH", "lineW",
  "main_class", "mapping.args", "mfun", "nby", "ndiscr", "ord__",
  "outer.margins", "pages", "panel.label.height",
  "panel.labels", "panel.type", "panel.wrap.pos", "panel.xtab.pos", "pos.h",
  "pos.h.id", "pos.v", "pos.v.id", "red", "rows", "s1", "s2", "s3", "s4",
  "scale.factor", "set.bounds",
  "split_geometry_collection", "stack_auto", "t1",
  "t2", "t3", "t4", "text.fontface", "text.fontfamily", "title.bg.alpha",
  "tmapID__", "vneutral"
))
