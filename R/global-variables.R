# Will need investigating, maybe some are false positive
# But this will reduce clutter in R CMD CHECK
utils::globalVariables(c(
  ".", "..cols", "X__", "Y__", "aes", "alpha", "asp", "attr.color", "bbox",
  "between.margin", "bi", "blue", "by1", "by1__", "by2", "by2__", "by2b__",
  "by3", "by3__", "ca", "cell.h", "cell.v", "col_alpha", "columns", "comp",
  "crs", "crs_parameters", "dasp", "devsize", "facet_col", "facet_row", "fl",
  "fn", "frame", "gp", "green", "grid.labels.format",
  "grid.labels.inside.frame", "grid.labels.margin.x", "grid.labels.margin.y",
  "grid.labels.pos", "grid.labels.rot", "grid.labels.size",
  "grid.labels.space.x", "grid.labels.space.y", "grid.show", "hover", "id",
  "ids", "label.na", "label.show", "labels.cardinal", "labels.inside.frame",
  "labels.pos", "labels.rot", "legH", "legW", "legend", "legend.bg.alpha",
  "legend.present.auto", "legend.present.fix", "lfmv", "lin", "lineH", "lineW",
  "m", "main_class", "mapping.args", "mfun", "n", "nby", "ndiscr", "ord__",
  "outer.margins", "overlays_tiles", "pages", "panel.label.height",
  "panel.labels", "panel.type", "panel.wrap.pos", "panel.xtab.pos", "pos.h",
  "pos.h.id", "pos.v", "pos.v.id", "red", "rows", "s1", "s2", "s3", "s4",
  "scale.factor", "set.bounds", "show", "show.labels", "show.warnings",
  "split_geometry_collection", "stack_auto", "strheight", "strwidth", "t1",
  "t2", "t3", "t4", "text.fontface", "text.fontfamily", "title.bg.alpha",
  "tmapID__", "total", "trans.args", "type", "values", "vneutral",
  "xlab.rotation", "xlab.show", "xlab.side", "xlab.space", "xlab.text",
  "ylab.rotation", "ylab.show", "ylab.side", "ylab.space", "ylab.text", "z"
))
