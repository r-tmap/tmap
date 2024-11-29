tm_shape(NLD_dist) +
  tm_polygons("edu_appl_sci",
    fill.scale = tm_scale_intervals(values = "pu_gn", style = "kmeans", n = 7)) +
  tm_facets(by = "province") +
tm_shape(NLD_muni) +
  tm_borders(lwd = 3) +
  tm_facets(by = "province") +
tm_title("Population with a univeristy degree (incl appl. sciences), percentages")

tm_shape(World) +
  tm_polygons(c("gender", "press"),
    fill.scale = list(tm_scale_intervals(values = "bu_br_div", midpoint = 0.5),
      tm_scale_intervals(values = "pu_gn_div", midpoint = 50)),
    fill.legend = tm_legend("")) +
tm_layout(panel.labels = c("Gender Inequality Index (GII)", "World Press Freedom Index"))
