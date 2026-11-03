# all pattern types and shortcuts
tmap_show_values("pattern")

# a pattern set, in the order of use, and all pattern sets
tmap_show_values("pattern", values = "classic")
tmap_show_values("pattern", values = c("mixed", "classic", "lines", "shapes"))

# specific patterns
tmap_show_values("pattern", values = tm_patterns("hatch", angle = c(0, 30, 60)))

tmap_show_values("shape")
tmap_show_values("lty")
tmap_show_values("lty", values = c("44", "13", "1343"))
tmap_show_values("fontface")
