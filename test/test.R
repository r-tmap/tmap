# test scaling

data(Europe)
data(rivers)


g <- geo_shape(Europe) +
	geo_fill("pop_est_dens") +
	geo_borders() +
	geo_bubbles(size="gdp_md_est", col="economy") +
	geo_shape(rivers) +
	geo_lines("type", lwd="strokelwd", scale=3) + 
	geo_shape(Europe) +
	geo_text("name", cex="AREA3", bg.alpha=0, ymod=-.02) +
	geo_theme_Europe()

g2 <- geo_shape(Europe) +
	geo_fill(c("pop_est_dens", "part")) +
	geo_borders() +
	geo_bubbles(size="gdp_md_est", col="economy") +
	geo_shape(rivers) +
	geo_lines("type", lwd="strokelwd", scale=3) + 
	geo_shape(Europe) +
	geo_text("name", cex="AREA3", bg.alpha=0, ymod=-.02) +
	geo_theme_Europe()


pdf("../test/test_scale1.pdf", width=7, height=7)
g
dev.off()

pdf("../test/test_scale2.pdf", width=14, height=14)
g
dev.off()


pdf("../test/test_scale1_f.pdf", width=7, height=7)
g2
dev.off()

pdf("../test/test_scale2_f.pdf", width=14, height=14)
g2
dev.off()


pdf("../test/test_scale_ref1.pdf", width=7, height=7)
plot(1:10)
dev.off()

pdf("../test/test_scale_ref2.pdf", width=14, height=14)
plot(1:10)
dev.off()