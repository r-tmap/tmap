data(NLD_muni)

png("../test1.png", width = 700, height=700)
qtm(NLD_muni)
dev.off()

png("../test2.png", width = 700, height=700)
qtm(NLD_muni) + tm_layout(outer.margins=0)
dev.off()

png("../test3.png", width = 700, height=700)
qtm(NLD_muni) + tm_layout(outer.margins=0, asp=0)
dev.off()


data(Europe)

png("../test4.png", width = 700, height=700)
qtm(Europe, text = "name")
dev.off()

png("../test5.png", width = 700, height=700)
qtm(Europe, text = "name") + tm_layout(outer.margins=0)
dev.off()

png("../test6.png", width = 700, height=700)
qtm(Europe, text = "name") + tm_layout(outer.margins=0, asp=0)
dev.off()

png("../test7.png", width = 700, height=700)
qtm(Europe, text = "name") + tm_layout(draw.frame=FALSE)
dev.off()

png("../test8.png", width = 700, height=700)
qtm(Europe, text = "name") + tm_layout(outer.margins=0, draw.frame=FALSE)
dev.off()

png("../test9.png", width = 700, height=700)
qtm(Europe, text = "name") + tm_layout(outer.margins=0, asp=0, draw.frame=FALSE)
dev.off()