library(grid)


light <- "#FFFFFF"
dark <- "#000000"

s <- c(.5, .5, .57, .5, .5, .43, 0, .5, .43, 1, .5, .57)
grid.polygon(x=rep.int(s, 2), y=s[c(10:12, 10:12, 1:3, 1:3, 7:9, 7:9, 4:6, 4:6)], id=rep(1:8, each=3), gp=gpar(fill=c(dark, light, dark, light, light, dark, light, dark)))

