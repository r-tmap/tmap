data(NLD_muni)
data(NLD_prov)

qtm(NLD_muni)

library(ggplot2)



NLD_data <- NLD_muni@data

NLD_data$name <- factor(NLD_data$name, levels=NLD_data$name[order(NLD_data$population)])

NLD_data_sel <- head(NLD_data[order(NLD_data$population, decreasing=TRUE), ], 20)

ggplot(NLD_data_sel, aes(y=population, x=name)) + geom_bar(stat="identity") + coord_flip()


qtm(NLD_muni, fill="gold", borders="purple", title="Netherlands")

p <- par(mar=c(0,0,0,0))
plot(NLD_muni, col = "gold", border="purple")
title("Netherlands", line = -2)
