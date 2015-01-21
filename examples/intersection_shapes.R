data(NLD_prov)
data(NLD_muni)

m <- intersection_shapes(NLD_muni, NLD_prov, id.from="name", id.to="name")

# this intersection matrix can be used to find the province of each municipality
NLD_muni <- append_data(NLD_muni, data.frame(Province=colnames(m)[apply(m, MARGIN = 1, which.max)]), fixed.order = TRUE)

qtm(NLD_muni, fill = "Province")


