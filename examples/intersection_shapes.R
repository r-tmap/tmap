data(NLD_prov)
data(NLD_muni)

m <- intersection_shapes(NLD_muni, NLD_prov, id.from="name", id.to="name")

