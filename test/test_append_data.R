data(NLD_prov)

NLD_prov@data <- NLD_prov@data[,1, drop=FALSE]


## under/over coverage, without duplicates

x <- data.frame(CODE=c(21, 24:32), value=1:10)

NLD_prov2 <- append_data(NLD_prov, x, key.shp="code", key.data="code") # should throw error
NLD_prov2 <- append_data(NLD_prov, x, key.shp="code", key.data="CODE")

NLD_prov2@data

under_coverage()
over_coverage()


## perfect match
x <- data.frame(CODE=20:31, value=1:12)
NLD_prov2 <- append_data(NLD_prov, x, key.shp="code", key.data="CODE")
under_coverage()
over_coverage()

## duplicated data keys
x <- data.frame(CODE=c(21, 28, 24:32), value=1:11)
NLD_prov2 <- append_data(NLD_prov, x, key.shp="code", key.data="CODE")
NLD_prov2 <- append_data(NLD_prov, x, key.shp="code", key.data="CODE", ignore.duplicates = TRUE)
NLD_prov2@data
under_coverage()
over_coverage()


## duplicated shape keys
NLD_provB <- NLD_prov[c(1:12,5), ]

x <- data.frame(CODE=c(21, 24:32), value=1:10)

NLD_prov2 <- append_data(NLD_provB, x, key.shp="Code", key.data="CODE") # should throw error

NLD_prov2 <- append_data(NLD_provB, x, key.shp="code", key.data="CODE")
under_coverage()
over_coverage()

