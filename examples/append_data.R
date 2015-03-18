data(Europe)

f <- tempfile()
download.file("http://kejser.org/wp-content/uploads/2014/06/Country.csv", destfile = f)
domain_codes <- read.table(f, header=TRUE, sep="|")
unlink(f)


domain_codes <- subset(domain_codes, select = c("Alpha3Code", "TopLevelDomain"))
domain_codes$Alpha3Code <- toupper(domain_codes$Alpha3Code)

Europe <- append_data(Europe, domain_codes, key.shp = "iso_a3", key.data = "Alpha3Code",
					  ignore.na = TRUE)

qtm(Europe, text="TopLevelDomain")

