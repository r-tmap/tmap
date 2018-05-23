library(testthat)
library(tmap)

print(getwd())
print(list.files(path = "testthat"))

test_check("tmap", filter = "^[a]")
