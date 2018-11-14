# devtools::install_github("ropensci/taxize")
# devtools::install_github("ropensci/rgbif")
library(taxize)
library(rgbif)

n <- species$scientific_name[1]
nres <- taxize::gnr_resolve(n)
nres[1,]
