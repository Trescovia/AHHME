library(data.table)
library(countrycode)

produc_panel <- read.csv("producticity.csv")
naylor_dic <- read.csv("who_whoc_wb_Naylor2021.csv")
## might not work check importing first

# produc_panel <- producticity
colnames(produc_panel)

# change input into iso3c  
produc_panel$iso3c <-countrycode(produc_panel$Entity, origin="country.name", destination="iso3c")

# merge with naylor_dic
test_produc_panel <- merge(produc_panel, naylor_dic, by="iso3c")

length(which(test_produc_panel$Code==test_produc_panel$iso3c))

