library(data.table)
library(countrycode)
library(taRifx)
library(tidyverse)
library(dplyr)
library(xlsx)

setwd("C:/Users/tresc/Desktop/AMR-Model/General model/input data")

#get country names
dictionary <- read.csv("who_whoc_wb_Naylor2021.csv")


# population --------------------------------------------------------------

#get
population <- read.csv("population.csv")
colnames(population) <- c("country", "country code", "indicator name", "indicator code", "year", "population")

#keep only the most recent year
population <- within(population, {max_year = ave(year,country, FUN=max)} )
population <- population[population$year == population$max_year,]

#get iso3c
population$iso3c <-countrycode(population$country, origin="country.name", destination = "iso3c")

#drop columns
population <- population[c(6,8)]

#merge
dictionary <- merge(population, dictionary, by="iso3c", all=T)
dictionary <- dictionary[!is.na(dictionary$iso3c),]

# WTP ---------------------------------------------------------------------

#get
wtp <- read.csv("WTP woods.csv")
colnames(wtp) <- c("country", "min wtp ppp", "max wtp ppp", "average wtp ppp", 
                          "min wtp nominal", "max wtp nominal", "average wtp nominal")

#parse expressions
wtp[,5:6] <- str_split_fixed(wtp$`min wtp nominal`, " - ", 2)
wtp[,5] <- gsub("," , "", wtp[,5])
wtp[,6] <- gsub("," , "", wtp[,6])
wtp[,5] <- as.numeric(wtp[,5])
wtp[,6] <- as.numeric(wtp[,6])

wtp[,7] <- 0.5*(wtp[,5] + wtp[,6])

#get iso3c
wtp$iso3c <-countrycode(wtp$country, origin="country.name", destination = "iso3c")

#drop columns
wtp <- wtp[,7:8]

#merge
dictionary <- merge(wtp, dictionary, by="iso3c", all=T)
dictionary <- dictionary[!is.na(dictionary$iso3c),]

# gdp pc growth -----------------------------------------------------------

#get
growth <- read.csv("wb gdppc growth.csv")
colnames(growth) <- c("country", "year", "growth")

#get 2010-2019 average for each country
growth <- growth[2010 < growth$year & growth$year < 2020,]

growth <- growth %>%
  group_by(country) %>%
  summarize(mean(growth))

#get iso3c
growth$iso3c <-countrycode(growth$country, origin="country.name", destination = "iso3c")

#drop columns
growth <- growth[,2:3]

#merge
dictionary <- merge(growth, dictionary, by="iso3c", all=T)
dictionary <- dictionary[!is.na(dictionary$iso3c),]

# Unpaid productivity -----------------------------------------------------

#calculated separately

# sepsis incidence --------------------------------------------------------

#get
sepsis <- read.csv("sepsis incidence.csv")
colnames(sepsis) <- c("country", "income_status", "incidence per 100k", "mortality per 100k", "mortality rate")

#destring and remove comas

sepsis$`mortality per 100k` <- gsub("," , "" , sepsis$`mortality per 100k`)
sepsis$`mortality per 100k` <- as.numeric(sepsis$`mortality per 100k`)

sepsis$`incidence per 100k` <- gsub("," , "" , sepsis$`incidence per 100k`)
sepsis$`incidence per 100k` <- as.numeric(sepsis$`incidence per 100k`)

#get mortality rate
sepsis$`mortality rate` <- sepsis$`mortality per 100k` / sepsis$`incidence per 100k`

#get iso3c
sepsis$iso3c <-countrycode(sepsis$country, origin="country.name", destination = "iso3c")

#drop columns
sepsis <- sepsis[,c(3,5:6)]

#merge
dictionary <- merge(sepsis, dictionary, by="iso3c", all=T)
dictionary <- dictionary[!is.na(dictionary$iso3c),]

# drug resistance index ---------------------------------------------------

#get
resistance <- read.csv("Resistomap DRI.csv")
colnames(resistance) <- c("country", "DRI", "GDPPCPPP", "Income Status")

#get iso3c
resistance$iso3c <-countrycode(resistance$country, origin="country.name", destination = "iso3c")

#drop columns
resistance <- resistance[,c(2,5)]

#merge
dictionary <- merge(resistance, dictionary, by = "iso3c", all=T)
dictionary <- dictionary[!is.na(dictionary$iso3c),]

# productivity ------------------------------------------------------------

#get
productivity <- read.csv("productivity.csv")
colnames(productivity) <- c("country", "code", "year", "productivity")

#keep only most recent year
productivity <- within(productivity, {max_year = ave(year, country, FUN=max)})
productivity <- productivity[productivity$year == productivity$max_year,]

#get iso3c
productivity$iso3c <-countrycode(productivity$country, origin="country.name", destination = "iso3c")

#drop columns
productivity <- productivity[,c(4,6)]

#merge
dictionary <- merge(productivity, dictionary, by = "iso3c", all=T)
dictionary <- dictionary[!is.na(dictionary$iso3c),]

# portion working ---------------------------------------------------------

#get
portionworking <- read.csv("portion working.csv")
colnames(portionworking) <- c("country", "age dependency ratio", "lfpr",
                              "working age population", "portion working",
                              "income status")

#get iso3c
portionworking$iso3c <-countrycode(portionworking$country, origin="country.name", destination = "iso3c")

#drop columns
portionworking <- portionworking[,c(5,7)]

#merge
dictionary <- merge(portionworking, dictionary, by = "iso3c", all=T)
dictionary <- dictionary[!is.na(dictionary$iso3c),]

# pig weight --------------------------------------------------------------

#get
pigweight <- read.csv("pig weight.csv")
colnames(pigweight) <- c("country", "item", "element", "unit", "year", "pigweight")

#get iso3c
pigweight$iso3c <-countrycode(pigweight$country, origin="country.name", destination = "iso3c")

#get weight in kg
pigweight$pigweight <- pigweight$pigweight / 10

#keep max year only
pigweight <- within(pigweight, {max_year = ave(year, country, FUN=max)})
pigweight <- pigweight[pigweight$year == pigweight$max_year,]

#drop columns
pigweight <- pigweight[,6:7]

#merge
dictionary <- merge(pigweight, dictionary, by = "iso3c", all=T)
dictionary <- dictionary[!is.na(dictionary$iso3c),]


# meat supply -------------------------------------------------------------

#get
meatsupply <- read.csv("meat-supply-per-person.csv")
colnames(meatsupply) <- c("country", "year", "meatsupply")

#keep max year
meatsupply <- within(meatsupply, {max_year = ave(year, country, FUN=max)})
meatsupply <- meatsupply[meatsupply$year == meatsupply$max_year,]

#get iso3c
meatsupply$iso3c <-countrycode(meatsupply$country, origin="country.name", destination = "iso3c")

#keep columns
meatsupply <- meatsupply[,c(3,5)]

#merge
dictionary <- merge(meatsupply, dictionary, by = "iso3c", all=T)
dictionary <- dictionary[!is.na(dictionary$iso3c),]


# bed day cost ------------------------------------------------------------

#get
bedday <- read.csv("whoc_cc_2019USD.csv")

#keep columns
bedday <- bedday[,c(2,8)]

#colnames
colnames(bedday)[2] <- "beddaycost"

#merge
dictionary <- merge(bedday, dictionary, by = "iso3c", all=T)
dictionary <- dictionary[!is.na(dictionary$iso3c),]


# chicken weights ---------------------------------------------------------

#get
chickenweight <- read.csv("chicken weights.csv")
colnames(chickenweight) <- c("domain code", "domain", "area code", "country",
                             "element code", "element", "item code", "item", "year code",
                             "year", "unit", "chickenweight", "flag", "flag description")

#keep only chicken weight
chickenweight <- chickenweight[chickenweight$item == "Meat, chicken",]

#put weight in kg
chickenweight$chickenweight <- chickenweight$chickenweight / 10000

#get iso3c
chickenweight$iso3c <-countrycode(chickenweight$country, origin="country.name", destination = "iso3c")

#keep columns
chickenweight <- chickenweight[,c(12,15)]

#merge
dictionary <- merge(chickenweight, dictionary, by = "iso3c", all=T)
dictionary <- dictionary[!is.na(dictionary$iso3c),]


# chicken price -----------------------------------------------------------

#get
chickenprice <- read.csv("chickenprice.csv")
colnames(chickenprice) <- c("domain code", "domain", "area code", "country", "element code",
                            "element", "item code", "item", "year code", "year", 
                            "months code", "months", "unit", "chickenprice", "flag",
                            "description")

#put price per kg
chickenprice$chickenprice <- chickenprice$chickenprice / 1000

#get iso3c
chickenprice$iso3c <-countrycode(chickenprice$country, origin="country.name", destination = "iso3c")

#get columns
chickenprice <- chickenprice[,c(14,17)]

#merge
dictionary <- merge(chickenprice, dictionary, by = "iso3c", all=T)
dictionary <- dictionary[!is.na(dictionary$iso3c),]

# pig price ---------------------------------------------------------------

#get
pigprice <- read.csv("pigprice.csv")
colnames(pigprice) <- c("domain code", "domain", "area code", "country", "element code",
                        "element", "item code", "item", "year code", "year", 
                        "months code", "months", "unit", "pigprice", "flag",
                        "description")

#put price per kg
pigprice$pigprice <- pigprice$pigprice / 1000

#get iso3c
pigprice$iso3c <-countrycode(pigprice$country, origin="country.name", destination = "iso3c")

#get columns
pigprice <- pigprice[,c(14,17)]

#merge
dictionary <- merge(pigprice, dictionary, by = "iso3c", all=T)
dictionary <- dictionary[!is.na(dictionary$iso3c),]

# meat production ---------------------------------------------------------

#get
meatproduction <- read.csv("global-meat-production.csv")
colnames(meatproduction) <- c("country", "code", "year", "meatproduction")

#keep last year
meatproduction <- within(meatproduction, {max_year = ave(year, country, FUN=max)})
meatproduction <- meatproduction[meatproduction$year == meatproduction$max_year,]

#get iso3c
meatproduction$iso3c <-countrycode(meatproduction$country, origin="country.name", destination = "iso3c")

#keep columns
meatproduction <- meatproduction[,c(4,6)]

#merge
dictionary <- merge(meatproduction, dictionary, by = "iso3c", all=T)
dictionary <- dictionary[!is.na(dictionary$iso3c),]


# save combined dataset ---------------------------------------------------

#combine LMIC and UMIC
dictionary$Income.group[dictionary$Income.group == "Lower middle income" | 
                          dictionary$Income.group == "Upper middle income"] <- "Middle income"

#save
write.xlsx(dictionary, "countrydata.xlsx")


# get average values by income group --------------------------------------

#remove datasets other than our main one to avoid things with the same name
rm("bedday", "chickenprice", "chickenweight", "growth", "meatproduction", 
   "meatsupply", "pigprice", "pigweight", "population", "portionworking",
   "productivity", "resistance", "sepsis", "wtp")

#create dataframe
vars <- colnames(dictionary)[2:15]
levels <- c("Low income", "Middle income", "High income")
averages <- matrix(rep(0), ncol = length(vars), nrow = length(levels))
averages <- as.data.frame(averages)
colnames(averages) <- vars
rownames(averages) <- levels

for(i in 1:length(vars)){
  for(j in 1:length(levels)){
    averages[j,i] <- mean(as.vector(na.omit(dictionary[dictionary$Income.group == levels[j],i+1])))
  }
} #works but not population-weighted

dictionary2 <- dictionary %>%
  filter( is.na(`average wtp nominal`) == FALSE) %>%
  group_by(Income.group) %>%
  mutate(`WAwtp` = weighted.mean(`average wtp nominal`, `population`))
