library(data.table)
library(countrycode)
library(taRifx)
library(tidyverse)
library(dplyr)
library(xlsx)
library(forecast)
library(Hmisc)

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


# Population growth -------------------------------------------------------

#get
population <- read.csv("population.csv")
colnames(population) <- c("country", "country code", "indicator name", "indicator code", "year", "population")

#get iso3c
population$iso3c <-countrycode(population$country, origin="country.name", destination = "iso3c")

#keep columns
population <- population[,5:7]

#get income levels
income_levels <- dictionary[,c(1,21)]

#merge
population <- merge(population, income_levels, by = "iso3c", all=T)

#drop NAs
population <- population[complete.cases(population),]

#get total population of each income category over time
population <- population %>%
  group_by(Income.group, year) %>%
  summarize(totalpopulation = sum(population))

#split into a composite time series
populationLIC <- population[population$Income.group == "Low income",]
populationMIC <- population[population$Income.group == "Middle income",]
populationHIC <- population[population$Income.group == "High income",]

for(i in 1:nrow(populationMIC)){
  populationMIC$totalpopulation[i] <- populationMIC$totalpopulation[i] - populationHIC$totalpopulation[i] - populationLIC$totalpopulation[i]
}

populationLIC <- ts(populationLIC$totalpopulation, start = 1960, frequency = 1)
populationMIC <- ts(populationMIC$totalpopulation, start = 1960, frequency = 1)
populationHIC <- ts(populationHIC$totalpopulation, start = 1960, frequency = 1)

#get predictions
ARIMALIC <- auto.arima(populationLIC, stepwise = F, approximation = F)
ARIMAMIC <- auto.arima(populationMIC, stepwise = F, approximation = F)
ARIMAHIC <- auto.arima(populationHIC, stepwise = F, approximation = F)

LICpoppredict <- forecast(ARIMALIC, 20)
MICpoppredict <- forecast(ARIMAMIC, 20)
HICpoppredict <- forecast(ARIMAHIC, 20)

LICmean <- LICpoppredict$mean
HICmean <- HICpoppredict$mean
MICmean <- MICpoppredict$mean

LICupper <- LICpoppredict$upper[,2]
LIClower <- LICpoppredict$lower[,2]

HICupper <- HICpoppredict$upper[,2]
HIClower <- HICpoppredict$lower[,2]

MICupper <- MICpoppredict$upper[,2]
MIClower <- MICpoppredict$lower[,2]

#convert to rate
LICpopgrow <- ((LICmean[20] / populationLIC[61])^0.05) - 1
LICpopgrowhigh <- ((LICupper[20] / populationLIC[61])^0.05) - 1
LICpopgrowlow <- ((LIClower[20] / populationLIC[61])^0.05) - 1

MICpopgrow <- ((MICmean[20] / populationMIC[61])^0.05) - 1
MICpopgrowhigh <- ((MICupper[20] / populationMIC[61])^0.05) - 1
MICpopgrowlow <- ((MIClower[20] / populationMIC[61])^0.05) - 1

HICpopgrow <- ((HICmean[20] / populationHIC[61])^0.05) - 1
HICpopgrowhigh <- ((HICupper[20] / populationHIC[61])^0.05) - 1
HICpopgrowlow <- ((HIClower[20] / populationHIC[61])^0.05) - 1

#get standard deviation
LICsd1 <- (LICpopgrowhigh - LICpopgrow) / 1.96
LICsd2 <- (LICpopgrowlow - LICpopgrow) / 1.96
LICsd <- 0.5*(abs(LICsd1) + abs(LICsd2))

MICsd1 <- (MICpopgrowhigh - MICpopgrow) / 1.96
MICsd2 <- (MICpopgrowlow - MICpopgrow) / 1.96
MICsd <- 0.5*(abs(MICsd1) + abs(MICsd2))

HICsd1 <- (HICpopgrowhigh - HICpopgrow) / 1.96
HICsd2 <- (MICpopgrowlow - MICpopgrow) / 1.96
HICsd <- 0.5*(abs(HICsd1) + abs(HICsd2))

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
levels <- c("High income", "Low income", "Middle income", "World")
averages <- matrix(rep(0), ncol = length(vars), nrow = length(levels))
averages <- as.data.frame(averages)
colnames(averages) <- vars
rownames(averages) <- levels

#meatproduction
dictionarymeatproduction <- dictionary %>%
  filter( is.na(`meatproduction`) == FALSE) %>%
  group_by(Income.group) %>%
  summarize(mean_wtp = weighted.mean(`meatproduction`, `population`))

averages[1:3,1] <- dictionarymeatproduction[1:3,2]

rm(dictionarymeatproduction)

meatproductionavg <- dictionary %>%
  filter( is.na(`meatproduction`) == FALSE & is.na(`population`) == FALSE) %>%
  summarize(mean_wtp = weighted.mean(`meatproduction`, `population`))
averages[4,1] <- meatproductionavg

#pigprice
dictionarypigprice <- dictionary %>%
  filter( is.na(`pigprice`) == FALSE) %>%
  group_by(Income.group) %>%
  summarize(mean_wtp = weighted.mean(`pigprice`, `population`))

averages[1:3,2] <- dictionarypigprice[1:3,2]

rm(dictionarypigprice)

pigpriceavg <- dictionary %>%
  filter( is.na(`pigprice`) == FALSE & is.na(`population`) == FALSE) %>%
  summarize(mean_wtp = weighted.mean(`pigprice`, `population`))
averages[4,2] <- pigpriceavg

#chickenprice
dictionarychickenprice <- dictionary %>%
  filter( is.na(`chickenprice`) == FALSE) %>%
  group_by(Income.group) %>%
  summarize(mean_wtp = weighted.mean(`chickenprice`, `population`))

averages[1:3,3] <- dictionarychickenprice[1:3,2]

rm(dictionarychickenprice)

chickenpriceavg <- dictionary %>%
  filter( is.na(`chickenprice`) == FALSE & is.na(`population`) == FALSE) %>%
  summarize(mean_wtp = weighted.mean(`chickenprice`, `population`))
averages[4,3] <- chickenpriceavg

#chickenweight
dictionarychickenweight <- dictionary %>%
  filter( is.na(`chickenweight`) == FALSE) %>%
  group_by(Income.group) %>%
  summarize(mean_wtp = weighted.mean(`chickenweight`, `population`))

averages[1:3,4] <- dictionarychickenweight[1:3,2]

rm(dictionarychickenweight)

chickenweightavg <- dictionary %>%
  filter( is.na(`chickenweight`) == FALSE & is.na(`population`) == FALSE) %>%
  summarize(mean_wtp = weighted.mean(`chickenweight`, `population`))
averages[4,4] <- chickenweightavg

#beddaycost
dictionarybeddaycost <- dictionary %>%
  filter( is.na(`beddaycost`) == FALSE) %>%
  group_by(Income.group) %>%
  summarize(mean_wtp = weighted.mean(`beddaycost`, `population`))

averages[1:3,5] <- dictionarybeddaycost[1:3,2]

rm(dictionarybeddaycost)

beddaycostavg <- dictionary %>%
  filter( is.na(`beddaycost`) == FALSE & is.na(`population`) == FALSE) %>%
  summarize(mean_wtp = weighted.mean(`beddaycost`, `population`))
averages[4,5] <- beddaycostavg

#meatsupply
dictionarymeatsupply <- dictionary %>%
  filter( is.na(`meatsupply`) == FALSE) %>%
  group_by(Income.group) %>%
  summarize(mean_wtp = weighted.mean(`meatsupply`, `population`))

averages[1:3,6] <- dictionarymeatsupply[1:3,2]

rm(dictionarymeatsupply)

meatsupplyavg <- dictionary %>%
  filter( is.na(`meatsupply`) == FALSE & is.na(`population`) == FALSE) %>%
  summarize(mean_wtp = weighted.mean(`meatsupply`, `population`))
averages[4,6] <- meatsupplyavg

#pigweight
dictionarypigweight <- dictionary %>%
  filter( is.na(`pigweight`) == FALSE) %>%
  group_by(Income.group) %>%
  summarize(mean_wtp = weighted.mean(`pigweight`, `population`))

averages[1:3,7] <- dictionarypigweight[1:3,2]

rm(dictionarypigweight)

pigweightavg <- dictionary %>%
  filter( is.na(`pigweight`) == FALSE & is.na(`population`) == FALSE) %>%
  summarize(mean_wtp = weighted.mean(`pigweight`, `population`))
averages[4,7] <- pigweightavg

#portion working
dictionaryportionworking <- dictionary %>%
  filter( is.na(`portion working`) == FALSE) %>%
  group_by(Income.group) %>%
  summarize(mean_wtp = weighted.mean(`portion working`, `population`))

averages[1:3,8] <- dictionaryportionworking[1:3,2]

rm(dictionaryportionworking)

portionworkingavg <- dictionary %>%
  filter( is.na(`portion working`) == FALSE & is.na(`population`) == FALSE) %>%
  summarize(mean_wtp = weighted.mean(`portion working`, `population`))
averages[4,8] <- portionworkingavg

#productivity
dictionaryproductivity <- dictionary %>%
  filter( is.na(`productivity`) == FALSE) %>%
  group_by(Income.group) %>%
  summarize(mean_wtp = weighted.mean(`productivity`, `population`))

averages[1:3,9] <- dictionaryproductivity[1:3,2]

rm(dictionaryproductivity)

productivityavg <- dictionary %>%
  filter( is.na(`productivity`) == FALSE & is.na(`population`) == FALSE) %>%
  summarize(mean_wtp = weighted.mean(`productivity`, `population`))
averages[4,9] <- productivityavg

#DRI
dictionaryDRI <- dictionary %>%
  filter( is.na(`DRI`) == FALSE) %>%
  group_by(Income.group) %>%
  summarize(mean_wtp = weighted.mean(`DRI`, `population`))

averages[1:3,10] <- dictionaryDRI[1:3,2]

rm(dictionaryDRI)

DRIavg <- dictionary %>%
  filter( is.na(`DRI`) == FALSE & is.na(`population`) == FALSE) %>%
  summarize(mean_wtp = weighted.mean(`DRI`, `population`))
averages[4,10] <- DRIavg

#incidence per 100k
dictionaryincidenceper100k <- dictionary %>%
  filter( is.na(`incidence per 100k`) == FALSE) %>%
  group_by(Income.group) %>%
  summarize(mean_wtp = weighted.mean(`incidence per 100k`, `population`))

averages[1:3,11] <- dictionaryincidenceper100k[1:3,2]

rm(dictionaryincidenceper100k)

incidenceper100kavg <- dictionary %>%
  filter( is.na(`incidence per 100k`) == FALSE & is.na(`population`) == FALSE) %>%
  summarize(mean_wtp = weighted.mean(`incidence per 100k`, `population`))
averages[4,11] <- incidenceper100kavg

#mortality rate
dictionarymortalityrate <- dictionary %>%
  filter( is.na(`mortality rate`) == FALSE) %>%
  group_by(Income.group) %>%
  summarize(mean_wtp = weighted.mean(`mortality rate`, `population`))

averages[1:3,12] <- dictionarymortalityrate[1:3,2]

rm(dictionarymortalityrate)

mortalityrateavg <- dictionary %>%
  filter( is.na(`mortality rate`) == FALSE & is.na(`population`) == FALSE) %>%
  summarize(mean_wtp = weighted.mean(`mortality rate`, `population`))
averages[4,12] <- mortalityrateavg

#mean(growth)
dictionarymeangrowth <- dictionary %>%
  filter( is.na(`mean(growth)`) == FALSE) %>%
  group_by(Income.group) %>%
  summarize(mean_wtp = weighted.mean(`mean(growth)`, `population`))

averages[1:3,13] <- dictionarymeangrowth[1:3,2]

rm(dictionarymeangrowth)

meangrowthavg <- dictionary %>%
  filter( is.na(`mean(growth)`) == FALSE & is.na(`population`) == FALSE) %>%
  summarize(mean_wtp = weighted.mean(`mean(growth)`, `population`))
averages[4,13] <- meangrowthavg

#average wtp nominal
dictionaryaveragewtpnominal <- dictionary %>%
  filter( is.na(`average wtp nominal`) == FALSE) %>%
  group_by(Income.group) %>%
  summarize(mean_wtp = weighted.mean(`average wtp nominal`, `population`))

averages[1:3,14] <- dictionaryaveragewtpnominal[1:3,2]

rm(dictionaryaveragewtpnominal)

averagewtpnominalavg <- dictionary %>%
  filter( is.na(`average wtp nominal`) == FALSE & is.na(`population`) == FALSE) %>%
  summarize(mean_wtp = weighted.mean(`average wtp nominal`, `population`))
averages[4,14] <- averagewtpnominalavg

#remove clutter
rm("averagewtpnominalavg", "beddaycostavg", "chickenpriceavg",
   "chickenweightavg", "DRIavg", "incidenceper100kavg", "meangrowthavg", 
   "meatproductionavg", "meatsupplyavg", "mortalityrateavg", "pigpriceavg", 
   "pigweightavg", "portionworkingavg", "productivityavg")


# Distributions -----------------------------------------------------------

dictionaryLIC <- dictionary[dictionary$Income.group == "Low income",]
dictionaryMIC <- dictionary[dictionary$Income.group == "Middle income",]
dictionaryHIC <- dictionary[dictionary$Income.group == "High income",]


dictest <- dictionary %>%
  filter(dictionary$`mean(growth)` > -5 & !is.na(dictionary$`mean(growth)`)) %>%
  group_by(`Income.group`)
  summarise(var = wtd.var(dictionary$`mean(growth)`, dictionary$population))

LICgro <- dictionaryLIC %>%
  filter(!is.na(dictionaryLIC$`mean(growth)`))
LICgro <- LICgro$`mean(growth)`

LICpop <- dictionaryLIC %>%
  filter(!is.na(dictionaryLIC$`mean(growth)`))
LICpop <- LICpop$population

wtd.var(LICgro, LICpop)
