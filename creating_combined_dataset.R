# Packages and income group data ------------------------------------------

library(data.table)
library(countrycode)
library(taRifx)
library(tidyverse)
library(dplyr)
library(xlsx)
library(forecast)
library(fitdistrplus)
library(haven)

setwd("C:/Users/tresc/Desktop/AMR-Model/General model/input data")

#get country names
dictionary <- read.csv("who_whoc_wb_Naylor2021.csv")

#combine LMIC and UMIC
dictionary$Income.group[dictionary$Income.group == "Lower middle income" | 
                          dictionary$Income.group == "Upper middle income"] <- "Middle income"


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

# Population growth -------------------------------------------------------

#get
population <- read.csv("population.csv")
colnames(population) <- c("country", "country code", "indicator name", "indicator code", "year", "population")

#get iso3c
population$iso3c <-countrycode(population$country, origin="country.name", destination = "iso3c")

#keep columns
population <- population[,5:7]

#get income levels
income_levels <- dictionary[,c(1,7)]

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

#declutter
rm("ARIMAHIC", "ARIMALIC", "ARIMAMIC", "HICpoppredict", "LICpoppredict", "MICpoppredict",
   "HIClower", "HICmean", "HICpopgrowhigh", "HICpopgrowlow", "HICsd", "HICsd1", "HICsd2",
   "HICupper", "LIClower", "LICmean", "LICpopgrowhigh", "LICpopgrowlow", "LICsd",
   "LICsd1", "LICsd2", "LICupper", "MIClower", "MICmean", "MICpopgrowhigh", "MICpopgrowlow",
   "MICsd", "MICsd1", "MICsd2", "MICupper", "populationHIC", "populationLIC", "populationMIC")

rm("income_levels", "population")

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

#declutter
rm(wtp)

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

#declutter
rm(growth)


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

#declutter
rm("sepsis")

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

#declutter
rm("resistance")

# portion of sepsis from resistant bacteria -------------------------------

#find average DRI for HICs (pop-weighted)
temp <- dictionary %>%
  filter( is.na(`DRI`) == FALSE) %>%
  group_by(Income.group) %>%
  summarize(mean_wtp = weighted.mean(`DRI`, `population`))

temp <- temp[1,2]
temp <- as.numeric(temp)

#create new var
dictionary$portionresistant <- dictionary$DRI * (0.0451 / temp)

rm(temp)

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

#declutter
rm("productivity")

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

#declutter
rm("portionworking")

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

#declutter
rm("pigweight")

# meat supply -------------------------------------------------------------

# #get
# meatsupply <- read.csv("meat-supply-per-person.csv")
# colnames(meatsupply) <- c("country", "year", "meatsupply")
# 
# #keep max year
# meatsupply <- within(meatsupply, {max_year = ave(year, country, FUN=max)})
# meatsupply <- meatsupply[meatsupply$year == meatsupply$max_year,]
# 
# #get iso3c
# meatsupply$iso3c <-countrycode(meatsupply$country, origin="country.name", destination = "iso3c")
# 
# #keep columns
# meatsupply <- meatsupply[,c(3,5)]
# 
# #merge
# dictionary <- merge(meatsupply, dictionary, by = "iso3c", all=T)
# dictionary <- dictionary[!is.na(dictionary$iso3c),]


# meat production ---------------------------------------------------------

#get
meatproductiontonnes <- read.csv("global-meat-production.csv")
colnames(meatproductiontonnes) <- c("country", "code", "year", "meattonnes")

#keep max year
meatproductiontonnes <- within(meatproductiontonnes, {max_year = ave(year, country, FUN=max)})
meatproductiontonnes <- meatproductiontonnes[meatproductiontonnes$year == meatproductiontonnes$max_year,]

#get iso3c
meatproductiontonnes$iso3c <-countrycode(meatproductiontonnes$country, origin="country.name", destination = "iso3c")

#keep columns
meatproductiontonnes <- meatproductiontonnes[, c(4,6)]

#merge
dictionary <- merge(meatproductiontonnes, dictionary, by = "iso3c", all=T)
dictionary <- dictionary[!is.na(dictionary$iso3c),]

#declutter
rm("meatproductiontonnes")

#transform
dictionary$tonnesper100kpop <- (dictionary$meattonnes * 100000) / dictionary$population
#'world produced 342422466 tonnes and had population of 7752840547, giving 
#'4416.7356 tonnes per 100,000 population
#'There were 25.69bn chickens and 0.9783bn pigs worldwide, or 3.348 and 0.1275 per person, 
#'or (in a country of 100m people) 334,800,000 chickens and 12,750,000 pigs
#'therefore the imputed number of pigs is 12,750,000 * (tonnesper100kpop / 4416.7356)
#'and the imputed number of chickens is 334,800,000 * (tonnesper100kpop / 4416.7356)

dictionary$imputedchickens <- 334800000 * (dictionary$tonnesper100kpop / 4416.7356)
dictionary$imputedpigs <- 12750000 * (dictionary$tonnesper100kpop / 4416.7356)

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

#declutter
rm(bedday)

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

#declutter
rm(chickenweight)

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

#declutter
rm(chickenprice)

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

#declutter
rm(pigprice)


# remaining life and work years -------------------------------------------

#get
lifeexpectancy <- read.csv("lifeexpectancywb.csv")
colnames(lifeexpectancy) <- c("iso3c", "year", "lifeexpectancy")

#keep max year
lifeexpectancy <- within(lifeexpectancy, {max_year = ave(year, iso3c, FUN=max)})
lifeexpectancy <- lifeexpectancy[lifeexpectancy$year == lifeexpectancy$max_year,]

#keep columns
lifeexpectancy <- lifeexpectancy[,c(1,3)]

#merge
dictionary <- merge(lifeexpectancy, dictionary, by = "iso3c", all=T)
dictionary <- dictionary[!is.na(dictionary$iso3c),]

#declutter
rm(lifeexpectancy)

#get
medianage <- read.csv("medianage.csv")

#get iso3c
medianage$iso3c <-countrycode(medianage$country, origin="country.name", destination = "iso3c")

#keep columns
medianage <- medianage[,c(2:3)]

#merge
dictionary <- merge(medianage, dictionary, by = "iso3c", all=T)
dictionary <- dictionary[!is.na(dictionary$iso3c),]

#declutter
rm(medianage)

#transformations
dictionary$remainingly <- dictionary$lifeexpectancy - dictionary$medianage
dictionary$remainingwy <- 65 - dictionary$medianage


# remove duplicate countries and save combined dataset --------------------

dictionary <- dictionary[!duplicated(dictionary$iso3c), ]

#save
write.xlsx(dictionary, "countrydata.xlsx")


# get average values by income group --------------------------------------

#create dataframe
vars <- c("popgrowth", "prodgrowth", "dr", "wtp", "npigs", "nchickens","pigweight",
          "pigprice", "chickenweight", "chickenprice", "incidence", "mortality", 
          "portionresistant", "annualproductivity", "beddaycost", "remaininglifeyears",
          "remainingworkyears")
levels <- c("High income", "Low income", "Middle income", "World")
averages <- matrix(rep(0), ncol = length(vars), nrow = length(levels))
averages <- as.data.frame(averages)
colnames(averages) <- vars
rownames(averages) <- levels


# popgrowth ---------------------------------------------------------------

averages["High income","popgrowth"] <- HICpopgrow
averages["Low income","popgrowth"] <- LICpopgrow
averages["Middle income","popgrowth"] <- MICpopgrow
rm("LICpopgrow", "MICpopgrow", "HICpopgrow")

# prodgrowth --------------------------------------------------------------

dictionaryprodgrowth <- dictionary %>%
  filter( is.na(`mean(growth)`) == FALSE) %>%
  group_by(Income.group) %>%
  summarize(prodgrowth = weighted.mean(`mean(growth)`, `population`))

averages["High income", "prodgrowth"] <- dictionaryprodgrowth[1,2]
averages["Middle income", "prodgrowth"] <- dictionaryprodgrowth[3, 2]
averages["Low income", "prodgrowth"] <- dictionaryprodgrowth[2, 2]

rm(dictionaryprodgrowth)

# dr ----------------------------------------------------------------------

averages$dr[1:3] <- (averages$prodgrowth[1:3] * 1.4 / 100) + 0.01

# wtp ---------------------------------------------------------------------

#average wtp nominal
dictionaryaveragewtpnominal <- dictionary %>%
  filter( is.na(`average wtp nominal`) == FALSE) %>%
  group_by(Income.group) %>%
  summarize(mean_wtp = weighted.mean(`average wtp nominal`, `population`))

averages["High income", "wtp"] <- dictionaryaveragewtpnominal[1,2]
averages["Middle income", "wtp"] <- dictionaryaveragewtpnominal[3, 2]
averages["Low income", "wtp"] <- dictionaryaveragewtpnominal[2, 2]

rm(dictionaryaveragewtpnominal)

averages["High income", "wtp"] <- averages["High income", "wtp"] * 1.12270195
averages["Middle income", "wtp"] <- averages["Middle income", "wtp"] * 1.15243662
averages["Low income", "wtp"] <- averages["Low income", "wtp"] * 0.964223323

# npigs -------------------------------------------------------------------

dictionarynpigs <- dictionary %>%
  filter( is.na(`imputedpigs`) == FALSE) %>%
  group_by(Income.group) %>%
  summarize(mean_wtp = weighted.mean(`imputedpigs`, `population`))

averages["High income", "npigs"] <- dictionarynpigs[1,2]
averages["Middle income", "npigs"] <- dictionarynpigs[3, 2]
averages["Low income", "npigs"] <- dictionarynpigs[2, 2]

rm("dictionarynpigs")

#distributions
par(mfrow = c(2,2))
hist(dictionary$imputedpigs[dictionary$Income.group == "High income"])
hist(dictionary$imputedpigs[dictionary$Income.group == "Middle income"])
hist(dictionary$imputedpigs[dictionary$Income.group == "Low income"])

##npigs HIC
df <- dictionary[!is.na(dictionary$Income.group) & !is.na(dictionary$imputedpigs) &
                   dictionary$Income.group == "High income",]

df$scaled_weight <- round(df$population / 1000000, 0)

my_vector <- vector()

for (i in 1:nrow(df)){
  values <- rep(df$imputedpigs[i], df$scaled_weight[i])
  my_vector <- c(my_vector, values)
}

## find parameters
distr_npigs_HIC <- fitdist(my_vector,"lnorm")

rm("df", "i", "my_vector", "values")

#npigs MIC
df <- dictionary[!is.na(dictionary$Income.group) & !is.na(dictionary$imputedpigs) &
                   dictionary$Income.group == "Middle income",]

df$scaled_weight <- round(df$population / 1000000, 0)

my_vector <- vector()

for (i in 1:nrow(df)){
  values <- rep(df$imputedpigs[i], df$scaled_weight[i])
  my_vector <- c(my_vector, values)
}

## find parameters
distr_npigs_MIC <- fitdist(my_vector,"lnorm")

rm("df", "i", "my_vector", "values")

#npigs LIC
df <- dictionary[!is.na(dictionary$Income.group) & !is.na(dictionary$imputedpigs) &
                   dictionary$Income.group == "Low income",]

df$scaled_weight <- round(df$population / 1000000, 0)

my_vector <- vector()

for (i in 1:nrow(df)){
  values <- rep(df$imputedpigs[i], df$scaled_weight[i])
  my_vector <- c(my_vector, values)
}

## find parameters
distr_npigs_LIC <- fitdist(my_vector,"lnorm")

rm("df", "i", "my_vector", "values")

# nchickens ---------------------------------------------------------------

dictionarynchickens <- dictionary %>%
  filter( is.na(`imputedchickens`) == FALSE) %>%
  group_by(Income.group) %>%
  summarize(mean_wtp = weighted.mean(`imputedchickens`, `population`))

averages["High income", "nchickens"] <- dictionarynchickens[1,2]
averages["Middle income", "nchickens"] <- dictionarynchickens[3, 2]
averages["Low income", "nchickens"] <- dictionarynchickens[2, 2]

rm("dictionarynchickens")

#distributions
par(mfrow = c(2,2))
hist(dictionary$imputedchickens[dictionary$Income.group == "High income"])
hist(dictionary$imputedchickens[dictionary$Income.group == "Middle income"])
hist(dictionary$imputedchickens[dictionary$Income.group == "Low income"])

##nchickens HIC
df <- dictionary[!is.na(dictionary$Income.group) & !is.na(dictionary$imputedchickens) &
                   dictionary$Income.group == "High income",]

df$scaled_weight <- round(df$population / 1000000, 0)

my_vector <- vector()

for (i in 1:nrow(df)){
  values <- rep(df$imputedchickens[i], df$scaled_weight[i])
  my_vector <- c(my_vector, values)
}

## find parameters
distr_nchickens_HIC <- fitdist(my_vector,"lnorm")

rm("df", "i", "my_vector", "values")

#nchickens MIC
df <- dictionary[!is.na(dictionary$Income.group) & !is.na(dictionary$imputedchickens) &
                   dictionary$Income.group == "Middle income",]

df$scaled_weight <- round(df$population / 1000000, 0)

my_vector <- vector()

for (i in 1:nrow(df)){
  values <- rep(df$imputedchickens[i], df$scaled_weight[i])
  my_vector <- c(my_vector, values)
}

## find parameters
distr_nchickens_MIC <- fitdist(my_vector,"lnorm")

rm("df", "i", "my_vector", "values")

#nchickens LIC
df <- dictionary[!is.na(dictionary$Income.group) & !is.na(dictionary$imputedchickens) &
                   dictionary$Income.group == "Low income",]

df$scaled_weight <- round(df$population / 1000000, 0)

my_vector <- vector()

for (i in 1:nrow(df)){
  values <- rep(df$imputedchickens[i], df$scaled_weight[i])
  my_vector <- c(my_vector, values)
}

## find parameters
distr_nchickens_LIC <- fitdist(my_vector,"lnorm")

rm("df", "i", "my_vector", "values")

# pigweight (world only) --------------------------------------------------

pigweightavg <- dictionary %>%
  filter( is.na(`pigweight`) == FALSE & is.na(`population`) == FALSE) %>%
  summarize(mean_wtp = weighted.mean(`pigweight`, `population`))
averages["World", "pigweight"] <- pigweightavg

rm(pigweightavg)

# pigprice ----------------------------------------------------------------

dictionarypigprice <- dictionary %>%
  filter( is.na(`pigprice`) == FALSE) %>%
  group_by(Income.group) %>%
  summarize(mean_wtp = weighted.mean(`pigprice`, `population`))

averages["High income", "pigprice"] <- dictionarypigprice[1,2]
averages["Middle income", "pigprice"] <- dictionarypigprice[3, 2]
averages["Low income", "pigprice"] <- dictionarypigprice[2, 2]

rm("dictionarypigprice")

#distributions
par(mfrow = c(2,2))
hist(dictionary$pigprice[dictionary$Income.group == "High income"])
hist(dictionary$pigprice[dictionary$Income.group == "Middle income"])
hist(dictionary$pigprice[dictionary$Income.group == "Low income"])

##pigprice HIC
df <- dictionary[!is.na(dictionary$Income.group) & !is.na(dictionary$pigprice) &
                   dictionary$Income.group == "High income",]

df$scaled_weight <- round(df$population / 1000000, 0)

my_vector <- vector()

for (i in 1:nrow(df)){
  values <- rep(df$pigprice[i], df$scaled_weight[i])
  my_vector <- c(my_vector, values)
}

## find parameters
distr_pigprice_HIC <- fitdist(my_vector,"lnorm")

rm("df", "i", "my_vector", "values")

#pigprice MIC
df <- dictionary[!is.na(dictionary$Income.group) & !is.na(dictionary$pigprice) &
                   dictionary$Income.group == "Middle income",]

df$scaled_weight <- round(df$population / 1000000, 0)

my_vector <- vector()

for (i in 1:nrow(df)){
  values <- rep(df$pigprice[i], df$scaled_weight[i])
  my_vector <- c(my_vector, values)
}

## find parameters
distr_pigprice_MIC <- fitdist(my_vector,"lnorm")

rm("df", "i", "my_vector", "values")

#pigprice LIC
df <- dictionary[!is.na(dictionary$Income.group) & !is.na(dictionary$pigprice) &
                   dictionary$Income.group == "Low income",]

df$scaled_weight <- round(df$population / 1000000, 0)

my_vector <- vector()

for (i in 1:nrow(df)){
  values <- rep(df$pigprice[i], df$scaled_weight[i])
  my_vector <- c(my_vector, values)
}

## find parameters
distr_pigprice_LIC <- fitdist(my_vector,"lnorm")

rm("df", "i", "my_vector", "values")

# chickenweight (world only) ----------------------------------------------

chickenweightavg <- dictionary %>%
  filter( is.na(`chickenweight`) == FALSE & is.na(`population`) == FALSE) %>%
  summarize(mean_wtp = weighted.mean(`chickenweight`, `population`))
averages["World", "chickenweight"] <- chickenweightavg

rm(chickenweightavg)


# chickenprice ------------------------------------------------------------

dictionarychickenprice <- dictionary %>%
  filter( is.na(`chickenprice`) == FALSE) %>%
  group_by(Income.group) %>%
  summarize(mean_wtp = weighted.mean(`chickenprice`, `population`))

averages["High income", "chickenprice"] <- dictionarychickenprice[1,2]
averages["Middle income", "chickenprice"] <- dictionarychickenprice[3, 2]
averages["Low income", "chickenprice"] <- dictionarychickenprice[2, 2]

rm("dictionarychickenprice")

#distributions
par(mfrow = c(2,2))
hist(dictionary$chickenprice[dictionary$Income.group == "High income"])
hist(dictionary$chickenprice[dictionary$Income.group == "Middle income"])
hist(dictionary$chickenprice[dictionary$Income.group == "Low income"])

##chickenprice HIC
df <- dictionary[!is.na(dictionary$Income.group) & !is.na(dictionary$chickenprice) &
                   dictionary$Income.group == "High income",]

df$scaled_weight <- round(df$population / 1000000, 0)

my_vector <- vector()

for (i in 1:nrow(df)){
  values <- rep(df$chickenprice[i], df$scaled_weight[i])
  my_vector <- c(my_vector, values)
}

## find parameters
distr_chickenprice_HIC <- fitdist(my_vector,"lnorm")

rm("df", "i", "my_vector", "values")

#chickenprice MIC
df <- dictionary[!is.na(dictionary$Income.group) & !is.na(dictionary$chickenprice) &
                   dictionary$Income.group == "Middle income",]

df$scaled_weight <- round(df$population / 1000000, 0)

my_vector <- vector()

for (i in 1:nrow(df)){
  values <- rep(df$chickenprice[i], df$scaled_weight[i])
  my_vector <- c(my_vector, values)
}

## find parameters
distr_chickenprice_MIC <- fitdist(my_vector,"lnorm")

rm("df", "i", "my_vector", "values")

#chickenprice LIC
df <- dictionary[!is.na(dictionary$Income.group) & !is.na(dictionary$chickenprice) &
                   dictionary$Income.group == "Low income",]

df$scaled_weight <- round(df$population / 1000000, 0)

my_vector <- vector()

for (i in 1:nrow(df)){
  values <- rep(df$chickenprice[i], df$scaled_weight[i])
  my_vector <- c(my_vector, values)
}

## find parameters
distr_chickenprice_LIC <- fitdist(my_vector,"lnorm")

rm("df", "i", "my_vector", "values")

# incidence ---------------------------------------------------------------

dictionaryincidence <- dictionary %>%
  filter( is.na(`incidence per 100k`) == FALSE) %>%
  group_by(Income.group) %>%
  summarize(mean_wtp = weighted.mean(`incidence per 100k`, `population`))

averages["High income", "incidence"] <- dictionaryincidence[1,2] / 100000
averages["Middle income", "incidence"] <- dictionaryincidence[3, 2] / 100000
averages["Low income", "incidence"] <- dictionaryincidence[2, 2] / 100000

rm("dictionaryincidence")

#find distributions
names(dictionary)[names(dictionary) == "incidence per 100k"] <- "incidence"
dictionary$incidence <- dictionary$incidence / 100000

par(mfrow = c(2,2))
hist(dictionary$incidence[dictionary$Income.group == "High income"])
hist(dictionary$incidence[dictionary$Income.group == "Middle income"])
hist(dictionary$incidence[dictionary$Income.group == "Low income"])

##incidence HIC
df <- dictionary[!is.na(dictionary$Income.group) & !is.na(dictionary$incidence) &
                   dictionary$Income.group == "High income",]

df$scaled_weight <- round(df$population / 1000000, 0)

my_vector <- vector()

for (i in 1:nrow(df)){
  values <- rep(df$incidence[i], df$scaled_weight[i])
  my_vector <- c(my_vector, values)
}

## find parameters
distr_incidence_HIC <- fitdist(my_vector,"beta")

rm("df", "i", "my_vector", "values")

#incidence MIC
df <- dictionary[!is.na(dictionary$Income.group) & !is.na(dictionary$incidence) &
                   dictionary$Income.group == "Middle income",]

df$scaled_weight <- round(df$population / 1000000, 0)

my_vector <- vector()

for (i in 1:nrow(df)){
  values <- rep(df$incidence[i], df$scaled_weight[i])
  my_vector <- c(my_vector, values)
}

## find parameters
distr_incidence_MIC <- fitdist(my_vector,"beta")

rm("df", "i", "my_vector", "values")

#incidence LIC
df <- dictionary[!is.na(dictionary$Income.group) & !is.na(dictionary$incidence) &
                   dictionary$Income.group == "Low income",]

df$scaled_weight <- round(df$population / 1000000, 0)

my_vector <- vector()

for (i in 1:nrow(df)){
  values <- rep(df$incidence[i], df$scaled_weight[i])
  my_vector <- c(my_vector, values)
}

## find parameters
distr_incidence_LIC <- fitdist(my_vector,"beta")

rm("df", "i", "my_vector", "values")

# mortality ---------------------------------------------------------------

dictionarymortality <- dictionary %>%
  filter( is.na(`mortality rate`) == FALSE) %>%
  group_by(Income.group) %>%
  summarize(mean_wtp = weighted.mean(`mortality rate`, `population`))

averages["High income", "mortality"] <- dictionarymortality[1,2]
averages["Middle income", "mortality"] <- dictionarymortality[3, 2]
averages["Low income", "mortality"] <- dictionarymortality[2, 2]

rm("dictionarymortality")

#find distributions

par(mfrow = c(2,2))
hist(dictionary$`mortality rate`[dictionary$Income.group == "High income"])
hist(dictionary$`mortality rate`[dictionary$Income.group == "Middle income"])
hist(dictionary$`mortality rate`[dictionary$Income.group == "Low income"])

##mortality HIC
df <- dictionary[!is.na(dictionary$Income.group) & !is.na(dictionary$`mortality rate`) &
                   dictionary$Income.group == "High income",]

df$scaled_weight <- round(df$population / 1000000, 0)

my_vector <- vector()

for (i in 1:nrow(df)){
  values <- rep(df$`mortality rate`[i], df$scaled_weight[i])
  my_vector <- c(my_vector, values)
}

## find parameters
distr_mortality_HIC <- fitdist(my_vector,"beta")

rm("df", "i", "my_vector", "values")

#mortality MIC
df <- dictionary[!is.na(dictionary$Income.group) & !is.na(dictionary$`mortality rate`) &
                   dictionary$Income.group == "Middle income",]

df$scaled_weight <- round(df$population / 1000000, 0)

my_vector <- vector()

for (i in 1:nrow(df)){
  values <- rep(df$`mortality rate`[i], df$scaled_weight[i])
  my_vector <- c(my_vector, values)
}

## find parameters
distr_mortality_MIC <- fitdist(my_vector,"beta")

rm("df", "i", "my_vector", "values")

#mortality LIC
df <- dictionary[!is.na(dictionary$Income.group) & !is.na(dictionary$`mortality rate`) &
                   dictionary$Income.group == "Low income",]

df$scaled_weight <- round(df$population / 1000000, 0)

my_vector <- vector()

for (i in 1:nrow(df)){
  values <- rep(df$incidence[i], df$scaled_weight[i])
  my_vector <- c(my_vector, values)
}

## find parameters
distr_mortality_LIC <- fitdist(my_vector,"beta")

rm("df", "i", "my_vector", "values")

# portionresistant --------------------------------------------------------

dictionaryportionresistant <- dictionary %>%
  filter( is.na(`portionresistant`) == FALSE) %>%
  group_by(Income.group) %>%
  summarize(mean_wtp = weighted.mean(`portionresistant`, `population`))

averages["High income", "portionresistant"] <- dictionaryportionresistant[1,2]
averages["Middle income", "portionresistant"] <- dictionaryportionresistant[2, 2]

rm("dictionaryportionresistant")

#find distributions

par(mfrow = c(2,2))
hist(dictionary$`portionresistant`[dictionary$Income.group == "High income"])
hist(dictionary$`portionresistant`[dictionary$Income.group == "Middle income"])

#portionresistant HIC
df <- dictionary[!is.na(dictionary$Income.group) & !is.na(dictionary$`portionresistant`) &
                   dictionary$Income.group == "High income",]

df$scaled_weight <- round(df$population / 1000000, 0)

my_vector <- vector()

for (i in 1:nrow(df)){
  values <- rep(df$`portionresistant`[i], df$scaled_weight[i])
  my_vector <- c(my_vector, values)
}

## find parameters
distr_portionresistant_HIC <- fitdist(my_vector,"beta")

rm("df", "i", "my_vector", "values")

#portionresistant MIC
df <- dictionary[!is.na(dictionary$Income.group) & !is.na(dictionary$`portionresistant`) &
                   dictionary$Income.group == "Middle income",]

df$scaled_weight <- round(df$population / 1000000, 0)

my_vector <- vector()

for (i in 1:nrow(df)){
  values <- rep(df$`portionresistant`[i], df$scaled_weight[i])
  my_vector <- c(my_vector, values)
}

## find parameters
distr_portionresistant_MIC <- fitdist(my_vector,"beta")

rm("df", "i", "my_vector", "values")

# portion working ---------------------------------------------------------

averages$portionworking <- c(rep(0), nrow(averages))

dictionaryportionworking <- dictionary %>%
  filter( is.na(`portion working`) == FALSE) %>%
  group_by(Income.group) %>%
  summarize(mean_wtp = weighted.mean(`portion working`, `population`))

averages["High income", "portionworking"] <- dictionaryportionworking[1,2]
averages["Middle income", "portionworking"] <- dictionaryportionworking[3, 2]
averages["Low income", "portionworking"] <- dictionaryportionworking[2, 2]

rm("dictionaryportionworking")

# annualproductivity ------------------------------------------------------

#get hours worked 
hoursworked <- read_dta("pwt.dta")
hoursworked <- hoursworked[,c(1,4,9)]
colnames(hoursworked) <- c("iso3c", "year", "annualhours")

#keep max year
hoursworked <- within(hoursworked, {max_year = ave(year, iso3c, FUN=max)})
hoursworked <- hoursworked[hoursworked$year == hoursworked$max_year,]

#keep columns
hoursworked <- hoursworked[,c(1,3)]

#merge
dictionary <- merge(hoursworked, dictionary, by = "iso3c", all=T)
dictionary <- dictionary[!is.na(dictionary$iso3c),]

#declutter
rm(hoursworked)

#transformations
dictionary$annualproductivity <- dictionary$annualhours * dictionary$productivity

#get averages
dictionaryannualproductivity <- dictionary %>%
  filter( is.na(`annualproductivity`) == FALSE) %>%
  group_by(Income.group) %>%
  summarize(mean_annualproductivity = weighted.mean(`annualproductivity`, `population`))

averages["High income", "annualproductivity"] <- dictionaryannualproductivity[1,2]
averages["Middle income", "annualproductivity"] <- dictionaryannualproductivity[2, 2]

rm("dictionaryannualproductivity")

temp <- dictionary[!is.na(dictionary$annualproductivity),]
min(temp$annualproductivity)

temp <- temp[order(temp$annualproductivity),]

rm(temp)


# beddaycost --------------------------------------------------------------

dictionarybeddaycost <- dictionary %>%
  filter( is.na(`beddaycost`) == FALSE) %>%
  group_by(Income.group) %>%
  summarize(mean_beddaycost = weighted.mean(`beddaycost`, `population`))

averages["High income", "beddaycost"] <- dictionarybeddaycost[1,2]
averages["Middle income", "beddaycost"] <- dictionarybeddaycost[3, 2]
averages["Low income", "beddaycost"] <- dictionarybeddaycost[2, 2]

rm("dictionarybeddaycost")

#get distribution

par(mfrow = c(2,2))
hist(dictionary$`beddaycost`[dictionary$Income.group == "High income"])
hist(dictionary$`beddaycost`[dictionary$Income.group == "Middle income"])
hist(dictionary$`beddaycost`[dictionary$Income.group == "Low income"])

##beddaycost HIC
df <- dictionary[!is.na(dictionary$Income.group) & !is.na(dictionary$`beddaycost`) &
                   dictionary$Income.group == "High income",]

df$scaled_weight <- round(df$population / 1000000, 0)

my_vector <- vector()

for (i in 1:nrow(df)){
  values <- rep(df$`beddaycost`[i], df$scaled_weight[i])
  my_vector <- c(my_vector, values)
}

## find parameters
distr_beddaycost_HIC <- fitdist(my_vector,"lnorm")

rm("df", "i", "my_vector", "values")

#beddaycost MIC
df <- dictionary[!is.na(dictionary$Income.group) & !is.na(dictionary$`beddaycost`) &
                   dictionary$Income.group == "Middle income",]

df$scaled_weight <- round(df$population / 1000000, 0)

my_vector <- vector()

for (i in 1:nrow(df)){
  values <- rep(df$`beddaycost`[i], df$scaled_weight[i])
  my_vector <- c(my_vector, values)
}

## find parameters
distr_beddaycost_MIC <- fitdist(my_vector,"lnorm")

rm("df", "i", "my_vector", "values")

#beddaycost LIC
df <- dictionary[!is.na(dictionary$Income.group) & !is.na(dictionary$`beddaycost`) &
                   dictionary$Income.group == "Low income",]

df$scaled_weight <- round(df$population / 1000000, 0)

my_vector <- vector()

for (i in 1:nrow(df)){
  values <- rep(df$`beddaycost`[i], df$scaled_weight[i])
  my_vector <- c(my_vector, values)
}

## find parameters
distr_beddaycost_LIC <- fitdist(my_vector,"lnorm")

rm("df", "i", "my_vector", "values")

# remaininglifeyears ------------------------------------------------------

dictionaryremainingly <- dictionary %>%
  filter( is.na(`remainingly`) == FALSE) %>%
  group_by(Income.group) %>%
  summarize(mean_remainingly = weighted.mean(`remainingly`, `population`))

averages["High income", "remaininglifeyears"] <- dictionaryremainingly[1,2]
averages["Middle income", "remaininglifeyears"] <- dictionaryremainingly[3, 2]
averages["Low income", "remaininglifeyears"] <- dictionaryremainingly[2, 2]

rm("dictionaryremainingly")

# remainingworkyears ------------------------------------------------------

dictionaryremainingwy <- dictionary %>%
  filter( is.na(`remainingwy`) == FALSE) %>%
  group_by(Income.group) %>%
  summarize(mean_remainingly = weighted.mean(`remainingwy`, `population`))

averages["High income", "remainingworkyears"] <- dictionaryremainingwy[1,2]
averages["Middle income", "remainingworkyears"] <- dictionaryremainingwy[3, 2]
averages["Low income", "remainingworkyears"] <- dictionaryremainingwy[2, 2]

rm("dictionaryremainingwy")

#round remainingwy and remainingly, and make sure that wy doesn't exceed ly
averages[, "remainingworkyears"] <- round(averages[, "remainingworkyears"])
averages[, "remaininglifeyears"] <- round(averages[, "remaininglifeyears"])

for(i in 1:3){
  if(averages[i, "remainingworkyears"] > averages[i, "remaininglifeyears"])
    averages[i, "remainingworkyears"] <- averages[i, "remaininglifeyears"]
}

# save both datasets ------------------------------------------------------

write.xlsx(dictionary, "countrydata.xlsx")
write.xlsx(averages, "countrydataaverages.xlsx")


