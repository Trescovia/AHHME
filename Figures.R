# Packages ----------------------------------------------------------------

library("data.table")
library("readxl")
library("stargazer")
library("tidyverse")
library("tseries") 
library("forecast") 
library("dynlm") 
library("seastests")
library("forecast")
library("TSA")
library("epiR")
library("extraDistr")
library("MonoInc")
library("pksensi")
library("sensitivity")
library("xlsx")
library("gridExtra")
library("ggplot2")
library("reshape2")
library("here")
library("devtools")
library("multisensi")
library("rsq")
library("forcats")

# Data files --------------------------------------------------------------

inputs_general <- read.csv(here("inputs - general model.csv"))
inputs_general <- as.data.table(inputs_general)
colnames(inputs_general) <- c("parameter", "description", "HIC", "MIC-I", "MIC-S", "LIC", "Value", "Min", "Lo", "Med", "Hi", "Max", 
                      "LIC min", "LIC max", "MIC min", "MIC max", "HIC min", "HIC max")

pop <- read.csv(here("Vietnam Population.csv"))

dependency <- read.csv(here("Viet Nam dependency ratio.csv"))

inputs_casestudy <- read.csv(here("inputs - case study - Copy.csv"))
inputs_casestudy <- as.data.table(inputs_casestudy)
colnames(inputs_casestudy) <- c("parameter", "description", "Viet Nam", "Value", "Min", "Lo", "Med", "Hi", "Max")

source("Functions2.R")


# Default scenario --------------------------------------------------------

scenario_income <- "LIC"
scenario_prod <- "HCA"
scenario_transmission <- "med"
scenario_farm_effect <- "hi"

number_runs <- 10

# Table 1 - Maximum Cost under Each Scenario ------------------------------

scenario_prod      <- "HCA"  

scenario_analysis_LIC <- matrix(rep(0), ncol = 5, nrow = 4)
colnames(scenario_analysis_LIC) <- c("animal productivity falls 2%", 
                                     "animal productivity falls 1%",
                                     "animal productivity stays constant",
                                     "animal productivity rises 1%",
                                     "animal productivity rises 2%")
rownames(scenario_analysis_LIC) <- c("human AMR falls 2.5%",
                                     "human AMR falls 5%",
                                     "human AMR falls 10%",
                                     "human AMR falls 16%")

scenario_analysis_MIC_I <- scenario_analysis_LIC
scenario_analysis_MIC_S <- scenario_analysis_LIC
scenario_analysis_HIC <- scenario_analysis_LIC

#LIC
scenario_income    <- "LIC"

scenario_transmission <- "low"

scenario_farm_effect       <- "min"
scenario_analysis_LIC[1,1] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "lo"
scenario_analysis_LIC[1,2] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "med"
scenario_analysis_LIC[1,3] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "hi"
scenario_analysis_LIC[1,4] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "max"
scenario_analysis_LIC[1,5] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

scenario_transmission <- "med"

scenario_farm_effect       <- "min"
scenario_analysis_LIC[2,1] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "lo"
scenario_analysis_LIC[2,2] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "med"
scenario_analysis_LIC[2,3] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "hi"
scenario_analysis_LIC[2,4] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "max"
scenario_analysis_LIC[2,5] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

scenario_transmission <- "hi"

scenario_farm_effect       <- "min"
scenario_analysis_LIC[3,1] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "lo"
scenario_analysis_LIC[3,2] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "med"
scenario_analysis_LIC[3,3] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "hi"
scenario_analysis_LIC[3,4] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "max"
scenario_analysis_LIC[3,5] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

scenario_transmission <- "max"

scenario_farm_effect       <- "min"
scenario_analysis_LIC[4,1] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "lo"
scenario_analysis_LIC[4,2] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "med"
scenario_analysis_LIC[4,3] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "hi"
scenario_analysis_LIC[4,4] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "max"
scenario_analysis_LIC[4,5] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

#MIC-I
scenario_income    <- "MIC-I"

scenario_transmission <- "low"

scenario_farm_effect       <- "min"
scenario_analysis_MIC_I[1,1] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "lo"
scenario_analysis_MIC_I[1,2] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "med"
scenario_analysis_MIC_I[1,3] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "hi"
scenario_analysis_MIC_I[1,4] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "max"
scenario_analysis_MIC_I[1,5] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

scenario_transmission <- "med"

scenario_farm_effect       <- "min"
scenario_analysis_MIC_I[2,1] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "lo"
scenario_analysis_MIC_I[2,2] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "med"
scenario_analysis_MIC_I[2,3] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "hi"
scenario_analysis_MIC_I[2,4] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "max"
scenario_analysis_MIC_I[2,5] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

scenario_transmission <- "hi"

scenario_farm_effect       <- "min"
scenario_analysis_MIC_I[3,1] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "lo"
scenario_analysis_MIC_I[3,2] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "med"
scenario_analysis_MIC_I[3,3] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "hi"
scenario_analysis_MIC_I[3,4] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "max"
scenario_analysis_MIC_I[3,5] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

scenario_transmission <- "max"

scenario_farm_effect       <- "min"
scenario_analysis_MIC_I[4,1] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "lo"
scenario_analysis_MIC_I[4,2] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "med"
scenario_analysis_MIC_I[4,3] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "hi"
scenario_analysis_MIC_I[4,4] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "max"
scenario_analysis_MIC_I[4,5] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

#MIC-S
scenario_income    <- "MIC-S"

scenario_transmission <- "low"

scenario_farm_effect       <- "min"
scenario_analysis_MIC_S[1,1] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "lo"
scenario_analysis_MIC_S[1,2] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "med"
scenario_analysis_MIC_S[1,3] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "hi"
scenario_analysis_MIC_S[1,4] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "max"
scenario_analysis_MIC_S[1,5] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

scenario_transmission <- "med"

scenario_farm_effect       <- "min"
scenario_analysis_MIC_S[2,1] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "lo"
scenario_analysis_MIC_S[2,2] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "med"
scenario_analysis_MIC_S[2,3] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "hi"
scenario_analysis_MIC_S[2,4] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "max"
scenario_analysis_MIC_S[2,5] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

scenario_transmission <- "hi"

scenario_farm_effect       <- "min"
scenario_analysis_MIC_S[3,1] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "lo"
scenario_analysis_MIC_S[3,2] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "med"
scenario_analysis_MIC_S[3,3] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "hi"
scenario_analysis_MIC_S[3,4] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "max"
scenario_analysis_MIC_S[3,5] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

scenario_transmission <- "max"

scenario_farm_effect       <- "min"
scenario_analysis_MIC_S[4,1] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "lo"
scenario_analysis_MIC_S[4,2] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "med"
scenario_analysis_MIC_S[4,3] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "hi"
scenario_analysis_MIC_S[4,4] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "max"
scenario_analysis_MIC_S[4,5] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

#HIC
scenario_income    <- "HIC"

scenario_transmission <- "low"

scenario_farm_effect       <- "min"
scenario_analysis_HIC[1,1] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "lo"
scenario_analysis_HIC[1,2] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "med"
scenario_analysis_HIC[1,3] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "hi"
scenario_analysis_HIC[1,4] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "max"
scenario_analysis_HIC[1,5] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

scenario_transmission <- "med"

scenario_farm_effect       <- "min"
scenario_analysis_HIC[2,1] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "lo"
scenario_analysis_HIC[2,2] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "med"
scenario_analysis_HIC[2,3] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "hi"
scenario_analysis_HIC[2,4] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "max"
scenario_analysis_HIC[2,5] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

scenario_transmission <- "hi"

scenario_farm_effect       <- "min"
scenario_analysis_HIC[3,1] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "lo"
scenario_analysis_HIC[3,2] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "med"
scenario_analysis_HIC[3,3] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "hi"
scenario_analysis_HIC[3,4] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "max"
scenario_analysis_HIC[3,5] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

scenario_transmission <- "max"

scenario_farm_effect       <- "min"
scenario_analysis_HIC[4,1] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "lo"
scenario_analysis_HIC[4,2] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "med"
scenario_analysis_HIC[4,3] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "hi"
scenario_analysis_HIC[4,4] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
scenario_farm_effect       <- "max"
scenario_analysis_HIC[4,5] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

write.xlsx(scenario_analysis_LIC, "Outputs/Table 1 A.xlsx")
write.xlsx(scenario_analysis_MIC_I, "Outputs/Table 1 B.xlsx")
write.xlsx(scenario_analysis_MIC_S, "Outputs/Table 1 C.xlsx")
write.xlsx(scenario_analysis_HIC, "Outputs/Table 1 D.xlsx")


# Figure 2 - Distribution of Results from Montecarlo Simulation -----------

scenario_prod <- "HCA" 
scenario_transmission <- "med"

###LIC

scenario_income <- "LIC"

LIC_MC_Vector <- rep(0, number_runs)

inputs_LIC <- read.csv(here("inputs - general model.csv"))
inputs_LIC <- as.data.table(inputs_LIC)
colnames(inputs_LIC) <- c("parameter", "description", "HIC", "MIC-I", "MIC-S", "LIC", "Value", "Min", "Lo", "Med", "Hi", "Max", 
                          "LIC min", "LIC max", "MIC min", "MIC max", "HIC min", "HIC max")

set.seed(42069)

for(i in 1:number_runs){
  #load dataset
  inputsPSA <- inputs_LIC
  
  for(j in c(3,5:12,15,17,19:20, 27:38, 42:60)){
    inputsPSA[j,6] <- runif(1,as.numeric(inputsPSA[j,13]),as.numeric(inputsPSA[j,14])) 
  }
  
  inputsPSA[21,10] <- runif(1,as.numeric(inputsPSA[21,14]),as.numeric(inputsPSA[21,13]))
  
  #store result in vector
  LIC_MC_Vector[i] <- as.data.frame(Model(inputsPSA, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect))[1,1]
}

min_LIC <- min(LIC_MC_Vector)
max_LIC <- max(LIC_MC_Vector)
avg_LIC <- median(LIC_MC_Vector)

jpeg("Outputs/Figure 2 A.jpg")

hist(LIC_MC_Vector,
     xlab = "Maximum Annual Cost",
     ylab = "Cumulative Density",
     main = "Distribution of Values from Montecarlo Simulation - LIC")

dev.off()

write.xlsx(LIC_MC_Vector, "Outputs/MC Results LIC.xlsx")

##MIC

scenario_income <- "MIC-S"

MIC_MC_Vector <- rep(0, number_runs)

inputs_MIC <- read.csv(here("inputs - general model.csv"))
inputs_MIC <- as.data.table(inputs_MIC)
colnames(inputs_MIC) <- c("parameter", "description", "HIC", "MIC-I", "MIC-S", "LIC", "Value", "Min", "Lo", "Med", "Hi", "Max", 
                          "LIC min", "LIC max", "MIC min", "MIC max", "HIC min", "HIC max")

set.seed(42069)

for(i in 1:number_runs){
  #load dataset
  inputsPSA <- inputs_MIC
  
  for(j in c(3,5:12,15,17,19:20, 27:38, 42:60)){
    inputsPSA[j,5] <- runif(1,as.numeric(inputsPSA[j,15]),as.numeric(inputsPSA[j,16])) 
  }
  
  inputsPSA[21,10] <- runif(1,as.numeric(inputsPSA[21,16]),as.numeric(inputsPSA[21,15]))
  
  #store result in vector
  MIC_MC_Vector[i] <- as.data.frame(Model(inputsPSA, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect))[1,1]
}

min_MIC <- min(MIC_MC_Vector)
max_MIC <- max(MIC_MC_Vector)
avg_MIC <- median(MIC_MC_Vector)

jpeg("Outputs/Figure 2 B.jpg")

hist(MIC_MC_Vector,
     xlab = "Maximum Annual Cost",
     ylab = "Cumulative Density",
     main = "Distribution of Values from Montecarlo Simulation - MIC")

dev.off()

write.xlsx(MIC_MC_Vector, "Outputs/MC Results MIC.xlsx")

##HIC
scenario_income <- "HIC"

HIC_MC_Vector <- rep(0, number_runs)

inputs_HIC <- read.csv(here("inputs - general model.csv"))
inputs_HIC <- as.data.table(inputs_HIC)
colnames(inputs_HIC) <- c("parameter", "description", "HIC", "MIC-I", "MIC-S", "LIC", "Value", "Min", "Lo", "Med", "Hi", "Max", 
                          "LIC min", "LIC max", "MIC min", "MIC max", "HIC min", "HIC max")

set.seed(42069)

for(i in 1:number_runs){
  #load dataset
  inputsPSA <- inputs_HIC
  
  for(j in c(3,5:12,15,17,19:20, 27:38, 42:60)){
    inputsPSA[j,3] <- runif(1,as.numeric(inputsPSA[j,17]),as.numeric(inputsPSA[j,18])) 
  }
  
  inputsPSA[21,10] <- runif(1,as.numeric(inputsPSA[21,18]),as.numeric(inputsPSA[21,17]))
  
  #store result in vector
  HIC_MC_Vector[i] <- as.data.frame(Model(inputsPSA, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect))[1,1]
}

min_HIC <- min(HIC_MC_Vector)
max_HIC <- max(HIC_MC_Vector)
avg_HIC <- median(HIC_MC_Vector)

jpeg("Outputs/Figure 2 C.jpg")

hist(HIC_MC_Vector,
     xlab = "Maximum Annual Cost",
     ylab = "Cumulative Density",
     main = "Distribution of Values from Montecarlo Simulation - HIC")

dev.off()

write.xlsx(HIC_MC_Vector, "Outputs/MC Results HIC.xlsx")


# Figure 3 - Tornado Plots ------------------------------------------------

#'Human parameters to include in the plots: human AMR effect, discount rate, 
#'AMR growth, WTP, LFPR, background sickness, background AMR

#'Animal parameters to include: pig income effect, chicken income effect, 
#'farm size (x4), number of pigs, number of chickens

scenario_prod <- "HCA" 
scenario_transmission <- "med"
scenario_farm_effect <- "hi"

##LIC

#get the base case max cost
scenario_income <- "LIC"

LIC_base <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

#human AMR effect
inputs_tornado <- inputs_general

inputs_tornado[21,10] <- inputs_tornado[21,13]
LIC_human_AMR_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[21,10] <- inputs_tornado[21,14]
LIC_human_AMR_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

LIC_human_AMR_low <- min((LIC_human_AMR_low - LIC_base), (LIC_human_AMR_high - LIC_base))
LIC_human_AMR_high <- max((LIC_human_AMR_low - LIC_base), (LIC_human_AMR_high - LIC_base))

#discount rate
inputs_tornado <- inputs_general

inputs_tornado[3,6] <- inputs_tornado[3,13]
LIC_human_dr_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[3,6] <- inputs_tornado[3,14]
LIC_human_dr_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

LIC_human_dr_low <- max((LIC_human_dr_low - LIC_base), (LIC_human_dr_high - LIC_base))
LIC_human_dr_high <- min((LIC_human_dr_low - LIC_base), (LIC_human_dr_high - LIC_base))

#AMR growth
inputs_tornado <- inputs_general

inputs_tornado[38,6] <- inputs_tornado[38,13]
LIC_human_amrgro_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[38,6] <- inputs_tornado[38,14]
LIC_human_amrgro_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

LIC_human_amrgro_low <- min((LIC_human_amrgro_low - LIC_base), (LIC_human_amrgro_high - LIC_base))
LIC_human_amrgro_high <- max((LIC_human_amrgro_low - LIC_base), (LIC_human_amrgro_high - LIC_base))

#background AMR
inputs_tornado <- inputs_general

inputs_tornado[28,6] <- inputs_tornado[28,13]
LIC_human_amrbase_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[28,6] <- inputs_tornado[28,14]
LIC_human_amrbase_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

LIC_human_amrbase_low <- min((LIC_human_amrbase_low - LIC_base), (LIC_human_amrbase_high - LIC_base))
LIC_human_amrbase_high <- max((LIC_human_amrbase_low - LIC_base), (LIC_human_amrbase_high - LIC_base))

#WTP
inputs_tornado <- inputs_general

inputs_tornado[6,6] <- inputs_tornado[6,13]
LIC_human_wtp_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[6,6] <- inputs_tornado[6,14]
LIC_human_wtp_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

LIC_human_wtp_low <- min((LIC_human_wtp_low - LIC_base), (LIC_human_wtp_high - LIC_base))
LIC_human_wtp_high <- max((LIC_human_wtp_low - LIC_base), (LIC_human_wtp_high - LIC_base))

#LFPR
inputs_tornado <- inputs_general

inputs_tornado[10,6] <- inputs_tornado[10,13]
LIC_human_lfpr_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[10,6] <- inputs_tornado[10,14]
LIC_human_lfpr_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

LIC_human_lfpr_low <- min((LIC_human_lfpr_low - LIC_base), (LIC_human_lfpr_high - LIC_base))
LIC_human_lfpr_high <- max((LIC_human_lfpr_low - LIC_base), (LIC_human_lfpr_high - LIC_base))

#disease incidence
inputs_tornado <- inputs_general

inputs_tornado[27,6] <- inputs_tornado[27,13]
LIC_human_disease_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[27,6] <- inputs_tornado[27,14]
LIC_human_disease_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

LIC_human_disease_low <- min((LIC_human_disease_low - LIC_base), (LIC_human_disease_high - LIC_base))
LIC_human_disease_high <- max((LIC_human_disease_low - LIC_base), (LIC_human_disease_high - LIC_base))

#/#

#pig income effect
inputs_tornado <- inputs_general

inputs_tornado[15,11] <- inputs_tornado[15,13]
LIC_animal_piginc_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[15,11] <- inputs_tornado[15,14]
LIC_animal_piginc_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

LIC_animal_piginc_low <- min((LIC_animal_piginc_low - LIC_base), (LIC_animal_piginc_high - LIC_base))
LIC_animal_piginc_high <- max((LIC_animal_piginc_low - LIC_base), (LIC_animal_piginc_high - LIC_base))

#chicken income effect
inputs_tornado <- inputs_general

inputs_tornado[17,11] <- inputs_tornado[17,13]
LIC_animal_chickinc_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[17,11] <- inputs_tornado[17,14]
LIC_animal_chickinc_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

LIC_animal_chickinc_low <- min((LIC_animal_chickinc_low - LIC_base), (LIC_animal_chickinc_high - LIC_base))
LIC_animal_chickinc_high <- max((LIC_animal_chickinc_low - LIC_base), (LIC_animal_chickinc_high - LIC_base))

#farm size (pig - I)
inputs_tornado <- inputs_general

inputs_tornado[46,6] <- inputs_tornado[46,13]
LIC_animal_pigfarmi_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[46,6] <- inputs_tornado[46,14]
LIC_animal_pigfarmi_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

LIC_animal_pigfarmi_low <- min((LIC_animal_pigfarmi_low - LIC_base), (LIC_animal_pigfarmi_high - LIC_base))
LIC_animal_pigfarmi_high <- max((LIC_animal_pigfarmi_low - LIC_base), (LIC_animal_pigfarmi_high - LIC_base))

#farm size (pig - S)
inputs_tornado <- inputs_general

inputs_tornado[47,6] <- inputs_tornado[47,13]
LIC_animal_pigfarms_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[47,6] <- inputs_tornado[47,14]
LIC_animal_pigfarms_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

LIC_animal_pigfarms_low <- min((LIC_animal_pigfarms_low - LIC_base), (LIC_animal_pigfarms_high - LIC_base))
LIC_animal_pigfarms_high <- max((LIC_animal_pigfarms_low - LIC_base), (LIC_animal_pigfarms_high - LIC_base))

#farm size (chicken - I)
inputs_tornado <- inputs_general

inputs_tornado[44,6] <- inputs_tornado[44,13]
LIC_animal_chickfarmi_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[44,6] <- inputs_tornado[44,14]
LIC_animal_chickfarmi_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

LIC_animal_chickfarmi_low <- min((LIC_animal_chickfarmi_low - LIC_base), (LIC_animal_chickfarmi_high - LIC_base))
LIC_animal_chickfarmi_high <- max((LIC_animal_chickfarmi_low - LIC_base), (LIC_animal_chickfarmi_high - LIC_base))

#farm size (chicken - S)
inputs_tornado <- inputs_general

inputs_tornado[45,6] <- inputs_tornado[45,13]
LIC_animal_chickfarms_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[45,6] <- inputs_tornado[45,14]
LIC_animal_chickfarms_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

LIC_animal_chickfarms_low <- min((LIC_animal_chickfarms_low - LIC_base), (LIC_animal_chickfarms_high - LIC_base))
LIC_animal_chickfarms_high <- max((LIC_animal_chickfarms_low - LIC_base), (LIC_animal_chickfarms_high - LIC_base))

#number of pigs
inputs_tornado <- inputs_general

inputs_tornado[42,6] <- inputs_tornado[42,13]
LIC_animal_npigs_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[42,6] <- inputs_tornado[42,14]
LIC_animal_npigs_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

LIC_animal_npigs_low <- min((LIC_animal_npigs_low - LIC_base), (LIC_animal_npigs_high - LIC_base))
LIC_animal_npigs_high <- max((LIC_animal_npigs_low - LIC_base), (LIC_animal_npigs_high - LIC_base))

#number of chickens
inputs_tornado <- inputs_general

inputs_tornado[43,6] <- inputs_tornado[43,13]
LIC_animal_nchicks_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[43,6] <- inputs_tornado[43,14]
LIC_animal_nchicks_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

LIC_animal_nchicks_low <- min((LIC_animal_nchicks_low - LIC_base), (LIC_animal_nchicks_high - LIC_base))
LIC_animal_nchicks_high <- max((LIC_animal_nchicks_low - LIC_base), (LIC_animal_nchicks_high - LIC_base))

##MIC-I

#get the base case max cost
scenario_income <- "MIC-I"

MICI_base <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

#human AMR effect
inputs_tornado <- inputs_general

inputs_tornado[21,10] <- inputs_tornado[21,15]
MICI_human_AMR_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[21,10] <- inputs_tornado[21,16]
MICI_human_AMR_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

MICI_human_AMR_low <- min((MICI_human_AMR_low - MICI_base), (MICI_human_AMR_high - MICI_base))
MICI_human_AMR_high <- max((MICI_human_AMR_low - MICI_base), (MICI_human_AMR_high - MICI_base))

#discount rate
inputs_tornado <- inputs_general

inputs_tornado[3,4] <- inputs_tornado[3,15]
MICI_human_dr_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[3,4] <- inputs_tornado[3,16]
MICI_human_dr_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

MICI_human_dr_low <- max((MICI_human_dr_low - MICI_base), (MICI_human_dr_high - MICI_base))
MICI_human_dr_high <- min((MICI_human_dr_low - MICI_base), (MICI_human_dr_high - MICI_base))

#AMR growth
inputs_tornado <- inputs_general

inputs_tornado[38,4] <- inputs_tornado[38,15]
MICI_human_amrgro_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[38,4] <- inputs_tornado[38,16]
MICI_human_amrgro_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

MICI_human_amrgro_low <- min((MICI_human_amrgro_low - MICI_base), (MICI_human_amrgro_high - MICI_base))
MICI_human_amrgro_high <- max((MICI_human_amrgro_low - MICI_base), (MICI_human_amrgro_high - MICI_base))

#background AMR
inputs_tornado <- inputs_general

inputs_tornado[28,4] <- inputs_tornado[28,15]
MICI_human_amrbase_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[28,4] <- inputs_tornado[28,16]
MICI_human_amrbase_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

MICI_human_amrbase_low <- min((MICI_human_amrbase_low - MICI_base), (MICI_human_amrbase_high - MICI_base))
MICI_human_amrbase_high <- max((MICI_human_amrbase_low - MICI_base), (MICI_human_amrbase_high - MICI_base))

#WTP
inputs_tornado <- inputs_general

inputs_tornado[6,4] <- inputs_tornado[6,15]
MICI_human_wtp_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[6,4] <- inputs_tornado[6,16]
MICI_human_wtp_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

MICI_human_wtp_low <- min((MICI_human_wtp_low - MICI_base), (MICI_human_wtp_high - MICI_base))
MICI_human_wtp_high <- max((MICI_human_wtp_low - MICI_base), (MICI_human_wtp_high - MICI_base))

#LFPR
inputs_tornado <- inputs_general

inputs_tornado[10,4] <- inputs_tornado[10,15]
MICI_human_lfpr_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[10,4] <- inputs_tornado[10,16]
MICI_human_lfpr_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

MICI_human_lfpr_low <- min((MICI_human_lfpr_low - MICI_base), (MICI_human_lfpr_high - MICI_base))
MICI_human_lfpr_high <- max((MICI_human_lfpr_low - MICI_base), (MICI_human_lfpr_high - MICI_base))

#disease incidence
inputs_tornado <- inputs_general

inputs_tornado[27,4] <- inputs_tornado[27,15]
MICI_human_disease_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[27,4] <- inputs_tornado[27,16]
MICI_human_disease_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

MICI_human_disease_low <- min((MICI_human_disease_low - MICI_base), (MICI_human_disease_high - MICI_base))
MICI_human_disease_high <- max((MICI_human_disease_low - MICI_base), (MICI_human_disease_high - MICI_base))

#/#

#pig income effect
inputs_tornado <- inputs_general

inputs_tornado[15,11] <- inputs_tornado[15,15]
MICI_animal_piginc_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[15,11] <- inputs_tornado[15,16]
MICI_animal_piginc_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

MICI_animal_piginc_low <- min((MICI_animal_piginc_low - MICI_base), (MICI_animal_piginc_high - MICI_base))
MICI_animal_piginc_high <- max((MICI_animal_piginc_low - MICI_base), (MICI_animal_piginc_high - MICI_base))

#chicken income effect
inputs_tornado <- inputs_general

inputs_tornado[17,11] <- inputs_tornado[17,15]
MICI_animal_chickinc_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[17,11] <- inputs_tornado[17,16]
MICI_animal_chickinc_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

MICI_animal_chickinc_low <- min((MICI_animal_chickinc_low - MICI_base), (MICI_animal_chickinc_high - MICI_base))
MICI_animal_chickinc_high <- max((MICI_animal_chickinc_low - MICI_base), (MICI_animal_chickinc_high - MICI_base))

#farm size (pig - I)
inputs_tornado <- inputs_general

inputs_tornado[46,4] <- inputs_tornado[46,15]
MICI_animal_pigfarmi_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[46,4] <- inputs_tornado[46,16]
MICI_animal_pigfarmi_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

MICI_animal_pigfarmi_low <- min((MICI_animal_pigfarmi_low - MICI_base), (MICI_animal_pigfarmi_high - MICI_base))
MICI_animal_pigfarmi_high <- max((MICI_animal_pigfarmi_low - MICI_base), (MICI_animal_pigfarmi_high - MICI_base))

#farm size (pig - S)
inputs_tornado <- inputs_general

inputs_tornado[47,4] <- inputs_tornado[47,15]
MICI_animal_pigfarms_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[47,4] <- inputs_tornado[47,16]
MICI_animal_pigfarms_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

MICI_animal_pigfarms_low <- min((MICI_animal_pigfarms_low - MICI_base), (MICI_animal_pigfarms_high - MICI_base))
MICI_animal_pigfarms_high <- max((MICI_animal_pigfarms_low - MICI_base), (MICI_animal_pigfarms_high - MICI_base))

#farm size (chicken - I)
inputs_tornado <- inputs_general

inputs_tornado[44,4] <- inputs_tornado[44,15]
MICI_animal_chickfarmi_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[44,4] <- inputs_tornado[44,16]
MICI_animal_chickfarmi_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

MICI_animal_chickfarmi_low <- min((MICI_animal_chickfarmi_low - MICI_base), (MICI_animal_chickfarmi_high - MICI_base))
MICI_animal_chickfarmi_high <- max((MICI_animal_chickfarmi_low - MICI_base), (MICI_animal_chickfarmi_high - MICI_base))

#farm size (chicken - S)
inputs_tornado <- inputs_general

inputs_tornado[45,4] <- inputs_tornado[45,15]
MICI_animal_chickfarms_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[45,4] <- inputs_tornado[45,16]
MICI_animal_chickfarms_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

MICI_animal_chickfarms_low <- min((MICI_animal_chickfarms_low - MICI_base), (MICI_animal_chickfarms_high - MICI_base))
MICI_animal_chickfarms_high <- max((MICI_animal_chickfarms_low - MICI_base), (MICI_animal_chickfarms_high - MICI_base))

#number of pigs
inputs_tornado <- inputs_general

inputs_tornado[42,4] <- inputs_tornado[42,15]
MICI_animal_npigs_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[42,4] <- inputs_tornado[42,16]
MICI_animal_npigs_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

MICI_animal_npigs_low <- min((MICI_animal_npigs_low - MICI_base), (MICI_animal_npigs_high - MICI_base))
MICI_animal_npigs_high <- max((MICI_animal_npigs_low - MICI_base), (MICI_animal_npigs_high - MICI_base))

#number of chickens
inputs_tornado <- inputs_general

inputs_tornado[43,4] <- inputs_tornado[43,15]
MICI_animal_nchicks_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[43,4] <- inputs_tornado[43,16]
MICI_animal_nchicks_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

MICI_animal_nchicks_low <- min((MICI_animal_nchicks_low - MICI_base), (MICI_animal_nchicks_high - MICI_base))
MICI_animal_nchicks_high <- max((MICI_animal_nchicks_low - MICI_base), (MICI_animal_nchicks_high - MICI_base))

##MIC-S

#get the base case max cost
scenario_income <- "MIC-S"

MICS_base <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

#human AMR effect
inputs_tornado <- inputs_general

inputs_tornado[21,10] <- inputs_tornado[21,15]
MICS_human_AMR_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[21,10] <- inputs_tornado[21,16]
MICS_human_AMR_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

MICS_human_AMR_low <- min((MICS_human_AMR_low - MICS_base), (MICS_human_AMR_high - MICS_base))
MICS_human_AMR_high <- max((MICS_human_AMR_low - MICS_base), (MICS_human_AMR_high - MICS_base))

#discount rate
inputs_tornado <- inputs_general

inputs_tornado[3,5] <- inputs_tornado[3,15]
MICS_human_dr_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[3,5] <- inputs_tornado[3,16]
MICS_human_dr_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

MICS_human_dr_low <- max((MICS_human_dr_low - MICS_base), (MICS_human_dr_high - MICS_base))
MICS_human_dr_high <- min((MICS_human_dr_low - MICS_base), (MICS_human_dr_high - MICS_base))

#AMR growth
inputs_tornado <- inputs_general

inputs_tornado[38,5] <- inputs_tornado[38,15]
MICS_human_amrgro_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[38,5] <- inputs_tornado[38,16]
MICS_human_amrgro_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

MICS_human_amrgro_low <- min((MICS_human_amrgro_low - MICS_base), (MICS_human_amrgro_high - MICS_base))
MICS_human_amrgro_high <- max((MICS_human_amrgro_low - MICS_base), (MICS_human_amrgro_high - MICS_base))

#background AMR
inputs_tornado <- inputs_general

inputs_tornado[28,5] <- inputs_tornado[28,15]
MICS_human_amrbase_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[28,5] <- inputs_tornado[28,16]
MICS_human_amrbase_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

MICS_human_amrbase_low <- min((MICS_human_amrbase_low - MICS_base), (MICS_human_amrbase_high - MICS_base))
MICS_human_amrbase_high <- max((MICS_human_amrbase_low - MICS_base), (MICS_human_amrbase_high - MICS_base))

#WTP
inputs_tornado <- inputs_general

inputs_tornado[6,5] <- inputs_tornado[6,15]
MICS_human_wtp_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[6,5] <- inputs_tornado[6,16]
MICS_human_wtp_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

MICS_human_wtp_low <- min((MICS_human_wtp_low - MICS_base), (MICS_human_wtp_high - MICS_base))
MICS_human_wtp_high <- max((MICS_human_wtp_low - MICS_base), (MICS_human_wtp_high - MICS_base))

#LFPR
inputs_tornado <- inputs_general

inputs_tornado[10,5] <- inputs_tornado[10,15]
MICS_human_lfpr_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[10,5] <- inputs_tornado[10,16]
MICS_human_lfpr_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

MICS_human_lfpr_low <- min((MICS_human_lfpr_low - MICS_base), (MICS_human_lfpr_high - MICS_base))
MICS_human_lfpr_high <- max((MICS_human_lfpr_low - MICS_base), (MICS_human_lfpr_high - MICS_base))

#disease incidence
inputs_tornado <- inputs_general

inputs_tornado[27,5] <- inputs_tornado[27,15]
MICS_human_disease_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[27,5] <- inputs_tornado[27,16]
MICS_human_disease_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

MICS_human_disease_low <- min((MICS_human_disease_low - MICS_base), (MICS_human_disease_high - MICS_base))
MICS_human_disease_high <- max((MICS_human_disease_low - MICS_base), (MICS_human_disease_high - MICS_base))

#/#

#pig income effect
inputs_tornado <- inputs_general

inputs_tornado[15,11] <- inputs_tornado[15,15]
MICS_animal_piginc_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[15,11] <- inputs_tornado[15,16]
MICS_animal_piginc_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

MICS_animal_piginc_low <- min((MICS_animal_piginc_low - MICS_base), (MICS_animal_piginc_high - MICS_base))
MICS_animal_piginc_high <- max((MICS_animal_piginc_low - MICS_base), (MICS_animal_piginc_high - MICS_base))

#chicken income effect
inputs_tornado <- inputs_general

inputs_tornado[17,11] <- inputs_tornado[17,15]
MICS_animal_chickinc_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[17,11] <- inputs_tornado[17,16]
MICS_animal_chickinc_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

MICS_animal_chickinc_low <- min((MICS_animal_chickinc_low - MICS_base), (MICS_animal_chickinc_high - MICS_base))
MICS_animal_chickinc_high <- max((MICS_animal_chickinc_low - MICS_base), (MICS_animal_chickinc_high - MICS_base))

#farm size (pig - I)
inputs_tornado <- inputs_general

inputs_tornado[46,5] <- inputs_tornado[46,15]
MICS_animal_pigfarmi_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[46,5] <- inputs_tornado[46,16]
MICS_animal_pigfarmi_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

MICS_animal_pigfarmi_low <- min((MICS_animal_pigfarmi_low - MICS_base), (MICS_animal_pigfarmi_high - MICS_base))
MICS_animal_pigfarmi_high <- max((MICS_animal_pigfarmi_low - MICS_base), (MICS_animal_pigfarmi_high - MICS_base))

#farm size (pig - S)
inputs_tornado <- inputs_general

inputs_tornado[47,5] <- inputs_tornado[47,15]
MICS_animal_pigfarms_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[47,5] <- inputs_tornado[47,16]
MICS_animal_pigfarms_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

MICS_animal_pigfarms_low <- min((MICS_animal_pigfarms_low - MICS_base), (MICS_animal_pigfarms_high - MICS_base))
MICS_animal_pigfarms_high <- max((MICS_animal_pigfarms_low - MICS_base), (MICS_animal_pigfarms_high - MICS_base))

#farm size (chicken - I)
inputs_tornado <- inputs_general

inputs_tornado[44,5] <- inputs_tornado[44,15]
MICS_animal_chickfarmi_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[44,5] <- inputs_tornado[44,16]
MICS_animal_chickfarmi_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

MICS_animal_chicfarmi_low <- min((MICS_animal_chickfarmi_low - MICS_base), (MICS_animal_chickfarmi_high - MICS_base))
MICS_animal_chicfarmi_high <- max((MICS_animal_chickfarmi_low - MICS_base), (MICS_animal_chickfarmi_high - MICS_base))

#farm size (chicken - S)
inputs_tornado <- inputs_general

inputs_tornado[45,5] <- inputs_tornado[45,15]
MICS_animal_chickfarms_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[45,5] <- inputs_tornado[45,16]
MICS_animal_chickfarms_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

MICS_animal_chickfarms_low <- min((MICS_animal_chickfarms_low - MICS_base), (MICS_animal_chickfarms_high - MICS_base))
MICS_animal_chickfarms_high <- max((MICS_animal_chickfarms_low - MICS_base), (MICS_animal_chickfarms_high - MICS_base))

#number of pigs
inputs_tornado <- inputs_general

inputs_tornado[42,5] <- inputs_tornado[42,15]
MICS_animal_npigs_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[42,5] <- inputs_tornado[42,16]
MICS_animal_npigs_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

MICS_animal_npigs_low <- min((MICS_animal_npigs_low - MICS_base), (MICS_animal_npigs_high - MICS_base))
MICS_animal_npigs_high <- max((MICS_animal_npigs_low - MICS_base), (MICS_animal_npigs_high - MICS_base))

#number of chickens
inputs_tornado <- inputs_general

inputs_tornado[43,5] <- inputs_tornado[43,15]
MICS_animal_nchicks_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[43,5] <- inputs_tornado[43,16]
MICS_animal_nchicks_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

MICS_animal_nchicks_low <- min((MICS_animal_nchicks_low - MICS_base), (MICS_animal_nchicks_high - MICS_base))
MICS_animal_nchicks_high <- max((MICS_animal_nchicks_low - MICS_base), (MICS_animal_nchicks_high - MICS_base))

##HIC

#get the base case max cost
scenario_income <- "HIC"

HIC_base <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

#human AMR effect
inputs_tornado <- inputs_general

inputs_tornado[21,10] <- inputs_tornado[21,17]
HIC_human_AMR_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[21,10] <- inputs_tornado[21,18]
HIC_human_AMR_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

HIC_human_AMR_low <- min((HIC_human_AMR_low - HIC_base), (HIC_human_AMR_high - HIC_base))
HIC_human_AMR_high <- max((HIC_human_AMR_low - HIC_base), (HIC_human_AMR_high - HIC_base))

#discount rate
inputs_tornado <- inputs_general

inputs_tornado[3,3] <- inputs_tornado[3,17]
HIC_human_dr_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[3,3] <- inputs_tornado[3,18]
HIC_human_dr_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

HIC_human_dr_low <- max((HIC_human_dr_low - HIC_base), (HIC_human_dr_high - HIC_base))
HIC_human_dr_high <- min((HIC_human_dr_low - HIC_base), (HIC_human_dr_high - HIC_base))

#AMR growth
inputs_tornado <- inputs_general

inputs_tornado[38,3] <- inputs_tornado[38,17]
HIC_human_amrgro_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[38,3] <- inputs_tornado[38,18]
HIC_human_amrgro_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

HIC_human_amrgro_low <- min((HIC_human_amrgro_low - HIC_base), (HIC_human_amrgro_high - HIC_base))
HIC_human_amrgro_high <- max((HIC_human_amrgro_low - HIC_base), (HIC_human_amrgro_high - HIC_base))

#background AMR
inputs_tornado <- inputs_general

inputs_tornado[28,3] <- inputs_tornado[28,17]
HIC_human_amrbase_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[28,3] <- inputs_tornado[28,18]
HIC_human_amrbase_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

HIC_human_amrbase_low <- min((HIC_human_amrbase_low - HIC_base), (HIC_human_amrbase_high - HIC_base))
HIC_human_amrbase_high <- max((HIC_human_amrbase_low - HIC_base), (HIC_human_amrbase_high - HIC_base))

#WTP
inputs_tornado <- inputs_general

inputs_tornado[6,3] <- inputs_tornado[6,17]
HIC_human_wtp_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[6,3] <- inputs_tornado[6,18]
HIC_human_wtp_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

HIC_human_wtp_low <- min((HIC_human_wtp_low - HIC_base), (HIC_human_wtp_high - HIC_base))
HIC_human_wtp_high <- max((HIC_human_wtp_low - HIC_base), (HIC_human_wtp_high - HIC_base))

#LFPR
inputs_tornado <- inputs_general

inputs_tornado[10,3] <- inputs_tornado[10,17]
HIC_human_lfpr_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[10,3] <- inputs_tornado[10,18]
HIC_human_lfpr_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

HIC_human_lfpr_low <- min((HIC_human_lfpr_low - HIC_base), (HIC_human_lfpr_high - HIC_base))
HIC_human_lfpr_high <- max((HIC_human_lfpr_low - HIC_base), (HIC_human_lfpr_high - HIC_base))

#disease incidence
inputs_tornado <- inputs_general

inputs_tornado[27,3] <- inputs_tornado[27,17]
HIC_human_disease_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[27,3] <- inputs_tornado[27,18]
HIC_human_disease_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

HIC_human_disease_low <- min((HIC_human_disease_low - HIC_base), (HIC_human_disease_high - HIC_base))
HIC_human_disease_high <- max((HIC_human_disease_low - HIC_base), (HIC_human_disease_high - HIC_base))

#/#

#pig income effect
inputs_tornado <- inputs_general

inputs_tornado[15,11] <- inputs_tornado[15,17]
HIC_animal_piginc_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[15,11] <- inputs_tornado[15,18]
HIC_animal_piginc_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

HIC_animal_piginc_low <- min((HIC_animal_piginc_low - HIC_base), (HIC_animal_piginc_high - HIC_base))
HIC_animal_piginc_high <- max((HIC_animal_piginc_low - HIC_base), (HIC_animal_piginc_high - HIC_base))

#chicken income effect
inputs_tornado <- inputs_general

inputs_tornado[17,11] <- inputs_tornado[17,17]
HIC_animal_chickinc_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[17,11] <- inputs_tornado[17,18]
HIC_animal_chickinc_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

HIC_animal_chickinc_low <- min((HIC_animal_chickinc_low - HIC_base), (HIC_animal_chickinc_high - HIC_base))
HIC_animal_chickinc_high <- max((HIC_animal_chickinc_low - HIC_base), (HIC_animal_chickinc_high - HIC_base))

#farm size (pig - I)
inputs_tornado <- inputs_general

inputs_tornado[46,3] <- inputs_tornado[46,17]
HIC_animal_pigfarmi_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[46,3] <- inputs_tornado[46,18]
HIC_animal_pigfarmi_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

HIC_animal_pigfarmi_low <- min((HIC_animal_pigfarmi_low - HIC_base), (HIC_animal_pigfarmi_high - HIC_base))
HIC_animal_pigfarmi_high <- max((HIC_animal_pigfarmi_low - HIC_base), (HIC_animal_pigfarmi_high - HIC_base))

#farm size (pig - S)
inputs_tornado <- inputs_general

inputs_tornado[47,3] <- inputs_tornado[47,17]
HIC_animal_pigfarms_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[47,3] <- inputs_tornado[47,18]
HIC_animal_pigfarms_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

HIC_animal_pigfarms_low <- min((HIC_animal_pigfarms_low - HIC_base), (HIC_animal_pigfarms_high - HIC_base))
HIC_animal_pigfarms_high <- max((HIC_animal_pigfarms_low - HIC_base), (HIC_animal_pigfarms_high - HIC_base))

#farm size (chicken - I)
inputs_tornado <- inputs_general

inputs_tornado[44,3] <- inputs_tornado[44,17]
HIC_animal_chickfarmi_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[44,3] <- inputs_tornado[44,18]
HIC_animal_chickfarmi_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

HIC_animal_chickfarmi_low <- min((HIC_animal_chickfarmi_low - HIC_base), (HIC_animal_chickfarmi_high - HIC_base))
HIC_animal_chickfarmi_high <- max((HIC_animal_chickfarmi_low - HIC_base), (HIC_animal_chickfarmi_high - HIC_base))

#farm size (chicken - S)
inputs_tornado <- inputs_general

inputs_tornado[45,3] <- inputs_tornado[45,17]
HIC_animal_chickfarms_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[45,3] <- inputs_tornado[45,18]
HIC_animal_chickfarms_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

HIC_animal_chickfarms_low <- min((HIC_animal_chickfarms_low - HIC_base), (HIC_animal_chickfarms_high - HIC_base))
HIC_animal_chickfarms_high <- max((HIC_animal_chickfarms_low - HIC_base), (HIC_animal_chickfarms_high - HIC_base))

#number of pigs
inputs_tornado <- inputs_general

inputs_tornado[42,3] <- inputs_tornado[42,17]
HIC_animal_npigs_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[42,3] <- inputs_tornado[42,18]
HIC_animal_npigs_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

HIC_animal_npigs_low <- min((HIC_animal_npigs_low - HIC_base), (HIC_animal_npigs_high - HIC_base))
HIC_animal_npigs_high <- max((HIC_animal_npigs_low - HIC_base), (HIC_animal_npigs_high - HIC_base))

#number of chickens
inputs_tornado <- inputs_general

inputs_tornado[43,3] <- inputs_tornado[43,17]
HIC_animal_nchicks_low <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

inputs_tornado[43,3] <- inputs_tornado[43,18]
HIC_animal_nchicks_high <- as.numeric(Model(inputs_tornado, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

HIC_animal_nchicks_low <- min((HIC_animal_nchicks_low - HIC_base), (HIC_animal_nchicks_high - HIC_base))
HIC_animal_nchicks_high <- max((HIC_animal_nchicks_low - HIC_base), (HIC_animal_nchicks_high - HIC_base))

#Plot - HIC - Human

tornado_HIC_human <- data.frame(variable = c("Effect on Human AMR",
                                             "Discount Rate",
                                             "Growth Rate of AMR",
                                             "Baseline AMR prevalence",
                                             "Willingness to Pay per QALY",
                                             "Labour Force Participation Rate",
                                             "Infection Prevalence"),
                                min = c(HIC_human_AMR_low,
                                        HIC_human_dr_low,
                                        HIC_human_amrgro_low,
                                        HIC_human_amrbase_low,
                                        HIC_human_wtp_low,
                                        HIC_human_lfpr_low,
                                        HIC_human_disease_low),
                                max = c(HIC_human_AMR_high,
                                        HIC_human_dr_high,
                                        HIC_human_amrgro_high,
                                        HIC_human_amrbase_high,
                                        HIC_human_wtp_high,
                                        HIC_human_lfpr_high,
                                        HIC_human_disease_high))

jpeg("Outputs/Figure 3 D 1.jpg")

ggplot(tornado_HIC_human, aes(variable, ymin = min, ymax = max)) +
  geom_linerange(size = 10) +
  coord_flip() +
  xlab("") +
  ggtitle("Change in Maximum Acceptable Annual Cost along Range of Each Parameter")+
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw() +
  theme(axis.text = element_text(size = 15))

dev.off()

#Plot - HIC - Animal

tornado_HIC_animal <- data.frame(variable = c("Effect on Pig Productivity",
                                              "Effect on Chicken Productivity",
                                              "Size of Industrial Pig Farms",
                                              "Size of Smallholder Pig Farms",
                                              "Size of Industrial Chicken Farms",
                                              "Size of Smallholder Chicken Farms",
                                              "Number of Pigs",
                                              "Number of Chickens"),
                                 min = c(HIC_animal_piginc_low,
                                         HIC_animal_chickinc_low,
                                         HIC_animal_pigfarmi_low,
                                         HIC_animal_pigfarms_low,
                                         HIC_animal_chickfarmi_low,
                                         HIC_animal_chickfarms_low,
                                         HIC_animal_npigs_low,
                                         HIC_animal_nchicks_low),
                                 max = c(HIC_animal_piginc_high,
                                         HIC_animal_chickinc_high,
                                         HIC_animal_pigfarmi_high,
                                         HIC_animal_pigfarms_high,
                                         HIC_animal_chickfarmi_high,
                                         HIC_animal_chickfarms_high,
                                         HIC_animal_npigs_high,
                                         HIC_animal_nchicks_high))

jpeg("Outputs/Figure 3 D 2.jpg")

ggplot(tornado_HIC_animal, aes(variable, ymin = min, ymax = max)) +
  geom_linerange(size = 10) +
  coord_flip() +
  xlab("") +
  ggtitle("Change in Maximum Acceptable Annual Cost along Range of Each Parameter")+
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw() +
  theme(axis.text = element_text(size = 15))

dev.off()

#Plot - MICI - Human

tornado_MICI_human <- data.frame(variable = c("Effect on Human AMR",
                                              "Discount Rate",
                                              "Growth Rate of AMR",
                                              "Baseline AMR prevalence",
                                              "Willingness to Pay per QALY",
                                              "Labour Force Participation Rate",
                                              "Infection Prevalence"),
                                 min = c(MICI_human_AMR_low,
                                         MICI_human_dr_low,
                                         MICI_human_amrgro_low,
                                         MICI_human_amrbase_low,
                                         MICI_human_wtp_low,
                                         MICI_human_lfpr_low,
                                         MICI_human_disease_low),
                                 max = c(MICI_human_AMR_high,
                                         MICI_human_dr_high,
                                         MICI_human_amrgro_high,
                                         MICI_human_amrbase_high,
                                         MICI_human_wtp_high,
                                         MICI_human_lfpr_high,
                                         MICI_human_disease_high))

jpeg("Outputs/Figure 3 B 1.jpg")

ggplot(tornado_MICI_human, aes(variable, ymin = min, ymax = max)) +
  geom_linerange(size = 10) +
  coord_flip() +
  xlab("") +
  ggtitle("Change in Maximum Acceptable Annual Cost along Range of Each Parameter")+
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw() +
  theme(axis.text = element_text(size = 15))

dev.off()

#Plot - MICI - Animal

tornado_MICI_animal <- data.frame(variable = c("Effect on Pig Productivity",
                                               "Effect on Chicken Productivity",
                                               "Size of Industrial Pig Farms",
                                               "Size of Smallholder Pig Farms",
                                               "Size of Industrial Chicken Farms",
                                               "Size of Smallholder Chicken Farms",
                                               "Number of Pigs",
                                               "Number of Chickens"),
                                  min = c(MICI_animal_piginc_low,
                                          MICI_animal_chickinc_low,
                                          MICI_animal_pigfarmi_low,
                                          MICI_animal_pigfarms_low,
                                          MICI_animal_chickfarmi_low,
                                          MICI_animal_chickfarms_low,
                                          MICI_animal_npigs_low,
                                          MICI_animal_nchicks_low),
                                  max = c(MICI_animal_piginc_high,
                                          MICI_animal_chickinc_high,
                                          MICI_animal_pigfarmi_high,
                                          MICI_animal_pigfarms_high,
                                          MICI_animal_chickfarmi_high,
                                          MICI_animal_chickfarms_high,
                                          MICI_animal_npigs_high,
                                          MICI_animal_nchicks_high))

jpeg("Outputs/Figure 3 B 2.jpg")

ggplot(tornado_MICI_animal, aes(variable, ymin = min, ymax = max)) +
  geom_linerange(size = 10) +
  coord_flip() +
  xlab("") +
  ggtitle("Change in Maximum Acceptable Annual Cost along Range of Each Parameter")+
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw() +
  theme(axis.text = element_text(size = 15))

dev.off()

#Plot - MICS - Human

tornado_MICS_human <- data.frame(variable = c("Effect on Human AMR",
                                              "Discount Rate",
                                              "Growth Rate of AMR",
                                              "Baseline AMR prevalence",
                                              "Willingness to Pay per QALY",
                                              "Labour Force Participation Rate",
                                              "Infection Prevalence"),
                                 min = c(MICS_human_AMR_low,
                                         MICS_human_dr_low,
                                         MICS_human_amrgro_low,
                                         MICS_human_amrbase_low,
                                         MICS_human_wtp_low,
                                         MICS_human_lfpr_low,
                                         MICS_human_disease_low),
                                 max = c(MICS_human_AMR_high,
                                         MICS_human_dr_high,
                                         MICS_human_amrgro_high,
                                         MICS_human_amrbase_high,
                                         MICS_human_wtp_high,
                                         MICS_human_lfpr_high,
                                         MICS_human_disease_high))

jpeg("Outputs/Figure 3 C 1.jpg")

ggplot(tornado_MICS_human, aes(variable, ymin = min, ymax = max)) +
  geom_linerange(size = 10) +
  coord_flip() +
  xlab("") +
  ggtitle("Change in Maximum Acceptable Annual Cost along Range of Each Parameter")+
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw() +
  theme(axis.text = element_text(size = 15))

dev.off()

#Plot - MICS - Animal

tornado_MICS_animal <- data.frame(variable = c("Effect on Pig Productivity",
                                               "Effect on Chicken Productivity",
                                               "Size of Industrial Pig Farms",
                                               "Size of Smallholder Pig Farms",
                                               "Size of Industrial Chicken Farms",
                                               "Size of Smallholder Chicken Farms",
                                               "Number of Pigs",
                                               "Number of Chickens"),
                                  min = c(MICS_animal_piginc_low,
                                          MICS_animal_chickinc_low,
                                          MICS_animal_pigfarmi_low,
                                          MICS_animal_pigfarms_low,
                                          MICS_animal_chickfarmi_low,
                                          MICS_animal_chickfarms_low,
                                          MICS_animal_npigs_low,
                                          MICS_animal_nchicks_low),
                                  max = c(MICS_animal_piginc_high,
                                          MICS_animal_chickinc_high,
                                          MICS_animal_pigfarmi_high,
                                          MICS_animal_pigfarms_high,
                                          MICS_animal_chickfarmi_high,
                                          MICS_animal_chickfarms_high,
                                          MICS_animal_npigs_high,
                                          MICS_animal_nchicks_high))

jpeg("Outputs/Figure 3 C 2.jpg")

ggplot(tornado_MICS_animal, aes(variable, ymin = min, ymax = max)) +
  geom_linerange(size = 10) +
  coord_flip() +
  xlab("") +
  ggtitle("Change in Maximum Acceptable Annual Cost along Range of Each Parameter")+
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw() +
  theme(axis.text = element_text(size = 15))

dev.off()

#Plot - LIC - Human

tornado_LIC_human <- data.frame(variable = c("Effect on Human AMR",
                                             "Discount Rate",
                                             "Growth Rate of AMR",
                                             "Baseline AMR prevalence",
                                             "Willingness to Pay per QALY",
                                             "Labour Force Participation Rate",
                                             "Infection Prevalence"),
                                min = c(LIC_human_AMR_low,
                                        LIC_human_dr_low,
                                        LIC_human_amrgro_low,
                                        LIC_human_amrbase_low,
                                        LIC_human_wtp_low,
                                        LIC_human_lfpr_low,
                                        LIC_human_disease_low),
                                max = c(LIC_human_AMR_high,
                                        LIC_human_dr_high,
                                        LIC_human_amrgro_high,
                                        LIC_human_amrbase_high,
                                        LIC_human_wtp_high,
                                        LIC_human_lfpr_high,
                                        LIC_human_disease_high))

jpeg("Outputs/Figure 3 A 1.jpg")

ggplot(tornado_LIC_human, aes(variable, ymin = min, ymax = max)) +
  geom_linerange(size = 10) +
  coord_flip() +
  xlab("") +
  ggtitle("Change in Maximum Acceptable Annual Cost along Range of Each Parameter")+
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw() +
  theme(axis.text = element_text(size = 15))

dev.off()

#Plot - LIC - Animal

tornado_LIC_animal <- data.frame(variable = c("Effect on Pig Productivity",
                                              "Effect on Chicken Productivity",
                                              "Size of Industrial Pig Farms",
                                              "Size of Smallholder Pig Farms",
                                              "Size of Industrial Chicken Farms",
                                              "Size of Smallholder Chicken Farms",
                                              "Number of Pigs",
                                              "Number of Chickens"),
                                 min = c(LIC_animal_piginc_low,
                                         LIC_animal_chickinc_low,
                                         LIC_animal_pigfarmi_low,
                                         LIC_animal_pigfarms_low,
                                         LIC_animal_chickfarmi_low,
                                         LIC_animal_chickfarms_low,
                                         LIC_animal_npigs_low,
                                         LIC_animal_nchicks_low),
                                 max = c(LIC_animal_piginc_high,
                                         LIC_animal_chickinc_high,
                                         LIC_animal_pigfarmi_high,
                                         LIC_animal_pigfarms_high,
                                         LIC_animal_chickfarmi_high,
                                         LIC_animal_chickfarms_high,
                                         LIC_animal_npigs_high,
                                         LIC_animal_nchicks_high))

jpeg("Outputs/Figure 3 A 2.jpg")

ggplot(tornado_LIC_animal, aes(variable, ymin = min, ymax = max)) +
  geom_linerange(size = 10) +
  coord_flip() +
  xlab("") +
  ggtitle("Change in Maximum Acceptable Annual Cost along Range of Each Parameter")+
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw() +
  theme(axis.text = element_text(size = 15))

dev.off()

# Table 2 - Global Sensitivity Analysis -----------------------------------

scenario_prod <- "HCA" 
scenario_transmission <- "med"

###LIC

scenario_income <- "LIC"

LIC_reg_matrix <- matrix(rep(0), nrow = number_runs, ncol = nrow(inputs_general)+1)

colnames(LIC_reg_matrix) <- c("Output", rep(0, nrow(inputs_general)))

for(i in 2:ncol(LIC_reg_matrix)){
  colnames(LIC_reg_matrix)[i] <- inputs_general[i-1,1]
}

LIC_reg_matrix <- as.data.frame(LIC_reg_matrix)

inputs_reg_LIC <- read.csv(here("inputs - general model.csv"))
inputs_reg_LIC <- as.data.table(inputs_reg_LIC)
colnames(inputs_reg_LIC) <- c("parameter", "description", "HIC", "MIC-I", "MIC-S", "LIC", "Value", "Min", "Lo", "Med", "Hi", "Max",
                              "LIC min", "LIC max", "MIC min", "MIC max", "HIC min", "HIC max")


set.seed(42069)

for(i in 1:number_runs){
  inputsreg <- inputs_reg_LIC
  
  for(j in c(3,5:12,15,17,19:20, 27:38, 42:60)){
    
    inputsreg[j,6] <- runif(1,as.numeric(inputsreg[j,13]),as.numeric(inputsreg[j,14]))
    LIC_reg_matrix[i,j+1] <- inputsreg[j,6]
    
  }
  
  inputsreg[21,10] <- runif(1, as.numeric(inputsreg[21,14]), as.numeric(inputsreg[21,13]))
  LIC_reg_matrix[i,22] <- inputsreg[21,10]
  
  LIC_reg_matrix[i,1] <- as.data.frame(Model(inputsreg, scenario_income, scenario_prod, scenario_transmission, scenario_farm_effect))[1,1]
  
}

write.xlsx(LIC_reg_matrix, "Outputs/reg matrix LIC.xlsx")

### MICI

scenario_income <- "MIC-I"

MICI_reg_matrix <- matrix(rep(0), nrow = number_runs, ncol = nrow(inputs_general)+1)

colnames(MICI_reg_matrix) <- c("Output", rep(0, nrow(inputs_general)))

for(i in 2:ncol(MICI_reg_matrix)){
  colnames(MICI_reg_matrix)[i] <- inputs_general[i-1,1]
}

MICI_reg_matrix <- as.data.frame(MICI_reg_matrix)

inputs_reg_MICI <- read.csv(here("inputs - general model.csv"))
inputs_reg_MICI <- as.data.table(inputs_reg_MICI)
colnames(inputs_reg_MICI) <- c("parameter", "description", "HIC", "MIC-I", "MIC-S", "LIC", "Value", "Min", "Lo", "Med", "Hi", "Max",
                               "LIC min", "LIC max", "MIC min", "MIC max", "HIC min", "HIC max")


set.seed(42069)

for(i in 1:number_runs){
  inputsreg <- inputs_reg_MICI
  
  for(j in c(3,5:12,15,17,19:20, 27:38, 42:60)){
    
    inputsreg[j,6] <- runif(1,as.numeric(inputsreg[j,13]),as.numeric(inputsreg[j,14]))
    MICI_reg_matrix[i,j+1] <- inputsreg[j,6]
    
  }
  
  inputsreg[21,10] <- runif(1, as.numeric(inputsreg[21,14]), as.numeric(inputsreg[21,13]))
  MICI_reg_matrix[i,22] <- inputsreg[21,10]
  
  MICI_reg_matrix[i,1] <- as.data.frame(Model(inputsreg, scenario_income, scenario_prod, scenario_transmission, scenario_farm_effect))[1,1]
  
}

write.xlsx(MICI_reg_matrix, "Outputs/reg matrix MICI.xlsx")

### MICS

scenario_income <- "MIC-S"

MICS_reg_matrix <- matrix(rep(0), nrow = number_runs, ncol = nrow(inputs_general)+1)

colnames(MICS_reg_matrix) <- c("Output", rep(0, nrow(inputs_general)))

for(i in 2:ncol(MICS_reg_matrix)){
  colnames(MICS_reg_matrix)[i] <- inputs_general[i-1,1]
}

MICS_reg_matrix <- as.data.frame(MICS_reg_matrix)

inputs_reg_MICS <- read.csv(here("inputs - general model.csv"))
inputs_reg_MICS <- as.data.table(inputs_reg_MICS)
colnames(inputs_reg_MICS) <- c("parameter", "description", "HIC", "MIC-I", "MIC-S", "LIC", "Value", "Min", "Lo", "Med", "Hi", "Max",
                               "LIC min", "LIC max", "MIC min", "MIC max", "HIC min", "HIC max")


set.seed(42069)

for(i in 1:number_runs){
  inputsreg <- inputs_reg_MICS
  
  for(j in c(3,5:12,15,17,19:20, 27:38, 42:60)){
    
    inputsreg[j,6] <- runif(1,as.numeric(inputsreg[j,13]),as.numeric(inputsreg[j,14]))
    MICS_reg_matrix[i,j+1] <- inputsreg[j,6]
    
  }
  
  inputsreg[21,10] <- runif(1, as.numeric(inputsreg[21,14]), as.numeric(inputsreg[21,13]))
  MICS_reg_matrix[i,22] <- inputsreg[21,10]
  
  MICS_reg_matrix[i,1] <- as.data.frame(Model(inputsreg, scenario_income, scenario_prod, scenario_transmission, scenario_farm_effect))[1,1]
  
}

write.xlsx(MICS_reg_matrix, "Outputs/reg matrix MICS.xlsx")

###HIC

scenario_income <- "HIC"

HIC_reg_matrix <- matrix(rep(0), nrow = number_runs, ncol = nrow(inputs_general)+1)

colnames(HIC_reg_matrix) <- c("Output", rep(0, nrow(inputs_general)))

for(i in 2:ncol(HIC_reg_matrix)){
  colnames(HIC_reg_matrix)[i] <- inputs_general[i-1,1]
}

HIC_reg_matrix <- as.data.frame(HIC_reg_matrix)

inputs_reg_HIC <- read.csv(here("inputs - general model.csv"))
inputs_reg_HIC <- as.data.table(inputs_reg_HIC)
colnames(inputs_reg_HIC) <- c("parameter", "description", "HIC", "MIC-I", "MIC-S", "LIC", "Value", "Min", "Lo", "Med", "Hi", "Max",
                              "LIC min", "LIC max", "MIC min", "MIC max", "HIC min", "HIC max")


set.seed(42069)

for(i in 1:number_runs){
  inputsreg <- inputs_reg_HIC
  
  for(j in c(3,5:12,15,17,19:20, 27:38, 42:60)){
    
    inputsreg[j,6] <- runif(1,as.numeric(inputsreg[j,13]),as.numeric(inputsreg[j,14]))
    HIC_reg_matrix[i,j+1] <- inputsreg[j,6]
    
  }
  
  inputsreg[21,10] <- runif(1, as.numeric(inputsreg[21,14]), as.numeric(inputsreg[21,13]))
  HIC_reg_matrix[i,22] <- inputsreg[21,10]
  
  HIC_reg_matrix[i,1] <- as.data.frame(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission, scenario_farm_effect))[1,1]
  
}

write.xlsx(HIC_reg_matrix, "Outputs/reg matrix HIC.xlsx")

##regressions 

#'Human parameters to include in the plots: human AMR effect, discount rate, 
#'AMR growth, WTP, LFPR, background sickness, background AMR

#'Animal parameters to include: pig income effect, chicken income effect, 
#'farm size (x4), number of pigs, number of chickens

###HIC

HIC_reg <- lm(Output ~ res_change + dr + amr_grow + wtp +
                lfpr + well_sick + portion_res + 
                pig_income_effect + chicken_income_effect +
                n_chickens_farm_ind + n_chickens_farm_small +
                n_pigs_farm_ind + n_pigs_farm_small + 
                n_pigs + n_chickens, data = HIC_reg_matrix)

stargazer(HIC_reg, type = "text") 
#' only the LFPR and the effect on human AMR
#' significant after controlling for everything

rsq.partial(HIC_reg) #effect on human AMR dominates

HIC_reg_constrained <- lm(Output ~ dr + amr_grow + wtp +
                            lfpr + well_sick + portion_res + 
                            pig_income_effect + chicken_income_effect +
                            n_chickens_farm_ind + n_chickens_farm_small +
                            n_pigs_farm_ind + n_pigs_farm_small + 
                            n_pigs + n_chickens, data = HIC_reg_matrix)

stargazer(HIC_reg_constrained, type = "text") 
#'dr and LFPR significant

rsq.partial(HIC_reg_constrained)

HIC_prsq <- rsq.partial(HIC_reg_constrained)

HIC_prsq_var <- HIC_prsq$variable
HIC_prsq_val <- HIC_prsq$partial.rsq

HIC_prsq <- as.data.frame(cbind(HIC_prsq_var, HIC_prsq_val))

jpeg("Outputs/Table 2 A.jpg")

HIC_prsq %>%
  mutate(HIC_prsq_var = fct_reorder(HIC_prsq_var, HIC_prsq_val)) %>%
  ggplot( aes(x = HIC_prsq_var, y = HIC_prsq_val)) +
  geom_bar(stat = "identity", fill="#f68060", alpha=.6, width=.4) +
  xlab("Parameter") +
  ylab("Partial R^2")+
  ggtitle("Portion of Variation in Output Caused by Each Parameter (Excluding
          the Effect on Human AMR) - HIC")

dev.off()

###MIC-I

MICI_reg <- lm(Output ~ res_change + dr + amr_grow + wtp +
                 lfpr + well_sick + portion_res + 
                 pig_income_effect + chicken_income_effect +
                 n_chickens_farm_ind + n_chickens_farm_small +
                 n_pigs_farm_ind + n_pigs_farm_small + 
                 n_pigs + n_chickens, data = MICI_reg_matrix)

stargazer(MICI_reg, type = "text") 
#'again, only lfpr and the effect on human AMR

rsq.partial(MICI_reg) #again, effect on human AMR dominates

MICI_reg_constrained <- lm(Output ~ dr + amr_grow + wtp +
                             lfpr + well_sick + portion_res + 
                             pig_income_effect + chicken_income_effect +
                             n_chickens_farm_ind + n_chickens_farm_small +
                             n_pigs_farm_ind + n_pigs_farm_small + 
                             n_pigs + n_chickens, data = MICI_reg_matrix)

stargazer(MICI_reg_constrained, type = "text") 
#'again, dr and LFPR significant

rsq.partial(MICI_reg_constrained)

MICI_prsq <- rsq.partial(MICI_reg_constrained)

MICI_prsq_var <- MICI_prsq$variable
MICI_prsq_val <- MICI_prsq$partial.rsq

MICI_prsq <- as.data.frame(cbind(MICI_prsq_var, MICI_prsq_val))

jpeg("Outputs/Table 2 B.jpg")

MICI_prsq %>%
  mutate(MICI_prsq_var = fct_reorder(MICI_prsq_var, MICI_prsq_val)) %>%
  ggplot( aes(x = MICI_prsq_var, y = MICI_prsq_val)) +
  geom_bar(stat = "identity", fill="#f68060", alpha=.6, width=.4) +
  xlab("Parameter") +
  ylab("Partial R^2")+
  ggtitle("Portion of Variation in Output Caused by Each Parameter (Excluding
          the Effect on Human AMR) - MIC-I")

dev.off()

###MIC-S

MICS_reg <- lm(Output ~ res_change + dr + amr_grow + wtp +
                 lfpr + well_sick + portion_res + 
                 pig_income_effect + chicken_income_effect +
                 n_chickens_farm_ind + n_chickens_farm_small +
                 n_pigs_farm_ind + n_pigs_farm_small + 
                 n_pigs + n_chickens, data = MICS_reg_matrix)

stargazer(MICS_reg, type = "text") 
#'again, only lfpr and the effect on human AMR

rsq.partial(MICS_reg) #again, effect on human AMR dominates

MICS_reg_constrained <- lm(Output ~ dr + amr_grow + wtp +
                             lfpr + well_sick + portion_res + 
                             pig_income_effect + chicken_income_effect +
                             n_chickens_farm_ind + n_chickens_farm_small +
                             n_pigs_farm_ind + n_pigs_farm_small + 
                             n_pigs + n_chickens, data = MICS_reg_matrix)

stargazer(MICS_reg_constrained, type = "text") 
#'again, dr and LFPR significant

rsq.partial(MICS_reg_constrained)

MICS_prsq <- rsq.partial(MICS_reg_constrained)

MICS_prsq_var <- MICS_prsq$variable
MICS_prsq_val <- MICS_prsq$partial.rsq

MICS_prsq <- as.data.frame(cbind(MICS_prsq_var, MICS_prsq_val))

jpeg("Outputs/Table 2 C.jpg")

MICS_prsq %>%
  mutate(MICS_prsq_var = fct_reorder(MICS_prsq_var, MICS_prsq_val)) %>%
  ggplot( aes(x = MICS_prsq_var, y = MICS_prsq_val)) +
  geom_bar(stat = "identity", fill="#f68060", alpha=.6, width=.4) +
  xlab("Parameter") +
  ylab("Partial R^2")+
  ggtitle("Portion of Variation in Output Caused by Each Parameter (Excluding
          the Effect on Human AMR) - MIC-S")

dev.off()

###LIC

LIC_reg <- lm(Output ~ res_change + dr + amr_grow + wtp +
                lfpr + well_sick + portion_res + 
                pig_income_effect + chicken_income_effect +
                n_chickens_farm_ind + n_chickens_farm_small +
                n_pigs_farm_ind + n_pigs_farm_small + 
                n_pigs + n_chickens, data = LIC_reg_matrix)

stargazer(LIC_reg, type = "text") 
#'effect on human AMR, the discount rate, the AMR growth rate, the LFPR,
#'the chance of illness all significant

rsq.partial(LIC_reg) #effect on human AMR does not dominate

# LIC_reg_constrained <- lm(Output ~ dr + amr_grow + wtp +
#                             lfpr + well_sick + portion_res + 
#                             pig_income_effect + chicken_income_effect +
#                             n_chickens_farm_ind + n_chickens_farm_small +
#                             n_pigs_farm_ind + n_pigs_farm_small + 
#                             n_pigs + n_chickens, data = LIC_reg_matrix)

#' stargazer(LIC_reg_constrained, type = "text") 
#' #'again, dr and LFPR significant

LIC_prsq <- rsq.partial(LIC_reg)

LIC_prsq_var <- LIC_prsq$variable
LIC_prsq_val <- LIC_prsq$partial.rsq

LIC_prsq <- as.data.frame(cbind(LIC_prsq_var, LIC_prsq_val))

jpeg("Outputs/Table 2 D.jpg")

LIC_prsq %>%
  mutate(LIC_prsq_var = fct_reorder(LIC_prsq_var, LIC_prsq_val)) %>%
  ggplot( aes(x = LIC_prsq_var, y = LIC_prsq_val)) +
  geom_bar(stat = "identity", fill="#f68060", alpha=.6, width=.4) +
  xlab("Parameter") +
  ylab("Partial R^2")+
  ggtitle("Portion of Variation in Output Caused by Each Parameter (Including
          the Effect on Human AMR) - LIC")

dev.off()

# Table 3 - Cost Borne by Each Sector -------------------------------------

scenario_prod      <- "HCA"  
scenario_farm_effect <- "hi"
scenario_transmission <- "med"

table_3 <- matrix(rep(0), nrow = 4, ncol = 9)
colnames(table_3) <- c("Income Group", 
                       "Maximum Acceptable Cost (Annual)",
                       "Value of Productivity Gained",
                       "Cost Saved for Healthcare",
                       "Value of DALYs Averted",
                       "Increased Profit - Smallholder Pig Farms",
                       "Increased Profit - Industrial Pig Farms",
                       "Increased Profit - Smallholder Chicken Farms",
                       "Increased Profit - Industrial Chicken Farms")

table_3[1,1] <- "LIC"
table_3[2,1] <- "MIC (Smallholder)"
table_3[3,1] <- "MIC (Industrial)"
table_3[4,1] <- "HIC"

scenario_income <- "LIC"
table_3[1,2:9] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect))

scenario_income <- "MIC-S"
table_3[2,2:9] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect))

scenario_income <- "MIC-I"
table_3[3,2:9] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect))

scenario_income <- "HIC"
table_3[4,2:9] <- as.numeric(Model(inputs_general, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect))

write.xlsx(table_3, "Outputs/Table 3.xlsx")



# Table 4 - NMB under different scenarios (case study) --------------------

scenario_income <- "Viet Nam"
scenario_prod <- "HCA"
scenario_amr_grow <- "med"
scenario_intervention_level <- "Village"
scenario_outcomes <- "All"
scenario_farm_effect <- "med"
intervention_followup_period <- 2
scenario_transmission <- "med"

table_4 <- matrix(rep(0), nrow = 4, ncol = 2)
colnames(table_4) <- c("Effect on Human AMR", "Macro-Level Net Monetary Benefit ($USD)")
table_4[,1] <- c(-0.025, -0.05, -0.10, -0.16)

scenario_transmission <- "lo"
table_4[1,2] <- as.numeric(Model_Case_Study(inputs_casestudy, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

scenario_transmission <- "med" 
table_4[2,2] <- as.numeric(Model_Case_Study(inputs_casestudy, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

scenario_transmission <- "hi" 
table_4[3,2] <- as.numeric(Model_Case_Study(inputs_casestudy, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

scenario_transmission <- "max"
table_4[4,2] <- as.numeric(Model_Case_Study(inputs_casestudy, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

write.xlsx(table_4, "Outputs/Table 4.xlsx")



# Figure 4 - composition of NMB in default scenario (case study) ----------

scenario <- "HCA"
scenario_amr_grow <- "med"
scenario_intervention_level <- "Village"
scenario_outcomes <- "All"
intervention_followup_period <- 2
scenario_transmission <- "med"

counts <- rep(0,5)

counts[1] <- as.numeric(Model_Case_Study(inputs_casestudy, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,3])
counts[2] <- as.numeric(Model_Case_Study(inputs_casestudy, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,4])
counts[3] <- as.numeric(Model_Case_Study(inputs_casestudy, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,5])
counts[4] <- as.numeric(Model_Case_Study(inputs_casestudy, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,6])
counts[5] <- as.numeric(-1 * Model_Case_Study(inputs_casestudy, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,7])

counts <- counts / 1000000000

names <- c("Healthcare (incl. QALYs saved", "Productivity", "Poultry Sector",
           "Pig Sector", "Implementation Cost")


fig4_matrix <- as.data.frame(cbind(names, counts))

jpeg("Outputs/Figure 4.jpg")

fig4_matrix %>%
  mutate(names = fct_reorder(names, counts)) %>%
  ggplot(aes(x = names, y = counts)) +
  geom_bar(position = "identity", stat = "identity", fill="#f68060", alpha=.6, width=.4) +
  ggtitle("Contribution to Overall Net Monetary Benefit in Default Scenario, bn $USD")

dev.off()