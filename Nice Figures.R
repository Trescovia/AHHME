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
library("patchwork")

# Data files --------------------------------------------------------------

main_dir <- here()
sub_dir <- "Outputs"
ifelse(!dir.exists(file.path(main_dir, sub_dir)), dir.create(file.path(main_dir, sub_dir)), F)
rm("main_dir", "sub_dir")

inputs_general <- read.csv(here("inputs - general model.csv"))
inputs_general <- as.data.table(inputs_general)
colnames(inputs_general) <- c("parameter", "description", "HIC", "MIC-I", "MIC-S", "LIC", "Value", "Min", "Lo", "Med", "Hi", "Max", 
                      "LIC min", "LIC max", "MIC min", "MIC max", "HIC min", "HIC max",
                      "Distribution", "LIC param 1", "LIC param 2", "MIC param 1", "MIC param 2",
                      "HIC param 1", "HIC param 2")

pop <- read.csv(here("Vietnam Population.csv"))

dependency <- read.csv(here("Viet Nam dependency ratio.csv"))

inputs_casestudy <- read.csv(here("inputs - case study.csv"))
inputs_casestudy <- as.data.table(inputs_casestudy)
colnames(inputs_casestudy) <- c("parameter", "description", "Viet Nam", "Value", "Min", "Lo", "Med", "Hi", "Max")

source("Functions.R")


# Default scenario --------------------------------------------------------

scenario_income <- "LIC"
scenario_prod <- "HCA"
scenario_transmission <- "med"
scenario_farm_effect <- "hi"

number_runs <- 10000

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

## ggplot Table A outputs
tableA <- as.data.frame(rbind(scenario_analysis_LIC, scenario_analysis_MIC_I, scenario_analysis_MIC_S, scenario_analysis_HIC))
colnames(tableA) <- c(-2,-1,0,1,2)
tableA$income_group <- c(rep(c("LIC","MIC_I","MIC_S","HIC"),each = nrow(scenario_analysis_LIC)))
tableA$human_amr <- c(-2.5,-5,-10,-16)
rownames(tableA) <- NULL
tableA <- tableA %>% pivot_longer(cols = seq(-2:2), names_to = "animal_prod")
tableA$animal_prod <- factor(tableA$animal_prod,levels = c(-2,-1,0,1,2) )
tableA <- tableA %>% mutate(default = ifelse(human_amr == -5.0 & animal_prod == "1",1,""))

tb1 <-  ggplot(tableA, aes(x=factor(human_amr), y = value, fill = animal_prod)) + geom_bar(position="dodge", stat="identity", aes(colour = factor(default))) + 
  facet_wrap(~income_group, scales = "free") + 
  scale_x_discrete("Impact of intervention on prevalence of AMR in human sepsis infections") + 
  scale_y_continuous("Maximum Annual Cost (2020 $USD)") + 
  scale_fill_manual(values = c("#d7191c","#fdae61","#ffffbf","#abd9e9","#2c7bb6"), "Impact of\nintervention on\nanimal production") + 
  scale_colour_manual(values = c("black","red")) + guides(colour = "none")
ggsave("Outputs/table1.pdf")

write.xlsx(scenario_analysis_LIC, "Outputs/Table 1 A.xlsx")
write.xlsx(scenario_analysis_MIC_I, "Outputs/Table 1 B.xlsx")
write.xlsx(scenario_analysis_MIC_S, "Outputs/Table 1 C.xlsx")
write.xlsx(scenario_analysis_HIC, "Outputs/Table 1 D.xlsx")


# Figure 2 - Distribution of Results from Montecarlo Simulation -----------

scenario_prod <- "HCA" 
scenario_transmission <- "med"
scenrio_farm_effect <- "med"

###LIC

scenario_income <- "LIC"

LIC_MC_Vector <- rep(0, number_runs)

inputs_LIC <- read.csv(here("inputs - general model.csv"))
inputs_LIC <- as.data.table(inputs_LIC)
colnames(inputs_LIC) <- c("parameter", "description", "HIC", "MIC-I", "MIC-S", "LIC", "Value", "Min", "Lo", "Med", "Hi", "Max", 
                          "LIC min", "LIC max", "MIC min", "MIC max", "HIC min", "HIC max",
                          "Distribution", "LIC param 1", "LIC param 2", "MIC param 1", "MIC param 2",
                          "HIC param 1", "HIC param 2")

set.seed(42069)

for(i in 1:number_runs){
  print(i)
  #load dataset
  inputsPSA <- inputs_LIC
  
  inputsPSA[parameter == "well_sick", "LIC"] <- rbeta(1, as.numeric(inputsPSA[parameter == "well_sick", "LIC param 1"]), 
                                                           as.numeric(inputsPSA[parameter == "well_sick", "LIC param 2"]))
  
  inputsPSA[parameter == "portion_res", "LIC"] <- rbeta(1, as.numeric(inputsPSA[parameter == "portion_res", "LIC param 1"]), 
                                                           as.numeric(inputsPSA[parameter == "portion_res", "LIC param 2"]))
  
  inputsPSA[parameter == "mort_res", "LIC"] <- 1.62 * rbeta(1,as.numeric(inputsPSA[parameter == "mort_res", "LIC param 1"]), 
                                                           as.numeric(inputsPSA[parameter == "mort_res", "LIC param 2"]))
  
  inputsPSA[parameter == "mort_sus", "LIC"] <- rbeta(1, as.numeric(inputsPSA[parameter == "mort_sus", "LIC param 1"]), 
                                                           as.numeric(inputsPSA[parameter == "mort_sus", "LIC param 2"]))
  
  inputsPSA[parameter == "los_sus", "LIC"] <- (1/365.25) * rlnorm(1, as.numeric(inputsPSA[parameter == "los_sus", "LIC param 1"]), 
                                                           as.numeric(inputsPSA[parameter == "los_sus", "LIC param 2"]))
  
  inputsPSA[parameter == "los_res", "LIC"] <- (1.27/365.25) * rlnorm(1, as.numeric(inputsPSA[parameter == "los_res", "LIC param 1"]), 
                                                           as.numeric(inputsPSA[parameter == "los_res", "LIC param 2"]))
  
  inputsPSA[parameter == "bed_day_cost", "LIC"] <- rlnorm(1, as.numeric(inputsPSA[parameter == "bed_day_cost", "LIC param 1"]), 
                                                           as.numeric(inputsPSA[parameter == "bed_day_cost", "LIC param 2"]))
  
  inputsPSA[parameter == "qol_sick", "LIC"] <- rbeta(1, as.numeric(inputsPSA[parameter == "qol_sick", "LIC param 1"]), 
                                                           as.numeric(inputsPSA[parameter == "qol_sick", "LIC param 2"]))
  
  inputsPSA[parameter == "qol_seq", "LIC"] <- rbeta(1, as.numeric(inputsPSA[parameter == "qol_seq", "LIC param 1"]), 
                                                           as.numeric(inputsPSA[parameter == "qol_seq", "LIC param 2"]))
  
  inputsPSA[parameter == "amr_grow", "LIC"] <- 0.01 * rgamma(1, as.numeric(inputsPSA[parameter == "amr_grow", "LIC param 1"]), 
                                                           scale = as.numeric(inputsPSA[parameter == "amr_grow", "LIC param 2"]))
  
  inputsPSA[parameter == "n_pigs", "LIC"] <- rlnorm(1, as.numeric(inputsPSA[parameter == "n_pigs", "LIC param 1"]), 
                                                           as.numeric(inputsPSA[parameter == "n_pigs", "LIC param 2"]))
  
  inputsPSA[parameter == "n_chickens", "LIC"] <- rlnorm(1, as.numeric(inputsPSA[parameter == "n_chickens", "LIC param 1"]), 
                                                           as.numeric(inputsPSA[parameter == "n_chickens", "LIC param 2"]))
  
  inputsPSA[parameter == "n_chickens_farm_ind", "LIC"] <- rlnorm(1, as.numeric(inputsPSA[parameter == "n_chickens_farm_ind", "LIC param 1"]), 
                                                           as.numeric(inputsPSA[parameter == "n_chickens_farm_ind", "LIC param 2"]))
  
  inputsPSA[parameter == "n_chickens_farm_small", "LIC"] <- rlnorm(1, as.numeric(inputsPSA[parameter == "n_chickens_farm_small", "LIC param 1"]), 
                                                           as.numeric(inputsPSA[parameter == "n_chickens_farm_small", "LIC param 2"]))
  
  inputsPSA[parameter == "n_pigs_farm_ind", "LIC"] <- rlnorm(1, as.numeric(inputsPSA[parameter == "n_pigs_farm_ind", "LIC param 1"]), 
                                                           as.numeric(inputsPSA[parameter == "n_pigs_farm_ind", "LIC param 2"]))
  
  inputsPSA[parameter == "n_pigs_farm_small", "LIC"] <- rlnorm(1, as.numeric(inputsPSA[parameter == "n_pigs_farm_small", "LIC param 1"]), 
                                                           as.numeric(inputsPSA[parameter == "n_pigs_farm_small", "LIC param 2"]))
  
  inputsPSA[parameter == "portion_animals_ind", "LIC"] <- rbeta(1, as.numeric(inputsPSA[parameter == "portion_animals_ind", "LIC param 1"]), 
                                                           as.numeric(inputsPSA[parameter == "portion_animals_ind", "LIC param 2"]))
  
  inputsPSA[parameter == "pig_price", "LIC"] <- rlnorm(1, as.numeric(inputsPSA[parameter == "pig_price", "LIC param 1"]), 
                                                           as.numeric(inputsPSA[parameter == "pig_price", "LIC param 2"]))
  
  inputsPSA[parameter == "chicken_price", "LIC"] <- rlnorm(1, as.numeric(inputsPSA[parameter == "chicken_price", "LIC param 1"]), 
                                                           as.numeric(inputsPSA[parameter == "chicken_price", "LIC param 2"]))
  
  inputsPSA[parameter == "c_mort_ind", "LIC"] <- rbeta(1, as.numeric(inputsPSA[parameter == "c_mort_ind", "LIC param 1"]), 
                                                           as.numeric(inputsPSA[parameter == "c_mort_ind", "LIC param 2"]))
  
  inputsPSA[parameter == "c_mort_small", "LIC"] <- rbeta(1, as.numeric(inputsPSA[parameter == "c_mort_small", "LIC param 1"]), 
                                                           as.numeric(inputsPSA[parameter == "c_mort_small", "LIC param 2"]))
  
  inputsPSA[parameter == "p_mort_ind", "LIC"] <- rbeta(1, as.numeric(inputsPSA[parameter == "p_mort_ind", "LIC param 1"]), 
                                                           as.numeric(inputsPSA[parameter == "p_mort_ind", "LIC param 2"]))
  
  inputsPSA[parameter == "p_mort_small", "LIC"] <- rbeta(1, as.numeric(inputsPSA[parameter == "p_mort_small", "LIC param 1"]), 
                                                           as.numeric(inputsPSA[parameter == "p_mort_small", "LIC param 2"]))
  
  inputsPSA[parameter == "res_change", "Med"] <- -1 * rbeta(1, as.numeric(inputsPSA[parameter == "res_change", "LIC param 1"]), 
                                                            as.numeric(inputsPSA[parameter == "res_change", "LIC param 2"]))
  
  inputsPSA[parameter == "seq_sus", "LIC"] <- 2.3791717 * rbeta(1, as.numeric(inputsPSA[parameter == "seq_sus", "LIC param 1"]), 
                                                            as.numeric(inputsPSA[parameter == "seq_sus", "LIC param 2"]))
  
  inputsPSA[parameter == "seq_res", "LIC"] <- 3.85425815 * rbeta(1, as.numeric(inputsPSA[parameter == "seq_res", "LIC param 1"]), 
                                                              as.numeric(inputsPSA[parameter == "seq_res", "LIC param 2"]))
  
  q <- runif(1)
  
  if(q < 0.1){
    inputsPSA[parameter == "pig_income_effect", "Med"] <- -1 * rbeta(1, as.numeric(inputsPSA[parameter == "pig_income_effect", "LIC param 1"]),
                                                                as.numeric(inputsPSA[parameter == "pig_income_effect", "LIC param 2"]))
  } else if (q >= 0.1){
    inputsPSA[parameter == "pig_income_effect", "Med"] <- rbeta(1, as.numeric(inputsPSA[parameter == "pig_income_effect", "LIC param 1"]),
                                                                     as.numeric(inputsPSA[parameter == "pig_income_effect", "LIC param 2"]))
  }
  
  p <- runif(1)
  
  if(p < 0.1){
    inputsPSA[parameter == "chicken_income_effect", "Med"] <- -1 * rbeta(1, as.numeric(inputsPSA[parameter == "chicken_income_effect", "LIC param 1"]),
                                                                     as.numeric(inputsPSA[parameter == "chicken_income_effect", "LIC param 2"]))
  } else if (p >= 0.1){
    inputsPSA[parameter == "chicken_income_effect", "Med"] <- rbeta(1, as.numeric(inputsPSA[parameter == "chicken_income_effect", "LIC param 1"]),
                                                                as.numeric(inputsPSA[parameter == "chicken_income_effect", "LIC param 2"]))
  }
  
  r <- runif(1)
  
  if(r < 0.9){
    inputsPSA[parameter == "pig_mort_effect", "Med"] <- -1 * rbeta(1, as.numeric(inputsPSA[parameter == "pig_mort_effect", "LIC param 1"]),
                                                                     as.numeric(inputsPSA[parameter == "pig_mort_effect", "LIC param 2"]))
  } else if (r >= 0.9){
    inputsPSA[parameter == "pig_mort_effect", "Med"] <- rbeta(1, as.numeric(inputsPSA[parameter == "pig_mort_effect", "LIC param 1"]),
                                                                as.numeric(inputsPSA[parameter == "pig_mort_effect", "LIC param 2"]))
  }
  
  s <- runif(1)
  
  if(s < 0.9){
    inputsPSA[parameter == "chicken_mort_effect", "Med"] <- -1 * rbeta(1, as.numeric(inputsPSA[parameter == "chicken_mort_effect", "LIC param 1"]),
                                                                         as.numeric(inputsPSA[parameter == "chicken_mort_effect", "LIC param 2"]))
  } else if (s >= 0.9){
    inputsPSA[parameter == "chicken_mort_effect", "Med"] <- rbeta(1, as.numeric(inputsPSA[parameter == "chicken_mort_effect", "LIC param 1"]),
                                                                    as.numeric(inputsPSA[parameter == "chicken_mort_effect", "LIC param 2"]))
  }

  #store result in vector
  LIC_MC_Vector[i] <- as.data.frame(Model(inputsPSA, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect))[1,1]
}

min_LIC <- min(LIC_MC_Vector)
max_LIC <- max(LIC_MC_Vector)
avg_LIC <- median(LIC_MC_Vector)

# Define output holder
montecarlo_output <- as.data.frame(matrix(0,number_runs,4))
colnames(montecarlo_output) <- c("run","LIC","MIC","HIC")
montecarlo_output$run <- seq(1,number_runs,1)

montecarlo_output$LIC <- LIC_MC_Vector

# jpeg("Outputs/Figure 2 A.jpg")
# 
# hist(LIC_MC_Vector,
#      xlab = "Maximum Annual Cost",
#      ylab = "Cumulative Density",
#      main = "Distribution of Values from Montecarlo Simulation - LIC")
# 
# dev.off()

write.xlsx(LIC_MC_Vector, "Outputs/MC Results LIC.xlsx")

##MIC

scenario_income <- "MIC-S"

MIC_MC_Vector <- rep(0, number_runs)

inputs_MIC <- read.csv(here("inputs - general model.csv"))
inputs_MIC <- as.data.table(inputs_MIC)
colnames(inputs_MIC) <- c("parameter", "description", "HIC", "MIC-I", "MIC-S", "LIC", "Value", "Min", "Lo", "Med", "Hi", "Max", 
                          "LIC min", "LIC max", "MIC min", "MIC max", "HIC min", "HIC max",
                          "Distribution", "LIC param 1", "LIC param 2", "MIC param 1", "MIC param 2",
                          "HIC param 1", "HIC param 2")

set.seed(42069)

for(i in 1:number_runs){
  #load dataset
  inputsPSA <- inputs_MIC
  
  inputsPSA[parameter == "well_sick", "MIC-S"] <- rbeta(1, as.numeric(inputsPSA[parameter == "well_sick", "MIC param 1"]), 
                                                      as.numeric(inputsPSA[parameter == "well_sick", "MIC param 2"]))
  
  inputsPSA[parameter == "portion_res", "MIC-S"] <- rbeta(1, as.numeric(inputsPSA[parameter == "portion_res", "MIC param 1"]), 
                                                        as.numeric(inputsPSA[parameter == "portion_res", "MIC param 2"]))
  
  inputsPSA[parameter == "mort_res", "MIC-S"] <- 1.62 * rbeta(1,as.numeric(inputsPSA[parameter == "mort_res", "MIC param 1"]), 
                                                            as.numeric(inputsPSA[parameter == "mort_res", "MIC param 2"]))
  
  inputsPSA[parameter == "mort_sus", "MIC-S"] <- rbeta(1, as.numeric(inputsPSA[parameter == "mort_sus", "MIC param 1"]), 
                                                     as.numeric(inputsPSA[parameter == "mort_sus", "MIC param 2"]))
  
  inputsPSA[parameter == "los_sus", "MIC-S"] <- (1/365.25) * rlnorm(1, as.numeric(inputsPSA[parameter == "los_sus", "MIC param 1"]), 
                                                                  as.numeric(inputsPSA[parameter == "los_sus", "MIC param 2"]))
  
  inputsPSA[parameter == "los_res", "MIC-S"] <- (1.27/365.25) * rlnorm(1, as.numeric(inputsPSA[parameter == "los_res", "MIC param 1"]), 
                                                                     as.numeric(inputsPSA[parameter == "los_res", "MIC param 2"]))
  
  inputsPSA[parameter == "bed_day_cost", "MIC-S"] <- rlnorm(1, as.numeric(inputsPSA[parameter == "bed_day_cost", "MIC param 1"]), 
                                                          as.numeric(inputsPSA[parameter == "bed_day_cost", "MIC param 2"]))
  
  inputsPSA[parameter == "qol_sick", "MIC-S"] <- rbeta(1, as.numeric(inputsPSA[parameter == "qol_sick", "MIC param 1"]), 
                                                     as.numeric(inputsPSA[parameter == "qol_sick", "MIC param 2"]))
  
  inputsPSA[parameter == "qol_seq", "MIC-S"] <- rbeta(1, as.numeric(inputsPSA[parameter == "qol_seq", "MIC param 1"]), 
                                                    as.numeric(inputsPSA[parameter == "qol_seq", "MIC param 2"]))
  
  inputsPSA[parameter == "amr_grow", "MIC-S"] <- 0.01 * rgamma(1, as.numeric(inputsPSA[parameter == "amr_grow", "MIC param 1"]), 
                                                             scale = as.numeric(inputsPSA[parameter == "amr_grow", "MIC param 2"]))
  
  inputsPSA[parameter == "n_pigs", "MIC-S"] <- rlnorm(1, as.numeric(inputsPSA[parameter == "n_pigs", "MIC param 1"]), 
                                                    as.numeric(inputsPSA[parameter == "n_pigs", "MIC param 2"]))
  
  inputsPSA[parameter == "n_chickens", "MIC-S"] <- rlnorm(1, as.numeric(inputsPSA[parameter == "n_chickens", "MIC param 1"]), 
                                                        as.numeric(inputsPSA[parameter == "n_chickens", "MIC param 2"]))
  
  inputsPSA[parameter == "n_chickens_farm_ind", "MIC-S"] <- rlnorm(1, as.numeric(inputsPSA[parameter == "n_chickens_farm_ind", "MIC param 1"]), 
                                                                 as.numeric(inputsPSA[parameter == "n_chickens_farm_ind", "MIC param 2"]))
  
  inputsPSA[parameter == "n_chickens_farm_small", "MIC-S"] <- rlnorm(1, as.numeric(inputsPSA[parameter == "n_chickens_farm_small", "MIC param 1"]), 
                                                                   as.numeric(inputsPSA[parameter == "n_chickens_farm_small", "MIC param 2"]))
  
  inputsPSA[parameter == "n_pigs_farm_ind", "MIC-S"] <- rlnorm(1, as.numeric(inputsPSA[parameter == "n_pigs_farm_ind", "MIC param 1"]), 
                                                             as.numeric(inputsPSA[parameter == "n_pigs_farm_ind", "MIC param 2"]))
  
  inputsPSA[parameter == "n_pigs_farm_small", "MIC-S"] <- rlnorm(1, as.numeric(inputsPSA[parameter == "n_pigs_farm_small", "MIC param 1"]), 
                                                               as.numeric(inputsPSA[parameter == "n_pigs_farm_small", "MIC param 2"]))
  
  inputsPSA[parameter == "portion_animals_ind", "MIC-S"] <- rbeta(1, as.numeric(inputsPSA[parameter == "portion_animals_ind", "MIC param 1"]), 
                                                                as.numeric(inputsPSA[parameter == "portion_animals_ind", "MIC param 2"]))
  
  inputsPSA[parameter == "pig_price", "MIC-S"] <- rlnorm(1, as.numeric(inputsPSA[parameter == "pig_price", "MIC param 1"]), 
                                                       as.numeric(inputsPSA[parameter == "pig_price", "MIC param 2"]))
  
  inputsPSA[parameter == "chicken_price", "MIC-S"] <- rlnorm(1, as.numeric(inputsPSA[parameter == "chicken_price", "MIC param 1"]), 
                                                           as.numeric(inputsPSA[parameter == "chicken_price", "MIC param 2"]))
  
  inputsPSA[parameter == "c_mort_ind", "MIC-S"] <- rbeta(1, as.numeric(inputsPSA[parameter == "c_mort_ind", "MIC param 1"]), 
                                                       as.numeric(inputsPSA[parameter == "c_mort_ind", "MIC param 2"]))
  
  inputsPSA[parameter == "c_mort_small", "MIC-S"] <- rbeta(1, as.numeric(inputsPSA[parameter == "c_mort_small", "MIC param 1"]), 
                                                         as.numeric(inputsPSA[parameter == "c_mort_small", "MIC param 2"]))
  
  inputsPSA[parameter == "p_mort_ind", "MIC-S"] <- rbeta(1, as.numeric(inputsPSA[parameter == "p_mort_ind", "MIC param 1"]), 
                                                       as.numeric(inputsPSA[parameter == "p_mort_ind", "MIC param 2"]))
  
  inputsPSA[parameter == "p_mort_small", "MIC-S"] <- rbeta(1, as.numeric(inputsPSA[parameter == "p_mort_small", "MIC param 1"]), 
                                                         as.numeric(inputsPSA[parameter == "p_mort_small", "MIC param 2"]))
  
  inputsPSA[parameter == "res_change", "Med"] <- -1 * rbeta(1, as.numeric(inputsPSA[parameter == "res_change", "MIC param 1"]), 
                                                            as.numeric(inputsPSA[parameter == "res_change", "MIC param 2"]))
  
  inputsPSA[parameter == "seq_sus", "MIC-S"] <- 1.54158796 * rbeta(1, as.numeric(inputsPSA[parameter == "seq_sus", "MIC param 1"]), 
                                                              as.numeric(inputsPSA[parameter == "seq_sus", "MIC param 2"]))
  
  inputsPSA[parameter == "seq_res", "MIC-S"] <- 2.4973725 * rbeta(1, as.numeric(inputsPSA[parameter == "seq_res", "MIC param 1"]), 
                                                               as.numeric(inputsPSA[parameter == "seq_res", "MIC param 2"]))
  
  q <- runif(1)
  
  if(q < 0.1){
    inputsPSA[parameter == "pig_income_effect", "Med"] <- -1 * rbeta(1, as.numeric(inputsPSA[parameter == "pig_income_effect", "MIC param 1"]),
                                                                     as.numeric(inputsPSA[parameter == "pig_income_effect", "MIC param 2"]))
  } else if (q >= 0.1){
    inputsPSA[parameter == "pig_income_effect", "Med"] <- rbeta(1, as.numeric(inputsPSA[parameter == "pig_income_effect", "MIC param 1"]),
                                                                as.numeric(inputsPSA[parameter == "pig_income_effect", "MIC param 2"]))
  }
  
  p <- runif(1)
  
  if(p < 0.1){
    inputsPSA[parameter == "chicken_income_effect", "Med"] <- -1 * rbeta(1, as.numeric(inputsPSA[parameter == "chicken_income_effect", "MIC param 1"]),
                                                                         as.numeric(inputsPSA[parameter == "chicken_income_effect", "MIC param 2"]))
  } else if (p >= 0.1){
    inputsPSA[parameter == "chicken_income_effect", "Med"] <- rbeta(1, as.numeric(inputsPSA[parameter == "chicken_income_effect", "MIC param 1"]),
                                                                    as.numeric(inputsPSA[parameter == "chicken_income_effect", "MIC param 2"]))
  }
  
  r <- runif(1)
  
  if(r < 0.9){
    inputsPSA[parameter == "pig_mort_effect", "Med"] <- -1 * rbeta(1, as.numeric(inputsPSA[parameter == "pig_mort_effect", "MIC param 1"]),
                                                                   as.numeric(inputsPSA[parameter == "pig_mort_effect", "MIC param 2"]))
  } else if (r >= 0.9){
    inputsPSA[parameter == "pig_mort_effect", "Med"] <- rbeta(1, as.numeric(inputsPSA[parameter == "pig_mort_effect", "MIC param 1"]),
                                                              as.numeric(inputsPSA[parameter == "pig_mort_effect", "MIC param 2"]))
  }
  
  s <- runif(1)
  
  if(s < 0.9){
    inputsPSA[parameter == "chicken_mort_effect", "Med"] <- -1 * rbeta(1, as.numeric(inputsPSA[parameter == "chicken_mort_effect", "MIC param 1"]),
                                                                       as.numeric(inputsPSA[parameter == "chicken_mort_effect", "MIC param 2"]))
  } else if (s >= 0.9){
    inputsPSA[parameter == "chicken_mort_effect", "Med"] <- rbeta(1, as.numeric(inputsPSA[parameter == "chicken_mort_effect", "MIC param 1"]),
                                                                  as.numeric(inputsPSA[parameter == "chicken_mort_effect", "MIC param 2"]))
  }
  
  #store result in vector
  MIC_MC_Vector[i] <- as.data.frame(Model(inputsPSA, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect))[1,1]
}

min_MIC <- min(MIC_MC_Vector)
max_MIC <- max(MIC_MC_Vector)
avg_MIC <- median(MIC_MC_Vector)

montecarlo_output$MIC <- MIC_MC_Vector

# jpeg("Outputs/Figure 2 B.jpg")
# 
# hist(MIC_MC_Vector,
#      xlab = "Maximum Annual Cost",
#      ylab = "Cumulative Density",
#      main = "Distribution of Values from Montecarlo Simulation - MIC")
# 
# dev.off()

write.xlsx(MIC_MC_Vector, "Outputs/MC Results MIC.xlsx")

##HIC
scenario_income <- "HIC"

HIC_MC_Vector <- rep(0, number_runs)

inputs_HIC <- read.csv(here("inputs - general model.csv"))
inputs_HIC <- as.data.table(inputs_HIC)
colnames(inputs_HIC) <- c("parameter", "description", "HIC", "MIC-I", "MIC-S", "LIC", "Value", "Min", "Lo", "Med", "Hi", "Max", 
                          "LIC min", "LIC max", "MIC min", "MIC max", "HIC min", "HIC max",
                          "Distribution", "LIC param 1", "LIC param 2", "MIC param 1", "MIC param 2",
                          "HIC param 1", "HIC param 2")

set.seed(42069)

for(i in 1:number_runs){
  #load dataset
  inputsPSA <- inputs_HIC
  
  inputsPSA[parameter == "well_sick", "HIC"] <- rbeta(1, as.numeric(inputsPSA[parameter == "well_sick", "HIC param 1"]), 
                                                        as.numeric(inputsPSA[parameter == "well_sick", "HIC param 2"]))
  
  inputsPSA[parameter == "portion_res", "HIC"] <- rbeta(1, as.numeric(inputsPSA[parameter == "portion_res", "HIC param 1"]), 
                                                          as.numeric(inputsPSA[parameter == "portion_res", "HIC param 2"]))
  
  inputsPSA[parameter == "mort_res", "HIC"] <- 1.62 * rbeta(1,as.numeric(inputsPSA[parameter == "mort_res", "HIC param 1"]), 
                                                              as.numeric(inputsPSA[parameter == "mort_res", "HIC param 2"]))
  
  inputsPSA[parameter == "mort_sus", "HIC"] <- rbeta(1, as.numeric(inputsPSA[parameter == "mort_sus", "HIC param 1"]), 
                                                       as.numeric(inputsPSA[parameter == "mort_sus", "HIC param 2"]))
  
  inputsPSA[parameter == "los_sus", "HIC"] <- (1/365.25) * rlnorm(1, as.numeric(inputsPSA[parameter == "los_sus", "HIC param 1"]), 
                                                                    as.numeric(inputsPSA[parameter == "los_sus", "HIC param 2"]))
  
  inputsPSA[parameter == "los_res", "HIC"] <- (1.27/365.25) * rlnorm(1, as.numeric(inputsPSA[parameter == "los_res", "HIC param 1"]), 
                                                                       as.numeric(inputsPSA[parameter == "los_res", "HIC param 2"]))
  
  inputsPSA[parameter == "bed_day_cost", "HIC"] <- rlnorm(1, as.numeric(inputsPSA[parameter == "bed_day_cost", "HIC param 1"]), 
                                                            as.numeric(inputsPSA[parameter == "bed_day_cost", "HIC param 2"]))
  
  inputsPSA[parameter == "qol_sick", "HIC"] <- rbeta(1, as.numeric(inputsPSA[parameter == "qol_sick", "HIC param 1"]), 
                                                       as.numeric(inputsPSA[parameter == "qol_sick", "HIC param 2"]))
  
  inputsPSA[parameter == "qol_seq", "HIC"] <- rbeta(1, as.numeric(inputsPSA[parameter == "qol_seq", "HIC param 1"]), 
                                                      as.numeric(inputsPSA[parameter == "qol_seq", "HIC param 2"]))
  
  inputsPSA[parameter == "amr_grow", "HIC"] <- 0.01 * rgamma(1, as.numeric(inputsPSA[parameter == "amr_grow", "HIC param 1"]), 
                                                               scale = as.numeric(inputsPSA[parameter == "amr_grow", "HIC param 2"]))
  
  inputsPSA[parameter == "n_pigs", "HIC"] <- rlnorm(1, as.numeric(inputsPSA[parameter == "n_pigs", "HIC param 1"]), 
                                                      as.numeric(inputsPSA[parameter == "n_pigs", "HIC param 2"]))
  
  inputsPSA[parameter == "n_chickens", "HIC"] <- rlnorm(1, as.numeric(inputsPSA[parameter == "n_chickens", "HIC param 1"]), 
                                                          as.numeric(inputsPSA[parameter == "n_chickens", "HIC param 2"]))
  
  inputsPSA[parameter == "n_chickens_farm_ind", "HIC"] <- rlnorm(1, as.numeric(inputsPSA[parameter == "n_chickens_farm_ind", "HIC param 1"]), 
                                                                   as.numeric(inputsPSA[parameter == "n_chickens_farm_ind", "HIC param 2"]))
  
  inputsPSA[parameter == "n_chickens_farm_small", "HIC"] <- rlnorm(1, as.numeric(inputsPSA[parameter == "n_chickens_farm_small", "HIC param 1"]), 
                                                                     as.numeric(inputsPSA[parameter == "n_chickens_farm_small", "HIC param 2"]))
  
  inputsPSA[parameter == "n_pigs_farm_ind", "HIC"] <- rlnorm(1, as.numeric(inputsPSA[parameter == "n_pigs_farm_ind", "HIC param 1"]), 
                                                               as.numeric(inputsPSA[parameter == "n_pigs_farm_ind", "HIC param 2"]))
  
  inputsPSA[parameter == "n_pigs_farm_small", "HIC"] <- rlnorm(1, as.numeric(inputsPSA[parameter == "n_pigs_farm_small", "HIC param 1"]), 
                                                                 as.numeric(inputsPSA[parameter == "n_pigs_farm_small", "HIC param 2"]))
  
  inputsPSA[parameter == "portion_animals_ind", "HIC"] <- rbeta(1, as.numeric(inputsPSA[parameter == "portion_animals_ind", "HIC param 1"]), 
                                                                  as.numeric(inputsPSA[parameter == "portion_animals_ind", "HIC param 2"]))
  
  inputsPSA[parameter == "pig_price", "HIC"] <- rlnorm(1, as.numeric(inputsPSA[parameter == "pig_price", "HIC param 1"]), 
                                                         as.numeric(inputsPSA[parameter == "pig_price", "HIC param 2"]))
  
  inputsPSA[parameter == "chicken_price", "HIC"] <- rlnorm(1, as.numeric(inputsPSA[parameter == "chicken_price", "HIC param 1"]), 
                                                             as.numeric(inputsPSA[parameter == "chicken_price", "HIC param 2"]))
  
  inputsPSA[parameter == "c_mort_ind", "HIC"] <- rbeta(1, as.numeric(inputsPSA[parameter == "c_mort_ind", "HIC param 1"]), 
                                                         as.numeric(inputsPSA[parameter == "c_mort_ind", "HIC param 2"]))
  
  inputsPSA[parameter == "c_mort_small", "HIC"] <- rbeta(1, as.numeric(inputsPSA[parameter == "c_mort_small", "HIC param 1"]), 
                                                           as.numeric(inputsPSA[parameter == "c_mort_small", "HIC param 2"]))
  
  inputsPSA[parameter == "p_mort_ind", "HIC"] <- rbeta(1, as.numeric(inputsPSA[parameter == "p_mort_ind", "HIC param 1"]), 
                                                         as.numeric(inputsPSA[parameter == "p_mort_ind", "HIC param 2"]))
  
  inputsPSA[parameter == "p_mort_small", "HIC"] <- rbeta(1, as.numeric(inputsPSA[parameter == "p_mort_small", "HIC param 1"]), 
                                                           as.numeric(inputsPSA[parameter == "p_mort_small", "HIC param 2"]))
  
  inputsPSA[parameter == "res_change", "Med"] <- -1 * rbeta(1, as.numeric(inputsPSA[parameter == "res_change", "HIC param 1"]), 
                                                            as.numeric(inputsPSA[parameter == "res_change", "HIC param 2"]))
  
  inputsPSA[parameter == "seq_sus", "HIC"] <- rbeta(1, as.numeric(inputsPSA[parameter == "seq_sus", "HIC param 1"]), 
                                                                 as.numeric(inputsPSA[parameter == "seq_sus", "HIC param 2"]))
  
  inputsPSA[parameter == "seq_res", "HIC"] <- 1.62 * rbeta(1, as.numeric(inputsPSA[parameter == "seq_res", "HIC param 1"]), 
                                                                as.numeric(inputsPSA[parameter == "seq_res", "HIC param 2"]))
  
  q <- runif(1)
  
  if(q < 0.1){
    inputsPSA[parameter == "pig_income_effect", "Med"] <- -1 * rbeta(1, as.numeric(inputsPSA[parameter == "pig_income_effect", "HIC param 1"]),
                                                                     as.numeric(inputsPSA[parameter == "pig_income_effect", "HIC param 2"]))
  } else if (q >= 0.1){
    inputsPSA[parameter == "pig_income_effect", "Med"] <- rbeta(1, as.numeric(inputsPSA[parameter == "pig_income_effect", "HIC param 1"]),
                                                                as.numeric(inputsPSA[parameter == "pig_income_effect", "HIC param 2"]))
  }
  
  p <- runif(1)
  
  if(p < 0.1){
    inputsPSA[parameter == "chicken_income_effect", "Med"] <- -1 * rbeta(1, as.numeric(inputsPSA[parameter == "chicken_income_effect", "HIC param 1"]),
                                                                         as.numeric(inputsPSA[parameter == "chicken_income_effect", "HIC param 2"]))
  } else if (p >= 0.1){
    inputsPSA[parameter == "chicken_income_effect", "Med"] <- rbeta(1, as.numeric(inputsPSA[parameter == "chicken_income_effect", "HIC param 1"]),
                                                                    as.numeric(inputsPSA[parameter == "chicken_income_effect", "HIC param 2"]))
  }
  
  r <- runif(1)
  
  if(r < 0.9){
    inputsPSA[parameter == "pig_mort_effect", "Med"] <- -1 * rbeta(1, as.numeric(inputsPSA[parameter == "pig_mort_effect", "HIC param 1"]),
                                                                   as.numeric(inputsPSA[parameter == "pig_mort_effect", "HIC param 2"]))
  } else if (r >= 0.9){
    inputsPSA[parameter == "pig_mort_effect", "Med"] <- rbeta(1, as.numeric(inputsPSA[parameter == "pig_mort_effect", "HIC param 1"]),
                                                              as.numeric(inputsPSA[parameter == "pig_mort_effect", "HIC param 2"]))
  }
  
  s <- runif(1)
  
  if(s < 0.9){
    inputsPSA[parameter == "chicken_mort_effect", "Med"] <- -1 * rbeta(1, as.numeric(inputsPSA[parameter == "chicken_mort_effect", "HIC param 1"]),
                                                                       as.numeric(inputsPSA[parameter == "chicken_mort_effect", "HIC param 2"]))
  } else if (s >= 0.9){
    inputsPSA[parameter == "chicken_mort_effect", "Med"] <- rbeta(1, as.numeric(inputsPSA[parameter == "chicken_mort_effect", "HIC param 1"]),
                                                                  as.numeric(inputsPSA[parameter == "chicken_mort_effect", "HIC param 2"]))
  }
  
  #store result in vector
  HIC_MC_Vector[i] <- as.data.frame(Model(inputsPSA, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect))[1,1]
}

min_HIC <- min(HIC_MC_Vector)
max_HIC <- max(HIC_MC_Vector)
avg_HIC <- median(HIC_MC_Vector)

montecarlo_output$HIC <- HIC_MC_Vector

# jpeg("Outputs/Figure 2 C.jpg")
# 
# hist(HIC_MC_Vector,
#      xlab = "Maximum Annual Cost",
#      ylab = "Cumulative Density",
#      main = "Distribution of Values from Montecarlo Simulation - HIC")
# 
# dev.off()

write.xlsx(HIC_MC_Vector, "Outputs/MC Results HIC.xlsx")

# ggplot
monte_output_plot <- montecarlo_output %>% pivot_longer(LIC:HIC)
monte_output_plot$name <- factor(monte_output_plot$name, levels = c("LIC","MIC","HIC"))
theme_set(theme_bw(base_size = 12))
ggplot(monte_output_plot, aes(value)) + geom_histogram(bins = 20, aes(fill = name)) + 
  facet_wrap(~name, scales = "free") + 
  scale_y_continuous("Number of simulations") + 
  scale_x_continuous("Maximum Annual Cost (USD)") + 
  scale_fill_discrete("Income group") + 
  geom_vline(xintercept = 0, linetype = "dashed")
ggsave("Outputs/monte_carlo.jpeg", width = 13, height = 5)

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

#HIC - Human

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


#HIC - Animal

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


#MICI - Human

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


#MICI - Animal

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

#MICS - Human

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


#MICS - Animal

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

#LIC - Human

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

#LIC - Animal

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

#### plotting
tornado_HIC_animal$ic <- "HIC"
tornado_HIC_animal$sp <- "animal"
tornado_HIC_human$ic <- "HIC"
tornado_HIC_human$sp <- "human"
tornado_MICI_animal$ic <- "MICI"
tornado_MICI_animal$sp <- "animal"
tornado_MICI_human$ic <- "MICI"
tornado_MICI_human$sp <- "human"
tornado_MICS_animal$ic <- "MICS"
tornado_MICS_animal$sp <- "animal"
tornado_MICS_human$ic <- "MICS"
tornado_MICS_human$sp <- "human"
tornado_LIC_animal$ic <- "LIC"
tornado_LIC_animal$sp <- "animal"
tornado_LIC_human$ic <- "LIC"
tornado_LIC_human$sp <- "human"

width = 1
base.value = 0



fig3_data_orig <- rbind(tornado_HIC_animal, tornado_HIC_human,
                   tornado_MICI_animal, tornado_MICI_human,
                   tornado_MICS_animal, tornado_MICS_human,
                   tornado_LIC_animal, tornado_LIC_human) %>% 
  mutate(difference = abs(max-min)) 

order.parameters.animal <- fig3_data_orig %>% filter(ic == "LIC", sp == "animal") %>% 
  arrange(difference) %>%
  mutate(Parameter=factor(x=variable, levels=variable)) %>%
  select(Parameter) %>% unlist() %>% levels()
order.parameters.human <- fig3_data_orig %>% filter(ic == "LIC", sp == "human") %>% 
  arrange(difference) %>%
  mutate(Parameter=factor(x=variable, levels=variable)) %>%
  select(Parameter) %>% unlist() %>% levels()

fig3_data <- fig3_data_orig %>% 
  #group_by(sp) %>%
  #arrange(difference) %>% 
  group_by(ic, sp) %>%
  mutate(orderng = factor(variable, levels = c(order.parameters.animal,order.parameters.human))) %>%
  arrange(orderng) %>% 
  mutate(para = seq(1,n())) %>% 
  pivot_longer(cols = min:max) %>%
  mutate(ymin=pmin(value, base.value),
         ymax=pmax(value, base.value),
         xmin=as.numeric(para)-width/2,
         xmax=as.numeric(para)+width/2) 
fig3_data$ic <- factor(fig3_data$ic, levels = c("LIC","MICI","MICS","HIC"))

                   
g1 <- ggplot() + 
  geom_rect(data = fig3_data %>% filter(sp == "animal"), 
            aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=variable)) +
  theme_bw() + 
  theme(axis.title.y=element_blank(), legend.position = 'bottom',
        legend.title = element_blank()) + 
  geom_hline(yintercept = base.value) +
  facet_grid(sp ~ ic, scales = "free") + 
  scale_x_continuous(breaks = seq(1:8), labels = order.parameters.animal) +
  coord_flip()

g2 <- ggplot() + 
  geom_rect(data = fig3_data %>% filter(sp == "human"), 
            aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=variable)) +
  theme_bw() + 
  theme(axis.title.y=element_blank(), legend.position = 'bottom',
        legend.title = element_blank()) + 
  geom_hline(yintercept = base.value) +
  facet_grid(sp ~ ic, scales = "free") + 
  scale_x_continuous(breaks = seq(1:length(order.parameters.human)), labels = order.parameters.human) +
  coord_flip()

g1 / g2
ggsave("Outputs/figure3_ggplot.jpeg",width = 20, height = 14)

# Table 2 - Global Sensitivity Analysis -----------------------------------
#' 
#' scenario_prod <- "HCA" 
#' scenario_transmission <- "med"
#' scenario_farm_effect <- "med"
#' 
#' ###LIC
#' 
#' scenario_income <- "LIC"
#' 
#' LIC_reg_matrix <- matrix(rep(0), nrow = number_runs, ncol = nrow(inputs_general)+1)
#' 
#' colnames(LIC_reg_matrix) <- c("Output", rep(0, nrow(inputs_general)))
#' 
#' for(i in 2:ncol(LIC_reg_matrix)){
#'   colnames(LIC_reg_matrix)[i] <- inputs_general[i-1,1]
#' }
#' 
#' LIC_reg_matrix <- as.data.frame(LIC_reg_matrix)
#' 
#' inputs_reg_LIC <- read.csv(here("inputs - general model.csv"))
#' inputs_reg_LIC <- as.data.table(inputs_reg_LIC)
#' colnames(inputs_reg_LIC) <- c("parameter", "description", "HIC", "MIC-I", "MIC-S", "LIC", "Value", "Min", "Lo", "Med", "Hi", "Max", 
#'                               "LIC min", "LIC max", "MIC min", "MIC max", "HIC min", "HIC max",
#'                               "Distribution", "LIC param 1", "LIC param 2", "MIC param 1", "MIC param 2",
#'                               "HIC param 1", "HIC param 2")
#' 
#' 
#' set.seed(42069)
#' 
#' for(i in 1:number_runs){
#'   inputsreg <- inputs_reg_LIC
#'   
#'   inputsreg[parameter == "well_sick", "LIC"] <- rbeta(1, as.numeric(inputsreg[parameter == "well_sick", "LIC param 1"]), 
#'                                                       as.numeric(inputsreg[parameter == "well_sick", "LIC param 2"]))
#'   
#'   LIC_reg_matrix[i, "well_sick"] <- inputsreg[parameter == "well_sick", "LIC"]
#'   
#'   inputsreg[parameter == "portion_res", "LIC"] <- rbeta(1, as.numeric(inputsreg[parameter == "portion_res", "LIC param 1"]), 
#'                                                         as.numeric(inputsreg[parameter == "portion_res", "LIC param 2"]))
#'   
#'   LIC_reg_matrix[i, "portion_res"] <- inputsreg[parameter == "portion_res", "LIC"]
#'   
#'   inputsreg[parameter == "mort_res", "LIC"] <- 1.62 * rbeta(1,as.numeric(inputsreg[parameter == "mort_res", "LIC param 1"]), 
#'                                                             as.numeric(inputsreg[parameter == "mort_res", "LIC param 2"]))
#'   
#'   LIC_reg_matrix[i, "mort_res"] <- inputsreg[parameter == "mort_res", "LIC"]
#'   
#'   inputsreg[parameter == "mort_sus", "LIC"] <- rbeta(1, as.numeric(inputsreg[parameter == "mort_sus", "LIC param 1"]), 
#'                                                      as.numeric(inputsreg[parameter == "mort_sus", "LIC param 2"]))
#'   
#'   LIC_reg_matrix[i, "mort_sus"] <- inputsreg[parameter == "mort_sus", "LIC"]
#'   
#'   inputsreg[parameter == "los_sus", "LIC"] <- (1/365.25) * rlnorm(1, as.numeric(inputsreg[parameter == "los_sus", "LIC param 1"]), 
#'                                                                   as.numeric(inputsreg[parameter == "los_sus", "LIC param 2"]))
#'   
#'   LIC_reg_matrix[i, "los_sus"] <- inputsreg[parameter == "los_sus", "LIC"]
#'   
#'   inputsreg[parameter == "los_res", "LIC"] <- (1.27/365.25) * rlnorm(1, as.numeric(inputsreg[parameter == "los_res", "LIC param 1"]), 
#'                                                                      as.numeric(inputsreg[parameter == "los_res", "LIC param 2"]))
#'   
#'   LIC_reg_matrix[i, "los_res"] <- inputsreg[parameter == "los_res", "LIC"]
#'   
#'   inputsreg[parameter == "bed_day_cost", "LIC"] <- rlnorm(1, as.numeric(inputsreg[parameter == "bed_day_cost", "LIC param 1"]), 
#'                                                           as.numeric(inputsreg[parameter == "bed_day_cost", "LIC param 2"]))
#'   
#'   LIC_reg_matrix[i, "bed_day_cost"] <- inputsreg[parameter == "bed_day_cost", "LIC"]
#'   
#'   inputsreg[parameter == "qol_sick", "LIC"] <- rbeta(1, as.numeric(inputsreg[parameter == "qol_sick", "LIC param 1"]), 
#'                                                      as.numeric(inputsreg[parameter == "qol_sick", "LIC param 2"]))
#'   
#'   LIC_reg_matrix[i, "qol_sick"] <- inputsreg[parameter == "qol_sick", "LIC"]
#'   
#'   inputsreg[parameter == "qol_seq", "LIC"] <- rbeta(1, as.numeric(inputsreg[parameter == "qol_seq", "LIC param 1"]), 
#'                                                     as.numeric(inputsreg[parameter == "qol_seq", "LIC param 2"]))
#'   
#'   LIC_reg_matrix[i, "qol_seq"] <- inputsreg[parameter == "qol_seq", "LIC"]
#'   
#'   inputsreg[parameter == "amr_grow", "LIC"] <- 0.01 * rgamma(1, as.numeric(inputsreg[parameter == "amr_grow", "LIC param 1"]), 
#'                                                              scale = as.numeric(inputsreg[parameter == "amr_grow", "LIC param 2"]))
#'   
#'   LIC_reg_matrix[i, "amr_grow"] <- inputsreg[parameter == "amr_grow", "LIC"]
#'   
#'   inputsreg[parameter == "n_pigs", "LIC"] <- rlnorm(1, as.numeric(inputsreg[parameter == "n_pigs", "LIC param 1"]), 
#'                                                     as.numeric(inputsreg[parameter == "n_pigs", "LIC param 2"]))
#'   
#'   LIC_reg_matrix[i, "n_pigs"] <- inputsreg[parameter == "n_pigs", "LIC"]
#'   
#'   inputsreg[parameter == "n_chickens", "LIC"] <- rlnorm(1, as.numeric(inputsreg[parameter == "n_chickens", "LIC param 1"]), 
#'                                                         as.numeric(inputsreg[parameter == "n_chickens", "LIC param 2"]))
#'   
#'   LIC_reg_matrix[i, "n_chickens"] <- inputsreg[parameter == "n_chickens", "LIC"]
#'   
#'   inputsreg[parameter == "n_chickens_farm_ind", "LIC"] <- rlnorm(1, as.numeric(inputsreg[parameter == "n_chickens_farm_ind", "LIC param 1"]), 
#'                                                                  as.numeric(inputsreg[parameter == "n_chickens_farm_ind", "LIC param 2"]))
#'   
#'   LIC_reg_matrix[i, "n_chickens_farm_ind"] <- inputsreg[parameter == "n_chickens_farm_ind", "LIC"]
#'   
#'   inputsreg[parameter == "n_chickens_farm_small", "LIC"] <- rlnorm(1, as.numeric(inputsreg[parameter == "n_chickens_farm_small", "LIC param 1"]), 
#'                                                                    as.numeric(inputsreg[parameter == "n_chickens_farm_small", "LIC param 2"]))
#'   
#'   LIC_reg_matrix[i, "n_chickens_farm_small"] <- inputsreg[parameter == "n_chickens_farm_small", "LIC"]
#'   
#'   inputsreg[parameter == "n_pigs_farm_ind", "LIC"] <- rlnorm(1, as.numeric(inputsreg[parameter == "n_pigs_farm_ind", "LIC param 1"]), 
#'                                                              as.numeric(inputsreg[parameter == "n_pigs_farm_ind", "LIC param 2"]))
#'   
#'   LIC_reg_matrix[i, "n_pigs_farm_ind"] <- inputsreg[parameter == "n_pigs_farm_ind", "LIC"]
#'   
#'   inputsreg[parameter == "n_pigs_farm_small", "LIC"] <- rlnorm(1, as.numeric(inputsreg[parameter == "n_pigs_farm_small", "LIC param 1"]), 
#'                                                                as.numeric(inputsreg[parameter == "n_pigs_farm_small", "LIC param 2"]))
#'   
#'   LIC_reg_matrix[i, "n_pigs_farm_small"] <- inputsreg[parameter == "n_pigs_farm_small", "LIC"]
#'   
#'   inputsreg[parameter == "portion_animals_ind", "LIC"] <- rbeta(1, as.numeric(inputsreg[parameter == "portion_animals_ind", "LIC param 1"]), 
#'                                                                 as.numeric(inputsreg[parameter == "portion_animals_ind", "LIC param 2"]))
#'   
#'   LIC_reg_matrix[i, "portion_animals_ind"] <- inputsreg[parameter == "portion_animals_ind", "LIC"]
#'   
#'   inputsreg[parameter == "pig_price", "LIC"] <- rlnorm(1, as.numeric(inputsreg[parameter == "pig_price", "LIC param 1"]), 
#'                                                        as.numeric(inputsreg[parameter == "pig_price", "LIC param 2"]))
#'   
#'   LIC_reg_matrix[i, "pig_price"] <- inputsreg[parameter == "pig_price", "LIC"]
#'   
#'   inputsreg[parameter == "chicken_price", "LIC"] <- rlnorm(1, as.numeric(inputsreg[parameter == "chicken_price", "LIC param 1"]), 
#'                                                            as.numeric(inputsreg[parameter == "chicken_price", "LIC param 2"]))
#'   
#'   LIC_reg_matrix[i, "chicken_price"] <- inputsreg[parameter == "chicken_price", "LIC"]
#'   
#'   inputsreg[parameter == "c_mort_ind", "LIC"] <- rbeta(1, as.numeric(inputsreg[parameter == "c_mort_ind", "LIC param 1"]), 
#'                                                        as.numeric(inputsreg[parameter == "c_mort_ind", "LIC param 2"]))
#'   
#'   LIC_reg_matrix[i, "c_mort_ind"] <- inputsreg[parameter == "c_mort_ind", "LIC"]
#'   
#'   inputsreg[parameter == "c_mort_small", "LIC"] <- rbeta(1, as.numeric(inputsreg[parameter == "c_mort_small", "LIC param 1"]), 
#'                                                          as.numeric(inputsreg[parameter == "c_mort_small", "LIC param 2"]))
#'   
#'   LIC_reg_matrix[i, "c_mort_small"] <- inputsreg[parameter == "c_mort_small", "LIC"]
#'   
#'   inputsreg[parameter == "p_mort_ind", "LIC"] <- rbeta(1, as.numeric(inputsreg[parameter == "p_mort_ind", "LIC param 1"]), 
#'                                                        as.numeric(inputsreg[parameter == "p_mort_ind", "LIC param 2"]))
#'   
#'   LIC_reg_matrix[i, "p_mort_ind"] <- inputsreg[parameter == "p_mort_ind", "LIC"]
#'   
#'   inputsreg[parameter == "p_mort_small", "LIC"] <- rbeta(1, as.numeric(inputsreg[parameter == "p_mort_small", "LIC param 1"]), 
#'                                                          as.numeric(inputsreg[parameter == "p_mort_small", "LIC param 2"]))
#'   
#'   LIC_reg_matrix[i, "p_mort_small"] <- inputsreg[parameter == "p_mort_small", "LIC"]
#'   
#'   inputsreg[parameter == "res_change", "Med"] <- -1 * rbeta(1, as.numeric(inputsreg[parameter == "res_change", "LIC param 1"]), 
#'                                                          as.numeric(inputsreg[parameter == "res_change", "LIC param 2"]))
#'   
#'   LIC_reg_matrix[i, "res_change"] <- inputsreg[parameter == "res_change", "Med"]
#'   
#'   inputsreg[parameter == "seq_sus", "LIC"] <- 2.3791717 * rbeta(1, as.numeric(inputsreg[parameter == "seq_sus", "LIC param 1"]), 
#'                                                                as.numeric(inputsreg[parameter == "seq_sus", "LIC param 2"]))
#'   
#'   LIC_reg_matrix[i, "seq_sus"] <- inputsreg[parameter == "seq_sus", "LIC"]
#'   
#'   inputsreg[parameter == "seq_res", "LIC"] <- 3.85425815 * rbeta(1, as.numeric(inputsreg[parameter == "seq_res", "LIC param 1"]), 
#'                                                               as.numeric(inputsreg[parameter == "seq_res", "LIC param 2"]))
#'   
#'   LIC_reg_matrix[i, "seq_res"] <- inputsreg[parameter == "seq_res", "LIC"]
#'   
#'   q <- runif(1)
#'   
#'   if(q < 0.1){
#'     inputsreg[parameter == "pig_income_effect", "Med"] <- -1 * rbeta(1, as.numeric(inputsreg[parameter == "pig_income_effect", "LIC param 1"]),
#'                                                                      as.numeric(inputsreg[parameter == "pig_income_effect", "LIC param 2"]))
#'   } else if (q >= 0.1){
#'     inputsreg[parameter == "pig_income_effect", "Med"] <- rbeta(1, as.numeric(inputsreg[parameter == "pig_income_effect", "LIC param 1"]),
#'                                                                 as.numeric(inputsreg[parameter == "pig_income_effect", "LIC param 2"]))
#'   }
#'   
#'   LIC_reg_matrix[i, "pig_income_effect"] <- inputsreg[parameter == "pig_income_effect", "Med"]
#'   
#'   p <- runif(1)
#'   
#'   if(p < 0.1){
#'     inputsreg[parameter == "chicken_income_effect", "Med"] <- -1 * rbeta(1, as.numeric(inputsreg[parameter == "chicken_income_effect", "LIC param 1"]),
#'                                                                          as.numeric(inputsreg[parameter == "chicken_income_effect", "LIC param 2"]))
#'   } else if (p >= 0.1){
#'     inputsreg[parameter == "chicken_income_effect", "Med"] <- rbeta(1, as.numeric(inputsreg[parameter == "chicken_income_effect", "LIC param 1"]),
#'                                                                     as.numeric(inputsreg[parameter == "chicken_income_effect", "LIC param 2"]))
#'   }
#'   
#'   LIC_reg_matrix[i, "chicken_income_effect"] <- inputsreg[parameter == "chicken_income_effect", "Med"]
#'   
#'   r <- runif(1)
#'   
#'   if(r < 0.9){
#'     inputsreg[parameter == "pig_mort_effect", "Med"] <- -1 * rbeta(1, as.numeric(inputsreg[parameter == "pig_mort_effect", "LIC param 1"]),
#'                                                                    as.numeric(inputsreg[parameter == "pig_mort_effect", "LIC param 2"]))
#'   } else if (r >= 0.9){
#'     inputsreg[parameter == "pig_mort_effect", "Med"] <- rbeta(1, as.numeric(inputsreg[parameter == "pig_mort_effect", "LIC param 1"]),
#'                                                               as.numeric(inputsreg[parameter == "pig_mort_effect", "LIC param 2"]))
#'   }
#'   
#'   LIC_reg_matrix[i, "pig_mort_effect"] <- inputsreg[parameter == "pig_mort_effect", "Med"]
#'   
#'   s <- runif(1)
#'   
#'   if(s < 0.9){
#'     inputsreg[parameter == "chicken_mort_effect", "Med"] <- -1 * rbeta(1, as.numeric(inputsreg[parameter == "chicken_mort_effect", "LIC param 1"]),
#'                                                                        as.numeric(inputsreg[parameter == "chicken_mort_effect", "LIC param 2"]))
#'   } else if (s >= 0.9){
#'     inputsreg[parameter == "chicken_mort_effect", "Med"] <- rbeta(1, as.numeric(inputsreg[parameter == "chicken_mort_effect", "LIC param 1"]),
#'                                                                   as.numeric(inputsreg[parameter == "chicken_mort_effect", "LIC param 2"]))
#'   }
#'   
#'   LIC_reg_matrix[i, "chicken_mort_effect"] <- inputsreg[parameter == "chicken_mort_effect", "Med"]
#'   
#'   LIC_reg_matrix[i, "Output"] <- as.data.frame(Model(inputsreg, scenario_income, scenario_prod, scenario_transmission, scenario_farm_effect))[1,1]
#'   
#' }
#' 
#' write.xlsx(LIC_reg_matrix, "Outputs/reg matrix LIC.xlsx")
#' 
#' ### MIC
#' 
#' scenario_income <- "MIC-S"
#' 
#' MIC_reg_matrix <- matrix(rep(0), nrow = number_runs, ncol = nrow(inputs_general)+1)
#' 
#' colnames(MIC_reg_matrix) <- c("Output", rep(0, nrow(inputs_general)))
#' 
#' for(i in 2:ncol(MIC_reg_matrix)){
#'   colnames(MIC_reg_matrix)[i] <- inputs_general[i-1,1]
#' }
#' 
#' MIC_reg_matrix <- as.data.frame(MIC_reg_matrix)
#' 
#' inputs_reg_MIC <- read.csv(here("inputs - general model.csv"))
#' inputs_reg_MIC <- as.data.table(inputs_reg_MIC)
#' colnames(inputs_reg_MIC) <- c("parameter", "description", "HIC", "MIC-I", "MIC-S", "LIC", "Value", "Min", "Lo", "Med", "Hi", "Max", 
#'                                "LIC min", "LIC max", "MIC min", "MIC max", "HIC min", "HIC max",
#'                                "Distribution", "LIC param 1", "LIC param 2", "MIC param 1", "MIC param 2",
#'                                "HIC param 1", "HIC param 2")
#' 
#' 
#' set.seed(42069)
#' 
#' for(i in 1:number_runs){
#'   inputsreg <- inputs_reg_MIC
#'   
#'   inputsreg[parameter == "well_sick", "MIC"] <- rbeta(1, as.numeric(inputsreg[parameter == "well_sick", "MIC param 1"]), 
#'                                                       as.numeric(inputsreg[parameter == "well_sick", "MIC param 2"]))
#'   
#'   MIC_reg_matrix[i, "well_sick"] <- inputsreg[parameter == "well_sick", "MIC"]
#'   
#'   inputsreg[parameter == "portion_res", "MIC"] <- rbeta(1, as.numeric(inputsreg[parameter == "portion_res", "MIC param 1"]), 
#'                                                         as.numeric(inputsreg[parameter == "portion_res", "MIC param 2"]))
#'   
#'   MIC_reg_matrix[i, "portion_res"] <- inputsreg[parameter == "portion_res", "MIC"]
#'   
#'   inputsreg[parameter == "mort_res", "MIC"] <- 1.62 * rbeta(1,as.numeric(inputsreg[parameter == "mort_res", "MIC param 1"]), 
#'                                                             as.numeric(inputsreg[parameter == "mort_res", "MIC param 2"]))
#'   
#'   MIC_reg_matrix[i, "mort_res"] <- inputsreg[parameter == "mort_res", "MIC"]
#'   
#'   inputsreg[parameter == "mort_sus", "MIC"] <- rbeta(1, as.numeric(inputsreg[parameter == "mort_sus", "MIC param 1"]), 
#'                                                      as.numeric(inputsreg[parameter == "mort_sus", "MIC param 2"]))
#'   
#'   MIC_reg_matrix[i, "mort_sus"] <- inputsreg[parameter == "mort_sus", "MIC"]
#'   
#'   inputsreg[parameter == "los_sus", "MIC"] <- (1/365.25) * rlnorm(1, as.numeric(inputsreg[parameter == "los_sus", "MIC param 1"]), 
#'                                                                   as.numeric(inputsreg[parameter == "los_sus", "MIC param 2"]))
#'   
#'   MIC_reg_matrix[i, "los_sus"] <- inputsreg[parameter == "los_sus", "MIC"]
#'   
#'   inputsreg[parameter == "los_res", "MIC"] <- (1.27/365.25) * rlnorm(1, as.numeric(inputsreg[parameter == "los_res", "MIC param 1"]), 
#'                                                                      as.numeric(inputsreg[parameter == "los_res", "MIC param 2"]))
#'   
#'   MIC_reg_matrix[i, "los_res"] <- inputsreg[parameter == "los_res", "MIC"]
#'   
#'   inputsreg[parameter == "bed_day_cost", "MIC"] <- rlnorm(1, as.numeric(inputsreg[parameter == "bed_day_cost", "MIC param 1"]), 
#'                                                           as.numeric(inputsreg[parameter == "bed_day_cost", "MIC param 2"]))
#'   
#'   MIC_reg_matrix[i, "bed_day_cost"] <- inputsreg[parameter == "bed_day_cost", "MIC"]
#'   
#'   inputsreg[parameter == "qol_sick", "MIC"] <- rbeta(1, as.numeric(inputsreg[parameter == "qol_sick", "MIC param 1"]), 
#'                                                      as.numeric(inputsreg[parameter == "qol_sick", "MIC param 2"]))
#'   
#'   MIC_reg_matrix[i, "qol_sick"] <- inputsreg[parameter == "qol_sick", "MIC"]
#'   
#'   inputsreg[parameter == "qol_seq", "MIC"] <- rbeta(1, as.numeric(inputsreg[parameter == "qol_seq", "MIC param 1"]), 
#'                                                     as.numeric(inputsreg[parameter == "qol_seq", "MIC param 2"]))
#'   
#'   MIC_reg_matrix[i, "qol_seq"] <- inputsreg[parameter == "qol_seq", "MIC"]
#'   
#'   inputsreg[parameter == "amr_grow", "MIC"] <- 0.01 * rgamma(1, as.numeric(inputsreg[parameter == "amr_grow", "MIC param 1"]), 
#'                                                              scale = as.numeric(inputsreg[parameter == "amr_grow", "MIC param 2"]))
#'   
#'   MIC_reg_matrix[i, "amr_grow"] <- inputsreg[parameter == "amr_grow", "MIC"]
#'   
#'   inputsreg[parameter == "n_pigs", "MIC"] <- rlnorm(1, as.numeric(inputsreg[parameter == "n_pigs", "MIC param 1"]), 
#'                                                     as.numeric(inputsreg[parameter == "n_pigs", "MIC param 2"]))
#'   
#'   MIC_reg_matrix[i, "n_pigs"] <- inputsreg[parameter == "n_pigs", "MIC"]
#'   
#'   inputsreg[parameter == "n_chickens", "MIC"] <- rlnorm(1, as.numeric(inputsreg[parameter == "n_chickens", "MIC param 1"]), 
#'                                                         as.numeric(inputsreg[parameter == "n_chickens", "MIC param 2"]))
#'   
#'   MIC_reg_matrix[i, "n_chickens"] <- inputsreg[parameter == "n_chickens", "MIC"]
#'   
#'   inputsreg[parameter == "n_chickens_farm_ind", "MIC"] <- rlnorm(1, as.numeric(inputsreg[parameter == "n_chickens_farm_ind", "MIC param 1"]), 
#'                                                                  as.numeric(inputsreg[parameter == "n_chickens_farm_ind", "MIC param 2"]))
#'   
#'   MIC_reg_matrix[i, "n_chickens_farm_ind"] <- inputsreg[parameter == "n_chickens_farm_ind", "MIC"]
#'   
#'   inputsreg[parameter == "n_chickens_farm_small", "MIC"] <- rlnorm(1, as.numeric(inputsreg[parameter == "n_chickens_farm_small", "MIC param 1"]), 
#'                                                                    as.numeric(inputsreg[parameter == "n_chickens_farm_small", "MIC param 2"]))
#'   
#'   MIC_reg_matrix[i, "n_chickens_farm_small"] <- inputsreg[parameter == "n_chickens_farm_small", "MIC"]
#'   
#'   inputsreg[parameter == "n_pigs_farm_ind", "MIC"] <- rlnorm(1, as.numeric(inputsreg[parameter == "n_pigs_farm_ind", "MIC param 1"]), 
#'                                                              as.numeric(inputsreg[parameter == "n_pigs_farm_ind", "MIC param 2"]))
#'   
#'   MIC_reg_matrix[i, "n_pigs_farm_ind"] <- inputsreg[parameter == "n_pigs_farm_ind", "MIC"]
#'   
#'   inputsreg[parameter == "n_pigs_farm_small", "MIC"] <- rlnorm(1, as.numeric(inputsreg[parameter == "n_pigs_farm_small", "MIC param 1"]), 
#'                                                                as.numeric(inputsreg[parameter == "n_pigs_farm_small", "MIC param 2"]))
#'   
#'   MIC_reg_matrix[i, "n_pigs_farm_small"] <- inputsreg[parameter == "n_pigs_farm_small", "MIC"]
#'   
#'   inputsreg[parameter == "portion_animals_ind", "MIC"] <- rbeta(1, as.numeric(inputsreg[parameter == "portion_animals_ind", "MIC param 1"]), 
#'                                                                 as.numeric(inputsreg[parameter == "portion_animals_ind", "MIC param 2"]))
#'   
#'   MIC_reg_matrix[i, "portion_animals_ind"] <- inputsreg[parameter == "portion_animals_ind", "MIC"]
#'   
#'   inputsreg[parameter == "pig_price", "MIC"] <- rlnorm(1, as.numeric(inputsreg[parameter == "pig_price", "MIC param 1"]), 
#'                                                        as.numeric(inputsreg[parameter == "pig_price", "MIC param 2"]))
#'   
#'   MIC_reg_matrix[i, "pig_price"] <- inputsreg[parameter == "pig_price", "MIC"]
#'   
#'   inputsreg[parameter == "chicken_price", "MIC"] <- rlnorm(1, as.numeric(inputsreg[parameter == "chicken_price", "MIC param 1"]), 
#'                                                            as.numeric(inputsreg[parameter == "chicken_price", "MIC param 2"]))
#'   
#'   MIC_reg_matrix[i, "chicken_price"] <- inputsreg[parameter == "chicken_price", "MIC"]
#'   
#'   inputsreg[parameter == "c_mort_ind", "MIC"] <- rbeta(1, as.numeric(inputsreg[parameter == "c_mort_ind", "MIC param 1"]), 
#'                                                        as.numeric(inputsreg[parameter == "c_mort_ind", "MIC param 2"]))
#'   
#'   MIC_reg_matrix[i, "c_mort_ind"] <- inputsreg[parameter == "c_mort_ind", "MIC"]
#'   
#'   inputsreg[parameter == "c_mort_small", "MIC"] <- rbeta(1, as.numeric(inputsreg[parameter == "c_mort_small", "MIC param 1"]), 
#'                                                          as.numeric(inputsreg[parameter == "c_mort_small", "MIC param 2"]))
#'   
#'   MIC_reg_matrix[i, "c_mort_small"] <- inputsreg[parameter == "c_mort_small", "MIC"]
#'   
#'   inputsreg[parameter == "p_mort_ind", "MIC"] <- rbeta(1, as.numeric(inputsreg[parameter == "p_mort_ind", "MIC param 1"]), 
#'                                                        as.numeric(inputsreg[parameter == "p_mort_ind", "MIC param 2"]))
#'   
#'   MIC_reg_matrix[i, "p_mort_ind"] <- inputsreg[parameter == "p_mort_ind", "MIC"]
#'   
#'   inputsreg[parameter == "p_mort_small", "MIC"] <- rbeta(1, as.numeric(inputsreg[parameter == "p_mort_small", "MIC param 1"]), 
#'                                                          as.numeric(inputsreg[parameter == "p_mort_small", "MIC param 2"]))
#'   
#'   MIC_reg_matrix[i, "p_mort_small"] <- inputsreg[parameter == "p_mort_small", "MIC"]
#'   
#'   inputsreg[parameter == "res_change", "Med"] <- -1 * rbeta(1, as.numeric(inputsreg[parameter == "res_change", "MIC param 1"]), 
#'                                                             as.numeric(inputsreg[parameter == "res_change", "MIC param 2"]))
#'   
#'   MIC_reg_matrix[i, "res_change"] <- inputsreg[parameter == "res_change", "Med"]
#'   
#'   inputsreg[parameter == "seq_sus", "MIC"] <- 1.54158796 * rbeta(1, as.numeric(inputsreg[parameter == "seq_sus", "MIC param 1"]), 
#'                                                   as.numeric(inputsreg[parameter == "seq_sus", "MIC param 2"]))
#'   
#'   MIC_reg_matrix[i, "seq_sus"] <- inputsreg[parameter == "seq_sus", "MIC"]
#'   
#'   inputsreg[parameter == "seq_res", "MIC"] <- 2.4973725 * rbeta(1, as.numeric(inputsreg[parameter == "seq_res", "MIC param 1"]), 
#'                                                          as.numeric(inputsreg[parameter == "seq_res", "MIC param 2"]))
#'   
#'   MIC_reg_matrix[i, "seq_res"] <- inputsreg[parameter == "seq_res", "MIC"]
#'   
#'   q <- runif(1)
#'   
#'   if(q < 0.1){
#'     inputsreg[parameter == "pig_income_effect", "Med"] <- -1 * rbeta(1, as.numeric(inputsreg[parameter == "pig_income_effect", "MIC param 1"]),
#'                                                                      as.numeric(inputsreg[parameter == "pig_income_effect", "MIC param 2"]))
#'   } else if (q >= 0.1){
#'     inputsreg[parameter == "pig_income_effect", "Med"] <- rbeta(1, as.numeric(inputsreg[parameter == "pig_income_effect", "MIC param 1"]),
#'                                                                 as.numeric(inputsreg[parameter == "pig_income_effect", "MIC param 2"]))
#'   }
#'   
#'   MIC_reg_matrix[i, "pig_income_effect"] <- inputsreg[parameter == "pig_income_effect", "Med"]
#'   
#'   p <- runif(1)
#'   
#'   if(p < 0.1){
#'     inputsreg[parameter == "chicken_income_effect", "Med"] <- -1 * rbeta(1, as.numeric(inputsreg[parameter == "chicken_income_effect", "MIC param 1"]),
#'                                                                          as.numeric(inputsreg[parameter == "chicken_income_effect", "MIC param 2"]))
#'   } else if (p >= 0.1){
#'     inputsreg[parameter == "chicken_income_effect", "Med"] <- rbeta(1, as.numeric(inputsreg[parameter == "chicken_income_effect", "MIC param 1"]),
#'                                                                     as.numeric(inputsreg[parameter == "chicken_income_effect", "MIC param 2"]))
#'   }
#'   
#'   MIC_reg_matrix[i, "chicken_income_effect"] <- inputsreg[parameter == "chicken_income_effect", "Med"]
#'   
#'   r <- runif(1)
#'   
#'   if(r < 0.9){
#'     inputsreg[parameter == "pig_mort_effect", "Med"] <- -1 * rbeta(1, as.numeric(inputsreg[parameter == "pig_mort_effect", "MIC param 1"]),
#'                                                                    as.numeric(inputsreg[parameter == "pig_mort_effect", "MIC param 2"]))
#'   } else if (r >= 0.9){
#'     inputsreg[parameter == "pig_mort_effect", "Med"] <- rbeta(1, as.numeric(inputsreg[parameter == "pig_mort_effect", "MIC param 1"]),
#'                                                               as.numeric(inputsreg[parameter == "pig_mort_effect", "MIC param 2"]))
#'   }
#'   
#'   MIC_reg_matrix[i, "pig_mort_effect"] <- inputsreg[parameter == "pig_mort_effect", "Med"]
#'   
#'   s <- runif(1)
#'   
#'   if(s < 0.9){
#'     inputsreg[parameter == "chicken_mort_effect", "Med"] <- -1 * rbeta(1, as.numeric(inputsreg[parameter == "chicken_mort_effect", "MIC param 1"]),
#'                                                                        as.numeric(inputsreg[parameter == "chicken_mort_effect", "MIC param 2"]))
#'   } else if (s >= 0.9){
#'     inputsreg[parameter == "chicken_mort_effect", "Med"] <- rbeta(1, as.numeric(inputsreg[parameter == "chicken_mort_effect", "MIC param 1"]),
#'                                                                   as.numeric(inputsreg[parameter == "chicken_mort_effect", "MIC param 2"]))
#'   }
#'   
#'   MIC_reg_matrix[i, "chicken_mort_effect"] <- inputsreg[parameter == "chicken_mort_effect", "Med"]
#'   
#'   MIC_reg_matrix[i, "Output"] <- as.data.frame(Model(inputsreg, scenario_income, scenario_prod, scenario_transmission, scenario_farm_effect))[1,1]
#'   
#' }
#' 
#' write.xlsx(MIC_reg_matrix, "Outputs/reg matrix MIC.xlsx")
#' 
#' ###HIC
#' 
#' scenario_income <- "HIC"
#' 
#' HIC_reg_matrix <- matrix(rep(0), nrow = number_runs, ncol = nrow(inputs_general)+1)
#' 
#' colnames(HIC_reg_matrix) <- c("Output", rep(0, nrow(inputs_general)))
#' 
#' for(i in 2:ncol(HIC_reg_matrix)){
#'   colnames(HIC_reg_matrix)[i] <- inputs_general[i-1,1]
#' }
#' 
#' HIC_reg_matrix <- as.data.frame(HIC_reg_matrix)
#' 
#' inputs_reg_HIC <- read.csv(here("inputs - general model.csv"))
#' inputs_reg_HIC <- as.data.table(inputs_reg_HIC)
#' colnames(inputs_reg_HIC) <- c("parameter", "description", "HIC", "MIC-I", "MIC-S", "LIC", "Value", "Min", "Lo", "Med", "Hi", "Max", 
#'                               "LIC min", "LIC max", "MIC min", "MIC max", "HIC min", "HIC max",
#'                               "Distribution", "LIC param 1", "LIC param 2", "MIC param 1", "MIC param 2",
#'                               "HIC param 1", "HIC param 2")
#' 
#' 
#' set.seed(42069)
#' 
#' for(i in 1:number_runs){
#'   inputsreg <- inputs_reg_HIC
#'   
#'   inputsreg[parameter == "well_sick", "HIC"] <- rbeta(1, as.numeric(inputsreg[parameter == "well_sick", "HIC param 1"]), 
#'                                                       as.numeric(inputsreg[parameter == "well_sick", "HIC param 2"]))
#'   
#'   HIC_reg_matrix[i, "well_sick"] <- inputsreg[parameter == "well_sick", "HIC"]
#'   
#'   inputsreg[parameter == "portion_res", "HIC"] <- rbeta(1, as.numeric(inputsreg[parameter == "portion_res", "HIC param 1"]), 
#'                                                         as.numeric(inputsreg[parameter == "portion_res", "HIC param 2"]))
#'   
#'   HIC_reg_matrix[i, "portion_res"] <- inputsreg[parameter == "portion_res", "HIC"]
#'   
#'   inputsreg[parameter == "mort_res", "HIC"] <- 1.62 * rbeta(1,as.numeric(inputsreg[parameter == "mort_res", "HIC param 1"]), 
#'                                                             as.numeric(inputsreg[parameter == "mort_res", "HIC param 2"]))
#'   
#'   HIC_reg_matrix[i, "mort_res"] <- inputsreg[parameter == "mort_res", "HIC"]
#'   
#'   inputsreg[parameter == "mort_sus", "HIC"] <- rbeta(1, as.numeric(inputsreg[parameter == "mort_sus", "HIC param 1"]), 
#'                                                      as.numeric(inputsreg[parameter == "mort_sus", "HIC param 2"]))
#'   
#'   HIC_reg_matrix[i, "mort_sus"] <- inputsreg[parameter == "mort_sus", "HIC"]
#'   
#'   inputsreg[parameter == "los_sus", "HIC"] <- (1/365.25) * rlnorm(1, as.numeric(inputsreg[parameter == "los_sus", "HIC param 1"]), 
#'                                                                   as.numeric(inputsreg[parameter == "los_sus", "HIC param 2"]))
#'   
#'   HIC_reg_matrix[i, "los_sus"] <- inputsreg[parameter == "los_sus", "HIC"]
#'   
#'   inputsreg[parameter == "los_res", "HIC"] <- (1.27/365.25) * rlnorm(1, as.numeric(inputsreg[parameter == "los_res", "HIC param 1"]), 
#'                                                                      as.numeric(inputsreg[parameter == "los_res", "HIC param 2"]))
#'   
#'   HIC_reg_matrix[i, "los_res"] <- inputsreg[parameter == "los_res", "HIC"]
#'   
#'   inputsreg[parameter == "bed_day_cost", "HIC"] <- rlnorm(1, as.numeric(inputsreg[parameter == "bed_day_cost", "HIC param 1"]), 
#'                                                           as.numeric(inputsreg[parameter == "bed_day_cost", "HIC param 2"]))
#'   
#'   HIC_reg_matrix[i, "bed_day_cost"] <- inputsreg[parameter == "bed_day_cost", "HIC"]
#'   
#'   inputsreg[parameter == "qol_sick", "HIC"] <- rbeta(1, as.numeric(inputsreg[parameter == "qol_sick", "HIC param 1"]), 
#'                                                      as.numeric(inputsreg[parameter == "qol_sick", "HIC param 2"]))
#'   
#'   HIC_reg_matrix[i, "qol_sick"] <- inputsreg[parameter == "qol_sick", "HIC"]
#'   
#'   inputsreg[parameter == "qol_seq", "HIC"] <- rbeta(1, as.numeric(inputsreg[parameter == "qol_seq", "HIC param 1"]), 
#'                                                     as.numeric(inputsreg[parameter == "qol_seq", "HIC param 2"]))
#'   
#'   HIC_reg_matrix[i, "qol_seq"] <- inputsreg[parameter == "qol_seq", "HIC"]
#'   
#'   inputsreg[parameter == "amr_grow", "HIC"] <- 0.01 * rgamma(1, as.numeric(inputsreg[parameter == "amr_grow", "HIC param 1"]), 
#'                                                              scale = as.numeric(inputsreg[parameter == "amr_grow", "HIC param 2"]))
#'   
#'   HIC_reg_matrix[i, "amr_grow"] <- inputsreg[parameter == "amr_grow", "HIC"]
#'   
#'   inputsreg[parameter == "n_pigs", "HIC"] <- rlnorm(1, as.numeric(inputsreg[parameter == "n_pigs", "HIC param 1"]), 
#'                                                     as.numeric(inputsreg[parameter == "n_pigs", "HIC param 2"]))
#'   
#'   HIC_reg_matrix[i, "n_pigs"] <- inputsreg[parameter == "n_pigs", "HIC"]
#'   
#'   inputsreg[parameter == "n_chickens", "HIC"] <- rlnorm(1, as.numeric(inputsreg[parameter == "n_chickens", "HIC param 1"]), 
#'                                                         as.numeric(inputsreg[parameter == "n_chickens", "HIC param 2"]))
#'   
#'   HIC_reg_matrix[i, "n_chickens"] <- inputsreg[parameter == "n_chickens", "HIC"]
#'   
#'   inputsreg[parameter == "n_chickens_farm_ind", "HIC"] <- rlnorm(1, as.numeric(inputsreg[parameter == "n_chickens_farm_ind", "HIC param 1"]), 
#'                                                                  as.numeric(inputsreg[parameter == "n_chickens_farm_ind", "HIC param 2"]))
#'   
#'   HIC_reg_matrix[i, "n_chickens_farm_ind"] <- inputsreg[parameter == "n_chickens_farm_ind", "HIC"]
#'   
#'   inputsreg[parameter == "n_chickens_farm_small", "HIC"] <- rlnorm(1, as.numeric(inputsreg[parameter == "n_chickens_farm_small", "HIC param 1"]), 
#'                                                                    as.numeric(inputsreg[parameter == "n_chickens_farm_small", "HIC param 2"]))
#'   
#'   HIC_reg_matrix[i, "n_chickens_farm_small"] <- inputsreg[parameter == "n_chickens_farm_small", "HIC"]
#'   
#'   inputsreg[parameter == "n_pigs_farm_ind", "HIC"] <- rlnorm(1, as.numeric(inputsreg[parameter == "n_pigs_farm_ind", "HIC param 1"]), 
#'                                                              as.numeric(inputsreg[parameter == "n_pigs_farm_ind", "HIC param 2"]))
#'   
#'   HIC_reg_matrix[i, "n_pigs_farm_ind"] <- inputsreg[parameter == "n_pigs_farm_ind", "HIC"]
#'   
#'   inputsreg[parameter == "n_pigs_farm_small", "HIC"] <- rlnorm(1, as.numeric(inputsreg[parameter == "n_pigs_farm_small", "HIC param 1"]), 
#'                                                                as.numeric(inputsreg[parameter == "n_pigs_farm_small", "HIC param 2"]))
#'   
#'   HIC_reg_matrix[i, "n_pigs_farm_small"] <- inputsreg[parameter == "n_pigs_farm_small", "HIC"]
#'   
#'   inputsreg[parameter == "portion_animals_ind", "HIC"] <- rbeta(1, as.numeric(inputsreg[parameter == "portion_animals_ind", "HIC param 1"]), 
#'                                                                 as.numeric(inputsreg[parameter == "portion_animals_ind", "HIC param 2"]))
#'   
#'   HIC_reg_matrix[i, "portion_animals_ind"] <- inputsreg[parameter == "portion_animals_ind", "HIC"]
#'   
#'   inputsreg[parameter == "pig_price", "HIC"] <- rlnorm(1, as.numeric(inputsreg[parameter == "pig_price", "HIC param 1"]), 
#'                                                        as.numeric(inputsreg[parameter == "pig_price", "HIC param 2"]))
#'   
#'   HIC_reg_matrix[i, "pig_price"] <- inputsreg[parameter == "pig_price", "HIC"]
#'   
#'   inputsreg[parameter == "chicken_price", "HIC"] <- rlnorm(1, as.numeric(inputsreg[parameter == "chicken_price", "HIC param 1"]), 
#'                                                            as.numeric(inputsreg[parameter == "chicken_price", "HIC param 2"]))
#'   
#'   HIC_reg_matrix[i, "chicken_price"] <- inputsreg[parameter == "chicken_price", "HIC"]
#'   
#'   inputsreg[parameter == "c_mort_ind", "HIC"] <- rbeta(1, as.numeric(inputsreg[parameter == "c_mort_ind", "HIC param 1"]), 
#'                                                        as.numeric(inputsreg[parameter == "c_mort_ind", "HIC param 2"]))
#'   
#'   HIC_reg_matrix[i, "c_mort_ind"] <- inputsreg[parameter == "c_mort_ind", "HIC"]
#'   
#'   inputsreg[parameter == "c_mort_small", "HIC"] <- rbeta(1, as.numeric(inputsreg[parameter == "c_mort_small", "HIC param 1"]), 
#'                                                          as.numeric(inputsreg[parameter == "c_mort_small", "HIC param 2"]))
#'   
#'   HIC_reg_matrix[i, "c_mort_small"] <- inputsreg[parameter == "c_mort_small", "HIC"]
#'   
#'   inputsreg[parameter == "p_mort_ind", "HIC"] <- rbeta(1, as.numeric(inputsreg[parameter == "p_mort_ind", "HIC param 1"]), 
#'                                                        as.numeric(inputsreg[parameter == "p_mort_ind", "HIC param 2"]))
#'   
#'   HIC_reg_matrix[i, "p_mort_ind"] <- inputsreg[parameter == "p_mort_ind", "HIC"]
#'   
#'   inputsreg[parameter == "p_mort_small", "HIC"] <- rbeta(1, as.numeric(inputsreg[parameter == "p_mort_small", "HIC param 1"]), 
#'                                                          as.numeric(inputsreg[parameter == "p_mort_small", "HIC param 2"]))
#'   
#'   HIC_reg_matrix[i, "p_mort_small"] <- inputsreg[parameter == "p_mort_small", "HIC"]
#'   
#'   inputsreg[parameter == "res_change", "Med"] <- -1 * rbeta(1, as.numeric(inputsreg[parameter == "res_change", "HIC param 1"]), 
#'                                                             as.numeric(inputsreg[parameter == "res_change", "HIC param 2"]))
#'   
#'   HIC_reg_matrix[i, "res_change"] <- inputsreg[parameter == "res_change", "Med"]
#'   
#'   inputsreg[parameter == "seq_sus", "HIC"] <- rbeta(1, as.numeric(inputsreg[parameter == "seq_sus", "HIC param 1"]), 
#'                                                   as.numeric(inputsreg[parameter == "seq_sus", "HIC param 2"]))
#'   
#'   HIC_reg_matrix[i, "seq_sus"] <- inputsreg[parameter == "seq_sus", "HIC"]
#'   
#'   inputsreg[parameter == "seq_res", "HIC"] <- 1.62 * rbeta(1, as.numeric(inputsreg[parameter == "seq_res", "HIC param 1"]), 
#'                                                   as.numeric(inputsreg[parameter == "seq_res", "HIC param 2"]))
#'   
#'   HIC_reg_matrix[i, "seq_res"] <- inputsreg[parameter == "seq_res", "HIC"]
#'   
#'   q <- runif(1)
#'   
#'   if(q < 0.1){
#'     inputsreg[parameter == "pig_income_effect", "Med"] <- -1 * rbeta(1, as.numeric(inputsreg[parameter == "pig_income_effect", "HIC param 1"]),
#'                                                                      as.numeric(inputsreg[parameter == "pig_income_effect", "HIC param 2"]))
#'   } else if (q >= 0.1){
#'     inputsreg[parameter == "pig_income_effect", "Med"] <- rbeta(1, as.numeric(inputsreg[parameter == "pig_income_effect", "HIC param 1"]),
#'                                                                 as.numeric(inputsreg[parameter == "pig_income_effect", "HIC param 2"]))
#'   }
#'   
#'   HIC_reg_matrix[i, "pig_income_effect"] <- inputsreg[parameter == "pig_income_effect", "Med"]
#'   
#'   p <- runif(1)
#'   
#'   if(p < 0.1){
#'     inputsreg[parameter == "chicken_income_effect", "Med"] <- -1 * rbeta(1, as.numeric(inputsreg[parameter == "chicken_income_effect", "HIC param 1"]),
#'                                                                          as.numeric(inputsreg[parameter == "chicken_income_effect", "HIC param 2"]))
#'   } else if (p >= 0.1){
#'     inputsreg[parameter == "chicken_income_effect", "Med"] <- rbeta(1, as.numeric(inputsreg[parameter == "chicken_income_effect", "MIC param 1"]),
#'                                                                     as.numeric(inputsreg[parameter == "chicken_income_effect", "MIC param 2"]))
#'   }
#'   
#'   HIC_reg_matrix[i, "chicken_income_effect"] <- inputsreg[parameter == "chicken_income_effect", "Med"]
#'   
#'   r <- runif(1)
#'   
#'   if(r < 0.9){
#'     inputsreg[parameter == "pig_mort_effect", "Med"] <- -1 * rbeta(1, as.numeric(inputsreg[parameter == "pig_mort_effect", "HIC param 1"]),
#'                                                                    as.numeric(inputsreg[parameter == "pig_mort_effect", "HIC param 2"]))
#'   } else if (r >= 0.9){
#'     inputsreg[parameter == "pig_mort_effect", "Med"] <- rbeta(1, as.numeric(inputsreg[parameter == "pig_mort_effect", "HIC param 1"]),
#'                                                               as.numeric(inputsreg[parameter == "pig_mort_effect", "HIC param 2"]))
#'   }
#'   
#'   HIC_reg_matrix[i, "pig_mort_effect"] <- inputsreg[parameter == "pig_mort_effect", "Med"]
#'   
#'   s <- runif(1)
#'   
#'   if(s < 0.9){
#'     inputsreg[parameter == "chicken_mort_effect", "Med"] <- -1 * rbeta(1, as.numeric(inputsreg[parameter == "chicken_mort_effect", "HIC param 1"]),
#'                                                                        as.numeric(inputsreg[parameter == "chicken_mort_effect", "HIC param 2"]))
#'   } else if (s >= 0.9){
#'     inputsreg[parameter == "chicken_mort_effect", "Med"] <- rbeta(1, as.numeric(inputsreg[parameter == "chicken_mort_effect", "HIC param 1"]),
#'                                                                   as.numeric(inputsreg[parameter == "chicken_mort_effect", "HIC param 2"]))
#'   }
#'   
#'   HIC_reg_matrix[i, "chicken_mort_effect"] <- inputsreg[parameter == "chicken_mort_effect", "Med"]
#'   
#'   HIC_reg_matrix[i, "Output"] <- as.data.frame(Model(inputsreg, scenario_income, scenario_prod, scenario_transmission, scenario_farm_effect))[1,1]
#'   
#' }
#' 
#' write.xlsx(HIC_reg_matrix, "Outputs/reg matrix HIC.xlsx")
#' 
#' ##regressions 
#' 
#' #'Human parameters to include in the plots:
#' 
#' #'Animal parameters to include:
#' 
#' ###HIC
#' 
#' HIC_reg <- lm(Output ~ pig_income_effect + chicken_income_effect + pig_mort_effect +
#'                 chicken_mort_effect + res_change + well_sick + portion_res + mort_res +
#'                 mort_sus + los_sus + los_res + bed_day_cost + qol_sick + qol_seq +
#'                 amr_grow + n_pigs + n_chickens + n_chickens_farm_ind + n_chickens_farm_small +
#'                 n_pigs_farm_ind + n_pigs_farm_small + portion_animals_ind, data = HIC_reg_matrix)
#' 
#' stargazer(HIC_reg, type = "text") 
#' #'significant: pig and chicken income effects, affect on human AMR, disease incidence,
#' #'background AMR prevalence, mortality from resistant infection (pos),
#' #'QoL from sickness, LoS from resistant infections, AMR growth rate,
#' #'portion of animals in industrial farms
#' 
#' rsq.partial(HIC_reg) #
#' 
#' HIC_prsq <- rsq.partial(HIC_reg)
#' 
#' HIC_prsq_var <- as.vector(HIC_prsq$variable)
#' HIC_prsq_val <- as.vector(HIC_prsq$partial.rsq)
#' 
#' barplot(HIC_prsq_val,
#'         names.arg = HIC_prsq_var,
#'         main = "Partial R^2")
#' 
#' HIC_prsq_val2 <- c(HIC_prsq_val[5], HIC_prsq_val[2], HIC_prsq_val[1], HIC_prsq_val[7],
#'                   HIC_prsq_val[6], HIC_prsq_val[8])
#' 
#' HIC_prsq_var2 <- c("change in human AMR", "effect on chicken productivity", "effect on pig productivity",
#'                    "background AMR prevalence", "disease incidence", "mortality from resistant infections")
#' 
#' jpeg("Outputs/Table 2 A.jpg")
#' 
#' barplot(HIC_prsq_val2,
#'         names.arg = HIC_prsq_var2,
#'         main = "Portion of Variation in Cost-Effectiveness Explained by Variation in Each Parameter - HICs")
#' 
#' dev.off()
#' 
#' #'chicken income effect, then change in human AMR, then background AMR,
#' #'then pig income effect, then disease incidence, then mortality from resistant
#' #'infections
#' 
#' 
#' ###MIC
#' 
#' MIC_reg <- lm(Output ~ pig_income_effect + chicken_income_effect + pig_mort_effect +
#'                            chicken_mort_effect + res_change + well_sick + portion_res + mort_res +
#'                            mort_sus + los_sus + los_res + bed_day_cost + qol_sick + qol_seq +
#'                            amr_grow + n_pigs + n_chickens + n_chickens_farm_ind + n_chickens_farm_small +
#'                            n_pigs_farm_ind + n_pigs_farm_small + portion_animals_ind, data = MIC_reg_matrix)
#' 
#' stargazer(MIC_reg, type = "text") 
#' #'significant: pig and chicken income effects, change in human AMR, 
#' #'number of chickens (10%), number of chickens in industrial farms (10%)
#' 
#' rsq.partial(MIC_reg) #
#' 
#' MIC_prsq <- rsq.partial(MIC_reg)
#' 
#' MIC_prsq_var <- MIC_prsq$variable
#' MIC_prsq_val <- MIC_prsq$partial.rsq
#' 
#' barplot(MIC_prsq_val,
#'         names.arg = MIC_prsq_var,
#'         main = "Portion of Variation in Cost-Effectiveness Explained by Variation in Each Parameter - MICs")
#' 
#' MIC_prsq_val2 <- c(MIC_prsq_val[5], MIC_prsq_val[1], MIC_prsq_val[2])
#' MIC_prsq_var2 <- c("Effect on Human AMR", "Effect on Pig Productivity",
#'                    "Effect on Chicken Productivity")
#' 
#' jpeg("Outputs/Table 2 B.jpg")
#' 
#' barplot(MIC_prsq_val2,
#'         names.arg = MIC_prsq_var2,
#'         main = "Portion of Variation in Cost-Effectiveness Explained by Variation in Each Parameter - MICs")
#' 
#' dev.off()
#' 
#' #'Effect on human AMR, then effect on chicken productivity, then effect on pig
#' #'productivity, were similarly important and completely dominated
#' 
#' ###LIC
#' 
#' LIC_reg <- lm(Output ~ pig_income_effect + chicken_income_effect + pig_mort_effect +
#'                 chicken_mort_effect + res_change + well_sick + portion_res + mort_res +
#'                 mort_sus + los_sus + los_res + bed_day_cost + qol_sick + qol_seq +
#'                 amr_grow + n_pigs + n_chickens + n_chickens_farm_ind + n_chickens_farm_small +
#'                 n_pigs_farm_ind + n_pigs_farm_small + portion_animals_ind, data = LIC_reg_matrix)
#' 
#' stargazer(LIC_reg, type = "text") 
#' #'Significant: pig and chicken income effects, effect on human AMR, background disease prevalence,
#' #'background AMR prevalence, mortality from resistant infections (positive), 
#' #'mortality from susceptible infections (negative), LoS for resistant infections, AMR growth rate
#' 
#' rsq.partial(LIC_reg) 
#' #'big ones were pig and chicken income effect, background sickness, effect on human AMR,
#' #'mortality from resistant infections, and portion of animals in industrial farms
#' 
#' LIC_prsq <- rsq.partial(LIC_reg) 
#' 
#' LIC_prsq_var <- LIC_prsq$variable
#' LIC_prsq_val <- LIC_prsq$partial.rsq
#' 
#' barplot(LIC_prsq_val,
#'         names.arg = LIC_prsq_var,
#'         main = "Portion of Variation in Cost-Effectiveness Explained by Variation in Each Parameter - LICs")
#' 
#' LIC_prsq_val2 <- c(LIC_prsq_val[2], LIC_prsq_val[5], LIC_prsq_val[1], LIC_prsq_val[8],
#'                    LIC_prsq_val[6], LIC_prsq_val[7])
#' LIC_prsq_var2 <- c("Effect on Chicken Productivity", "Effect on Human AMR", "Effect on Pig Productivity",
#'                    "Mortality from Resistant Infections", "Background Disease Prevalence",
#'                    "AMR Prevalence")
#' 
#' jpeg("Outputs/Table 2 C.jpg")
#' 
#' barplot(LIC_prsq_val2,
#'         names.arg = LIC_prsq_var2,
#'         main = "Portion of Variation in Cost-Effectiveness Explained by Variation in Each Parameter - LICs")
#' 
#' dev.off()

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

scenario_transmission <- "low"
table_4[1,2] <- as.numeric(Model_Case_Study(inputs_casestudy, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

scenario_transmission <- "med" 
table_4[2,2] <- as.numeric(Model_Case_Study(inputs_casestudy, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

scenario_transmission <- "hi" 
table_4[3,2] <- as.numeric(Model_Case_Study(inputs_casestudy, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

scenario_transmission <- "max"
table_4[4,2] <- as.numeric(Model_Case_Study(inputs_casestudy, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])

write.xlsx(table_4, "Outputs/Table 4.xlsx")

# Figure 4 - composition of NMB in default scenario (case study) ----------

scenario_prod <- "HCA"
scenario_amr_grow <- "med"
scenario_intervention_level <- "Village"
scenario_outcomes <- "All"
intervention_followup_period <- 2
scenario_transmission <- "med"
scenario_income <- "Viet Nam"
scenario_farm_effect <- "med"

counts <- rep(0,5)

counts[1] <- as.numeric(Model_Case_Study(inputs_casestudy, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1, "Increased Profit - Chicken Farms"])
counts[2] <- as.numeric(Model_Case_Study(inputs_casestudy, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1, "Increased Profit - Pig Farms"])
counts[3] <- as.numeric(Model_Case_Study(inputs_casestudy, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1, "Value of Productivity Gained"])
counts[4] <- as.numeric(Model_Case_Study(inputs_casestudy, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1, "Cost Saved for Healthcare"]) +
  as.numeric(Model_Case_Study(inputs_casestudy, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1, "Value of DALYs Averted"])
counts[5] <- as.numeric(-1 * Model_Case_Study(inputs_casestudy, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1, "Implementation Cost"])

counts <- counts / 1000000000

names <- c("Poultry Sector", "Pig Sector", "Productivity", "Healthcare (incl. QALYs saved)", "Implementation Cost")

jpeg("Outputs/Figure 4.jpg")

barplot(counts,
        names.arg = names,
        main = "Contribution to Net Monetary Benefit ($bn USD)")

dev.off()

# Min, Max, and Median from Montecarlo ------------------------------------

summary_stats <- matrix(rep(0), ncol = 3, nrow = 3)
colnames(summary_stats) <- c("Minimum Value", "Median Value", "Maximum Value")
rownames(summary_stats) <- c("LIC", "MIC", "HIC")

summary_stats["LIC", "Minimum Value"] <- min_LIC
summary_stats["LIC", "Median Value"] <- avg_LIC
summary_stats["LIC", "Maximum Value"] <- max_LIC

summary_stats["MIC", "Minimum Value"] <- min_MIC
summary_stats["MIC", "Median Value"] <- avg_MIC
summary_stats["MIC", "Maximum Value"] <- max_MIC

summary_stats["HIC", "Minimum Value"] <- min_HIC
summary_stats["HIC", "Median Value"] <- avg_HIC
summary_stats["HIC", "Maximum Value"] <- max_HIC

write.xlsx(summary_stats, "Outputs/Summary Stats.xlsx")


### ggplot summary stats
summary_stats_plt <- as.data.frame(summary_stats)
colnames(summary_stats_plt) <- c("min","median","max")
summary_stats_plt$income_group <- c("LIC","MIC","HIC")
summary_stats_plt$income_group <- factor(summary_stats_plt$income_group, levels = c("LIC","MIC","HIC"))
#summary_stats_plt %>% pivot_longer(cols = min:max)
tb1_2 <- ggplot(summary_stats_plt, aes(x=income_group, y = median, fill = income_group)) + geom_bar(stat = "identity") + 
  geom_errorbar(aes(ymin = min, ymax = max)) +
  scale_y_log10("Maximum Annual Value\n(2020 $USD)\nfor default scenario") + 
  scale_x_discrete("Income group") + 
  scale_fill_discrete("Income group")
ggsave("Outputs/Summary_default.pdf")  

### New figure instead of table 2   
tb1_2 / tb1 + plot_layout(heights=c(1,2)) +
  plot_annotation(tag_levels = 'A')
ggsave("Outputs/newfig_nottbl2.jpeg")


