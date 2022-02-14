# What This File Does -----------------------------------------------------
#' This R script provides an example of how researchers and policymakers can
#' adapt the AHHME model to simulate and evaluate the rollout of antibiotic use
#' intervention packages in livestock production in their context of interest.

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
library("scales")

# Data Files --------------------------------------------------------------

main_dir <- here()
sub_dir <- "Example"
ifelse(!dir.exists(file.path(main_dir, sub_dir)), dir.create(file.path(main_dir, sub_dir)), F)
rm("main_dir", "sub_dir")

inputs_example <- read.csv(here("inputs - general model.csv"))
inputs_example <- as.data.table(inputs_example)
colnames(inputs_example) <- c("parameter", "description", "HIC", "MIC", "", "LIC", "Value", "Min", "Lo", "Med", "Hi", "Max", 
                              "LIC min", "LIC max", "MIC min", "MIC max", "HIC min", "HIC max",
                              "Distribution", "LIC param 1", "LIC param 2", "MIC param 1", "MIC param 2",
                              "HIC param 1", "HIC param 2")

source("Functions.R")

rm(Model_Case_Study)

# Adapting the Model to Our Context ---------------------------------------

#'In this example, you are a policymaker from the UK looking to simulate the 
#'rollout of an intervention in your own country. According to the World Bank,
#'your country is classified as 'high income', so you will use that income scenario

#First, set the default parameters (these are the ones that you will alter)

scenario_income <- "HIC" #as our country is high-income
scenario_prod <- "HCA" #use the human capital approach to estimating productivity losses from illness
scenario_transmission <- "med" #always set to med so that we know which column to edit
scenario_farm_effect <- "med" #always set to med so that we know which column to edit

number_runs <- 10 #choose how many times to run the model if you decide to do Monte Carlo simulation.
#Note that you will have to manually assign distributions to any parameters that you
#vary in probabilistic sensitivity analysis unless using the default values.

#' In this example, you know that your country's population is 68 million. You also 
#' have access to modelling and trial data which suggests that the intervention 
#' package that you are interested in evaluating would increase livestock productivity
#' by 0.5% and would reduce the prevalence of AMR in human infections by 3%. 
#' you do not have data on other relevant model parameters, so you will leave them 
#' as they are

inputs_example[parameter == "pop", "HIC"] <- 68000000
inputs_example[parameter == "pig_income_effect", "Med"] <- 0.005
inputs_example[parameter == "chicken_income_effect", "Med"] <- 0.005
inputs_example[parameter == "res_change", "Med"] <- 0.03

#'According to your data, the impact of the intervention package on the incidence
#'of AMR in human infections varies uniformly between 0 and -0.06 (between no effect
#'and a 6% fall). So, we can assign a distribution to this parameter
inputs_example[parameter == "res_change", "HIC param 1"] <- 0
inputs_example[parameter == "res_change", "HIC param 2"] <- 0.06

# Running the Model -------------------------------------------------------
results_example <- Model(inputs_example, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)


# Sensitivity Analysis ----------------------------------------------------
montecarlo_results_example <- rep(0, number_runs)

set.seed(42069)

for(i in 1:length(montecarlo_results_example)){
  inputs_example[parameter == "res_change", "Med"] <- runif(1, as.numeric(inputs_example[parameter == "res_change", "HIC param 1"]),
                                                            as.numeric(inputs_example[parameter == "res_change", "HIC param 2"]))
  
  montecarlo_results_example[i] <- as.data.frame(Model(inputs_example, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect))[1,1]
  
}

simulation_results_example <- hist(montecarlo_results_example,
                                   xlab = "Intervention Threshold Price ($2020 USD)",
                                   ylab = "Probability Density",
                                   main = "Distribution of Intervention Threshold Price after Monte Carlo Simulation")

summary_stats <- matrix(rep(0), ncol = 3, nrow = 1)
colnames(summary_stats) <- c("Min", "Median", "Max")
rownames(summary_stats) <- c("Threshold Price")
summary_stats[1,] <- c(min(montecarlo_results_example), median(montecarlo_results_example),
                        max(montecarlo_results_example))


# Storing Results ---------------------------------------------------------

write.xlsx(montecarlo_results_example, "Example/Simulation Results.xlsx")
write.xlsx(summary_stats, "Example/Summary Stats.xlsx")
write.xlsx(results_example, "Example/Model Outputs.xlsx")

pdf("Example/Montecarlo Results Histogram.pdf") 
simulation_results_example <- hist(montecarlo_results_example,
                                   xlab = "Intervention Threshold Price ($2020 USD)",
                                   ylab = "Probability Density",
                                   main = "Distribution of Intervention Threshold Price after Monte Carlo Simulation")
dev.off()


