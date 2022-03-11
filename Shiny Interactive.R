
# Introduction to this Document -------------------------------------------
#'This script creates an interactive Shiny App based on the AHHME model. Using this
#'online tool, researchers can use the model to simulate the effect of an 
#'intervention relating to antimicrobial use in food animal production, tailoring
#'it to both the geographical context and the intervention of their choosing.

# Loading Packages --------------------------------------------------------
library(shiny)
library(xlsx)
library(rsconnect)
library(tidyr)
library(data.table)
library("shinydashboard")
library("rsconnect")
library("PKI")
library("packrat")
library(tidyverse)


# Building the App --------------------------------------------------------

#creating the tabs in the dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Interactive AHHME Model Tool"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("How to Use This Tool", tabName = "howto", icon = icon("question")),
      menuItem("Inputs - Intervention Parameters", tabName = "inputsintervention", icon = icon("flask")),
      menuItem("Inputs - Methodological Assumptions", tabName = "inputsmethod", icon = icon("abacus")),
      menuItem("Inputs - Epidemiological Characteristics", tabName = "inputsepi", icon = icon("bacteria")),
      menuItem("Inputs - Country Characteristics", tabName = "inputscountry", icon = icon("earth-asia")),
      menuItem("Outputs", tabName = "outputs", icon = icon("chart-mixed")),
    )
  ),
  dashboardBody(
    tabItems(
      
#creating the "how to" tab      
      tabItem("howto",
              box(
                title = "Incremental Cost-Effectiveness Ratio (Single Draw from a Distribution)",
                background = "fuchsia",
                width = 12,
                textOutput("Hello and welcome to the interactive Agriculture Human Health
                           Micro-Economic (AHHME) modelling tool! This online tool is based on the
                           AHHME model (Emes et al., 2022), available on GitHub at <https://github.com/Trescovius/AHHME>.
                           The model simulates the rollout of an intervention targeting antimicrobial use (AMU) in food 
                           animal production at the national level. Considers the effect of the intervention on farm productivity, 
                           the effect on the prevalence of antimicrobial resistant (AMR) infections in humans,
                           and subsequently on the life years lost to morbidity and mortality, the cost borne by the healthcare system,
                           and the effect on labour productivity. The model does not specify the causal pathway of the intervention 
                           mechanisticallly. Instead, it is assumed to have a proportional effect on farm incomes (e.g. they increase
                           by 1%) and a proportional effect on the prevalence of AMR in sepsis in humans (e.g. it falls by 10%). 
                           In order to use this model, navigate to the "Inputs" tabs. Alter the input parameters as appopriate for
                           the intervention and geographical context being considered. Then, navigate to the "Outputs" tab to view 
                           the model results. The main output is the 'Threshold Price' of the intervention. This reflects the 
                           maximum annual budget that the public sector should devote to implementing an intervention with the 
                           specified impact. There is also a breakdown of the total discounted net monetary benefit of the intervention 
                           (across all sectors) over the time horizon considered")
              )
      ),
      
#creating the intervention parameter inputs tab      
      tabItem("inputsintervention",
              box(numericInput("pig_income_effect", 
                               em("Proportional Effect on Pig Farm Incomes:"), 
                               0.01, min = -1, max = 10000), width = 6),
              box(numericInput("chicken_income_effect", 
                               em("Proportional Effect on Chicken Farm Incomes:"), 
                               0.01, min = -1, max = 10000), width = 6),
              box(numericInput("res_change", 
                               em("Effect on the Portion of AMR in Human Sepsis:"), 
                               -0.05, min = -1, max = 10000), width = 6)
      ),

#creating the methodological assumptions inputs tab    
      tabItem("inputsmethod",
              box(numericInput("dr", 
                               em("Annual Discount Rate:"), 
                               0.05, min = 0, max = 1), width = 6),
              box(numericInput("n.t", 
                               em("Time Horizon Considered (Years):"), 
                               20, min = 2, max = 50), width = 6),
              box(numericInput("wtp", 
                               em("Willingness to Pay per Quality-Adjusted Life Year (QALY):"), 
                               2000, min = 0, max = 1000000), width = 6)
      ),




      








