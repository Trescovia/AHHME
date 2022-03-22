library("shiny")
library("rsconnect")
library("tidyr")
library("shinydashboard")
library("PKI")
library("packrat")
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

#creating the tabs in the dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Interactive AHHME Model Tool"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("How to Use This Tool", tabName = "howto", icon = icon("question")),
      menuItem("Inputs - Intervention Parameters", tabName = "inputsintervention", icon = icon("flask")),
      menuItem("Inputs - Methodological Assumptions", tabName = "inputsmethod", icon = icon("calculator")),
      menuItem("Inputs - Epidemiological Characteristics", tabName = "inputsepi", icon = icon("bacteria")),
      menuItem("Inputs - Country Characteristics", tabName = "inputscountry", icon = icon("globe")),
      menuItem("Outputs", tabName = "outputs", icon = icon("chart-area"))
    )
  ),
  dashboardBody(
    tabItems(
      
      #creating the "how to" tab      
      tabItem("howto",
              box(
                title = "How to Use This Tool",
                background = "green",
                width = 12,
                textOutput("introtext")
              ),
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
      
      #creating the epidemiological characteristics inputs tab
      tabItem("inputsepi",
              box(textInput("diseasename",
                            em("Disease Name:"),
              ), width = 6),
              box(numericInput("well_sick", 
                               em("Annual Disease Incidence (%):"), 
                               0.006753598, min = 0, max = 1), width = 6),
              box(numericInput("portion_res", 
                               em("Portion of Infections which are Antimicrobial-Resistant:"), 
                               0.106864, min = 0, max = 1), width = 6),
              box(numericInput("mort_res", 
                               em("Mortality Rate from Resistant Infections:"), 
                               0.407421738, min = 0, max = 1), width = 6),
              box(numericInput("mort_sus", 
                               em("Mortality Rate from Resistant Infections:"), 
                               0.2514949, min = 0, max = 1), width = 6),
              box(numericInput("seq_res", 
                               em("Chance of Developing Sequelae from Resistant Infections:"), 
                               0.379600619, min = 0, max = 1), width = 6),
              box(numericInput("seq_sus", 
                               em("Chance of Developing Sequelae from Susceptible Infections:"), 
                               0.23432137, min = 0, max = 1), width = 6),
              box(numericInput("los_sus", 
                               em("Hospital Length of Stay from Susceptible Infection (Years):"), 
                               0.01227, min = 0, max = 1), width = 6),
              box(numericInput("los_res", 
                               em("Hospital Length of Stay from Resistant Infection (Years):"), 
                               0.01559, min = 0, max = 1), width = 6),
              box(numericInput("qol_sick", 
                               em("Quality of Life Score for Illness (1 is perfect health, 0 is indifference to death):"), 
                               0.66035, min = 0, max = 1), width = 6),
              box(numericInput("qol_seq", 
                               em("Quality of Life Score for Sequelae (1 is perfect health, 0 is indifference to death):"), 
                               0.9355, min = 0, max = 1), width = 6),
      ),
      
      #creating the country characteristics inputs tab
      tabItem("inputscountry",
              box(textInput("countryname",
                            em("Country Name:"),
              ), width = 6),
              box(numericInput("pop", 
                               em("Country Population:"), 
                               1000000, min = 0, max = 10000000000), width = 6),
              box(numericInput("prod_growth", 
                               em("Annual Productivity Growth Rate:"), 
                               0.04131553, min = -1, max = 10), width = 6),
              box(numericInput("prod_pc", 
                               em("Annual Productivity per Person Working ($USD):"), 
                               22609.29, min = 0, max = 1000000000), width = 6),
              box(numericInput("remaining_work_years", 
                               em("65 minus Median Age:"), 
                               34, min = 1, max = 64), width = 6),
              box(numericInput("remaining_ly", 
                               em("Life Expectancy minus Median Age:"), 
                               41, min = 1, max = 100), width = 6),
              box(numericInput("bed_day_cost", 
                               em("Cost of Providing One Hospital Bed Day ($USD):"), 
                               39.73242, min = 0, max = 1000000), width = 6),
              box(numericInput("n_pigs", 
                               em("Number of Pigs Nationwide:"), 
                               10835635, min = 0, max = 1000000000000), width = 6),
              box(numericInput("n_chickens", 
                               em("Hospital Length of Stay from Resistant Infection (Years):"), 
                               284531028, min = 0, max = 1000000000000), width = 6)
      ),
      
      #creating the outputs tab
      
      tabItem("outputs",
              box(title = "Cost-Effectiveness Analysis of Interventions Targeting:",
                  background = "green",
                  width = 12,
                  textOutput("diseasename")),
              box(title = "In:",
                  background = "green",
                  width = 12,
                  textOutput("countryname")),
              box(
                title = "Composition of Total Net Monetary Benefit by Agent ($USD)",
                background = "green",
                width = 12,
                tableOutput("table")
              ),
              box(
                title = "Threshold Price (Maximum Acceptable Annual Intervention Cost ($USD))",
                background = "green",
                width = 6,
                textOutput("threshold_price")
              )
              
      )
    )
  )
)

server <- function(input, output) {
  
  #load main AHHME function
  source("Functions.R")
  
  #set model scenarios
  scenario_income <- "MIC"
  scenario_prod <- "HCA"
  scenario_transmission <- "med"
  scenario_farm_effect <- "hi"
  
  #the explanatory text
  introtext <-             "Hello and welcome to the interactive Agriculture Human Health
                           Micro-Economic (AHHME) modelling tool! This online tool is based on the
                           AHHME model (Emes et al., 2022), available on GitHub at <https://github.com/Trescovius/AHHME>.
                           The model simulates the rollout of an intervention targeting antimicrobial use (AMU) in food
                           animal production at the national level. Considers the effect of the intervention on farm productivity,
                           the effect on the prevalence of antimicrobial resistant (AMR) infections in humans,
                           and subsequently on the life years lost to morbidity and mortality, the cost borne by the healthcare system,
                           and the effect on labour productivity. The model does not specify the causal pathway of the intervention
                           mechanisticallly. Instead, it is assumed to have a proportional effect on farm incomes (e.g. they increase
                           by 1%) and a proportional effect on the prevalence of AMR in sepsis in humans (e.g. it falls by 10%).
                           In order to use this model, navigate to the Inputs tabs. Alter the input parameters as appopriate for
                           the intervention and geographical context being considered. Then, navigate to the Outputs tab to view
                           the model results. The main output is the 'Threshold Price' of the intervention. This reflects the
                           maximum annual budget that the public sector should devote to implementing an intervention with the
                           specified impact. There is also a breakdown of the total discounted net monetary benefit of the intervention
                           (across all sectors) over the time horizon considered"

  #create meta-function which alters inputs according to output, and then produces the output table
  outputtablefunction = function(inputs){
    
    inputs <- read.csv("inputs - general model.csv")
    inputs <- as.data.table(inputs)
    colnames(inputs) <- c("parameter", "description", "HIC", "MIC", "", "LIC", "Value", "Min", "Lo", "Med", "Hi", "Max",
                          "LIC min", "LIC max", "MIC min", "MIC max", "HIC min", "HIC max",
                          "Distribution", "LIC param 1", "LIC param 2", "MIC param 1", "MIC param 2",
                          "HIC param 1", "HIC param 2")
    
    inputs[parameter == "pig_income_effect", "Hi"] <- input$pig_income_effect
    inputs[parameter == "chicken_income_effect", "Hi"] <- input$chicken_income_effect
    inputs[parameter == "res_change", "Med"] <- input$res_change
    inputs[parameter == "dr", "MIC"] <- input$dr
    inputs[parameter == "n.t", "MIC"] <- input$n.t
    inputs[parameter == "wtp", "MIC"] <- input$wtp
    inputs[parameter == "well_sick", "MIC"] <- input$well_sick
    inputs[parameter == "portion_res", "MIC"] <- input$portion_res
    inputs[parameter == "mort_res", "MIC"] <- input$mort_res
    inputs[parameter == "mort_sus", "MIC"] <- input$mort_sus
    inputs[parameter == "seq_res", "MIC"] <- input$seq_res
    inputs[parameter == "seq_sus", "MIC"] <- input$seq_sus
    inputs[parameter == "los_sus", "MIC"] <- input$los_sus
    inputs[parameter == "los_res", "MIC"] <- input$los_res
    inputs[parameter == "qol_sick", "MIC"] <- input$qol_sick
    inputs[parameter == "qol_seq", "MIC"] <- input$qol_seq
    inputs[parameter == "pop", "MIC"] <- input$pop
    inputs[parameter == "prod_growth", "MIC"] <- input$prod_growth
    inputs[parameter == "prod_pc", "MIC"] <- input$prod_pc
    inputs[parameter == "remaining_work_years", "MIC"] <- input$remaining_work_years
    inputs[parameter == "remaining_ly", "MIC"] <- input$remaining_ly
    inputs[parameter == "bed_day_cost", "MIC"] <- input$bed_day_cost
    inputs[parameter == "n_pigs", "MIC"] <- input$n_pigs
    inputs[parameter == "n_chickens", "MIC"] <- input$n_chickens
    
    outputtable <- Model(inputs, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)
    
    return(outputtable)
    
  }
  
  #create meta-function which alters inputs according to output, and then produces the threshold price
  outputthresholdpricefunction = function(inputs){
    
    inputs <- read.csv("inputs - general model.csv")
    inputs <- as.data.table(inputs)
    colnames(inputs) <- c("parameter", "description", "HIC", "MIC", "", "LIC", "Value", "Min", "Lo", "Med", "Hi", "Max",
                          "LIC min", "LIC max", "MIC min", "MIC max", "HIC min", "HIC max",
                          "Distribution", "LIC param 1", "LIC param 2", "MIC param 1", "MIC param 2",
                          "HIC param 1", "HIC param 2")
    
    inputs[parameter == "pig_income_effect", "Hi"] <- input$pig_income_effect
    inputs[parameter == "chicken_income_effect", "Hi"] <- input$chicken_income_effect
    inputs[parameter == "res_change", "Med"] <- input$res_change
    inputs[parameter == "dr", "MIC"] <- input$dr
    inputs[parameter == "n.t", "MIC"] <- input$n.t
    inputs[parameter == "wtp", "MIC"] <- input$wtp
    inputs[parameter == "well_sick", "MIC"] <- input$well_sick
    inputs[parameter == "portion_res", "MIC"] <- input$portion_res
    inputs[parameter == "mort_res", "MIC"] <- input$mort_res
    inputs[parameter == "mort_sus", "MIC"] <- input$mort_sus
    inputs[parameter == "seq_res", "MIC"] <- input$seq_res
    inputs[parameter == "seq_sus", "MIC"] <- input$seq_sus
    inputs[parameter == "los_sus", "MIC"] <- input$los_sus
    inputs[parameter == "los_res", "MIC"] <- input$los_res
    inputs[parameter == "qol_sick", "MIC"] <- input$qol_sick
    inputs[parameter == "qol_seq", "MIC"] <- input$qol_seq
    inputs[parameter == "pop", "MIC"] <- input$pop
    inputs[parameter == "prod_growth", "MIC"] <- input$prod_growth
    inputs[parameter == "prod_pc", "MIC"] <- input$prod_pc
    inputs[parameter == "remaining_work_years", "MIC"] <- input$remaining_work_years
    inputs[parameter == "remaining_ly", "MIC"] <- input$remaining_ly
    inputs[parameter == "bed_day_cost", "MIC"] <- input$bed_day_cost
    inputs[parameter == "n_pigs", "MIC"] <- input$n_pigs
    inputs[parameter == "n_chickens", "MIC"] <- input$n_chickens
    
    outputthresholdprice <- as.character(Model(inputs, scenario_income, scenario_prod, scenario_transmission,scenario_farm_effect)[1,1])
    
    return(outputthresholdprice)
    
  }
  
  #render outputs for the UI
  output$introtext <- renderText(introtext)
 
  output$diseasename <- renderText(input$diseasename)
  
  output$countryname <- renderText(input$countryname)
  
  output$threshold_price <- renderText(outputthresholdpricefunction(inputs))
  
  output$table <- renderTable(outputtablefunction(inputs))
  
}

shinyApp(ui, server)