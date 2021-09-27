# Libraries ---------------------------------------------------------------

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

# Global parameters and scenarios -----------------------------------------

n.t <- 47 ## time horizon - 46 years + cycle 0 (initial states)
tstop <- n.t + 3
dr <- 0.08 ## discount rate
wtp <- 2365 ## willingness to pay per QALY gained
emp_rate <- 0.98162 ##employment rate (portion of working-age population)
lfpr <- 0.767 ##labour force participation rate
hosp_time_res <- 1-0.923504449 ##portion of a year spent in hospital with a resistant infection
hosp_time_sus <- 1-0.94 ##portion of a year spent in hospital with a susceptible infection
remaining_working_years <- 34 ##remaining working years of average person

#Scenarios
scenario <- "HCA" #must be "HCA" or "FCA"
scenario_transmission <- "med" #for now, must be {'hi', 'low', 'med', 'max'}
scenario_outcomes <- "All" #must be either "Enterobacteria" or "All"
scenario_intervention_level <- "Village" #must be either "Village" or "Farm" ##added a scenario for implementing at the village level
intervention_followup_period <- 2
scenario_amr_grow <- "med" #must be "lo", "med", "hi" or "max" ##DTE added AMR growth scenarios

# Calculating per-farm intervention cost ----------------------------------

##farm-level
seminar_cost <- 25
visit_cost <- 25
farmers_per_seminar <- 10
hourly_compensation <- 4.3
seminar_length <- 3
visits_per_year <- 3
visit_length_individual <- 2
transport_cost <- 4

cost_per_farm_indiv <- visit_cost*visits_per_year + 
  visit_length_individual*hourly_compensation*visits_per_year +
  seminar_cost/farmers_per_seminar +
  seminar_length*hourly_compensation +
  transport_cost

##village-level

group_size <- 10
additional_time_per_farm <- 0.5
visit_length_village <- 2

cost_per_farm_village <- seminar_cost/farmers_per_seminar + 
  seminar_length*hourly_compensation + transport_cost +
  (visit_cost + ((group_size - 1)*additional_time_per_farm*hourly_compensation))*(visits_per_year/group_size) +
  visit_length_village*hourly_compensation*visits_per_year
  

# Population Projections --------------------------------------------------

###
# in this section, we use demographic data from Viet Nam (population, dependency ratio, etc.)
# to forecast the at-risk population and the working population throughout our time frame
# what we refer to as 'births' in this model is actually net births, and this is represented by the
# 'birth' transition parameter. We set background mortality to zero, as background mortality is already
# captured by net births. This does, however, mean that the rate of population growth  in our model
# is independent of the number of deaths from BSIs, although in reality the two may be related.
# This therefore relies on the assumption that the amount of deaths from BSIs is not systemically
# important in the sense that it can influence demographic trajectory
###

pop <- read.csv("C:/Users/tresc/Desktop/AMR-Model/Population data for ARIMA/Vietnam Population.csv")
pop <- ts(pop$Population, start = 1960, frequency = 1)#
ARIMApop <- auto.arima(pop, stepwise = F, approximation = F)
plot(forecast(ARIMApop, tstop))
forecast(ARIMApop, tstop)
predict <- forecast(ARIMApop, tstop)
predict$mean
futurepop <- predict$mean

popchange <- numeric()
for (i in 1:tstop) {
  popchange[i] <- futurepop[i+1] - futurepop[i]
} #atm births[1] shows NET births in 2020

popchange <- popchange[2:tstop] #now popchange[1] is NET popchange in 2021

popchange.r <- numeric()
for (i in 1:tstop) {
  popchange.r[i] <- (futurepop[i+1] - futurepop[i])/futurepop[i]
} #atm popchange.r[1] shows NET popchange in 2020

popchange.r <- popchange.r[2:tstop] #now popchange.r[1] shows NET popchange in 2021

#upper bound population projection
futurepop_up <- predict$upper
futurepop_up <- futurepop_up[,2]

popchange.r_up <- numeric()
for (i in 1:tstop) {
  popchange.r_up[i] <- (futurepop_up[i+1] - futurepop_up[i])/futurepop_up[i]
} #atm popchange.r_up[1] shows rate for 2020

popchange.r_up <- popchange.r_up[2:tstop] #now it shows upper rates for 2021

#lower bound population projection
futurepop_low <- predict$lower
futurepop_low <- futurepop_low[,2]

popchange.r_low <- numeric()
for (i in 1:tstop){
  popchange.r_low[i] <- (futurepop_low[i+1] - futurepop_low[i])/futurepop_low[i]
} #atm popchange.r_low[1] shows rate for 2020

popchange.r_low <- popchange.r_low[2:tstop] #now it shows lower rates for 2021

##here I have predicted net popchange and will use this as the 'birth' parameter. This means that the background 
##mortality will be set to 0 because the net popchange.r already accounts for background mortality

####dependency ratio

dependency <- read.csv("C:/Users/tresc/Desktop/AMR-Model/Population data for ARIMA/Viet Nam dependency ratio.csv")
dependency <- ts(dependency$Dependency.Ratio, start = 1960, frequency = 1)
arimadependency <- auto.arima(dependency, stepwise = F, approximation = F)
plot(forecast(arimadependency, tstop))
predictdependency <- forecast(arimadependency, tstop)
predictdependency$mean
futuredepend <- predictdependency$mean

dependency <- numeric()
for(i in 1:tstop) {
  dependency[i] <- futuredepend[i + 1]
} #now dependency[1] is the dependency in 2021, not 2020

rm(i)

#now get portion working age
dependency <- dependency * 0.01 ## *0.01 added to make it a portion rather than a %%

portion_working_age <- 1/(1+dependency)

plot(portion_working_age)

portion_working <- portion_working_age * emp_rate * lfpr #assuming unemployment rate equal to mean of 2016-2020 and LFPR equal to mean of 2016-2019

plot(portion_working)


###
# Here, we define our scenarios. We set the number of periods to be 46 (the expected
# remaining life years in our population). We let the discount rate be 0.08 and the 
# willingness to pay for a QALY be 2365 USD, both of which are derived from theory.
# We have possible scenarios for a) the way we estimate productivity outcomes from
# morbidity and mortality (either the human capital approach or the friction cost 
# approach), b) the link parameter between animal AMU and human AMR (either from Tang
# or from Booton), and c) the infection types looked at (either all BSIs or all BSIs
# caused by Enterobacteriaceae spp.)
###

inputs <- read.csv("C:/Users/tresc/Desktop/AMR-Model/intervention 1/inputs.csv")
inputs <- as.data.table(inputs)
colnames(inputs) <- c("scenario", "parameter", "description", "value", "distribution", "low", "high", "notes")


# Logistic Growth ---------------------------------------------------------

logit_year <- c(1,16)
((0.645+0.058+0.669+0.234)/4) #0.405
((0.77+0.118+0.582+0.528)/4) #0.4995
logit_prevalence <- c(0.4015,0.4995)
logit_data <- as.data.frame(cbind(logit_year, logit_prevalence))

logitselfstart <- nls(logit_prevalence ~ SSlogis(logit_year)) ##not enough data points to fit a logistic growth model


# Main Model --------------------------------------------------------------

# model <- function(inputs){

  inputs[ , value := as.numeric(as.character(value))]
  
  human <- inputs[scenario=="human"]
  chicken <- inputs[scenario=="chicken"]
  pig <- inputs[scenario=="pig"]
  intervention <- inputs[scenario=="intervention"]
  
  
  f_expvalue <- function(x,y,z){
    ## x is the epi matrix
    ## y is the cost matrix
    ## z is the reward matrix
    results <- matrix(c(sum(x*y),sum(x*z)),1,2)
    return(results)
    
  }
  
  f_di <- function(x,y){
    # function to apply a discount rate
    # x is cost
    # y is discount rate 
    x2 <- x - (x*y)
    return(x2)
  }
  
  
  
  # Human Epi Model -------------------------------------------------------------
  
  #for HCA and FCA, we only care about the losses in productivity, so we set the 
  #reward for 'well' to be zero. Going into the 'dead' state incurs a productivity
  #loss equal to the discounted value of future earnings, going into the 'res' or 'sus
  #states incurs a loss equal to the earnings that would have been made during the 
  #time in hospital. After 1 period, all people in 'dead' go to 'afterlife', which
  #has a reward of zero. There is also a sequelae state - people in this state only 
  #stay there for one period before continuing peacefully into the afterlife. This
  #is only for convenience and does not imply that people with sequelae die after one 
  #year. Rather, the cost of being in the sequelae state for one turn is equal to the
  #difference in discounted future welfare between a pefectly healthy expected remainder
  #of life and an expected remainder of life with sequelae
  
  state_names <- c("well", "res","sus","dead", "afterlife", "seq") ## the compartments
  transition_names  <- c("birth","r","s","mort_r", "mort_s","mort_w", "rec_r","rec_s", "dead_aft", "sick_seq")  ## the transition probabilities
  parameter_names <- c(state_names, transition_names)
  
  ## initial state vector (population starting in well)
  state_i <- c(human[parameter=="n_population",value], rep(0,length=length(state_names)-1))
  
  ## matrix of parameter values over cycles
  m_param <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names))
  colnames(m_param) <- parameter_names
  rownames(m_param) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  if(scenario_outcomes == "All"){
    tuning <- 2
  }else if (scenario_outcomes == "Enterobacteria"){
    tuning <- 1
  }
  
  m_param[ , "sick_seq"] <- rep(human[parameter=="sick_seq", value], n.t) 
  #chance of developing sequelae following infection
  
  m_param[ , "r"] <- rep(tuning*human[parameter=="well_r",value], n.t)
  #chance of developing a resistant infection in a year
  
  m_param[ , "s"] <- rep(tuning*human[parameter=="well_s",value], n.t)
  #chance of developing a susceptible infection in a year
  
  m_param[ , "mort_r"] <- rep(human[parameter=="r_dead",value], n.t)
  #fatality from resistant infection
  
  m_param[ , "mort_s"] <- rep(human[parameter=="s_dead",value], n.t)
  #fatality from susceptible infection
  
  m_param[ , "rec_r"] <- rep(1-(m_param[1,"mort_r"]+m_param[1,"sick_seq"]), n.t)
  #chance of recovering from a resistant infection
  
  m_param[ , "rec_s"] <- rep(1-(m_param[1,"mort_s"]+m_param[1,"sick_seq"]), n.t)
  #chance of recovering from a susceptible infection
  
  m_param[ , "birth"] <- popchange[1:n.t]
  #predicted net births in a given year
  
  m_param[ , "mort_w"] <- rep(0, n.t) 
  #chance of dying without infection from 'well', set to zero because background mortality is included in net births
  
  m_param[ , "dead_aft"] <- rep(1, n.t) 
  #all those who die go to the afterlife
  
  m_param[1, 1:length(state_names)] <- state_i ## adding initial cycle 0 values
  
  #letting the chance of getting infected with resistant bacteria grow in each period
  
  amr_growth <- 1 ##DTE added scenarios for AMR growth
  
  if(scenario_amr_grow == "lo"){ 
    amr_growth <- 1.01
  } else if (scenario_amr_grow == "med"){
    amr_growth <- human[parameter=="amr_growth", value]
  } else if (scenario_amr_grow == "hi") {
    amr_growth <- 1.05
  } else if (scenario_amr_grow == "max"){
    amr_growth <- 1.1
  } ##DTE added scenarios for AMR growth
  
  for (i in 2:(n.t)){
    m_param[i, "r"] <- m_param[i-1, "r"]*amr_growth
  }
  
  #keeping the overall prevalence of disease constant, we make sure that the number
  #of infections with susceptible and resistant bacteria sum to the total number of
  #infections. Thus, the highest resistance outcome is one in which all infections are
  #resistant but the total number of infections remains the same
  # disease_max <- m_param[1,"r"]+m_param[1,"s"] ## added 'tuning*' here
  
  for(i in 1:n.t){
    if(m_param[i, "r"] > 0.9*(m_param[1,"r"]+m_param[1,"s"])){ ##DTE changed maximum portion resistant to 90%
      m_param[i, "r"] <- 0.9*(m_param[1,"r"]+m_param[1,"s"]) ##DTE changed maximum portion resistant to 90%
    }
    m_param[i, "s"] <- m_param[1,"r"]+m_param[1,"s"] - m_param[i, "r"] 
  }
  
  ## the difference equation function: 
  f_human_epi <- function(m_param, n.t){
    for (i in 2:(n.t)){
      
      m_param[i,"well"] <- m_param[i-1,"well"] -(m_param[i-1,"r"]*m_param[i-1,"well"]) -
        (m_param[i-1,"s"]*m_param[i-1,"well"]) + m_param[i-1,"birth"] - 
        (m_param[i-1,"mort_w"]*m_param[i-1,"well"])+(m_param[i-1,"rec_r"]*m_param[i-1,"res"])+
        (m_param[i-1,"rec_s"]*m_param[i-1,"sus"])
      
      m_param[i,"res"] <- m_param[i-1,"res"] + (m_param[i-1,"r"]*m_param[i-1,"well"]) - 
        (m_param[i-1,"mort_r"]*m_param[i-1,"res"]) - (m_param[i-1,"rec_r"]*m_param[i-1,"res"]) -
        (m_param[i-1, "sick_seq"]*m_param[i-1,"res"]) ## added this so that the number of people in 'res' subtracts the people who went into 'seq'
      
      m_param[i,"sus"] <- m_param[i-1,"sus"] + (m_param[i-1,"s"]*m_param[i-1,"well"]) -
        (m_param[i-1,"mort_s"]*m_param[i-1,"sus"]) - (m_param[i-1,"rec_s"]*m_param[i-1,"sus"]) -
        (m_param[i-1, "sick_seq"]*m_param[i-1,"sus"]) ## added this so that the number of people in 'sus' subtracts the people who went into 'seq'
      
      m_param[i,"dead"] <- (m_param[i-1,"mort_r"]*m_param[i-1,"res"]) + (m_param[i-1,"mort_s"]*m_param[i-1,"sus"])+
        (m_param[i-1,"mort_w"]*m_param[i-1,"well"])
      
      m_param[i, "afterlife"] <- m_param[i-1, "afterlife"] + m_param[i-1, "dead"] + m_param[i-1, "seq"] #just keeps growing
      
      m_param[i, "seq"] <- m_param[i-1, "sick_seq"]*(m_param[i-1, "res"]+m_param[i-1, "sus"]) #only spend one period in 'seq' then go straight to the shadow realm
    }
    return(m_param)
  }
  
  m_param <- f_human_epi(m_param,n.t) #applying the epi function for humans (base case)
  
  state_i[1:4] <- m_param[3,1:4] ##DTE made it so that deaths and illness begin to accrue from year 1
  m_param[1, 1:length(state_names)] <- state_i ##DTE made it so that deaths and illness begin to accrue from year 1
  m_param <- f_human_epi(m_param,n.t) ##DTE made it so that deaths and illness begin to accrue from year 1
  
  # Healthcare Costs --------------------------------------------------------
  
  
  m_cost <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names))
  colnames(m_cost) <- parameter_names
  rownames(m_cost) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  c_r <- human[parameter=="r_cost",value]
  c_s <- human[parameter=="s_cost",value]
  
  cost_i <- c(0,c_r,c_s,0,0,0) #only infections incur a healthcare cost here
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_cost[2, 1:length(state_names)] <- cost_i
  
  for (j in 1:length(state_names)) {
    for (i in 3:(n.t)){
      m_cost[i,j] <- f_di(m_cost[i-1,j],dr)
    }  
  }
  
  
  # Healthcare Rewards ------------------------------------------------------
  
  
  m_rwd <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names))
  colnames(m_rwd) <- parameter_names
  rownames(m_rwd) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  #calculate the present value of expected remaining life years for a) a perfectly
  #healthy person and b) someone with sequelae. The negative difference is the 'reward' 
  #of being in the sequelae state 
  
  pv_fut_life <- c(rep(0,n.t-1)) #expected remaining life years ## changed '46' to 'n.t-1'
  for (i in 1:n.t-1){ ## changed '46' to 'n.t-1'
    pv_fut_life[i] <- human[parameter=="background_qol",value] * (1-dr)^(i-1)
  }
  pv_life <- sum(pv_fut_life)
  
  pv_fut_life_seq <- c(rep(0,n.t-1)) #expected remaining life years ## changed '46' to 'n.t-1'
  for (i in 1:n.t-1){
    pv_fut_life_seq[i] <- human[parameter=="hrqol_seq",value] * (1-dr)^(i-1)
  }
  pv_life_seq <- sum(pv_fut_life_seq)
  
  #the 'reward' for death is the negative of the present value of remaining life 
  #years, and the 'reward' for being infected is the loss of welfare while hospitalised
  #therefore the scenario with more deaths, infections and sequelae will have a negative
  #'reward' of larger absolute value
  
  r_s <- human[parameter=="background_qol",value]*(human[parameter=="hrqol_ill",value]-1) ## QoL lost from time in hospital
  r_r <- human[parameter=="background_qol",value]*(human[parameter=="hrqol_res",value]-1) ## same but adjusted for longer LoS
  r_d <- -1 * pv_life #discounted QoL loss from death
  r_seq <- pv_life_seq - pv_life # fixed this because we were previously assigning a benefit to sequelae (the subtraction was the wrong way around lol)
  
  ##the above 4 lines may need fixing!!!!!!
  
  rwd_i <- c(0,r_r,r_s,r_d,0,r_seq) 
  
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_rwd[2, 1:length(state_names)] <- rwd_i
  
  ### accounting for discounting
  for (j in 1:length(state_names)) {
    for (i in 3:(n.t)){
      m_rwd[i,j] <- f_di(m_rwd[i-1,j],dr)
    }  
  }
  
  
  
  # Productivity Costs ------------------------------------------------------
  
  #for HCA and FCA, we only care about the losses in productivity, so we set the 
  #reward for 'well' to be zero. Going into the 'dead' state incurs a productivity
  #loss equal to the discounted value of forgone future earnings, going into the
  #'res' or 'sus states incurs a loss equal to the earnings that would have
  #been made during the time in hospital. After 1 period, all people in 'dead' go
  #to 'afterlife', which has a reward of zero. 
  #Using the HCA, the forgone future earnings are those of expected remaining economically
  #active years. For FCA, it is the forgone earnings during the time that it takes
  #to find a replacement worker
  
  
  #all productivity rewards are set to zero, and we only see a difference between
  #the reward matrices of different scenarios
  m_cost_prod <- matrix(rep(0),nrow = n.t, ncol = length(parameter_names))
  colnames(m_cost_prod) <- parameter_names
  rownames(m_cost_prod) <- paste("cycle", 0:(n.t-1), sep = "")
  
  #c_r_prod <- 0 ## commented this out since we're not using it
  #c_s_prod <- 0 ## commented this out since we're not using it
  
  cost_i_prod <- rep(0,(length(state_names)))
  
  ## start at cycle 1 so you do not multiply initial state vector
  m_cost_prod[2,1:length(state_names)] <- cost_i_prod
  
  for (j in 1:length(state_names)) {
    for (i in 3:(n.t)){
      m_cost_prod[i,j] <- f_di(m_cost_prod[i-1,j],dr)
    }
  }
  
  
  #### Productivity Rewards #########
  
  #the 'reward' for being infected is equal to the negative forgone productivity
  #while hospitalised
  
  m_rwd_prod <- matrix(rep(0), nrow = n.t, ncol = length(parameter_names))
  colnames(m_rwd_prod) <- parameter_names
  rownames(m_rwd_prod) <- paste("cycle", 0:(n.t-1), sep = "")
  
  r_r_prod <- -1*(human[parameter=="prod",value]+human[parameter=="unpaid_prod",value])*(hosp_time_res) ## put a named parameter here rather than a number
  
  r_s_prod <- -1*(human[parameter=="prod",value]+human[parameter=="unpaid_prod",value])*(hosp_time_sus) ## put a named parameter here rather than a number
  
  r_w_prod <- 0
  
  r_aft_prod <- 0
  
  r_seq_prod <- 0 #importantly assumes that people with sequelae are equally productive
  
  #the reward for dead is the present discounted value of future work (either for the
  #remainder of economically active life, or for the 6 months needed to find a replacement)
  
  yearly_prod <- (human[parameter=="prod",value] + human[parameter=="unpaid_prod", value])
  pv_fut_prod <- c(rep(0,remaining_working_years)) #expected remaining working years ## put a named parameter here rather than a number
  for (i in 1:remaining_working_years){ ## put a named parameter here rather than a number
    pv_fut_prod[i] <- yearly_prod * (1-dr)^(i-1)
  }
  pv_life_prod <- sum(pv_fut_prod)
  
  if(scenario == "HCA"){
    r_d_prod <- -1 * pv_life_prod
  } else if(scenario == "FCA"){
    r_d_prod <- -0.5 * yearly_prod
  } else{
    paste("ERROR: PLEASE CHOOSE AN APPROACH TO ESTIMATING PRODUCTIVITY OUTCOMES")
  }
  
  #multiply the initial productivity rewards by the portion of people who are working
  rwd_i_prod <- c(r_w_prod, r_r_prod, r_s_prod, r_d_prod, r_aft_prod, r_seq_prod)
  ## removed 'portion_working[1]*', as we already account for this later
  
  ## start at cycle 1 so you do not multiply initial state vector
  m_rwd_prod[2,1:length(state_names)] <- rwd_i_prod 
  
  ### discount, but also account for labour productivity growth
  
  dr_pgrowth <- dr - human[parameter=="prod_growth", value] ##discount rate net of productivity growth (in theory it can exceed the discount rate) ## corrected the label
  
  for (j in 1:length(state_names)) {
    for (i in 3:(n.t)){
      m_rwd_prod[i,j] <- f_di(m_rwd_prod[i-1,j],dr_pgrowth)
    }
  }
  
  #now we multiply the productivity rewards across all states and time steps
  #(apart from initial, which we already did) by the portion of people working
  for (j in 1:length(state_names)) {
    for (i in 3:(n.t)) {
      m_rwd_prod[i,j] <- m_rwd_prod[i,j]*portion_working[i]
    }
  } 
  
  
  
  # Chicken Epi Model -----------------------------------------------------------
  
  ## removed an annotation here as we now account for the cost of buying a piglet or chick
  
  state_names_c <- c("well", "res","sus","fallen","sold") ## the compartments
  transition_names_c  <- c("birth","r","s","mort_r", "mort_s","mort_w", "rec_r","rec_s","w_sold")  ## the rates
  parameter_names_c <- c(state_names_c, transition_names_c)
  
  state_i_c <- c(chicken[parameter=="n_animals",value], rep(0,length=length(state_names_c)-1))
  
  m_param_c_base <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_c))
  colnames(m_param_c_base) <- parameter_names_c
  rownames(m_param_c_base) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  #here, we have a set mortality rate for all states - 
  #it only matters if animals are in 'res' or 'sus' for the purpose of calculating 
  #the cost of therapeutic treatment (set to zero for now) 
  ## updated this annotation as we now assume no resources spent to treat sick chickens
  #we are able to do this because the trial data only tells us the effect on overall mortality,
  #which inherently takes into account the portion which develop res and sus infections
  m_param_c_base[ , "r"] <- rep(chicken[parameter=="well_r",value], n.t)
  m_param_c_base[ , "s"] <- rep(chicken[parameter=="well_s",value], n.t)
  m_param_c_base[ , "mort_s"] <- rep(chicken[parameter=="all_dead",value], n.t) 
  m_param_c_base[ , "mort_r"] <- rep(chicken[parameter=="all_dead",value], n.t)
  m_param_c_base[ , "rec_r"] <- rep(1-(m_param_c_base[1,"mort_r"]), n.t)
  m_param_c_base[ , "rec_s"] <- rep(1-(m_param_c_base[1,"mort_s"]), n.t)
  m_param_c_base[ , "birth"] <- rep(chicken[parameter=="birth_well",value], n.t)
  m_param_c_base[ , "mort_w"] <- rep(chicken[parameter=="all_dead",value], n.t)
  m_param_c_base[ , "w_sold"] <- rep(1, n.t) 
  
  #make it so that the total incidence of infections stays the same, and only the
  #portion of them that are resistant changes
  for(i in 1:n.t){
    m_param_c_base[i, "s"] <- chicken[parameter=="disease_risk", value] - m_param_c_base[i, "r"]
  } ## this is already the case for now
  
  m_param_c_base[1, 1:length(state_names_c)] <- state_i_c
  
  # New Chicken Epi Function ------------------------------------------------
  
  f_chicken_epi <- function(m_param_c_base, n.t){
    
    m_param_c_temp <- m_param_c_base[1:4,]
    rownames(m_param_c_temp) <- NULL ##removing rownames
    
    i <- 2 ## getting sick
    m_param_c_temp[i,"well"] <- m_param_c_temp[i-1,"well"] -(m_param_c_temp[i-1,"r"]*m_param_c_temp[i-1,"well"]) -
      (m_param_c_temp[i-1,"s"]*m_param_c_temp[i-1,"well"])
    m_param_c_temp[i,"res"] <- m_param_c_temp[i-1,"res"] + (m_param_c_temp[i-1,"r"]*m_param_c_temp[i-1,"well"]) 
    m_param_c_temp[i,"sus"] <- m_param_c_temp[i-1,"sus"] + (m_param_c_temp[i-1,"s"]*m_param_c_temp[i-1,"well"])
    
    i <- 3 ## dying and recovering
    m_param_c_temp[i,"fallen"] <- (m_param_c_temp[i-1,"mort_w"]*m_param_c_temp[i-1,"well"]) +
      (m_param_c_temp[i-1,"mort_r"]*m_param_c_temp[i-1,"res"]) + 
      (m_param_c_temp[i-1,"mort_s"]*m_param_c_temp[i-1,"sus"])
    m_param_c_temp[i, "well"] <- m_param_c_temp[i-1,"well"] - (m_param_c_temp[i-1,"well"]*m_param_c_temp[i-1,"mort_w"]) +
      (m_param_c_temp[i-1,"rec_r"]*m_param_c_temp[i-1,"res"])+ 
      (m_param_c_temp[i-1,"rec_s"]*m_param_c_temp[i-1,"sus"])
    
    i <- 4 ##sold
    m_param_c_temp[i,"sold"] <- (m_param_c_temp[i-1,"w_sold"]*m_param_c_temp[i-1,"well"])
    
    ## final states
    m_c_sum <- m_param_c_temp[4,]
    m_c_sum["res"] <- m_param_c_temp[2,"res"]
    m_c_sum["sus"] <- m_param_c_temp[2,"sus"]
    m_c_sum["well"] <- chicken[parameter=="n_animals", value] - m_c_sum["res"] - m_c_sum["sus"] ## reset the number in 'well'
    
    m_c_sum[1:5] <- chicken[parameter=="annual_cycles",value] * m_c_sum[1:5] #multiply by the number of annual cycles
    
    m_param_c <- matrix(rep(m_c_sum), nrow=n.t, ncol =length(parameter_names_c))
    m_param_c <- t(replicate(n.t,m_c_sum))
    colnames(m_param_c) <- parameter_names_c
    rownames(m_param_c) <- paste("cycle", 0:(n.t-1), sep  =  "")
    
    m_param_c
    
    return(m_param_c)

  }
  
  #apply the chicken epi function: ## named for chicken
  m_param_c <- f_chicken_epi(m_param_c_base,n.t) ## named for chicken
  ### ignore totals of transition probs etc. as they are over counted etc.
  ## just want to focus on health state totals
  
  
  # Pig Epi Model -----------------------------------------------------------
  
  state_names_p <- c("well", "res","sus","fallen","sold") ## the compartments
  transition_names_p  <- c("birth","r","s","mort_r", "mort_s","mort_w", "rec_r","rec_s","w_sold")  ## the rates
  parameter_names_p <- c(state_names_p, transition_names_p) ## fixed names to make them _p instead of _c
  
  state_i_p <- c(pig[parameter=="n_animals",value], rep(0,length=length(state_names_p)-1))
  
  m_param_p_base <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_p))
  colnames(m_param_p_base) <- parameter_names_p
  rownames(m_param_p_base) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  #here, we have a set mortality rate for all states - 
  #it only matters if animals are in 'res' or 'sus' for the purpose of calculating 
  #the cost of therapeutic treatment
  #we are able to do this because the trial data only tells us the effect on overall mortality,
  #which inherently takes into account the portion which develop res and sus infections
  m_param_p_base[ , "r"] <- rep(pig[parameter=="well_r",value], n.t)
  m_param_p_base[ , "s"] <- rep(pig[parameter=="well_s",value], n.t)
  m_param_p_base[ , "mort_s"] <- rep(pig[parameter=="all_dead",value], n.t) 
  m_param_p_base[ , "mort_r"] <- rep(pig[parameter=="all_dead",value], n.t)
  m_param_p_base[ , "rec_r"] <- rep(1-(m_param_p_base[1,"mort_r"]), n.t)
  m_param_p_base[ , "rec_s"] <- rep(1-(m_param_p_base[1,"mort_s"]), n.t)
  m_param_p_base[ , "birth"] <- rep(pig[parameter=="birth_well",value], n.t)
  m_param_p_base[ , "mort_w"] <- rep(pig[parameter=="all_dead",value], n.t)
  m_param_p_base[ , "w_sold"] <- rep(1, n.t) 
  
  #make it so that the total incidence of infections stays the same, and only the
  #portion of them that are resistant changes
  for(i in 1:n.t){
    m_param_p_base[i, "s"] <- pig[parameter=="disease_risk", value] - m_param_p_base[i, "r"]
  }
  
  m_param_p_base[1, 1:length(state_names_p)] <- state_i_p
  
  
  # New Pig Epi Function ----------------------------------------------------
  
  f_pig_epi <- function(m_param_p_base, n.t){
    
    m_param_p_temp <- m_param_p_base[1:4,]
    rownames(m_param_p_temp) <- NULL ##removing rownames
    
    i <- 2 ## getting sick
    m_param_p_temp[i,"well"] <- m_param_p_temp[i-1,"well"] -(m_param_p_temp[i-1,"r"]*m_param_p_temp[i-1,"well"]) -
      (m_param_p_temp[i-1,"s"]*m_param_p_temp[i-1,"well"])
    m_param_p_temp[i,"res"] <- m_param_p_temp[i-1,"res"] + (m_param_p_temp[i-1,"r"]*m_param_p_temp[i-1,"well"]) 
    m_param_p_temp[i,"sus"] <- m_param_p_temp[i-1,"sus"] + (m_param_p_temp[i-1,"s"]*m_param_p_temp[i-1,"well"])
    
    i <- 3 ## dying and recovering
    m_param_p_temp[i,"fallen"] <- (m_param_p_temp[i-1,"mort_w"]*m_param_p_temp[i-1,"well"]) +
      (m_param_p_temp[i-1,"mort_r"]*m_param_p_temp[i-1,"res"]) + 
      (m_param_p_temp[i-1,"mort_s"]*m_param_p_temp[i-1,"sus"])
    m_param_p_temp[i, "well"] <- m_param_p_temp[i-1,"well"] - (m_param_p_temp[i-1,"well"]*m_param_p_temp[i-1,"mort_w"]) +
      (m_param_p_temp[i-1,"rec_r"]*m_param_p_temp[i-1,"res"])+ 
      (m_param_p_temp[i-1,"rec_s"]*m_param_p_temp[i-1,"sus"])
    
    i <- 4 ##sold
    m_param_p_temp[i,"sold"] <- (m_param_p_temp[i-1,"w_sold"]*m_param_p_temp[i-1,"well"])
    
    ## final states
    m_p_sum <- m_param_p_temp[4,]
    m_p_sum[2] <- m_param_p_temp[2,2]
    m_p_sum[3] <- m_param_p_temp[2,3]
    m_p_sum[1] <- pig[parameter=="n_animals", value] - m_p_sum[2] - m_p_sum[3] ## reset the number in 'well'
    
    m_p_sum[1:5] <- pig[parameter=="annual_cycles",value] * m_p_sum[1:5] #multiply by the number of annual cycles
    
    m_param_p <- matrix(rep(m_p_sum), nrow=n.t, ncol =length(parameter_names_p))
    m_param_p <- t(replicate(n.t,m_p_sum))
    colnames(m_param_p) <- parameter_names_p
    rownames(m_param_p) <- paste("cycle", 0:(n.t-1), sep  =  "")
    
    return(m_param_p)
    
  }
  
  #apply the pig epi function: ## renamed for pig
  m_param_p <- f_pig_epi(m_param_p_base,n.t) ## renamed for pig
  ### ignore totals of transition probs etc. as they are over counted etc.
  ## just want to focus on health state totals
  
  # Chicken Farm Costs --------------------------------------------------------------
  
  m_cost_c <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_c))
  colnames(m_cost_c) <- parameter_names_c
  rownames(m_cost_c) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  c_w_c <- chicken[parameter=="c_animal",value] ## defining cost of keeping the animal
  c_s_c <- chicken[parameter=="s_cost",value] ## defining cost of treating infections
  c_r_c <- chicken[parameter=="r_cost",value]
  
  cost_i_c <- c(c_w_c,c_w_c + c_r_c,c_w_c + c_s_c,0,0) #the farm still pays upkeep for sick animals
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_cost_c[2, 1:length(state_names_c)] <- cost_i_c 
  
  #discount the farm costs
  for (j in 1:length(state_names_c)) {
    for (i in 3:(n.t)){
      m_cost_c[i,j] <- f_di(m_cost_c[i-1,j],dr)
    }  
  }
  
  # Chicken Farm Rewards ------------------------------------------------------------
  
  m_rwd_c <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_c))
  colnames(m_rwd_c) <- parameter_names_c
  rownames(m_rwd_c) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  #only get a reward for selling animals
  r_sold_c <- chicken[parameter=="i_animal",value] ## defining the income from a sold animal
  rwd_i_c <- c(0,0,0,0,r_sold_c)
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_rwd_c[2, 1:length(state_names_c)] <- rwd_i_c
  
  #discount
  for (j in 1:length(state_names_c)) {
    for (i in 3:(n.t)){
      m_rwd_c[i,j] <- f_di(m_rwd_c[i-1,j],dr)
    }  
  }
  
  
  # Pig Farm Costs -----------------------------------------------------------
  
  m_cost_p <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_p))
  colnames(m_cost_p) <- parameter_names_p
  rownames(m_cost_p) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  c_w_p <- pig[parameter=="c_animal",value] ## defining cost of keeping the animal
  c_s_p <- pig[parameter=="s_cost",value] ## defining cost of treating infections
  c_r_p <- pig[parameter=="r_cost",value]
  
  cost_i_p <- c(c_w_p,c_w_p + c_r_p,c_w_p + c_s_p,0,0) #the farm still pays upkeep for sick animals
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_cost_p[2, 1:length(state_names_p)] <- cost_i_p
  
  #discount the farm costs
  for (j in 1:length(state_names_p)) {
    for (i in 3:(n.t)){
      m_cost_p[i,j] <- f_di(m_cost_p[i-1,j],dr) ## corrected as it initially edited m_cost_c rather than the pig cost matrix
    }  
  }
  
  # Pig Farm Rewards --------------------------------------------------------
  
  m_rwd_p <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_p))
  colnames(m_rwd_p) <- parameter_names_p
  rownames(m_rwd_p) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  #only get a reward for selling animals
  r_sold_p <- pig[parameter=="i_animal",value] ## defining the income from a sold animal
  rwd_i_p <- c(0,0,0,0,r_sold_p)
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_rwd_p[2, 1:length(state_names_p)] <- rwd_i_p
  
  #discount
  for (j in 1:length(state_names_p)) {
    for (i in 3:(n.t)){
      m_rwd_p[i,j] <- f_di(m_rwd_p[i-1,j],dr)
    }  
  }
  
  # Intervention ------------------------------------------------------------
  
  ### reduction in incidence of drug resistant infections in humans
  
  m_param2 <- m_param ## human parameter matrix for scenario 2
  
  #reduce the chance of getting a resistant infection in humans, depending on the link parameter used
  
  for(i in 1:n.t){ ## fixed because previously the intervention was erasing the background growth in human AMR 
    if(scenario_transmission == "low"){
      m_param2[i , "r"] <- m_param2[i , "r"]*(1+intervention[parameter=="u_RH_low",value])
    } else if (scenario_transmission == "med"){
      m_param2[i , "r"] <- m_param2[i , "r"]*(1+intervention[parameter=="u_RH_med",value])
    } else if (scenario_transmission == "hi"){
      m_param2[i , "r"] <- m_param2[i , "r"]*(1+intervention[parameter=="u_RH_hi",value])
    } else if (scenario_transmission == "max"){
      m_param2[i , "r"] <- m_param2[i , "r"]*(1+intervention[parameter=="u_RH_max",value])
    } else{
      paste("ERROR: PLEASE CHOOSE AN APPROACH TO ESTIMATING THE EFFECT ON HUMAN AMR")
    }
  }
  
  #make sure that the total number of infections remains constant
  for(i in 1:n.t){
    if(m_param2[i, "r"] > 0.9*(m_param2[1,"r"]+m_param2[1,"s"])){ ##DTE changed maximum portion resistant to 90%
      m_param2[i, "r"] <- 0.9*(m_param2[1,"r"]+m_param2[1,"s"]) ##DTE changed maximum portion resistant to 90%
    }
    m_param2[i, "s"] <- m_param2[1,"r"]+m_param2[1,"s"] - m_param2[i, "r"]
  }
  
  ## clear state values
  m_param2[ , 1:length(state_names)] <- 0
  m_param2[1, 1:length(state_names)] <- state_i
  
  m_param2 <- f_human_epi(m_param2, n.t) #apply the human epi function to the intervention case
  
  
  ## chickens
  m_param_c2 <- m_param_c_base #create an animal parameter spreadsheet for the intervention case
  
  #change in chicken mortality
  c_mort_int <- chicken[parameter=="all_dead", value] + (chicken[parameter=="all_dead", value]*intervention[parameter=="chicken_mort_effect", value])
  m_param_c2[ , "mort_w"] <- rep(c_mort_int, n.t)
  
  m_param_c2[ , "mort_s"] <- rep(chicken[parameter=="all_dead", value] + (chicken[parameter=="all_dead", value]*intervention[parameter=="chicken_mort_effect", value]), 
                                 n.t)
  
  m_param_c2[ , "mort_r"] <- rep(chicken[parameter=="all_dead", value] + (chicken[parameter=="all_dead", value]*intervention[parameter=="chicken_mort_effect", value]), 
                                 n.t)
  
  
  #make sure the total number of infections stays constant
  for(i in 1:n.t){
    m_param_c2[i, "s"] <- chicken[parameter=="disease_risk", value] - m_param_c2[i, "r"]
  }
  
  m_param_c2[ , 1:length(state_names_c)] <- 0
  m_param_c2[1, 1:length(state_names_c)] <- state_i_c
  
  #apply the chicken epi function to the intervention case parameter spreadsheet ## changed to be chicken
  m_param_c2 <- f_chicken_epi(m_param_c2, n.t) ## changed to be chicken
  
  #rewards
  m_rwd_c2 <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_c))
  colnames(m_rwd_c2) <- parameter_names_c
  rownames(m_rwd_c2) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  r_sold_c2 <- (chicken[parameter=="i_animal",value])*(1+intervention[parameter=="chicken_income_effect", value]) 
  rwd_i_c2 <- c(0,0,0,0,r_sold_c2)
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_rwd_c2[2, 1:length(state_names_c)] <- rwd_i_c2
  
  #discount
  for (j in 1:length(state_names_c)) {
    for (i in 3:(n.t)){
      m_rwd_c2[i,j] <- f_di(m_rwd_c2[i-1,j],dr)
    }  
  }
  
  #costs
  m_cost_c2 <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_c))
  colnames(m_cost_c2) <- parameter_names_c
  rownames(m_cost_c2) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  c_s_c <- chicken[parameter=="s_cost",value] ## changed to _c rather than _p
  c_r_c <- chicken[parameter=="r_cost",value] #removed double-paying ## changed to _c rather than _p
  
  cost_i_c2 <- c(c_w_c, c_w_c + c_r_c,c_w_c + c_s_c,0,0) #you still pay upkeep on animals who are treated or infected ## changed to _c rather than _p
  
  ## start at cycle 1 so you do not multiply initial state vector
  m_cost_c2[2, 1:length(state_names_c)] <- cost_i_c2
  
  #discount
  for (j in 1:length(state_names_c)) {
    for (i in 3:(n.t)){
      m_cost_c2[i,j] <- f_di(m_cost_c2[i-1,j],dr)
    }
  }
  
  ##pigs
  
  m_param_p2 <- m_param_p_base #create an animal parameter spreadsheet for the intervention case
  
  #change in pig mortality
  
  p_mort_int <- pig[parameter=="all_dead", value] + (pig[parameter=="all_dead", value]*intervention[parameter=="pig_mort_effect", value]*intervention[parameter=="uptake", value]) ##adjusted for uptake
  m_param_p2[ , "mort_w"] <- rep(p_mort_int, n.t)
  
  m_param_p2[ , "mort_s"] <- rep(pig[parameter=="all_dead", value] + (pig[parameter=="all_dead", value]*intervention[parameter=="pig_mort_effect", value]*intervention[parameter=="uptake", value]), ##adjusted for uptake
                                 n.t)
  
  m_param_p2[ , "mort_r"] <- rep(pig[parameter=="all_dead", value] + (pig[parameter=="all_dead", value]*intervention[parameter=="pig_mort_effect", value]*intervention[parameter=="uptake", value]), ##adjusted for uptake
                                 n.t)
  
  
  #make sure the total number of infections stays constant
  for(i in 1:n.t){
    m_param_p2[i, "s"] <- pig[parameter=="disease_risk", value] - m_param_p2[i, "r"]
  }
  
  m_param_p2[ , 1:length(state_names_p)] <- 0
  m_param_p2[1, 1:length(state_names_p)] <- state_i_p
  
  #apply the animal epi function to the intervention case parameter spreadsheet
  m_param_p2 <- f_pig_epi(m_param_p2, n.t) ## changed to f_pig_epi
  
  #rewards
  m_rwd_p2 <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_p))
  colnames(m_rwd_p2) <- parameter_names_p
  rownames(m_rwd_p2) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  r_sold_p2 <- (pig[parameter=="i_animal",value])*(1+intervention[parameter=="pig_income_effect", value]*intervention[parameter=="uptake", value])+intervention[parameter=="pig_money_saved",value]*intervention[parameter=="uptake", value] ##adjusted for uptake 
  rwd_i_p2 <- c(0,0,0,0,r_sold_p2)
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_rwd_p2[2, 1:length(state_names_p)] <- rwd_i_p2
  
  #discount
  for (j in 1:length(state_names_p)) {
    for (i in 3:(n.t)){
      m_rwd_p2[i,j] <- f_di(m_rwd_p2[i-1,j],dr)
    }  
  }
  
  #costs
  m_cost_p2 <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_p))
  colnames(m_cost_p2) <- parameter_names_p
  rownames(m_cost_p2) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  c_s_p <- pig[parameter=="s_cost",value]
  c_r_p <- pig[parameter=="r_cost",value] #removed double-paying 
  
  cost_i_p2 <- c(c_w_p, c_w_p + c_r_p,c_w_p + c_s_p,0,0) #you still pay upkeep on animals who are treated or infected
  
  ## start at cycle 1 so you do not multiply initial state vector
  m_cost_p2[2, 1:length(state_names_p)] <- cost_i_p2
  
  #discount
  for (j in 1:length(state_names_p)) {
    for (i in 3:(n.t)){
      m_cost_p2[i,j] <- f_di(m_cost_p2[i-1,j],dr)
    }
  }
  
  
  # Results -----------------------------------------------------------------
  
  
  #results matrix for healthcare
  results_base_h <- f_expvalue(m_param,m_cost,m_rwd)
  results_interv_h <- f_expvalue(m_param2,m_cost,m_rwd)
  
  total_results_HC<- matrix(rep(0), nrow=2, ncol=2)
  colnames(total_results_HC) <- c("Costs (Â£)", "QALYs")
  rownames(total_results_HC) <- c("Base Case", "Intervention")
  
  total_results_HC[1,] <- results_base_h[1,]
  total_results_HC[2,] <- results_interv_h[1,]
  
  #results matrix for productivity
  results_base_prod <- f_expvalue(m_param,m_cost_prod,m_rwd_prod)
  results_interv_prod <- f_expvalue(m_param2,m_cost_prod,m_rwd_prod)
  
  total_results_prod <- matrix(rep(0), nrow=2, ncol=2)
  colnames(total_results_prod) <- c("Productivity", "QALYs")
  rownames(total_results_prod) <- c("Base Case", "Intervention")
  
  total_results_prod[1,2] <- results_base_h[,2]
  total_results_prod[2,2] <- results_interv_h[,2]
  total_results_prod[1,1] <- results_base_prod[,2]   #will be negative
  total_results_prod[2,1] <- results_interv_prod[,2] #will be negative but hopefully closer to zero
  
  #because there technically aren't any costs for the productivity side,
  #we use the negative productivity gain from base to intervention (a productivity gain would incur a negative 'cost')
  
  #results matrix for chickens
  results_base_c <- f_expvalue(m_param_c,m_cost_c,m_rwd_c)
  results_interv_c <- f_expvalue(m_param_c2,m_cost_c2,m_rwd_c2)
  
  total_results_c<- matrix(rep(0), nrow=2, ncol=2)
  colnames(total_results_c) <- c("Costs ($)", "Benefits ($)")
  rownames(total_results_c) <- c("Base Case", "Intervention")
  
  #results matrix for pigs
  results_base_p <- f_expvalue(m_param_p,m_cost_p,m_rwd_p)
  results_interv_p <- f_expvalue(m_param_p2,m_cost_p2,m_rwd_p2)
  
  total_results_p<- matrix(rep(0), nrow=2, ncol=2)
  colnames(total_results_p) <- c("Costs ($)", "Benefits ($)")
  rownames(total_results_p) <- c("Base Case", "Intervention")
  
  #outputs for healthcare 
  incr_cost_health <- (results_interv_h[1,1] - results_base_h[1,1])
  QALYs_saved <-  (results_interv_h[1,2]-results_base_h[1,2])
  NMB_health <- (QALYs_saved*wtp)-(incr_cost_health)
  
  #outputs for productivity
  incr_cost_prod <- total_results_prod[1,1] - total_results_prod[2,1] #hopefully negative
  incr_benefit_prod <- total_results_prod[2,2] - total_results_prod[1,2] #hopefully positive
  NMB_prod <- total_results_prod[2,1] - total_results_prod[1,1] #hopefully positive
  
  #outputs for chickens
  incr_cost_c <- (results_interv_c[1,1] - results_base_c[1,1])
  incr_benefit_c <-  (results_interv_c[1,2]-results_base_c[1,2])
  
  total_results_c[1,] <- results_base_c[1,] 
  total_results_c[2,] <- results_interv_c[1,] 
  
  NMB_c <- (incr_benefit_c-incr_cost_c)*chicken[parameter=="n_farms",value] #net monetary benefit to poultry sector
  
  #outputs for pigs
  incr_cost_p <- (results_interv_p[1,1] - results_base_p[1,1])
  incr_benefit_p <-  (results_interv_p[1,2]-results_base_p[1,2])
  
  total_results_p[1,] <- results_base_p[1,] 
  total_results_p[2,] <- results_interv_p[1,] 
  
  NMB_p <- (incr_benefit_p-incr_cost_p)*pig[parameter=="n_farms",value] #net monetary benefit to piggy sector
  
  #outputs for implementation costs (fixed and per-farm) ##corresponding to village vs. farm level scenario
  if(scenario_intervention_level == "Farm"){
    int_cost_per <- cost_per_farm_indiv
  } else if (scenario_intervention_level == "Village"){
    int_cost_per <- cost_per_farm_village
  }
  
  intervention_cost_year <- (int_cost_per / intervention_followup_period) ##changed to int_cost_per
  dr_int_pgrowth <- dr - human[parameter=="prod_growth", value] ##discount rate net of productivity growth, as we assume that compensation for vet and farmer time increases to reflect wage growth
  int_cost_vector <- rep(0, n.t)
  for(i in 1:n.t){
    int_cost_vector[i] <- intervention_cost_year*((1-dr_int_pgrowth)^(i-1))
  }
  intervention_cost_all <- sum(int_cost_vector)
  
  implementation_cost <- intervention[parameter=="admin_cost", value] + intervention_cost_all*chicken[parameter=="n_farms", value] + intervention_cost_all*pig[parameter=="n_farms", value]
  #one-off legislative cost
  #implementation cost for all chicken farms over 46 years
  #implementation cost for all pig farms over 46 years
  
  #outputs for overall net monetary benefit
  NMB_macro <- NMB_c + NMB_p + NMB_prod + NMB_health - implementation_cost
  
  #the final outputs (some, such as macro-level ICER, are not useful)
  outputs <- data.table(NMB_macro=NMB_macro, QALYs_saved=QALYs_saved, 
                        NMB_health=NMB_health, NMB_prod=NMB_prod, NMB_c=NMB_c, 
                        NMB_p=NMB_p, implementation_cost=implementation_cost)
  
  outputs
  
#   return(outputs)
# 
# }
# 
# model(inputs)

# Scenario Analysis -------------------------------------------------------

##AMR Growth
scenario <- "HCA"
scenario_transmission <- "med"
scenario_outcomes <- "All"
intervention_followup_period <- 2
scenario_intervention_level <- "Village"
scenario_amr_grow <- "med"

scenario_analysis_amrgrowth <- matrix(rep(0), nrow = 4, ncol = 4)
colnames(scenario_analysis_amrgrowth) <- c("NMB Overall", "QALYs Saved", "NMB to Healthcare", "Productivity Gains")
rownames(scenario_analysis_amrgrowth) <- c("1% Annual Growth", "2.83% Annual Growth", "5% Annual Growth", "10% Annual Growth")

scenario_amr_grow <- "lo"
scenario_analysis_amrgrowth[1,1:4] <- as.numeric(model(inputs)[1,1:4]) 
scenario_amr_grow <- "med"
scenario_analysis_amrgrowth[2,1:4] <- as.numeric(model(inputs)[1,1:4]) 
scenario_amr_grow <- "hi"
scenario_analysis_amrgrowth[3,1:4] <- as.numeric(model(inputs)[1,1:4]) 
scenario_amr_grow <- "max"
scenario_analysis_amrgrowth[4,1:4] <- as.numeric(model(inputs)[1,1:4]) 

write.xlsx(scenario_analysis_amrgrowth, "C:/Users/tresc/Desktop/AMR-Model/Intervention 1/Scenario Analysis AMR Growth.xlsx")


##Transmission to Humans, Productivity Method, Bacteria Concerned

###Farm level
scenario <- "HCA"
scenario_transmission <- "med"
scenario_outcomes <- "All"
intervention_followup_period <- 1
scenario_intervention_level <- "Farm"
scenario_amr_grow <- "med" ##DTE reset to default AMR growth scenario

scenario_analysis_all <- matrix(rep(0), nrow = 4, ncol = 2)
colnames(scenario_analysis_all) <- c("Human Capital Approach", "Friction Cost Approach")
rownames(scenario_analysis_all) <- c("-0.025", "-0.05", "-0.10", "-0.16")

scenario_outcomes <- "All"

scenario <- "HCA"
scenario_transmission <- "low"
scenario_analysis_all[1,1] <- as.numeric(model(inputs)[1,1])
scenario_transmission <- "med"
scenario_analysis_all[2,1] <- as.numeric(model(inputs)[1,1])
scenario_transmission <- "hi"
scenario_analysis_all[3,1] <- as.numeric(model(inputs)[1,1])
scenario_transmission <- "max"
scenario_analysis_all[4,1] <- as.numeric(model(inputs)[1,1])

scenario <- "FCA"
scenario_transmission <- "low"
scenario_analysis_all[1,2] <- as.numeric(model(inputs)[1,1])
scenario_transmission <- "med"
scenario_analysis_all[2,2] <- as.numeric(model(inputs)[1,1])
scenario_transmission <- "hi"
scenario_analysis_all[3,2] <- as.numeric(model(inputs)[1,1])
scenario_transmission <- "max"
scenario_analysis_all[4,2] <- as.numeric(model(inputs)[1,1])

write.xlsx(scenario_analysis_all, "C:/Users/tresc/Desktop/AMR-Model/Intervention 1/Scenario Analysis All.xlsx")

scenario_analysis_Enterobacteriaceae <- matrix(rep(0), nrow = 4, ncol = 2)
colnames(scenario_analysis_Enterobacteriaceae) <- c("Human Capital Approach", "Friction Cost Approach")
rownames(scenario_analysis_Enterobacteriaceae) <- c("-0.025", "-0.05", "-0.10", "-0.16")

scenario_outcomes <- "Enterobacteria"

scenario <- "HCA"
scenario_transmission <- "low"
scenario_analysis_Enterobacteriaceae[1,1] <- as.numeric(model(inputs)[1,1])
scenario_transmission <- "med"
scenario_analysis_Enterobacteriaceae[2,1] <- as.numeric(model(inputs)[1,1])
scenario_transmission <- "hi"
scenario_analysis_Enterobacteriaceae[3,1] <- as.numeric(model(inputs)[1,1])
scenario_transmission <- "max"
scenario_analysis_Enterobacteriaceae[4,1] <- as.numeric(model(inputs)[1,1])

scenario <- "FCA"
scenario_transmission <- "low"
scenario_analysis_Enterobacteriaceae[1,2] <- as.numeric(model(inputs)[1,1])
scenario_transmission <- "med"
scenario_analysis_Enterobacteriaceae[2,2] <- as.numeric(model(inputs)[1,1])
scenario_transmission <- "hi"
scenario_analysis_Enterobacteriaceae[3,2] <- as.numeric(model(inputs)[1,1])
scenario_transmission <- "max"
scenario_analysis_Enterobacteriaceae[4,2] <- as.numeric(model(inputs)[1,1])

write.xlsx(scenario_analysis_Enterobacteriaceae, "C:/Users/tresc/Desktop/AMR-Model/Intervention 1/Scenario Analysis Enterobacteriaceae.xlsx")


##Intervention Frequeency and Village vs. Farm

scenario_analysis_intervention_frequency <- matrix(rep(0), nrow = 4, ncol = 8)
colnames(scenario_analysis_intervention_frequency) <- c("Portion of Farms Visited Every Year (%)", "Net Monetary Benefit (Overall)",
                                                        "QALYs Saved", "Net Monetary Benefit (Healthcare)", 
                                                        "Net Monetary Benefit (Productivity)", "Net Monetary Benefit (Poultry)",
                                                        "Net Monetary Benefit (Pig Sector)", "Implementation cost")

scenario <- "HCA"
scenario_transmission <- "med"
scenario_outcomes <- "All"
scenario_intervention_level <- "Farm"
scenario_amr_grow <- "med" ##DTE reset to default AMR growth scenario

intervention_followup_period <- 1
scenario_analysis_intervention_frequency[1,1] <- "100"
scenario_analysis_intervention_frequency[1,2:8] <- as.numeric(model(inputs)[1,1:7])

intervention_followup_period <- 2
scenario_analysis_intervention_frequency[2,1] <- "50"
scenario_analysis_intervention_frequency[2,2] <- as.numeric(model(inputs)[1,1])
scenario_analysis_intervention_frequency[2,2:8] <- as.numeric(model(inputs)[1,1:7])

intervention_followup_period <- 4
scenario_analysis_intervention_frequency[3,1] <- "25"
scenario_analysis_intervention_frequency[3,2] <- as.numeric(model(inputs)[1,1])
scenario_analysis_intervention_frequency[3,2:8] <- as.numeric(model(inputs)[1,1:7])

intervention_followup_period <- 10
scenario_analysis_intervention_frequency[4,1] <- "10"
scenario_analysis_intervention_frequency[4,2] <- as.numeric(model(inputs)[1,1])
scenario_analysis_intervention_frequency[4,2:8] <- as.numeric(model(inputs)[1,1:7])

write.xlsx(scenario_analysis_intervention_frequency, "C:/Users/tresc/Desktop/AMR-Model/Intervention 1/Scenario Analysis Intervention Frequency Farm.xlsx")

scenario <- "HCA"
scenario_transmission <- "med"
scenario_outcomes <- "All"
scenario_intervention_level <- "Village"
scenario_amr_grow <- "med" ##DTE reset to default AMR growth scenario

intervention_followup_period <- 1
scenario_analysis_intervention_frequency[1,1] <- "100"
scenario_analysis_intervention_frequency[1,2:8] <- as.numeric(model(inputs)[1,1:7])

intervention_followup_period <- 2
scenario_analysis_intervention_frequency[2,1] <- "50"
scenario_analysis_intervention_frequency[2,2] <- as.numeric(model(inputs)[1,1])
scenario_analysis_intervention_frequency[2,2:8] <- as.numeric(model(inputs)[1,1:7])

intervention_followup_period <- 4
scenario_analysis_intervention_frequency[3,1] <- "25"
scenario_analysis_intervention_frequency[3,2] <- as.numeric(model(inputs)[1,1])
scenario_analysis_intervention_frequency[3,2:8] <- as.numeric(model(inputs)[1,1:7])

intervention_followup_period <- 10
scenario_analysis_intervention_frequency[4,1] <- "10"
scenario_analysis_intervention_frequency[4,2] <- as.numeric(model(inputs)[1,1])
scenario_analysis_intervention_frequency[4,2:8] <- as.numeric(model(inputs)[1,1:7])

write.xlsx(scenario_analysis_intervention_frequency, "C:/Users/tresc/Desktop/AMR-Model/Intervention 1/Scenario Analysis Intervention Frequency Village.xlsx")

##Timeframe

scenario_analysis_timeframe <- matrix(rep(0), nrow = 5, ncol = 8)
colnames(scenario_analysis_timeframe) <- c("Timeframe (Years)", "Net Monetary Benefit (Overall)",
                                                        "QALYs Saved", "Net Monetary Benefit (Healthcare)", 
                                                        "Net Monetary Benefit (Productivity)", "Net Monetary Benefit (Poultry)",
                                                        "Net Monetary Benefit (Pig Sector)", "Implementation cost")
scenario <- "HCA"
scenario_transmission <- "med"
scenario_outcomes <- "All"
scenario_intervention_level <- "Village"
intervention_followup_period <- 2
scenario_amr_grow <- "med" ##DTE reset to default AMR growth scenario

n.t <- 11
scenario_analysis_timeframe[1,1] <- "10"
scenario_analysis_timeframe[1,2:8] <- as.numeric(model(inputs)[1,1:7])

n.t <- 21
scenario_analysis_timeframe[2,1] <- "20"
scenario_analysis_timeframe[2,2:8] <- as.numeric(model(inputs)[1,1:7])

n.t <- 31
scenario_analysis_timeframe[3,1] <- "30"
scenario_analysis_timeframe[3,2:8] <- as.numeric(model(inputs)[1,1:7])

n.t <- 41
scenario_analysis_timeframe[4,1] <- "40"
scenario_analysis_timeframe[4,2:8] <- as.numeric(model(inputs)[1,1:7])

n.t <- 47
scenario_analysis_timeframe[5,1] <- "46"
scenario_analysis_timeframe[5,2:8] <- as.numeric(model(inputs)[1,1:7])

write.xlsx(scenario_analysis_timeframe, "C:/Users/tresc/Desktop/AMR-Model/Intervention 1/Scenario Analysis Timeframe.xlsx")

##to plot NMB at different timeframes

timeframe <- matrix(rep(0), nrow = 37, ncol = 2)
colnames(timeframe) <- c("Timeframe (Years)", "Net Monetary Benefit (Overall), $USD")

timeframe[1:37,1] <- c(10:46)

for (i in 11:47) {
  n.t <- i
  timeframe[i-10,2] <- as.numeric(model(inputs)[1,1])
  rm(i)
}

plot(timeframe,
     main = "Net Monetary Benefit along Different Time Frames")

timeframe <- as.data.table(timeframe)
min(timeframe$`Net Monetary Benefit (Overall), $USD`)
max(timeframe$`Net Monetary Benefit (Overall), $USD`)

##plot various NMBs at different timeframes to see why it changes over time

timeframe2 <- matrix(rep(0), nrow = 37, ncol = 7)
colnames(timeframe2) <- c("timeframe", "nmb_all",
                         "nmb_health", "nmb_prod",
                         "nmb_chicken", "nmb_pig",
                         "imp_cost")

timeframe2[1:37,1] <- c(10:46)

for (i in 11:47) {
  n.t <- i
  timeframe2[i-10,2] <- as.numeric(model(inputs)[1,1])
  timeframe2[i-10,3] <- as.numeric(model(inputs)[1,3])
  timeframe2[i-10,4] <- as.numeric(model(inputs)[1,4])
  timeframe2[i-10,5] <- as.numeric(model(inputs)[1,5])
  timeframe2[i-10,6] <- as.numeric(model(inputs)[1,6])
  timeframe2[i-10,7] <- as.numeric(model(inputs)[1,7])
  rm(i)
}

timeframe2df <- as.data.frame(timeframe2)

ggplot(data = timeframe2df, aes(x = timeframe, y = nmb_all)) +
  geom_line(aes(x = timeframe, y = nmb_all), colour = 'yellow') +
  geom_line(aes(x = timeframe, y = nmb_health), colour = 'purple') +
  geom_line(aes(x = timeframe, y = nmb_prod), colour = 'black') +
  geom_line(aes(x = timeframe, y = nmb_chicken), colour = 'green') +
  geom_line(aes(x = timeframe, y = nmb_pig), colour = 'pink') +
  geom_line(aes(x = timeframe, y = imp_cost), colour = 'lightblue') +
  xlab("Timeframe (Years)") +
  ylab("Cost / Benefit, $USD") +
  ggtitle("Net Monetary Benefit over Different Timeframes, Decomposed") +
  theme_bw()

dfm <- melt(timeframe2df, id = "Timeframe") ##continue from here

# Bar Plots ---------------------------------------------------------------

counts_1 <- c(1.77, 0.828, 0.0295, 0.00959, -7.35)
barplot(counts_1, main="Contribution to Net Monetary Benefit, Default Scenario (bn $USD)", horiz=F,
        names.arg=c("Poultry Sector", "Pig Sector", "Labour Productivity", "Healthcare Sector", "Implementation Cost"))

counts_2 <- c(1.77, 0.828, 0.0295, 0.00959, -01.79)
barplot(counts_2, main="Contribution to Net Monetary Benefit, Default Scenario (bn $USD)", horiz=F,
        names.arg=c("Poultry Sector", "Pig Sector", "Labour Productivity", "Healthcare Sector", "Implementation Cost"))

# Pessimistic Scenario ----------------------------------------------------

inputs[68,4] <- inputs[parameter == "uptake", low] #uptake only 50% (pigs)
inputs[67,4] <- inputs[parameter == "pig_mort_effect", low] #pig mortality increases 5%
inputs[66,4] <- inputs[parameter == "chicken_mort_effect", low] #only 30% fall, not 40%
cost_per_farm_village <- inputs[parameter == "farm_int_cost", high] #80 USD instead of 58
inputs[64,4] <- inputs[parameter == "chicken_income_effect", low] #only gain 5%, not 7.1%
inputs[63,4] <- inputs[parameter == "pig_money_saved", low] #only save 1USD on pig feed instead of 2.07
inputs[62,4] <- inputs[parameter == "pig_income_effect", low] #pigs are now 2.5% lighter after the intervention
inputs[61,4] <- inputs[parameter == "admin_cost", low] #now the fixed cost is $10mUSD instead of $1m
inputs[58,4] <- inputs[parameter == "u_RH_med", low] #no effect on human AMR

scenario <- "HCA"
scenario_transmission <- "med"
scenario_outcomes <- "All"
scenario_intervention_level <- "Village"
scenario_amr_grow <- "med" ##DTE reset to default AMR growth scenario

intervention_followup_period <- 1
model(inputs)
intervention_followup_period <- 2
model(inputs)
intervention_followup_period <- 4
model(inputs)
intervention_followup_period <- 10
model(inputs)

inputs <- read.csv("C:/Users/tresc/Desktop/AMR-Model/intervention 1/inputs.csv")
inputs <- as.data.table(inputs)

# Montecarlo Simulation --------------------------------------

##Setting scenarios
scenario <- "HCA"
scenario_transmission <- "med"
scenario_outcomes <- "All"
scenario_intervention_level <- "Village"
intervention_followup_period <- 2
scenario_amr_grow <- "med" ##DTE reset to default AMR growth scenario
cost_per_farm_village <- seminar_cost/farmers_per_seminar + 
  seminar_length*hourly_compensation + transport_cost +
  (visit_cost + ((group_size - 1)*additional_time_per_farm*hourly_compensation))*(visits_per_year/group_size) +
  visit_length_village*hourly_compensation*visits_per_year

##Creating a vector to store the NMBs 
CEAC_NMB_vector <- c(rep(0,10000))

inputs <- read.csv("C:/Users/tresc/Desktop/AMR-Model/intervention 1/inputs.csv")
inputs <- as.data.table(inputs)
colnames(inputs) <- c("scenario", "parameter", "description", "value", "distribution", "low", "high", "notes")
inputsPSA <- inputs

set.seed(42069)

for(i in 1:10000){
  #load dataset
  inputsPSA <- inputs

  inputsPSA[1,4] <- runif(1,as.numeric(inputsPSA[1,6]),as.numeric(inputsPSA[1,7]))
  inputsPSA[3,4] <- runif(1,as.numeric(inputsPSA[3,6]),as.numeric(inputsPSA[3,7]))
  inputsPSA[4,4] <- runif(1,as.numeric(inputsPSA[4,6]),as.numeric(inputsPSA[4,7]))
  inputsPSA[5,4] <- runif(1,as.numeric(inputsPSA[5,6]),as.numeric(inputsPSA[5,7]))
  inputsPSA[6,4] <- runif(1,as.numeric(inputsPSA[6,6]),as.numeric(inputsPSA[6,7]))
  inputsPSA[8,4] <- runif(1,as.numeric(inputsPSA[8,6]),as.numeric(inputsPSA[8,7]))
  inputsPSA[9,4] <- runif(1,as.numeric(inputsPSA[9,6]),as.numeric(inputsPSA[9,7]))
  inputsPSA[11,4] <- runif(1,as.numeric(inputsPSA[11,6]),as.numeric(inputsPSA[11,7]))
  inputsPSA[12,4] <- runif(1,as.numeric(inputsPSA[12,6]),as.numeric(inputsPSA[12,7]))
  inputsPSA[13,4] <- runif(1,as.numeric(inputsPSA[13,6]),as.numeric(inputsPSA[13,7]))
  inputsPSA[14,4] <- runif(1,as.numeric(inputsPSA[14,6]),as.numeric(inputsPSA[14,7]))
  inputsPSA[15,4] <- runif(1,as.numeric(inputsPSA[15,6]),as.numeric(inputsPSA[15,7]))
  inputsPSA[16,4] <- runif(1,as.numeric(inputsPSA[16,6]),as.numeric(inputsPSA[16,7]))
  inputsPSA[19,4] <- runif(1,as.numeric(inputsPSA[19,6]),as.numeric(inputsPSA[19,7]))
  inputsPSA[20,4] <- runif(1,as.numeric(inputsPSA[20,6]),as.numeric(inputsPSA[20,7]))
  inputsPSA[21,4] <- runif(1,as.numeric(inputsPSA[21,6]),as.numeric(inputsPSA[21,7]))
  inputsPSA[22,4] <- runif(1,as.numeric(inputsPSA[22,6]),as.numeric(inputsPSA[22,7]))
  inputsPSA[23,4] <- runif(1,as.numeric(inputsPSA[23,6]),as.numeric(inputsPSA[23,7]))
  inputsPSA[24,4] <- runif(1,as.numeric(inputsPSA[24,6]),as.numeric(inputsPSA[24,7]))
  inputsPSA[29,4] <- runif(1,as.numeric(inputsPSA[29,6]),as.numeric(inputsPSA[29,7]))
  inputsPSA[31,4] <- runif(1,as.numeric(inputsPSA[31,6]),as.numeric(inputsPSA[31,7]))
  inputsPSA[32,4] <- runif(1,as.numeric(inputsPSA[32,6]),as.numeric(inputsPSA[32,7]))
  inputsPSA[33,4] <- runif(1,as.numeric(inputsPSA[33,6]),as.numeric(inputsPSA[33,7]))
  inputsPSA[34,4] <- runif(1,as.numeric(inputsPSA[34,6]),as.numeric(inputsPSA[34,7]))
  inputsPSA[36,4] <- runif(1,as.numeric(inputsPSA[36,6]),as.numeric(inputsPSA[36,7]))
  inputsPSA[37,4] <- runif(1,as.numeric(inputsPSA[37,6]),as.numeric(inputsPSA[37,7]))
  inputsPSA[38,4] <- runif(1,as.numeric(inputsPSA[38,6]),as.numeric(inputsPSA[38,7]))
  inputsPSA[43,4] <- runif(1,as.numeric(inputsPSA[43,6]),as.numeric(inputsPSA[43,7]))
  inputsPSA[45,4] <- runif(1,as.numeric(inputsPSA[45,6]),as.numeric(inputsPSA[45,7]))
  inputsPSA[46,4] <- runif(1,as.numeric(inputsPSA[46,6]),as.numeric(inputsPSA[46,7]))
  inputsPSA[47,4] <- runif(1,as.numeric(inputsPSA[47,6]),as.numeric(inputsPSA[47,7]))
  inputsPSA[48,4] <- runif(1,as.numeric(inputsPSA[48,6]),as.numeric(inputsPSA[48,7]))
  inputsPSA[50,4] <- runif(1,as.numeric(inputsPSA[50,6]),as.numeric(inputsPSA[50,7]))
  inputsPSA[51,4] <- runif(1,as.numeric(inputsPSA[51,6]),as.numeric(inputsPSA[51,7]))
  inputsPSA[52,4] <- runif(1,as.numeric(inputsPSA[52,6]),as.numeric(inputsPSA[52,7]))
  inputsPSA[53,4] <- runif(1,as.numeric(inputsPSA[53,6]),as.numeric(inputsPSA[53,7]))
  inputsPSA[58,4] <- runif(1,as.numeric(inputsPSA[58,7]),as.numeric(inputsPSA[58,6]))
  inputsPSA[61,4] <- runif(1,as.numeric(inputsPSA[61,7]),as.numeric(inputsPSA[61,6]))
  inputsPSA[62,4] <- runif(1,as.numeric(inputsPSA[62,6]),as.numeric(inputsPSA[62,7]))
  inputsPSA[63,4] <- runif(1,as.numeric(inputsPSA[63,6]),as.numeric(inputsPSA[63,7]))
  inputsPSA[64,4] <- runif(1,as.numeric(inputsPSA[64,6]),as.numeric(inputsPSA[64,7]))
  #inputsPSA[65,4] <- runif(1,as.numeric(inputsPSA[65,6]),as.numeric(inputsPSA[65,7]))
  cost_per_farm_village <- runif(1,as.numeric(inputsPSA[65,6]),as.numeric(inputsPSA[65,7])) ##DTE corrected so intervention cost included
  inputsPSA[66,4] <- runif(1,as.numeric(inputsPSA[66,7]),as.numeric(inputsPSA[66,6]))
  inputsPSA[67,4] <- runif(1,as.numeric(inputsPSA[67,7]),as.numeric(inputsPSA[67,6]))
  inputsPSA[68,4] <- runif(1,as.numeric(inputsPSA[68,6]),as.numeric(inputsPSA[68,7]))
  
  #store NMB in vector
  CEAC_NMB_vector[i] <- as.data.frame(model(inputsPSA))[1,1]
}

#write.xlsx(CEAC_NMB_vector, "C:/Users/tresc/Desktop/AMR-Model/Intervention 1/Montecarlo Results.xlsx")
write.xlsx(CEAC_NMB_vector, "C:/Users/tresc/Desktop/AMR-Model/Intervention 1/Montecarlo Results 2 July 2021.xlsx")

density <- ecdf(CEAC_NMB_vector)
plot(density,
     xlab = "Net Monetary Benefit ($USD)",
     ylab = "Cumulative Density",
     main = "Distribution of NMB Values from Montecarlo Simulation - 76.83% Cost-Effective")
abline(v = 0, col = "blue", lty = 2, lwd = 2) 

MCresults <- CEAC_NMB_vector

MCfail <- MCresults[MCresults < 0] #2317 were below zero, so the intervention was cost-effective 76.83% of the time
max(MCresults)
min(MCresults)
mean(MCresults)

MC <- read_xlsx("C:/Users/tresc/Desktop/AMR-Model/Intervention 1/Montecarlo Results 2 July 2021.xlsx")
MC <- MC[,2]
MC <- as.numeric(as.character(unlist(MC)))

pdensity <- density(MC)

plot(pdensity,
     xlab = "Net Monetary Benefit ($USD)",
     ylab = "Probability Density",
     main = "Distribution of NMB Values from Montecarlo Simulation - 76.83% Cost-Effective")
abline(v = 0, col = "blue", lty = 2, lwd = 2) 

# Tornado Plot ------------------------------------------------------------

##Setting Scenario
inputs <- read.csv("C:/Users/tresc/Desktop/AMR-Model/intervention 1/inputs.csv")
inputs <- as.data.table(inputs)
colnames(inputs) <- c("scenario", "parameter", "description", "value", "distribution", "low", "high", "notes")

scenario <- "HCA"
scenario_transmission <- "med"
scenario_outcomes <- "All"
scenario_intervention_level <- "Village"
intervention_followup_period <- 2
scenario_amr_grow <- "med" ##DTE reset to default AMR growth scenario
cost_per_farm_village <- seminar_cost/farmers_per_seminar + 
  seminar_length*hourly_compensation + transport_cost +
  (visit_cost + ((group_size - 1)*additional_time_per_farm*hourly_compensation))*(visits_per_year/group_size) +
  visit_length_village*hourly_compensation*visits_per_year

#get base case NMB
tornado_base <- as.data.frame(model(inputs))[1,1]

#uptake (pig side)
inputstornado <- inputs
inputstornado[68,4] <- inputs[68,6]
uptake_low <- as.data.frame(model(inputstornado))[1,1]
uptake_low <- uptake_low - tornado_base
inputstornado[68,4] <- inputs[68,7]
uptake_high <- as.data.frame(model(inputstornado))[1,1]
uptake_high <- uptake_high - tornado_base

#effect on pig mortality
inputstornado <- inputs
inputstornado[67,4] <- inputs[67,6]
pigmort_low <- as.data.frame(model(inputstornado))[1,1]
pigmort_low <- pigmort_low - tornado_base
inputstornado[67,4] <- inputs[67,7]
pigmort_high <- as.data.frame(model(inputstornado))[1,1]
pigmort_high <- pigmort_high - tornado_base

#effect on chicken mortality
inputstornado <- inputs
inputstornado[66,4] <- inputs[66,6]
chickmort_low <- as.data.frame(model(inputstornado))[1,1]
chickmort_low <- chickmort_low - tornado_base
inputstornado[66,4] <- inputs[66,7]
chickmort_high <- as.data.frame(model(inputstornado))[1,1]
chickmort_high <- chickmort_high - tornado_base

#farm intervention cost
inputstornado <- inputs
cost_per_farm_village <- as.numeric(inputs[65,7])
farmcost_low <- as.data.frame(model(inputstornado))[1,1]
farmcost_low <- farmcost_low - tornado_base
cost_per_farm_village <- as.numeric(inputs[65,6])
farmcost_high <- as.data.frame(model(inputstornado))[1,1]
farmcost_high <- farmcost_high - tornado_base

cost_per_farm_village <- seminar_cost/farmers_per_seminar + 
  seminar_length*hourly_compensation + transport_cost +
  (visit_cost + ((group_size - 1)*additional_time_per_farm*hourly_compensation))*(visits_per_year/group_size) +
  visit_length_village*hourly_compensation*visits_per_year

#effect on chicken bodyweight
inputstornado <- inputs
inputstornado[64,4] <- inputs[64,6]
chickweight_low <- as.data.frame(model(inputstornado))[1,1]
chickweight_low <- chickweight_low - tornado_base
inputstornado[64,4] <- inputs[64,7]
chickweight_high <- as.data.frame(model(inputstornado))[1,1]
chickweight_high <- chickweight_high - tornado_base

#effect on pig bodyweight
inputstornado <- inputs
inputstornado[62,4] <- inputs[62,6]
pigweight_low <- as.data.frame(model(inputstornado))[1,1]
pigweight_low <- pigweight_low - tornado_base
inputstornado[62,4] <- inputs[62,7]
pigweight_high <- as.data.frame(model(inputstornado))[1,1]
pigweight_high <- pigweight_high - tornado_base

#money saved on pig feed
inputstornado <- inputs
inputstornado[63,4] <- inputs[63,6]
pigfeed_low <- as.data.frame(model(inputstornado))[1,1]
pigfeed_low <- pigfeed_low - tornado_base
inputstornado[63,4] <- inputs[63,7]
pigfeed_high <- as.data.frame(model(inputstornado))[1,1]
pigfeed_high <- pigfeed_high - tornado_base

#fixed administrative cost
inputstornado <- inputs
inputstornado[61,4] <- inputs[61,6]
admin_low <- as.data.frame(model(inputstornado))[1,1]
admin_low <- admin_low - tornado_base
inputstornado[61,4] <- inputs[61,7]
admin_high <- as.data.frame(model(inputstornado))[1,1]
admin_high <- admin_high - tornado_base

#effect on human AMR
inputstornado <- inputs
inputstornado[58,4] <- inputs[58,6]
AMR_low <- as.data.frame(model(inputstornado))[1,1]
AMR_low <- AMR_low - tornado_base
inputstornado[58,4] <- inputs[58,7]
AMR_high <- as.data.frame(model(inputstornado))[1,1]
AMR_high <- AMR_high - tornado_base

tornado <- data.frame(variable = c("Uptake (Pigs)",
                                   "Effect on Pig Mortality",
                                   "Effect on Chicken Mortality",
                                   "Per-Farm Intervention Cost",
                                   "Effect on Chicken Bodyweight",
                                   "Effect on Pig Bodyweight",
                                   "Money Saved on Probiotic Pig Feed",
                                   "Administrative Cost",
                                   "Effect on Human AMR"),
                      min = c(uptake_low, pigmort_low, chickmort_low, farmcost_low, chickweight_low,
                              pigweight_low, pigfeed_low, admin_low, AMR_low),
                      max = c(uptake_high, pigmort_high, chickmort_high, farmcost_high, chickweight_high,
                              pigweight_high, pigfeed_high, admin_high, AMR_high))


ggplot(tornado, aes(variable, ymin = min, ymax = max)) +
  geom_linerange(size = 10) +
  coord_flip() +
  xlab("") +
  ggtitle("Change in Macro-Level Net Monetary Benefit along Range of Each Parameter")+
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw() +
  theme(axis.text = element_text(size = 15))


# Plotting Morbidity and Mortality over Time ------------------------------

##default

mparam_plot <- cbind(m_param[,1:4],m_param2[,2:4])
colnames(mparam_plot) <- c("year", "res_base", "sus_base", "dead_base", "res_int", "sus_int", "dead_int")
mparam_plot <- as.data.frame(mparam_plot)
mparam_plot$year <- c(2021:2067)

ggplot(data = mparam_plot, aes(x = year, y = res_base))+
  geom_point(aes(x = year, y = res_base), colour = 'blue')+
  geom_point(aes(x = year, y = res_int), colour = 'red') +
  xlab("Year") +
  ylab("New Resistant Infections") +
  ggtitle("Resistant Infections in the Baseline (Blue) and Intervention (Red) Scenarios") +
  theme_bw()

ggplot(data = mparam_plot, aes(x = year, y = sus_base))+
  geom_point(aes(x = year, y = sus_base), colour = 'blue')+
  geom_point(aes(x = year, y = sus_int), colour = 'red') +
  xlab("Year") +
  ylab("New Susceptible Infections") +
  ggtitle("Susceptible Infections in the Baseline (Blue) and Intervention (Red) Scenarios")+
  theme_bw()

ggplot(data = mparam_plot, aes(x = year, y = dead_base))+
  geom_point(aes(x = year, y = dead_base), colour = 'blue')+
  geom_point(aes(x = year, y = dead_int), colour = 'red') +
  xlab("Year") +
  ylab("New Deaths") +
  ggtitle("Deaths in the Baseline (Blue) and Intervention (Red) Scenarios") +
  theme_bw()

##different background growth rates

#run with lo
scenario_amr_grow <- "lo"

#run code

mparam_plot_lo <- cbind(m_param[,1:4],m_param2[,2:4])
colnames(mparam_plot_lo) <- c("year", "res_base", "sus_base", "dead_base", "res_int", "sus_int", "dead_int")
mparam_plot_lo <- as.data.frame(mparam_plot_lo)
mparam_plot_lo$year <- c(2021:2067)

plot_lo <- ggplot(data = mparam_plot_lo, aes(x = year, y = res_base))+
  geom_point(aes(x = year, y = res_base), colour = 'blue')+
  geom_point(aes(x = year, y = res_int), colour = 'red') +
  xlab("Year") +
  ylab("New Resistant Infections") +
  ggtitle("1%") +
  theme_bw()

#run with med
scenario_amr_grow <- "med"

#run code

mparam_plot_med <- cbind(m_param[,1:4],m_param2[,2:4])
colnames(mparam_plot_med) <- c("year", "res_base", "sus_base", "dead_base", "res_int", "sus_int", "dead_int")
mparam_plot_med <- as.data.frame(mparam_plot_med)
mparam_plot_med$year <- c(2021:2067)

plot_med <- ggplot(data = mparam_plot_med, aes(x = year, y = res_base))+
  geom_point(aes(x = year, y = res_base), colour = 'blue')+
  geom_point(aes(x = year, y = res_int), colour = 'red') +
  xlab("Year") +
  ylab("New Resistant Infections") +
  ggtitle("2.83%") +
  theme_bw()

#run with hi
scenario_amr_grow <- "hi"

#run code

mparam_plot_hi <- cbind(m_param[,1:4],m_param2[,2:4])
colnames(mparam_plot_hi) <- c("year", "res_base", "sus_base", "dead_base", "res_int", "sus_int", "dead_int")
mparam_plot_hi <- as.data.frame(mparam_plot_hi)
mparam_plot_hi$year <- c(2021:2067)

plot_hi <- ggplot(data = mparam_plot_hi, aes(x = year, y = res_base))+
  geom_point(aes(x = year, y = res_base), colour = 'blue')+
  geom_point(aes(x = year, y = res_int), colour = 'red') +
  xlab("Year") +
  ylab("New Resistant Infections") +
  ggtitle("5%") +
  theme_bw()

#run with max
scenario_amr_grow <- "max"

#run code

mparam_plot_max <- cbind(m_param[,1:4],m_param2[,2:4])
colnames(mparam_plot_max) <- c("year", "res_base", "sus_base", "dead_base", "res_int", "sus_int", "dead_int")
mparam_plot_max <- as.data.frame(mparam_plot_max)
mparam_plot_max$year <- c(2021:2067)

plot_max <- ggplot(data = mparam_plot_max, aes(x = year, y = res_base))+
  geom_point(aes(x = year, y = res_base), colour = 'blue')+
  geom_point(aes(x = year, y = res_int), colour = 'red') +
  xlab("Year") +
  ylab("New Resistant Infections") +
  ggtitle("10%") +
  theme_bw()

grid.arrange(plot_lo, plot_med, plot_hi, plot_max, top="New Annual Resistant Cases at Different Rates of Human AMR Growth")

##difference in deaths (intervention vs. baseline) by year
mort_diff <- c(rep(0,47))

for(i in 1:47){
  mort_diff[i] <- m_param[i,4] - m_param2[i,4]
  rm(i)
}

mort_diff

sum(mort_diff)

mean(mort_diff)

popchange2 <- popchange[1:46]

mean(popchange2)

mean(mort_diff) / mean(popchange2)
