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
library("rsq")
library("forcats")

# scenarios ---------------------------------------------------------------

scenario_income <- "LIC" #must be "LIC", "MIC-S", "MIC-I", or "HIC"
scenario_prod <- "HCA" #must be "HCA" or "FCA"
scenario_transmission <- "med" #must be "low", "med", "hi" or "max"
scenario_farm_effect <- "med" #must be "min", "lo", "med", "hi" or "max"

number_runs <- 1000

# inputs ------------------------------------------------------------------

inputs <- read.csv(here("inputs - general model.csv"))
inputs <- as.data.table(inputs)
colnames(inputs) <- c("parameter", "description", "HIC", "MIC-I", "MIC-S", "LIC", "Value", "Min", "Lo", "Med", "Hi", "Max", 
                      "LIC min", "LIC max", "MIC min", "MIC max", "HIC min", "HIC max")

## 
# Main Model --------------------------------------------------------

Model <- function(inputs, scenario_income, scenario_prod, scenario_transmission,
                  scenario_farm_effect){

  if(scenario_income == "HIC"){
    inputs[,"Value"] <- inputs[,"HIC"]
  } else if(scenario_income == "LIC"){
    inputs[,"Value"] <- inputs[,"LIC"]
  } else if(scenario_income == "MIC-I"){
    inputs[,"Value"] <- inputs[,"MIC-I"]
  } else if(scenario_income == "MIC-S"){
    inputs[,"Value"] <- inputs[,"MIC-S"]
  }
  
  inputs[ , Value := as.numeric(as.character(Value))]
  
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
  
  n.t <- inputs[parameter=="n.t",Value] + 1
  
  # Population growth -------------------------------------------------------
  
  population <- c(rep(0,n.t+1))
  population[1] <- inputs[parameter == "pop", Value]
  for(i in 2:length(population)){
    population[i] <- population[i-1]*(1+inputs[parameter=="pop_growth", Value])
  }
  
  popchange <- c(rep(0,n.t))
  for(i in 1:n.t){
    popchange[i] <- population[i+1]-population[i]
  }
  
  # Human Epi Model ---------------------------------------------------------
  
  #building parameter matrix
  
  state_names <- c("well", "res","sus","dead", "was_well", "seq") ## the compartments
  transition_names  <- c("birth","r","s","mort_r", "mort_s","mort_w", "rec_r","rec_s", "dead_aft", "r_seq", "s_seq")  ## the transition probabilities
  parameter_names <- c(state_names, transition_names)
  
  state_i <- c(inputs[parameter=="pop",Value], rep(0,length=length(state_names)-1))
  #initial state vector
  
  m_param <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names))
  colnames(m_param) <- parameter_names
  rownames(m_param) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  m_param[ , "r_seq"] <- rep(inputs[parameter=="seq_res", Value], n.t) 
  #chance of developing sequelae following resistant infection
  
  m_param[ , "s_seq"] <- rep(inputs[parameter=="seq_sus", Value], n.t) 
  #chance of developing sequelae following susceptible infection
  
  m_param[ , "r"] <- rep(inputs[parameter=="well_sick", Value]*inputs[parameter=="portion_res",Value], n.t)
  #chance of developing a resistant infection in a year
  
  m_param[ , "s"] <- rep(inputs[parameter=="well_sick", Value]*(1-inputs[parameter=="portion_res",Value]), n.t)
  #chance of developing a susceptible infection in a year
  
  m_param[ , "mort_r"] <- rep(inputs[parameter=="mort_res", Value], n.t)
  #fatality from resistant infection
  
  m_param[ , "mort_s"] <- rep(inputs[parameter=="mort_sus", Value], n.t)
  #fatality from susceptible infection
  
  m_param[ , "rec_r"] <- rep(max(0,(1-(m_param[1,"mort_r"]+m_param[1,"r_seq"]))), n.t)
  #chance of recovering from a resistant infection
  
  m_param[ , "rec_s"] <- rep(max(0,(1-(m_param[1,"mort_s"]+m_param[1,"s_seq"]))), n.t)
  #chance of recovering from a susceptible infection
  
  m_param[ , "mort_w"] <- rep(0, n.t) 
  #chance of dying without infection from 'well', set to zero because background mortality is included in net births
  
  m_param[ , "dead_aft"] <- rep(1, n.t) 
  #all those who die go to the afterlife
  
  m_param[ , "birth"] <- popchange[1:n.t]
  #predicted net births in a given year
  
  
  m_param[1, 1:length(state_names)] <- state_i 
  #adding initial cycle 0 values
  
  #have growing AMR prevalence
  #note that we will later ensure that the total disease incidence does not change,
  #only the portion of infections from resistant bacteria
  amr_growth <- inputs[parameter=="amr_grow", Value]
  
  #set the maximum portion of resistant infections
  max_res <- inputs[parameter == "max_r", Value]
  
  for (i in 2:(n.t)){
    m_param[i, "r"] <- m_param[i-1, "r"]*amr_growth
  }
  
  for(i in 1:n.t){
    if(m_param[i, "r"] > max_res *(m_param[1,"r"]+m_param[1,"s"])){ 
      m_param[i, "r"] <- max_res *(m_param[1,"r"]+m_param[1,"s"]) 
    }
    m_param[i, "s"] <- m_param[1,"r"]+m_param[1,"s"] - m_param[i, "r"] 
  }
  #made it so that, while the incidence of resistant infections increases,
  #the total number of infections doesn't increase. However, if we wanted to 
  #have a changing number of infections over time, we could do that, but would
  #need to replace "m_param[1,"r"]+m_param[1,"s"]" with a parameter called 
  #something like "chance_sick" which can change each period, and have that 
  #grow before allowing the AMR incidence to grow
  
  #make sure that the transition probabilities don't exceed 1
  for(i in 1:n.t){
    m_param[i, "mort_r"] <- m_param[i, "mort_r"] / (m_param[i, "mort_r"] + m_param[i, "rec_r"] + m_param[i, "r_seq"])
    m_param[i, "rec_r"] <- m_param[i, "rec_r"] / (m_param[i, "mort_r"] + m_param[i, "rec_r"] + m_param[i, "r_seq"])
    m_param[i, "r_seq"] <- m_param[i, "r_seq"] / (m_param[i, "mort_r"] + m_param[i, "rec_r"] + m_param[i, "r_seq"])
    
    m_param[i, "mort_s"] <- m_param[i, "mort_s"] / (m_param[i, "mort_s"] + m_param[i, "rec_s"] + m_param[i, "s_seq"])
    m_param[i, "rec_s"] <- m_param[i, "rec_s"] / (m_param[i, "mort_s"] + m_param[i, "rec_s"] + m_param[i, "s_seq"])
    m_param[i, "s_seq"] <- m_param[i, "s_seq"] / (m_param[i, "mort_s"] + m_param[i, "rec_s"] + m_param[i, "s_seq"])
  }
  
  ##set the initial state
  
  #born
  m_param[1, "well"] <- m_param[1, "well"] + m_param[1, "birth"]
  
  #transition out of well
  m_param[1, "was_well"] <- m_param[1, "well"]
  m_param[1, "well"] <- m_param[1, "was_well"] * (1 - m_param[1, "r"] - m_param[1, "s"] - m_param[1, "mort_w"])
  
  m_param[1, "res"] <- m_param[1, "was_well"] * m_param[1, "r"]
  m_param[1, "sus"] <- m_param[1, "was_well"] * m_param[1, "s"]
  m_param[1, "dead"] <- m_param[1, "was_well"] * m_param[1, "mort_w"]
  
  #transition out of sick
  m_param[1, "well"] <- m_param[1, "well"] + 
    (m_param[1, "rec_r"] * m_param[1, "res"]) + 
    (m_param[1, "rec_s"] * m_param[1, "sus"])
  
  m_param[1, "dead"] <- m_param[1, "dead"] +
    (m_param[1, "mort_s"] * m_param[1, "sus"]) + 
    (m_param[1, "mort_r"] * m_param[1, "res"])
  
  m_param[1, "seq"] <- m_param[1, "seq"] +
    (m_param[1, "s_seq"] * m_param[1, "sus"]) + 
    (m_param[1, "r_seq"] * m_param[1, "res"])
  
  ##difference equation
  
  f_human_epi <- function(m_param, n.t){
    
    n.t.val <- n.t
    
    for(i in 2:n.t.val){
      
      #carry over from last period and be born
      m_param[i, "well"] <- m_param[i-1, "well"] + m_param[i, "birth"]
      
      m_param[i, "was_well"] <- m_param[i, "well"]
      
      #transition out of well
      m_param[i, "well"] <- m_param[i, "was_well"] * (1 - m_param[i, "r"] - m_param[i, "s"] - m_param[i, "mort_w"])
      
      m_param[i, "res"] <- m_param[i, "was_well"] * m_param[i, "r"]
      m_param[i, "sus"] <- m_param[i, "was_well"] * m_param[i, "s"]
      m_param[i, "dead"] <- m_param[i, "was_well"] * m_param[i, "mort_w"]
      
      #transition out of sick
      m_param[i, "well"] <- m_param[i, "well"] + 
        (m_param[i, "rec_r"] * m_param[i, "res"]) + 
        (m_param[i, "rec_s"] * m_param[i, "sus"])
      
      m_param[i, "dead"] <- m_param[i, "dead"] +
        (m_param[i, "mort_s"] * m_param[i, "sus"]) + 
        (m_param[i, "mort_r"] * m_param[i, "res"])
      
      m_param[i, "seq"] <- m_param[i, "seq"] +
        (m_param[i, "s_seq"] * m_param[i, "sus"]) + 
        (m_param[i, "r_seq"] * m_param[i, "res"])
      
    }
    
    return(m_param)
  }
  
  m_param <- f_human_epi(m_param,n.t) 
  
  # Healthcare Costs --------------------------------------------------------
  
  dr <- inputs[parameter == "dr", Value]
  
  m_cost <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names))
  colnames(m_cost) <- parameter_names
  rownames(m_cost) <- paste("cycle", 0:(n.t-1), sep  =  "")  
  
  #cost of hospital stay for res and sus infection
  
  c_r <- inputs[parameter == "los_sus", Value] * inputs[parameter == "bed_day_cost", Value] * 365.25
  c_s <- inputs[parameter == "los_res", Value] * inputs[parameter == "bed_day_cost", Value] * 365.25
  
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
  
  pv_fut_life <- c(rep(0,n.t-1))
  
  for (i in 1:n.t-1){
    pv_fut_life[i] <- inputs[parameter=="background_qol", Value] * (1-dr)^(i-1)
  }
  pv_life <- sum(pv_fut_life)
  
  pv_fut_life_seq <- c(rep(0,n.t-1))
  for (i in 1:n.t-1){
    pv_fut_life_seq[i] <- inputs[parameter=="qol_seq", Value] * (1-dr)^(i-1)
  }
  pv_life_seq <- sum(pv_fut_life_seq)
  
  #the 'reward' for death is not sweet release, but in fact the negative of the present value of remaining life 
  #years, and the 'reward' for being infected is the loss of welfare while hospitalised
  #therefore in a scenario with more deaths, infections and sequelae will have a negative
  #'reward' of larger absolute value
  
  r_s <- inputs[parameter == "los_sus", Value] *
    (inputs[parameter == "qol_sick", Value] - inputs[parameter == "background_qol", Value])
  
  r_r <- inputs[parameter == "los_res", Value] * 
    (inputs[parameter == "qol_sick", Value] - inputs[parameter == "background_qol", Value])
  
  r_d <- -1 * pv_life #discounted QoL loss from death
  r_seq <- pv_life_seq - pv_life # fixed this because we were previously assigning a benefit to sequelae (the subtraction was the wrong way around lol)
  
  rwd_i <- c(0,r_r,r_s,r_d,0,r_seq) 
  
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_rwd[2, 1:length(state_names)] <- rwd_i
  
  ### accounting for discounting
  for (j in 1:length(state_names)) {
    for (i in 3:(n.t)){
      m_rwd[i,j] <- f_di(m_rwd[i-1,j],dr)
    }  
  }
  
  
  # Productivity Costs -------------------------------------------------------
  
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
  
  cost_i_prod <- rep(0,(length(state_names)))
  
  ## start at cycle 1 so you do not multiply initial state vector
  m_cost_prod[2,1:length(state_names)] <- cost_i_prod
  
  for (j in 1:length(state_names)) {
    for (i in 3:(n.t)){
      m_cost_prod[i,j] <- f_di(m_cost_prod[i-1,j],dr)
    }
  }
  
  
  # Productivity Rewards ----------------------------------------------------
  
  #the 'reward' for being infected is equal to the negative forgone productivity
  #while hospitalised
  
  m_rwd_prod <- matrix(rep(0), nrow = n.t, ncol = length(parameter_names))
  colnames(m_rwd_prod) <- parameter_names
  rownames(m_rwd_prod) <- paste("cycle", 0:(n.t-1), sep = "")
  
  r_r_prod <- -1 * inputs[parameter == "los_res", Value] * 
    ((inputs[parameter == "prod_pc", Value]*inputs[parameter == "lfpr", Value])* 
       inputs[parameter == "unpaid_prod_pc", Value])
  
  r_s_prod <- -1 * inputs[parameter == "los_sus", Value] *
    ((inputs[parameter == "prod_pc", Value]*inputs[parameter == "lfpr", Value])*
       inputs[parameter == "unpaid_prod_pc", Value])
  
  r_w_prod <- 0
  
  r_aft_prod <- 0
  
  r_seq_prod <- 0 #importantly assumes that people with sequelae are equally productive
  
  #the reward for dead is the present discounted value of future work (either for the
  #remainder of economically active life, or for the 6 months needed to find a replacement)
  
  remaining_work_years <- inputs[parameter == "remaining_work_years", Value]
  
  yearly_prod <- inputs[parameter == "prod_pc", Value]*inputs[parameter == "lfpr", Value]*
    inputs[parameter == "unpaid_prod_pc", Value]
  
  pv_fut_prod <- c(rep(0,remaining_work_years))
  
  for (i in 1:remaining_work_years){ 
    pv_fut_prod[i] <- yearly_prod * (1-dr)^(i-1)
  }
  
  pv_life_prod <- sum(pv_fut_prod)
  
  if(scenario_prod == "HCA"){
    r_d_prod <- -1 * pv_life_prod
  } else if(scenario_prod == "FCA"){
    r_d_prod <- -0.5 * yearly_prod
  } else{
    paste("ERROR: PLEASE CHOOSE AN APPROACH TO ESTIMATING PRODUCTIVITY OUTCOMES")
  }
  
  rwd_i_prod <- c(r_w_prod, r_r_prod, r_s_prod, r_d_prod, r_aft_prod, r_seq_prod)
  
  ## start at cycle 1 so you do not multiply initial state vector
  m_rwd_prod[2,1:length(state_names)] <- rwd_i_prod 
  
  ### discount, but also account for labour productivity growth
  
  dr_pgrowth <- dr - inputs[parameter=="prod_growth", Value] 
  ##discount rate net of productivity growth (in theory it can be negative)
  
  for (j in 1:length(state_names)) {
    for (i in 3:(n.t)){
      m_rwd_prod[i,j] <- f_di(m_rwd_prod[i-1,j],dr_pgrowth)
    }
  }
  
  # Animal Epi Model --------------------------------------------------------
  
  state_names_a <- c("well", "res","sus","fallen","sold") ## the compartments
  transition_names_a  <- c("birth","r","s","mort_r", "mort_s","mort_w", "rec_r","rec_s","w_sold")  ## the rates
  parameter_names_a <- c(state_names_a, transition_names_a)
  
  f_animal_epi <- function(m_param_a_base, n.t, scenario_animal){
    
    n.t.val <- n.t
    
    if(scenario_animal == "chicken_small"){
      n_animals_farm <- inputs[parameter == "n_chickens_farm_small", Value]
      annual_cycles <- inputs[parameter == "pcycles_chicken_small", Value]
    } else if (scenario_animal == "chicken_ind"){
      n_animals_farm <- inputs[parameter == "n_chickens_farm_ind", Value]
      annual_cycles <- inputs[parameter == "pcycles_chicken_ind", Value]
    } else if (scenario_animal == "pig_small"){
      n_animals_farm <- inputs[parameter == "n_pigs_farm_small", Value]
      annual_cycles <- inputs[parameter == "pcycles_pig_small", Value]
    } else if (scenario_animal == "pig_ind"){
      n_animals_farm <- inputs[parameter == "n_pigs_farm_ind", Value]
      annual_cycles <- inputs[parameter == "pcycles_pig_ind", Value]  
    }    
    
    m_param_a_temp <- m_param_a_base[1:4,]
    rownames(m_param_a_temp) <- NULL ##removing rownames
    
    i <- 2 ## getting sick
    m_param_a_temp[i,"well"] <- m_param_a_temp[i-1,"well"] -(m_param_a_temp[i-1,"r"]*m_param_a_temp[i-1,"well"]) -
      (m_param_a_temp[i-1,"s"]*m_param_a_temp[i-1,"well"])
    m_param_a_temp[i,"res"] <- m_param_a_temp[i-1,"res"] + (m_param_a_temp[i-1,"r"]*m_param_a_temp[i-1,"well"]) 
    m_param_a_temp[i,"sus"] <- m_param_a_temp[i-1,"sus"] + (m_param_a_temp[i-1,"s"]*m_param_a_temp[i-1,"well"])
    
    i <- 3 ## dying and recovering
    m_param_a_temp[i,"fallen"] <- (m_param_a_temp[i-1,"mort_w"]*m_param_a_temp[i-1,"well"]) +
      (m_param_a_temp[i-1,"mort_r"]*m_param_a_temp[i-1,"res"]) + 
      (m_param_a_temp[i-1,"mort_s"]*m_param_a_temp[i-1,"sus"])
    m_param_a_temp[i, "well"] <- m_param_a_temp[i-1,"well"] - (m_param_a_temp[i-1,"well"]*m_param_a_temp[i-1,"mort_w"]) +
      (m_param_a_temp[i-1,"rec_r"]*m_param_a_temp[i-1,"res"])+ 
      (m_param_a_temp[i-1,"rec_s"]*m_param_a_temp[i-1,"sus"])
    
    i <- 4 ##sold
    m_param_a_temp[i,"sold"] <- (m_param_a_temp[i-1,"w_sold"]*m_param_a_temp[i-1,"well"])
    
    ## final states
    m_a_sum <- m_param_a_temp[4,]
    m_a_sum["res"] <- m_param_a_temp[2,"res"]
    m_a_sum["sus"] <- m_param_a_temp[2,"sus"]
    m_a_sum["well"] <- n_animals_farm - m_a_sum["res"] - m_a_sum["sus"] ## reset the number in 'well'
    
    m_a_sum[1:5] <- annual_cycles * m_a_sum[1:5] #multiply by the number of annual cycles
    
    m_param_a <- matrix(rep(m_a_sum), nrow=n.t.val, ncol =length(parameter_names_a))
    m_param_a <- t(replicate(n.t.val,m_a_sum))
    colnames(m_param_a) <- parameter_names_a
    rownames(m_param_a) <- paste("cycle", 0:(n.t-1), sep  =  "")
    
    m_param_a
    
    return(m_param_a)
    
  }
  
  # Chicken Epi Module - Smallholder -------------------------------------------------------
  
  scenario_animal <- "chicken_small"
  
  state_i_c_s <- c(inputs[parameter == "n_chickens_farm_small", Value], rep(0,length=length(state_names_a)-1))
  
  m_param_c_s_base <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_param_c_s_base) <- parameter_names_a
  rownames(m_param_c_s_base) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  #here, we have a set mortality rate for all states - 
  #it only matters if animals are in 'res' or 'sus' for the purpose of calculating 
  #the cost of therapeutic treatment
  
  m_param_c_s_base[ , "r"] <- rep(inputs[parameter=="c_res_small", Value], n.t)
  m_param_c_s_base[ , "s"] <- rep(inputs[parameter=="c_sus_small", Value], n.t)
  m_param_c_s_base[ , "mort_s"] <- rep(inputs[parameter=="c_mort_small", Value], n.t) 
  m_param_c_s_base[ , "mort_r"] <- rep(inputs[parameter=="c_mort_small", Value], n.t)
  m_param_c_s_base[ , "rec_r"] <- rep(1-(m_param_c_s_base[1,"mort_r"]), n.t)
  m_param_c_s_base[ , "rec_s"] <- rep(1-(m_param_c_s_base[1,"mort_s"]), n.t)
  m_param_c_s_base[ , "birth"] <- rep(1, n.t)
  m_param_c_s_base[ , "mort_w"] <- rep(inputs[parameter=="c_mort_small", Value], n.t)
  m_param_c_s_base[ , "w_sold"] <- rep(1, n.t) 
  
  #make it so that the total incidence of infections stays the same, and only the
  #portion of them that are resistant changes
  for(i in 1:n.t){
    if(m_param_c_s_base[i, "r"] > max_res * (m_param_c_s_base[1, "r"] + m_param_c_s_base[1, "s"])){
      m_param_c_s_base[i, "r"] <- max_res * (m_param_c_s_base[1, "r"] + m_param_c_s_base[1, "s"])
    }
    m_param_c_s_base[i, "s"] <- m_param_c_s_base[1, "r"] + m_param_c_s_base[1, "s"] - m_param_c_s_base[i, "r"]
  }
  
  m_param_c_s_base[1, 1:length(state_names_a)] <- state_i_c_s
  
  m_param_c_s <- f_animal_epi(m_param_c_s_base,n.t, scenario_animal)
  ### ignore totals of transition probs etc. as they are over counted etc.
  ## just want to focus on health state totals
  
  
  # Chicken Epi Module - Industrial -----------------------------------------
  
  scenario_animal <- "chicken_ind"
  
  state_i_c_i <- c(inputs[parameter == "n_chickens_farm_ind", Value], rep(0,length=length(state_names_a)-1))
  
  m_param_c_i_base <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_param_c_i_base) <- parameter_names_a
  rownames(m_param_c_i_base) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  #here, we have a set mortality rate for all states - 
  #it only matters if animals are in 'res' or 'sus' for the purpose of calculating 
  #the cost of therapeutic treatment
  
  m_param_c_i_base[ , "r"] <- rep(inputs[parameter=="c_res_ind", Value], n.t)
  m_param_c_i_base[ , "s"] <- rep(inputs[parameter=="c_sus_ind", Value], n.t)
  m_param_c_i_base[ , "mort_s"] <- rep(inputs[parameter=="c_mort_ind", Value], n.t) 
  m_param_c_i_base[ , "mort_r"] <- rep(inputs[parameter=="c_mort_ind", Value], n.t)
  m_param_c_i_base[ , "rec_r"] <- rep(1-(m_param_c_i_base[1,"mort_r"]), n.t)
  m_param_c_i_base[ , "rec_s"] <- rep(1-(m_param_c_i_base[1,"mort_s"]), n.t)
  m_param_c_i_base[ , "birth"] <- rep(1, n.t)
  m_param_c_i_base[ , "mort_w"] <- rep(inputs[parameter=="c_mort_ind", Value], n.t)
  m_param_c_i_base[ , "w_sold"] <- rep(1, n.t) 
  
  #make it so that the total incidence of infections stays the same, and only the
  #portion of them that are resistant changes
  for(i in 1:n.t){
    if(m_param_c_i_base[i, "r"] > max_res * (m_param_c_i_base[1, "r"] + m_param_c_i_base[1, "s"])){
      m_param_c_i_base[i, "r"] <- max_res * (m_param_c_i_base[1, "r"] + m_param_c_i_base[1, "s"])
    }
    m_param_c_i_base[i, "s"] <- m_param_c_i_base[1, "r"] + m_param_c_i_base[1, "s"] - m_param_c_i_base[i, "r"]
  }
  
  m_param_c_i_base[1, 1:length(state_names_a)] <- state_i_c_i
  
  m_param_c_i <- f_animal_epi(m_param_c_i_base,n.t, scenario_animal)
  ### ignore totals of transition probs etc. as they are over counted etc.
  ## just want to focus on health state totals
  
  
  # Pig Epi Module - Smallholder --------------------------------------------
  
  scenario_animal <- "pig_small"
  
  state_i_p_s <- c(inputs[parameter == "n_pigs_farm_small", Value], rep(0,length=length(state_names_a)-1))
  
  m_param_p_s_base <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_param_p_s_base) <- parameter_names_a
  rownames(m_param_p_s_base) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  #here, we have a set mortality rate for all states - 
  #it only matters if animals are in 'res' or 'sus' for the purpose of calculating 
  #the cost of therapeutic treatment
  
  m_param_p_s_base[ , "r"] <- rep(inputs[parameter=="p_res_small", Value], n.t)
  m_param_p_s_base[ , "s"] <- rep(inputs[parameter=="p_sus_small", Value], n.t)
  m_param_p_s_base[ , "mort_s"] <- rep(inputs[parameter=="p_mort_small", Value], n.t) 
  m_param_p_s_base[ , "mort_r"] <- rep(inputs[parameter=="p_mort_small", Value], n.t)
  m_param_p_s_base[ , "rec_r"] <- rep(1-(m_param_p_s_base[1,"mort_r"]), n.t)
  m_param_p_s_base[ , "rec_s"] <- rep(1-(m_param_p_s_base[1,"mort_s"]), n.t)
  m_param_p_s_base[ , "birth"] <- rep(1, n.t)
  m_param_p_s_base[ , "mort_w"] <- rep(inputs[parameter=="p_mort_small", Value], n.t)
  m_param_p_s_base[ , "w_sold"] <- rep(1, n.t) 
  
  #make it so that the total incidence of infections stays the same, and only the
  #portion of them that are resistant changes
  for(i in 1:n.t){
    if(m_param_p_s_base[i, "r"] > max_res * (m_param_p_s_base[1, "r"] + m_param_p_s_base[1, "s"])){
      m_param_p_s_base[i, "r"] <- max_res * (m_param_p_s_base[1, "r"] + m_param_p_s_base[1, "s"])
    }
    m_param_p_s_base[i, "s"] <- m_param_p_s_base[1, "r"] + m_param_p_s_base[1, "s"] - m_param_p_s_base[i, "r"]
  }
  
  m_param_p_s_base[1, 1:length(state_names_a)] <- state_i_p_s
  
  m_param_p_s <- f_animal_epi(m_param_p_s_base,n.t, scenario_animal)
  ### ignore totals of transition probs etc. as they are over counted etc.
  ## just want to focus on health state totals
  
  
  # Pig Epi Module - Industrial ---------------------------------------------
  
  scenario_animal <- "pig_ind"
  
  state_i_p_i <- c(inputs[parameter == "n_pigs_farm_ind", Value], rep(0,length=length(state_names_a)-1))
  
  m_param_p_i_base <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_param_p_i_base) <- parameter_names_a
  rownames(m_param_p_i_base) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  #here, we have a set mortality rate for all states - 
  #it only matters if animals are in 'res' or 'sus' for the purpose of calculating 
  #the cost of therapeutic treatment
  
  m_param_p_i_base[ , "r"] <- rep(inputs[parameter=="p_res_ind", Value], n.t)
  m_param_p_i_base[ , "s"] <- rep(inputs[parameter=="p_sus_ind", Value], n.t)
  m_param_p_i_base[ , "mort_s"] <- rep(inputs[parameter=="p_mort_ind", Value], n.t) 
  m_param_p_i_base[ , "mort_r"] <- rep(inputs[parameter=="p_mort_ind", Value], n.t)
  m_param_p_i_base[ , "rec_r"] <- rep(1-(m_param_p_i_base[1,"mort_r"]), n.t)
  m_param_p_i_base[ , "rec_s"] <- rep(1-(m_param_p_i_base[1,"mort_s"]), n.t)
  m_param_p_i_base[ , "birth"] <- rep(1, n.t)
  m_param_p_i_base[ , "mort_w"] <- rep(inputs[parameter=="p_mort_ind", Value], n.t)
  m_param_p_i_base[ , "w_sold"] <- rep(1, n.t) 
  
  #make it so that the total incidence of infections stays the same, and only the
  #portion of them that are resistant changes
  for(i in 1:n.t){
    if(m_param_p_i_base[i, "r"] > max_res * (m_param_p_i_base[1, "r"] + m_param_p_i_base[1, "s"])){
      m_param_p_i_base[i, "r"] <- max_res * (m_param_p_i_base[1, "r"] + m_param_p_i_base[1, "s"])
    }
    m_param_p_i_base[i, "s"] <- m_param_p_i_base[1, "r"] + m_param_p_i_base[1, "s"] - m_param_p_i_base[i, "r"]
  }
  
  m_param_p_i_base[1, 1:length(state_names_a)] <- state_i_p_i
  
  m_param_p_i <- f_animal_epi(m_param_p_i_base,n.t, scenario_animal)
  ### ignore totals of transition probs etc. as they are over counted etc.
  ## just want to focus on health state totals  
  
  
  #'All of the farm costs are zero here, as any changes to profits per animal
  #'from the intervention will be reflected in changes to the 'farm rewards' 
  
  # Chicken Farm Costs - Smallholder ----------------------------------------
  
  m_cost_c_s <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_cost_c_s) <- parameter_names_a
  rownames(m_cost_c_s) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  # Chicken Farm Costs - Industrial -----------------------------------------
  
  m_cost_c_i <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_cost_c_i) <- parameter_names_a
  rownames(m_cost_c_i) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  # Pig Farm Costs - Smallholder --------------------------------------------
  
  m_cost_p_s <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_cost_p_s) <- parameter_names_a
  rownames(m_cost_p_s) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  # Pig Farm Costs - Industrial ---------------------------------------------
  
  m_cost_p_i <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_cost_p_i) <- parameter_names_a
  rownames(m_cost_p_i) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  
  # Chicken Farm Rewards - Smallholder --------------------------------------
  
  m_rwd_c_s <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_rwd_c_s) <- parameter_names_a
  rownames(m_rwd_c_s) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  #only get a reward for selling animals
  r_sold_c_s <- inputs[parameter == "chicken_weight", Value] * inputs[parameter == "chicken_price", Value]
  rwd_i_c_s <- c(0,0,0,0,r_sold_c_s)
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_rwd_c_s[2, 1:length(state_names_a)] <- rwd_i_c_s
  
  #discount
  for (j in 1:length(state_names_a)) {
    for (i in 3:(n.t)){
      m_rwd_c_s[i,j] <- f_di(m_rwd_c_s[i-1,j],dr)
    }  
  } 
  
  # Chicken Farm Rewards - Industrial ---------------------------------------
  
  m_rwd_c_i <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_rwd_c_i) <- parameter_names_a
  rownames(m_rwd_c_i) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  #only get a reward for selling animals
  r_sold_c_i <- inputs[parameter == "chicken_weight", Value] * inputs[parameter == "chicken_price", Value]
  rwd_i_c_i <- c(0,0,0,0,r_sold_c_i)
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_rwd_c_i[2, 1:length(state_names_a)] <- rwd_i_c_i
  
  #discount
  for (j in 1:length(state_names_a)) {
    for (i in 3:(n.t)){
      m_rwd_c_i[i,j] <- f_di(m_rwd_c_i[i-1,j],dr)
    }  
  } 
  
  # Pig Farm Rewards - Smallholder --------------------------------------
  
  m_rwd_p_s <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_rwd_p_s) <- parameter_names_a
  rownames(m_rwd_p_s) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  #only get a reward for selling animals
  r_sold_p_s <- inputs[parameter == "pig_weight", Value] * inputs[parameter == "pig_price", Value]
  rwd_i_p_s <- c(0,0,0,0,r_sold_p_s)
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_rwd_p_s[2, 1:length(state_names_a)] <- rwd_i_p_s
  
  #discount
  for (j in 1:length(state_names_a)) {
    for (i in 3:(n.t)){
      m_rwd_p_s[i,j] <- f_di(m_rwd_p_s[i-1,j],dr)
    }  
  } 
  
  # Pig Farm Rewards - Industrial ---------------------------------------
  
  m_rwd_p_i <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_rwd_p_i) <- parameter_names_a
  rownames(m_rwd_p_i) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  #only get a reward for selling animals
  r_sold_p_i <- inputs[parameter == "pig_weight", Value] * inputs[parameter == "pig_price", Value]
  rwd_i_p_i <- c(0,0,0,0,r_sold_p_i)
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_rwd_p_i[2, 1:length(state_names_a)] <- rwd_i_p_i
  
  #discount
  for (j in 1:length(state_names_a)) {
    for (i in 3:(n.t)){
      m_rwd_p_i[i,j] <- f_di(m_rwd_p_i[i-1,j],dr)
    }  
  }  
  
  
  # The Intervention --------------------------------------------------------
  
  pig_income_effect     <- as.numeric()
  chicken_income_effect <- as.numeric()
  
  if(scenario_farm_effect == "min"){
    pig_income_effect     <- inputs[parameter == "pig_income_effect", Min]
    chicken_income_effect <- inputs[parameter == "chicken_income_effect", Min]
  } else if (scenario_farm_effect == "lo"){
    pig_income_effect     <- inputs[parameter == "pig_income_effect", Lo]
    chicken_income_effect <- inputs[parameter == "chicken_income_effect", Lo]
  } else if (scenario_farm_effect == "med"){
    pig_income_effect     <- inputs[parameter == "pig_income_effect", Med]
    chicken_income_effect <- inputs[parameter == "chicken_income_effect", Med]
  } else if (scenario_farm_effect == "hi"){
    pig_income_effect     <- inputs[parameter == "pig_income_effect", Hi]
    chicken_income_effect <- inputs[parameter == "chicken_income_effect", Hi]
  } else if (scenario_farm_effect == "max"){
    pig_income_effect     <- inputs[parameter == "pig_income_effect", Max]
    chicken_income_effect <- inputs[parameter == "chicken_income_effect", Max]
  }  
  
  ##Intervention - Humans
  
  #reduction in incidence of drug resistant infections in humans
  m_param2 <- m_param ## human parameter matrix for the intervention scenario
  
  #reduce the chance of getting a resistant infection in humans, depending on the link parameter used
  
  for(i in 1:n.t){ ## fixed because previously the intervention was erasing the background growth in human AMR 
    if(scenario_transmission == "low"){
      m_param2[i , "r"] <- m_param2[i , "r"]*(1+inputs[parameter=="res_change", Lo])
    } else if (scenario_transmission == "med"){
      m_param2[i , "r"] <- m_param2[i , "r"]*(1+inputs[parameter=="res_change", Med])
    } else if (scenario_transmission == "hi"){
      m_param2[i , "r"] <- m_param2[i , "r"]*(1+inputs[parameter=="res_change", Hi])
    } else if (scenario_transmission == "max"){
      m_param2[i , "r"] <- m_param2[i , "r"]*(1+inputs[parameter=="res_change", Max])
    } 
  }
  
  #make sure that the total number of infections remains constant
  for(i in 1:n.t){
    if(m_param2[i, "r"] > max_res * (m_param2[1,"r"]+m_param2[1,"s"])){ 
      m_param2[i, "r"] <- max_res * (m_param2[1,"r"]+m_param2[1,"s"]) 
    }
    m_param2[i, "s"] <- m_param2[1,"r"]+m_param2[1,"s"] - m_param2[i, "r"]
  }
  
  ## clear state values
  m_param2[ , 1:length(state_names)] <- 0
  m_param2[1, 1:length(state_names)] <- state_i
  
  #apply the human epi function to the intervention case
  m_param2 <- f_human_epi(m_param2, n.t) 
  
  ##Intervention - Chickens - Industrial
  
  #create a parameter spreadsheet for the intervention case
  m_param_c_i2 <- m_param_c_i 
  
  #change in mortality
  
  m_param_c_i2[ , "mort_w"] <- m_param_c_i2[ , "mort_w"] * (1+inputs[parameter == "chicken_mort_effect", Value])
  
  m_param_c_i2[ , "mort_s"] <- m_param_c_i2[ , "mort_s"] * (1+inputs[parameter == "chicken_mort_effect", Value])
  
  m_param_c_i2[ , "mort_r"] <- m_param_c_i2[ , "mort_r"] * (1+inputs[parameter == "chicken_mort_effect", Value])
  
  #make sure the total number of infections stays constant
  for(i in 1:n.t){
    m_param_c_i2[i, "s"] <- m_param_c_i2[1, "r"] + m_param_c_i2[1, "s"] - m_param_c_i2[i, "r"]
  }
  
  m_param_c_i2[ , 1:length(state_names_a)] <- 0
  m_param_c_i2[1, 1:length(state_names_a)] <- state_i_c_i
  
  #apply the animal epi function
  scenario_animal <- "chicken_ind"
  m_param_c_i2 <- f_animal_epi(m_param_c_i2, n.t, scenario_animal)
  
  #rewards
  m_rwd_c_i2 <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_rwd_c_i2) <- parameter_names_a
  rownames(m_rwd_c_i2) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  r_sold_c_i2 <- r_sold_c_i * (1+chicken_income_effect)
  rwd_i_c_i2 <- c(0,0,0,0,r_sold_c_i2)
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_rwd_c_i2[2, 1:length(state_names_a)] <- rwd_i_c_i2
  
  #discount
  for (j in 1:length(state_names_a)) {
    for (i in 3:(n.t)){
      m_rwd_c_i2[i,j] <- f_di(m_rwd_c_i2[i-1,j],dr)
    }  
  }
  
  #costs
  m_cost_c_i2 <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_cost_c_i2) <- parameter_names_a
  rownames(m_cost_c_i2) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  ##Intervention - Chickens - Smallholder
  
  #create a parameter spreadsheet for the intervention case
  m_param_c_s2 <- m_param_c_s 
  
  #change in mortality
  
  m_param_c_s2[ , "mort_w"] <- m_param_c_s2[ , "mort_w"] * (1+inputs[parameter == "chicken_mort_effect", Value])
  
  m_param_c_s2[ , "mort_s"] <- m_param_c_s2[ , "mort_s"] * (1+inputs[parameter == "chicken_mort_effect", Value])
  
  m_param_c_s2[ , "mort_r"] <- m_param_c_s2[ , "mort_r"] * (1+inputs[parameter == "chicken_mort_effect", Value])
  
  #make sure the total number of infections stays constant
  for(i in 1:n.t){
    m_param_c_s2[i, "s"] <- m_param_c_s2[1, "r"] + m_param_c_s2[1, "s"] - m_param_c_s2[i, "r"]
  }
  
  m_param_c_s2[ , 1:length(state_names_a)] <- 0
  m_param_c_s2[1, 1:length(state_names_a)] <- state_i_c_s
  
  #apply the animal epi function
  scenario_animal <- "chicken_small"
  m_param_c_s2 <- f_animal_epi(m_param_c_s2, n.t, scenario_animal)
  
  #rewards
  m_rwd_c_s2 <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_rwd_c_s2) <- parameter_names_a
  rownames(m_rwd_c_s2) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  r_sold_c_s2 <- r_sold_c_s * (1+chicken_income_effect)
  rwd_i_c_s2 <- c(0,0,0,0,r_sold_c_s2)
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_rwd_c_s2[2, 1:length(state_names_a)] <- rwd_i_c_s2
  
  #discount
  for (j in 1:length(state_names_a)) {
    for (i in 3:(n.t)){
      m_rwd_c_s2[i,j] <- f_di(m_rwd_c_s2[i-1,j],dr)
    }  
  }
  
  #costs
  m_cost_c_s2 <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_cost_c_s2) <- parameter_names_a
  rownames(m_cost_c_s2) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  ##Intervention - Pigs - Industrial
  
  #create a parameter spreadsheet for the intervention case
  m_param_p_i2 <- m_param_p_i 
  
  #change in mortality
  
  m_param_p_i2[ , "mort_w"] <- m_param_p_i2[ , "mort_w"] * (1+inputs[parameter == "pig_mort_effect", Value])
  
  m_param_p_i2[ , "mort_s"] <- m_param_p_i2[ , "mort_s"] * (1+inputs[parameter == "pig_mort_effect", Value])
  
  m_param_p_i2[ , "mort_r"] <- m_param_p_i2[ , "mort_r"] * (1+inputs[parameter == "pig_mort_effect", Value])
  
  #make sure the total number of infections stays constant
  for(i in 1:n.t){
    m_param_p_i2[i, "s"] <- m_param_p_i2[1, "r"] + m_param_p_i2[1, "s"] - m_param_p_i2[i, "r"]
  }
  
  m_param_p_i2[ , 1:length(state_names_a)] <- 0
  m_param_p_i2[1, 1:length(state_names_a)] <- state_i_p_i
  
  #apply the animal epi function
  scenario_animal <- "pig_ind"
  m_param_p_i2 <- f_animal_epi(m_param_p_i2, n.t, scenario_animal)  
  
  #rewards
  m_rwd_p_i2 <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_rwd_p_i2) <- parameter_names_a
  rownames(m_rwd_p_i2) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  r_sold_p_i2 <- r_sold_p_i * (1+pig_income_effect)
  rwd_i_p_i2 <- c(0,0,0,0,r_sold_p_i2)
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_rwd_p_i2[2, 1:length(state_names_a)] <- rwd_i_p_i2
  
  #discount
  for (j in 1:length(state_names_a)) {
    for (i in 3:(n.t)){
      m_rwd_p_i2[i,j] <- f_di(m_rwd_p_i2[i-1,j],dr)
    }  
  }
  
  #costs
  m_cost_p_i2 <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_cost_p_i2) <- parameter_names_a
  rownames(m_cost_p_i2) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  ##Intervention - Pigs - Smallholder
  
  #create a parameter spreadsheet for the intervention case
  m_param_p_s2 <- m_param_p_s 
  
  #change in mortality
  
  m_param_p_s2[ , "mort_w"] <- m_param_p_s2[ , "mort_w"] * (1+inputs[parameter == "pig_mort_effect", Value])
  
  m_param_p_s2[ , "mort_s"] <- m_param_p_s2[ , "mort_s"] * (1+inputs[parameter == "pig_mort_effect", Value])
  
  m_param_p_s2[ , "mort_r"] <- m_param_p_s2[ , "mort_r"] * (1+inputs[parameter == "pig_mort_effect", Value])
  
  #make sure the total number of infections stays constant
  for(i in 1:n.t){
    m_param_p_s2[i, "s"] <- m_param_p_s2[1, "r"] + m_param_p_s2[1, "s"] - m_param_p_s2[i, "r"]
  }
  
  m_param_p_s2[ , 1:length(state_names_a)] <- 0
  m_param_p_s2[1, 1:length(state_names_a)] <- state_i_p_s
  
  #apply the animal epi function
  scenario_animal <- "pig_small"
  m_param_p_s2 <- f_animal_epi(m_param_p_s2, n.t, scenario_animal)
  
  #rewards
  m_rwd_p_s2 <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_rwd_p_s2) <- parameter_names_a
  rownames(m_rwd_p_s2) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  r_sold_p_s2 <- r_sold_p_s * (1+pig_income_effect)
  rwd_i_p_s2 <- c(0,0,0,0,r_sold_p_s2)
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_rwd_p_s2[2, 1:length(state_names_a)] <- rwd_i_p_s2
  
  #discount
  for (j in 1:length(state_names_a)) {
    for (i in 3:(n.t)){
      m_rwd_p_s2[i,j] <- f_di(m_rwd_p_s2[i-1,j],dr)
    }  
  }
  
  #costs
  m_cost_p_s2 <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_cost_p_s2) <- parameter_names_a
  rownames(m_cost_p_s2) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  
  # Results and Outputs -----------------------------------------------------
  
  #Get number of each type of farm nationally
  n_farms_chicken_small <- inputs[parameter == "n_chickens", Value] * 
    (1 - inputs[parameter == "portion_animals_ind", Value]) / 
    inputs[parameter == "n_chickens_farm_small", Value]
  
  n_farms_chicken_ind <- inputs[parameter == "n_chickens", Value] * 
    (inputs[parameter == "portion_animals_ind", Value]) / 
    inputs[parameter == "n_chickens_farm_ind", Value]
  
  n_farms_pig_small <- inputs[parameter == "n_pigs", Value] * 
    (1 - inputs[parameter == "portion_animals_ind", Value]) / 
    inputs[parameter == "n_pigs_farm_small", Value]
  
  n_farms_pig_ind <- inputs[parameter == "n_pigs", Value] * 
    (inputs[parameter == "portion_animals_ind", Value]) / 
    inputs[parameter == "n_pigs_farm_ind", Value]
  
  #results matrix for healthcare
  results_base_h <- f_expvalue(m_param,m_cost,m_rwd)
  results_interv_h <- f_expvalue(m_param2,m_cost,m_rwd)
  
  total_results_HC<- matrix(rep(0), nrow=2, ncol=2)
  colnames(total_results_HC) <- c("Costs ($)", "QALYs")
  rownames(total_results_HC) <- c("Base Case", "Intervention")
  
  total_results_HC[1,] <- results_base_h[1,]
  total_results_HC[2,] <- results_interv_h[1,]
  
  wtp <- inputs[parameter == "wtp", Value]
  
  incr_cost_health <- (results_interv_h[1,1] - results_base_h[1,1])
  QALYs_saved <-  (results_interv_h[1,2]-results_base_h[1,2])
  NMB_health <- (QALYs_saved*wtp)-(incr_cost_health)
  
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
  
  incr_cost_prod <- total_results_prod[1,1] - total_results_prod[2,1] #hopefully negative
  incr_benefit_prod <- total_results_prod[2,2] - total_results_prod[1,2] #hopefully positive
  NMB_prod <- total_results_prod[2,1] - total_results_prod[1,1] #hopefully positive
  
  #results matrix for industrial chicken farms
  results_base_c_i <- f_expvalue(m_param_c_i,m_cost_c_i,m_rwd_c_i)
  results_interv_c_i <- f_expvalue(m_param_c_i2,m_cost_c_i2,m_rwd_c_i2)
  
  total_results_c_i<- matrix(rep(0), nrow=2, ncol=2)
  colnames(total_results_c_i) <- c("Costs ($)", "Benefits ($)")
  rownames(total_results_c_i) <- c("Base Case", "Intervention")
  
  incr_cost_c_i <- (results_interv_c_i[1,1] - results_base_c_i[1,1])
  incr_benefit_c_i <-  (results_interv_c_i[1,2]-results_base_c_i[1,2])
  
  total_results_c_i[1,] <- results_base_c_i[1,] 
  total_results_c_i[2,] <- results_interv_c_i[1,] 
  
  NMB_c_i <- (incr_benefit_c_i-incr_cost_c_i) * n_farms_chicken_ind
  
  #results matrix for smallholder chicken farms
  results_base_c_s <- f_expvalue(m_param_c_s,m_cost_c_s,m_rwd_c_s)
  results_interv_c_s <- f_expvalue(m_param_c_s2,m_cost_c_s2,m_rwd_c_s2)
  
  total_results_c_s<- matrix(rep(0), nrow=2, ncol=2)
  colnames(total_results_c_s) <- c("Costs ($)", "Benefits ($)")
  rownames(total_results_c_s) <- c("Base Case", "Intervention")
  
  incr_cost_c_s <- (results_interv_c_s[1,1] - results_base_c_s[1,1])
  incr_benefit_c_s <-  (results_interv_c_s[1,2]-results_base_c_s[1,2])
  
  total_results_c_s[1,] <- results_base_c_s[1,] 
  total_results_c_s[2,] <- results_interv_c_s[1,] 
  
  NMB_c_s <- (incr_benefit_c_s-incr_cost_c_s) * n_farms_chicken_small
  
  #results matrix for industrial pig farms
  results_base_p_i <- f_expvalue(m_param_p_i,m_cost_p_i,m_rwd_p_i)
  results_interv_p_i <- f_expvalue(m_param_p_i2,m_cost_p_i2,m_rwd_p_i2)
  
  total_results_p_i<- matrix(rep(0), nrow=2, ncol=2)
  colnames(total_results_p_i) <- c("Costs ($)", "Benefits ($)")
  rownames(total_results_p_i) <- c("Base Case", "Intervention")
  
  incr_cost_p_i <- (results_interv_p_i[1,1] - results_base_p_i[1,1])
  incr_benefit_p_i <-  (results_interv_p_i[1,2]-results_base_p_i[1,2])
  
  total_results_p_i[1,] <- results_base_p_i[1,] 
  total_results_p_i[2,] <- results_interv_p_i[1,] 
  
  NMB_p_i <- (incr_benefit_p_i-incr_cost_p_i) * n_farms_pig_ind
  
  #results matrix for smallholder pig farms
  results_base_p_s <- f_expvalue(m_param_p_s,m_cost_p_s,m_rwd_p_s)
  results_interv_p_s <- f_expvalue(m_param_p_s2,m_cost_p_s2,m_rwd_p_s2)
  
  total_results_p_s<- matrix(rep(0), nrow=2, ncol=2)
  colnames(total_results_p_s) <- c("Costs ($)", "Benefits ($)")
  rownames(total_results_p_s) <- c("Base Case", "Intervention")
  
  incr_cost_p_s <- (results_interv_p_s[1,1] - results_base_p_s[1,1])
  incr_benefit_p_s <-  (results_interv_p_s[1,2]-results_base_p_s[1,2])
  
  total_results_p_s[1,] <- results_base_p_s[1,] 
  total_results_p_s[2,] <- results_interv_p_s[1,] 
  
  NMB_p_s <- (incr_benefit_p_s-incr_cost_p_s) * n_farms_pig_small
  
  max_cost_discounted <- NMB_c_i + NMB_c_s + NMB_p_i + NMB_p_s + NMB_health + NMB_prod
  
  #convert maximum total cost to maximum annual cost
  discount_vector <- rep(0,n.t)
  discount_vector[1] <- 1
  for(i in 2:length(discount_vector)){
    discount_vector[i] = discount_vector[i-1]*(1-dr)
  }
  discount_sum <- sum(discount_vector)
  
  max_cost_annual <- max_cost_discounted / discount_sum
  
  money_saved_health <- -1 * incr_cost_health
  valuation_QALYs <- QALYs_saved*wtp
  
  #Final outputs
  outputs <- data.table("Maximum Acceptable Cost (Annual)"=max_cost_annual,
                        "Value of Productivity Gained"=NMB_prod,
                        "Cost Saved for Healthcare"=money_saved_health,
                        "Value of DALYs Averted"=valuation_QALYs,
                        "Increased Profit - Smallholder Pig Farms"=NMB_p_s,
                        "Increased Profit - Industrial Pig Farms"=NMB_p_i,
                        "Increased Profit - Smallholder Chicken Farms"=NMB_c_s,
                        "Increased Profit - Industrial Chicken Farms"=NMB_c_i)
  outputs
  
  return(outputs)

}

Model(inputs, scenario_income, scenario_prod, scenario_transmission, scenario_farm_effect)

# Figures and Tables -------------------------------------------------------
  

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
scenario_analysis_LIC[1,1] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "lo"
scenario_analysis_LIC[1,2] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "med"
scenario_analysis_LIC[1,3] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "hi"
scenario_analysis_LIC[1,4] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "max"
scenario_analysis_LIC[1,5] <- as.numeric(Model(inputs)[1,1])

scenario_transmission <- "med"

scenario_farm_effect       <- "min"
scenario_analysis_LIC[2,1] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "lo"
scenario_analysis_LIC[2,2] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "med"
scenario_analysis_LIC[2,3] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "hi"
scenario_analysis_LIC[2,4] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "max"
scenario_analysis_LIC[2,5] <- as.numeric(Model(inputs)[1,1])

scenario_transmission <- "hi"

scenario_farm_effect       <- "min"
scenario_analysis_LIC[3,1] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "lo"
scenario_analysis_LIC[3,2] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "med"
scenario_analysis_LIC[3,3] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "hi"
scenario_analysis_LIC[3,4] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "max"
scenario_analysis_LIC[3,5] <- as.numeric(Model(inputs)[1,1])

scenario_transmission <- "max"

scenario_farm_effect       <- "min"
scenario_analysis_LIC[4,1] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "lo"
scenario_analysis_LIC[4,2] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "med"
scenario_analysis_LIC[4,3] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "hi"
scenario_analysis_LIC[4,4] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "max"
scenario_analysis_LIC[4,5] <- as.numeric(Model(inputs)[1,1])

#MIC-I
scenario_income    <- "MIC-I"

scenario_transmission <- "low"

scenario_farm_effect       <- "min"
scenario_analysis_MIC_I[1,1] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "lo"
scenario_analysis_MIC_I[1,2] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "med"
scenario_analysis_MIC_I[1,3] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "hi"
scenario_analysis_MIC_I[1,4] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "max"
scenario_analysis_MIC_I[1,5] <- as.numeric(Model(inputs)[1,1])

scenario_transmission <- "med"

scenario_farm_effect       <- "min"
scenario_analysis_MIC_I[2,1] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "lo"
scenario_analysis_MIC_I[2,2] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "med"
scenario_analysis_MIC_I[2,3] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "hi"
scenario_analysis_MIC_I[2,4] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "max"
scenario_analysis_MIC_I[2,5] <- as.numeric(Model(inputs)[1,1])

scenario_transmission <- "hi"

scenario_farm_effect       <- "min"
scenario_analysis_MIC_I[3,1] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "lo"
scenario_analysis_MIC_I[3,2] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "med"
scenario_analysis_MIC_I[3,3] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "hi"
scenario_analysis_MIC_I[3,4] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "max"
scenario_analysis_MIC_I[3,5] <- as.numeric(Model(inputs)[1,1])

scenario_transmission <- "max"

scenario_farm_effect       <- "min"
scenario_analysis_MIC_I[4,1] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "lo"
scenario_analysis_MIC_I[4,2] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "med"
scenario_analysis_MIC_I[4,3] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "hi"
scenario_analysis_MIC_I[4,4] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "max"
scenario_analysis_MIC_I[4,5] <- as.numeric(Model(inputs)[1,1])

#MIC-S
scenario_income    <- "MIC-S"

scenario_transmission <- "low"

scenario_farm_effect       <- "min"
scenario_analysis_MIC_S[1,1] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "lo"
scenario_analysis_MIC_S[1,2] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "med"
scenario_analysis_MIC_S[1,3] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "hi"
scenario_analysis_MIC_S[1,4] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "max"
scenario_analysis_MIC_S[1,5] <- as.numeric(Model(inputs)[1,1])

scenario_transmission <- "med"

scenario_farm_effect       <- "min"
scenario_analysis_MIC_S[2,1] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "lo"
scenario_analysis_MIC_S[2,2] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "med"
scenario_analysis_MIC_S[2,3] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "hi"
scenario_analysis_MIC_S[2,4] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "max"
scenario_analysis_MIC_S[2,5] <- as.numeric(Model(inputs)[1,1])

scenario_transmission <- "hi"

scenario_farm_effect       <- "min"
scenario_analysis_MIC_S[3,1] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "lo"
scenario_analysis_MIC_S[3,2] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "med"
scenario_analysis_MIC_S[3,3] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "hi"
scenario_analysis_MIC_S[3,4] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "max"
scenario_analysis_MIC_S[3,5] <- as.numeric(Model(inputs)[1,1])

scenario_transmission <- "max"

scenario_farm_effect       <- "min"
scenario_analysis_MIC_S[4,1] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "lo"
scenario_analysis_MIC_S[4,2] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "med"
scenario_analysis_MIC_S[4,3] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "hi"
scenario_analysis_MIC_S[4,4] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "max"
scenario_analysis_MIC_S[4,5] <- as.numeric(Model(inputs)[1,1])

#HIC
scenario_income    <- "HIC"

scenario_transmission <- "low"

scenario_farm_effect       <- "min"
scenario_analysis_HIC[1,1] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "lo"
scenario_analysis_HIC[1,2] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "med"
scenario_analysis_HIC[1,3] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "hi"
scenario_analysis_HIC[1,4] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "max"
scenario_analysis_HIC[1,5] <- as.numeric(Model(inputs)[1,1])

scenario_transmission <- "med"

scenario_farm_effect       <- "min"
scenario_analysis_HIC[2,1] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "lo"
scenario_analysis_HIC[2,2] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "med"
scenario_analysis_HIC[2,3] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "hi"
scenario_analysis_HIC[2,4] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "max"
scenario_analysis_HIC[2,5] <- as.numeric(Model(inputs)[1,1])

scenario_transmission <- "hi"

scenario_farm_effect       <- "min"
scenario_analysis_HIC[3,1] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "lo"
scenario_analysis_HIC[3,2] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "med"
scenario_analysis_HIC[3,3] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "hi"
scenario_analysis_HIC[3,4] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "max"
scenario_analysis_HIC[3,5] <- as.numeric(Model(inputs)[1,1])

scenario_transmission <- "max"

scenario_farm_effect       <- "min"
scenario_analysis_HIC[4,1] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "lo"
scenario_analysis_HIC[4,2] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "med"
scenario_analysis_HIC[4,3] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "hi"
scenario_analysis_HIC[4,4] <- as.numeric(Model(inputs)[1,1])
scenario_farm_effect       <- "max"
scenario_analysis_HIC[4,5] <- as.numeric(Model(inputs)[1,1])
  
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
  LIC_MC_Vector[i] <- as.data.frame(Model(inputsPSA))[1,1]
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
  MIC_MC_Vector[i] <- as.data.frame(Model(inputsPSA))[1,1]
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
  HIC_MC_Vector[i] <- as.data.frame(Model(inputsPSA))[1,1]
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

LIC_base <- as.numeric(Model(inputs)[1,1])

#human AMR effect
inputs_tornado <- inputs

inputs_tornado[21,10] <- inputs_tornado[21,13]
LIC_human_AMR_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[21,10] <- inputs_tornado[21,14]
LIC_human_AMR_high <- as.numeric(Model(inputs_tornado)[1,1])

LIC_human_AMR_low <- min((LIC_human_AMR_low - LIC_base), (LIC_human_AMR_high - LIC_base))
LIC_human_AMR_high <- max((LIC_human_AMR_low - LIC_base), (LIC_human_AMR_high - LIC_base))

#discount rate
inputs_tornado <- inputs

inputs_tornado[3,6] <- inputs_tornado[3,13]
LIC_human_dr_low <- as.numeric(Model(inputs_tornado)[1,1])
  
inputs_tornado[3,6] <- inputs_tornado[3,14]
LIC_human_dr_high <- as.numeric(Model(inputs_tornado)[1,1])

LIC_human_dr_low <- max((LIC_human_dr_low - LIC_base), (LIC_human_dr_high - LIC_base))
LIC_human_dr_high <- min((LIC_human_dr_low - LIC_base), (LIC_human_dr_high - LIC_base))

#AMR growth
inputs_tornado <- inputs

inputs_tornado[38,6] <- inputs_tornado[38,13]
LIC_human_amrgro_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[38,6] <- inputs_tornado[38,14]
LIC_human_amrgro_high <- as.numeric(Model(inputs_tornado)[1,1])

LIC_human_amrgro_low <- min((LIC_human_amrgro_low - LIC_base), (LIC_human_amrgro_high - LIC_base))
LIC_human_amrgro_high <- max((LIC_human_amrgro_low - LIC_base), (LIC_human_amrgro_high - LIC_base))

#background AMR
inputs_tornado <- inputs

inputs_tornado[28,6] <- inputs_tornado[28,13]
LIC_human_amrbase_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[28,6] <- inputs_tornado[28,14]
LIC_human_amrbase_high <- as.numeric(Model(inputs_tornado)[1,1])

LIC_human_amrbase_low <- min((LIC_human_amrbase_low - LIC_base), (LIC_human_amrbase_high - LIC_base))
LIC_human_amrbase_high <- max((LIC_human_amrbase_low - LIC_base), (LIC_human_amrbase_high - LIC_base))

#WTP
inputs_tornado <- inputs

inputs_tornado[6,6] <- inputs_tornado[6,13]
LIC_human_wtp_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[6,6] <- inputs_tornado[6,14]
LIC_human_wtp_high <- as.numeric(Model(inputs_tornado)[1,1])

LIC_human_wtp_low <- min((LIC_human_wtp_low - LIC_base), (LIC_human_wtp_high - LIC_base))
LIC_human_wtp_high <- max((LIC_human_wtp_low - LIC_base), (LIC_human_wtp_high - LIC_base))

#LFPR
inputs_tornado <- inputs

inputs_tornado[10,6] <- inputs_tornado[10,13]
LIC_human_lfpr_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[10,6] <- inputs_tornado[10,14]
LIC_human_lfpr_high <- as.numeric(Model(inputs_tornado)[1,1])

LIC_human_lfpr_low <- min((LIC_human_lfpr_low - LIC_base), (LIC_human_lfpr_high - LIC_base))
LIC_human_lfpr_high <- max((LIC_human_lfpr_low - LIC_base), (LIC_human_lfpr_high - LIC_base))

#disease incidence
inputs_tornado <- inputs

inputs_tornado[27,6] <- inputs_tornado[27,13]
LIC_human_disease_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[27,6] <- inputs_tornado[27,14]
LIC_human_disease_high <- as.numeric(Model(inputs_tornado)[1,1])

LIC_human_disease_low <- min((LIC_human_disease_low - LIC_base), (LIC_human_disease_high - LIC_base))
LIC_human_disease_high <- max((LIC_human_disease_low - LIC_base), (LIC_human_disease_high - LIC_base))

#/#

#pig income effect
inputs_tornado <- inputs

inputs_tornado[15,11] <- inputs_tornado[15,13]
LIC_animal_piginc_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[15,11] <- inputs_tornado[15,14]
LIC_animal_piginc_high <- as.numeric(Model(inputs_tornado)[1,1])

LIC_animal_piginc_low <- min((LIC_animal_piginc_low - LIC_base), (LIC_animal_piginc_high - LIC_base))
LIC_animal_piginc_high <- max((LIC_animal_piginc_low - LIC_base), (LIC_animal_piginc_high - LIC_base))

#chicken income effect
inputs_tornado <- inputs

inputs_tornado[17,11] <- inputs_tornado[17,13]
LIC_animal_chickinc_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[17,11] <- inputs_tornado[17,14]
LIC_animal_chickinc_high <- as.numeric(Model(inputs_tornado)[1,1])

LIC_animal_chickinc_low <- min((LIC_animal_chickinc_low - LIC_base), (LIC_animal_chickinc_high - LIC_base))
LIC_animal_chickinc_high <- max((LIC_animal_chickinc_low - LIC_base), (LIC_animal_chickinc_high - LIC_base))

#farm size (pig - I)
inputs_tornado <- inputs

inputs_tornado[46,6] <- inputs_tornado[46,13]
LIC_animal_pigfarmi_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[46,6] <- inputs_tornado[46,14]
LIC_animal_pigfarmi_high <- as.numeric(Model(inputs_tornado)[1,1])

LIC_animal_pigfarmi_low <- min((LIC_animal_pigfarmi_low - LIC_base), (LIC_animal_pigfarmi_high - LIC_base))
LIC_animal_pigfarmi_high <- max((LIC_animal_pigfarmi_low - LIC_base), (LIC_animal_pigfarmi_high - LIC_base))

#farm size (pig - S)
inputs_tornado <- inputs

inputs_tornado[47,6] <- inputs_tornado[47,13]
LIC_animal_pigfarms_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[47,6] <- inputs_tornado[47,14]
LIC_animal_pigfarms_high <- as.numeric(Model(inputs_tornado)[1,1])

LIC_animal_pigfarms_low <- min((LIC_animal_pigfarms_low - LIC_base), (LIC_animal_pigfarms_high - LIC_base))
LIC_animal_pigfarms_high <- max((LIC_animal_pigfarms_low - LIC_base), (LIC_animal_pigfarms_high - LIC_base))

#farm size (chicken - I)
inputs_tornado <- inputs

inputs_tornado[44,6] <- inputs_tornado[44,13]
LIC_animal_chickfarmi_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[44,6] <- inputs_tornado[44,14]
LIC_animal_chickfarmi_high <- as.numeric(Model(inputs_tornado)[1,1])

LIC_animal_chickfarmi_low <- min((LIC_animal_chickfarmi_low - LIC_base), (LIC_animal_chickfarmi_high - LIC_base))
LIC_animal_chickfarmi_high <- max((LIC_animal_chickfarmi_low - LIC_base), (LIC_animal_chickfarmi_high - LIC_base))

#farm size (chicken - S)
inputs_tornado <- inputs

inputs_tornado[45,6] <- inputs_tornado[45,13]
LIC_animal_chickfarms_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[45,6] <- inputs_tornado[45,14]
LIC_animal_chickfarms_high <- as.numeric(Model(inputs_tornado)[1,1])

LIC_animal_chickfarms_low <- min((LIC_animal_chickfarms_low - LIC_base), (LIC_animal_chickfarms_high - LIC_base))
LIC_animal_chickfarms_high <- max((LIC_animal_chickfarms_low - LIC_base), (LIC_animal_chickfarms_high - LIC_base))

#number of pigs
inputs_tornado <- inputs

inputs_tornado[42,6] <- inputs_tornado[42,13]
LIC_animal_npigs_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[42,6] <- inputs_tornado[42,14]
LIC_animal_npigs_high <- as.numeric(Model(inputs_tornado)[1,1])

LIC_animal_npigs_low <- min((LIC_animal_npigs_low - LIC_base), (LIC_animal_npigs_high - LIC_base))
LIC_animal_npigs_high <- max((LIC_animal_npigs_low - LIC_base), (LIC_animal_npigs_high - LIC_base))

#number of chickens
inputs_tornado <- inputs

inputs_tornado[43,6] <- inputs_tornado[43,13]
LIC_animal_nchicks_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[43,6] <- inputs_tornado[43,14]
LIC_animal_nchicks_high <- as.numeric(Model(inputs_tornado)[1,1])

LIC_animal_nchicks_low <- min((LIC_animal_nchicks_low - LIC_base), (LIC_animal_nchicks_high - LIC_base))
LIC_animal_nchicks_high <- max((LIC_animal_nchicks_low - LIC_base), (LIC_animal_nchicks_high - LIC_base))

##MIC-I

#get the base case max cost
scenario_income <- "MIC-I"

MICI_base <- as.numeric(Model(inputs)[1,1])

#human AMR effect
inputs_tornado <- inputs

inputs_tornado[21,10] <- inputs_tornado[21,15]
MICI_human_AMR_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[21,10] <- inputs_tornado[21,16]
MICI_human_AMR_high <- as.numeric(Model(inputs_tornado)[1,1])

MICI_human_AMR_low <- min((MICI_human_AMR_low - MICI_base), (MICI_human_AMR_high - MICI_base))
MICI_human_AMR_high <- max((MICI_human_AMR_low - MICI_base), (MICI_human_AMR_high - MICI_base))

#discount rate
inputs_tornado <- inputs

inputs_tornado[3,4] <- inputs_tornado[3,15]
MICI_human_dr_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[3,4] <- inputs_tornado[3,16]
MICI_human_dr_high <- as.numeric(Model(inputs_tornado)[1,1])

MICI_human_dr_low <- max((MICI_human_dr_low - MICI_base), (MICI_human_dr_high - MICI_base))
MICI_human_dr_high <- min((MICI_human_dr_low - MICI_base), (MICI_human_dr_high - MICI_base))

#AMR growth
inputs_tornado <- inputs

inputs_tornado[38,4] <- inputs_tornado[38,15]
MICI_human_amrgro_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[38,4] <- inputs_tornado[38,16]
MICI_human_amrgro_high <- as.numeric(Model(inputs_tornado)[1,1])

MICI_human_amrgro_low <- min((MICI_human_amrgro_low - MICI_base), (MICI_human_amrgro_high - MICI_base))
MICI_human_amrgro_high <- max((MICI_human_amrgro_low - MICI_base), (MICI_human_amrgro_high - MICI_base))

#background AMR
inputs_tornado <- inputs

inputs_tornado[28,4] <- inputs_tornado[28,15]
MICI_human_amrbase_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[28,4] <- inputs_tornado[28,16]
MICI_human_amrbase_high <- as.numeric(Model(inputs_tornado)[1,1])

MICI_human_amrbase_low <- min((MICI_human_amrbase_low - MICI_base), (MICI_human_amrbase_high - MICI_base))
MICI_human_amrbase_high <- max((MICI_human_amrbase_low - MICI_base), (MICI_human_amrbase_high - MICI_base))

#WTP
inputs_tornado <- inputs

inputs_tornado[6,4] <- inputs_tornado[6,15]
MICI_human_wtp_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[6,4] <- inputs_tornado[6,16]
MICI_human_wtp_high <- as.numeric(Model(inputs_tornado)[1,1])

MICI_human_wtp_low <- min((MICI_human_wtp_low - MICI_base), (MICI_human_wtp_high - MICI_base))
MICI_human_wtp_high <- max((MICI_human_wtp_low - MICI_base), (MICI_human_wtp_high - MICI_base))

#LFPR
inputs_tornado <- inputs

inputs_tornado[10,4] <- inputs_tornado[10,15]
MICI_human_lfpr_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[10,4] <- inputs_tornado[10,16]
MICI_human_lfpr_high <- as.numeric(Model(inputs_tornado)[1,1])

MICI_human_lfpr_low <- min((MICI_human_lfpr_low - MICI_base), (MICI_human_lfpr_high - MICI_base))
MICI_human_lfpr_high <- max((MICI_human_lfpr_low - MICI_base), (MICI_human_lfpr_high - MICI_base))

#disease incidence
inputs_tornado <- inputs

inputs_tornado[27,4] <- inputs_tornado[27,15]
MICI_human_disease_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[27,4] <- inputs_tornado[27,16]
MICI_human_disease_high <- as.numeric(Model(inputs_tornado)[1,1])

MICI_human_disease_low <- min((MICI_human_disease_low - MICI_base), (MICI_human_disease_high - MICI_base))
MICI_human_disease_high <- max((MICI_human_disease_low - MICI_base), (MICI_human_disease_high - MICI_base))

#/#

#pig income effect
inputs_tornado <- inputs

inputs_tornado[15,11] <- inputs_tornado[15,15]
MICI_animal_piginc_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[15,11] <- inputs_tornado[15,16]
MICI_animal_piginc_high <- as.numeric(Model(inputs_tornado)[1,1])

MICI_animal_piginc_low <- min((MICI_animal_piginc_low - MICI_base), (MICI_animal_piginc_high - MICI_base))
MICI_animal_piginc_high <- max((MICI_animal_piginc_low - MICI_base), (MICI_animal_piginc_high - MICI_base))

#chicken income effect
inputs_tornado <- inputs

inputs_tornado[17,11] <- inputs_tornado[17,15]
MICI_animal_chickinc_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[17,11] <- inputs_tornado[17,16]
MICI_animal_chickinc_high <- as.numeric(Model(inputs_tornado)[1,1])

MICI_animal_chickinc_low <- min((MICI_animal_chickinc_low - MICI_base), (MICI_animal_chickinc_high - MICI_base))
MICI_animal_chickinc_high <- max((MICI_animal_chickinc_low - MICI_base), (MICI_animal_chickinc_high - MICI_base))

#farm size (pig - I)
inputs_tornado <- inputs

inputs_tornado[46,4] <- inputs_tornado[46,15]
MICI_animal_pigfarmi_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[46,4] <- inputs_tornado[46,16]
MICI_animal_pigfarmi_high <- as.numeric(Model(inputs_tornado)[1,1])

MICI_animal_pigfarmi_low <- min((MICI_animal_pigfarmi_low - MICI_base), (MICI_animal_pigfarmi_high - MICI_base))
MICI_animal_pigfarmi_high <- max((MICI_animal_pigfarmi_low - MICI_base), (MICI_animal_pigfarmi_high - MICI_base))

#farm size (pig - S)
inputs_tornado <- inputs

inputs_tornado[47,4] <- inputs_tornado[47,15]
MICI_animal_pigfarms_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[47,4] <- inputs_tornado[47,16]
MICI_animal_pigfarms_high <- as.numeric(Model(inputs_tornado)[1,1])

MICI_animal_pigfarms_low <- min((MICI_animal_pigfarms_low - MICI_base), (MICI_animal_pigfarms_high - MICI_base))
MICI_animal_pigfarms_high <- max((MICI_animal_pigfarms_low - MICI_base), (MICI_animal_pigfarms_high - MICI_base))

#farm size (chicken - I)
inputs_tornado <- inputs

inputs_tornado[44,4] <- inputs_tornado[44,15]
MICI_animal_chickfarmi_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[44,4] <- inputs_tornado[44,16]
MICI_animal_chickfarmi_high <- as.numeric(Model(inputs_tornado)[1,1])

MICI_animal_chickfarmi_low <- min((MICI_animal_chickfarmi_low - MICI_base), (MICI_animal_chickfarmi_high - MICI_base))
MICI_animal_chickfarmi_high <- max((MICI_animal_chickfarmi_low - MICI_base), (MICI_animal_chickfarmi_high - MICI_base))

#farm size (chicken - S)
inputs_tornado <- inputs

inputs_tornado[45,4] <- inputs_tornado[45,15]
MICI_animal_chickfarms_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[45,4] <- inputs_tornado[45,16]
MICI_animal_chickfarms_high <- as.numeric(Model(inputs_tornado)[1,1])

MICI_animal_chickfarms_low <- min((MICI_animal_chickfarms_low - MICI_base), (MICI_animal_chickfarms_high - MICI_base))
MICI_animal_chickfarms_high <- max((MICI_animal_chickfarms_low - MICI_base), (MICI_animal_chickfarms_high - MICI_base))

#number of pigs
inputs_tornado <- inputs

inputs_tornado[42,4] <- inputs_tornado[42,15]
MICI_animal_npigs_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[42,4] <- inputs_tornado[42,16]
MICI_animal_npigs_high <- as.numeric(Model(inputs_tornado)[1,1])

MICI_animal_npigs_low <- min((MICI_animal_npigs_low - MICI_base), (MICI_animal_npigs_high - MICI_base))
MICI_animal_npigs_high <- max((MICI_animal_npigs_low - MICI_base), (MICI_animal_npigs_high - MICI_base))

#number of chickens
inputs_tornado <- inputs

inputs_tornado[43,4] <- inputs_tornado[43,15]
MICI_animal_nchicks_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[43,4] <- inputs_tornado[43,16]
MICI_animal_nchicks_high <- as.numeric(Model(inputs_tornado)[1,1])

MICI_animal_nchicks_low <- min((MICI_animal_nchicks_low - MICI_base), (MICI_animal_nchicks_high - MICI_base))
MICI_animal_nchicks_high <- max((MICI_animal_nchicks_low - MICI_base), (MICI_animal_nchicks_high - MICI_base))

##MIC-S

#get the base case max cost
scenario_income <- "MIC-S"

MICS_base <- as.numeric(Model(inputs)[1,1])

#human AMR effect
inputs_tornado <- inputs

inputs_tornado[21,10] <- inputs_tornado[21,15]
MICS_human_AMR_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[21,10] <- inputs_tornado[21,16]
MICS_human_AMR_high <- as.numeric(Model(inputs_tornado)[1,1])

MICS_human_AMR_low <- min((MICS_human_AMR_low - MICS_base), (MICS_human_AMR_high - MICS_base))
MICS_human_AMR_high <- max((MICS_human_AMR_low - MICS_base), (MICS_human_AMR_high - MICS_base))

#discount rate
inputs_tornado <- inputs

inputs_tornado[3,5] <- inputs_tornado[3,15]
MICS_human_dr_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[3,5] <- inputs_tornado[3,16]
MICS_human_dr_high <- as.numeric(Model(inputs_tornado)[1,1])

MICS_human_dr_low <- max((MICS_human_dr_low - MICS_base), (MICS_human_dr_high - MICS_base))
MICS_human_dr_high <- min((MICS_human_dr_low - MICS_base), (MICS_human_dr_high - MICS_base))

#AMR growth
inputs_tornado <- inputs

inputs_tornado[38,5] <- inputs_tornado[38,15]
MICS_human_amrgro_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[38,5] <- inputs_tornado[38,16]
MICS_human_amrgro_high <- as.numeric(Model(inputs_tornado)[1,1])

MICS_human_amrgro_low <- min((MICS_human_amrgro_low - MICS_base), (MICS_human_amrgro_high - MICS_base))
MICS_human_amrgro_high <- max((MICS_human_amrgro_low - MICS_base), (MICS_human_amrgro_high - MICS_base))

#background AMR
inputs_tornado <- inputs

inputs_tornado[28,5] <- inputs_tornado[28,15]
MICS_human_amrbase_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[28,5] <- inputs_tornado[28,16]
MICS_human_amrbase_high <- as.numeric(Model(inputs_tornado)[1,1])

MICS_human_amrbase_low <- min((MICS_human_amrbase_low - MICS_base), (MICS_human_amrbase_high - MICS_base))
MICS_human_amrbase_high <- max((MICS_human_amrbase_low - MICS_base), (MICS_human_amrbase_high - MICS_base))

#WTP
inputs_tornado <- inputs

inputs_tornado[6,5] <- inputs_tornado[6,15]
MICS_human_wtp_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[6,5] <- inputs_tornado[6,16]
MICS_human_wtp_high <- as.numeric(Model(inputs_tornado)[1,1])

MICS_human_wtp_low <- min((MICS_human_wtp_low - MICS_base), (MICS_human_wtp_high - MICS_base))
MICS_human_wtp_high <- max((MICS_human_wtp_low - MICS_base), (MICS_human_wtp_high - MICS_base))

#LFPR
inputs_tornado <- inputs

inputs_tornado[10,5] <- inputs_tornado[10,15]
MICS_human_lfpr_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[10,5] <- inputs_tornado[10,16]
MICS_human_lfpr_high <- as.numeric(Model(inputs_tornado)[1,1])

MICS_human_lfpr_low <- min((MICS_human_lfpr_low - MICS_base), (MICS_human_lfpr_high - MICS_base))
MICS_human_lfpr_high <- max((MICS_human_lfpr_low - MICS_base), (MICS_human_lfpr_high - MICS_base))

#disease incidence
inputs_tornado <- inputs

inputs_tornado[27,5] <- inputs_tornado[27,15]
MICS_human_disease_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[27,5] <- inputs_tornado[27,16]
MICS_human_disease_high <- as.numeric(Model(inputs_tornado)[1,1])

MICS_human_disease_low <- min((MICS_human_disease_low - MICS_base), (MICS_human_disease_high - MICS_base))
MICS_human_disease_high <- max((MICS_human_disease_low - MICS_base), (MICS_human_disease_high - MICS_base))

#/#

#pig income effect
inputs_tornado <- inputs

inputs_tornado[15,11] <- inputs_tornado[15,15]
MICS_animal_piginc_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[15,11] <- inputs_tornado[15,16]
MICS_animal_piginc_high <- as.numeric(Model(inputs_tornado)[1,1])

MICS_animal_piginc_low <- min((MICS_animal_piginc_low - MICS_base), (MICS_animal_piginc_high - MICS_base))
MICS_animal_piginc_high <- max((MICS_animal_piginc_low - MICS_base), (MICS_animal_piginc_high - MICS_base))

#chicken income effect
inputs_tornado <- inputs

inputs_tornado[17,11] <- inputs_tornado[17,15]
MICS_animal_chickinc_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[17,11] <- inputs_tornado[17,16]
MICS_animal_chickinc_high <- as.numeric(Model(inputs_tornado)[1,1])

MICS_animal_chickinc_low <- min((MICS_animal_chickinc_low - MICS_base), (MICS_animal_chickinc_high - MICS_base))
MICS_animal_chickinc_high <- max((MICS_animal_chickinc_low - MICS_base), (MICS_animal_chickinc_high - MICS_base))

#farm size (pig - I)
inputs_tornado <- inputs

inputs_tornado[46,5] <- inputs_tornado[46,15]
MICS_animal_pigfarmi_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[46,5] <- inputs_tornado[46,16]
MICS_animal_pigfarmi_high <- as.numeric(Model(inputs_tornado)[1,1])

MICS_animal_pigfarmi_low <- min((MICS_animal_pigfarmi_low - MICS_base), (MICS_animal_pigfarmi_high - MICS_base))
MICS_animal_pigfarmi_high <- max((MICS_animal_pigfarmi_low - MICS_base), (MICS_animal_pigfarmi_high - MICS_base))

#farm size (pig - S)
inputs_tornado <- inputs

inputs_tornado[47,5] <- inputs_tornado[47,15]
MICS_animal_pigfarms_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[47,5] <- inputs_tornado[47,16]
MICS_animal_pigfarms_high <- as.numeric(Model(inputs_tornado)[1,1])

MICS_animal_pigfarms_low <- min((MICS_animal_pigfarms_low - MICS_base), (MICS_animal_pigfarms_high - MICS_base))
MICS_animal_pigfarms_high <- max((MICS_animal_pigfarms_low - MICS_base), (MICS_animal_pigfarms_high - MICS_base))

#farm size (chicken - I)
inputs_tornado <- inputs

inputs_tornado[44,5] <- inputs_tornado[44,15]
MICS_animal_chickfarmi_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[44,5] <- inputs_tornado[44,16]
MICS_animal_chickfarmi_high <- as.numeric(Model(inputs_tornado)[1,1])

MICS_animal_chicfarmi_low <- min((MICS_animal_chickfarmi_low - MICS_base), (MICS_animal_chickfarmi_high - MICS_base))
MICS_animal_chicfarmi_high <- max((MICS_animal_chickfarmi_low - MICS_base), (MICS_animal_chickfarmi_high - MICS_base))

#farm size (chicken - S)
inputs_tornado <- inputs

inputs_tornado[45,5] <- inputs_tornado[45,15]
MICS_animal_chickfarms_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[45,5] <- inputs_tornado[45,16]
MICS_animal_chickfarms_high <- as.numeric(Model(inputs_tornado)[1,1])

MICS_animal_chickfarms_low <- min((MICS_animal_chickfarms_low - MICS_base), (MICS_animal_chickfarms_high - MICS_base))
MICS_animal_chickfarms_high <- max((MICS_animal_chickfarms_low - MICS_base), (MICS_animal_chickfarms_high - MICS_base))

#number of pigs
inputs_tornado <- inputs

inputs_tornado[42,5] <- inputs_tornado[42,15]
MICS_animal_npigs_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[42,5] <- inputs_tornado[42,16]
MICS_animal_npigs_high <- as.numeric(Model(inputs_tornado)[1,1])

MICS_animal_npigs_low <- min((MICS_animal_npigs_low - MICS_base), (MICS_animal_npigs_high - MICS_base))
MICS_animal_npigs_high <- max((MICS_animal_npigs_low - MICS_base), (MICS_animal_npigs_high - MICS_base))

#number of chickens
inputs_tornado <- inputs

inputs_tornado[43,5] <- inputs_tornado[43,15]
MICS_animal_nchicks_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[43,5] <- inputs_tornado[43,16]
MICS_animal_nchicks_high <- as.numeric(Model(inputs_tornado)[1,1])

MICS_animal_nchicks_low <- min((MICS_animal_nchicks_low - MICS_base), (MICS_animal_nchicks_high - MICS_base))
MICS_animal_nchicks_high <- max((MICS_animal_nchicks_low - MICS_base), (MICS_animal_nchicks_high - MICS_base))

##HIC

#get the base case max cost
scenario_income <- "HIC"

HIC_base <- as.numeric(Model(inputs)[1,1])

#human AMR effect
inputs_tornado <- inputs

inputs_tornado[21,10] <- inputs_tornado[21,17]
HIC_human_AMR_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[21,10] <- inputs_tornado[21,18]
HIC_human_AMR_high <- as.numeric(Model(inputs_tornado)[1,1])

HIC_human_AMR_low <- min((HIC_human_AMR_low - HIC_base), (HIC_human_AMR_high - HIC_base))
HIC_human_AMR_high <- max((HIC_human_AMR_low - HIC_base), (HIC_human_AMR_high - HIC_base))

#discount rate
inputs_tornado <- inputs

inputs_tornado[3,3] <- inputs_tornado[3,17]
HIC_human_dr_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[3,3] <- inputs_tornado[3,18]
HIC_human_dr_high <- as.numeric(Model(inputs_tornado)[1,1])

HIC_human_dr_low <- max((HIC_human_dr_low - HIC_base), (HIC_human_dr_high - HIC_base))
HIC_human_dr_high <- min((HIC_human_dr_low - HIC_base), (HIC_human_dr_high - HIC_base))

#AMR growth
inputs_tornado <- inputs

inputs_tornado[38,3] <- inputs_tornado[38,17]
HIC_human_amrgro_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[38,3] <- inputs_tornado[38,18]
HIC_human_amrgro_high <- as.numeric(Model(inputs_tornado)[1,1])

HIC_human_amrgro_low <- min((HIC_human_amrgro_low - HIC_base), (HIC_human_amrgro_high - HIC_base))
HIC_human_amrgro_high <- max((HIC_human_amrgro_low - HIC_base), (HIC_human_amrgro_high - HIC_base))

#background AMR
inputs_tornado <- inputs

inputs_tornado[28,3] <- inputs_tornado[28,17]
HIC_human_amrbase_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[28,3] <- inputs_tornado[28,18]
HIC_human_amrbase_high <- as.numeric(Model(inputs_tornado)[1,1])

HIC_human_amrbase_low <- min((HIC_human_amrbase_low - HIC_base), (HIC_human_amrbase_high - HIC_base))
HIC_human_amrbase_high <- max((HIC_human_amrbase_low - HIC_base), (HIC_human_amrbase_high - HIC_base))

#WTP
inputs_tornado <- inputs

inputs_tornado[6,3] <- inputs_tornado[6,17]
HIC_human_wtp_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[6,3] <- inputs_tornado[6,18]
HIC_human_wtp_high <- as.numeric(Model(inputs_tornado)[1,1])

HIC_human_wtp_low <- min((HIC_human_wtp_low - HIC_base), (HIC_human_wtp_high - HIC_base))
HIC_human_wtp_high <- max((HIC_human_wtp_low - HIC_base), (HIC_human_wtp_high - HIC_base))

#LFPR
inputs_tornado <- inputs

inputs_tornado[10,3] <- inputs_tornado[10,17]
HIC_human_lfpr_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[10,3] <- inputs_tornado[10,18]
HIC_human_lfpr_high <- as.numeric(Model(inputs_tornado)[1,1])

HIC_human_lfpr_low <- min((HIC_human_lfpr_low - HIC_base), (HIC_human_lfpr_high - HIC_base))
HIC_human_lfpr_high <- max((HIC_human_lfpr_low - HIC_base), (HIC_human_lfpr_high - HIC_base))

#disease incidence
inputs_tornado <- inputs

inputs_tornado[27,3] <- inputs_tornado[27,17]
HIC_human_disease_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[27,3] <- inputs_tornado[27,18]
HIC_human_disease_high <- as.numeric(Model(inputs_tornado)[1,1])

HIC_human_disease_low <- min((HIC_human_disease_low - HIC_base), (HIC_human_disease_high - HIC_base))
HIC_human_disease_high <- max((HIC_human_disease_low - HIC_base), (HIC_human_disease_high - HIC_base))

#/#

#pig income effect
inputs_tornado <- inputs

inputs_tornado[15,11] <- inputs_tornado[15,17]
HIC_animal_piginc_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[15,11] <- inputs_tornado[15,18]
HIC_animal_piginc_high <- as.numeric(Model(inputs_tornado)[1,1])

HIC_animal_piginc_low <- min((HIC_animal_piginc_low - HIC_base), (HIC_animal_piginc_high - HIC_base))
HIC_animal_piginc_high <- max((HIC_animal_piginc_low - HIC_base), (HIC_animal_piginc_high - HIC_base))

#chicken income effect
inputs_tornado <- inputs

inputs_tornado[17,11] <- inputs_tornado[17,17]
HIC_animal_chickinc_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[17,11] <- inputs_tornado[17,18]
HIC_animal_chickinc_high <- as.numeric(Model(inputs_tornado)[1,1])

HIC_animal_chickinc_low <- min((HIC_animal_chickinc_low - HIC_base), (HIC_animal_chickinc_high - HIC_base))
HIC_animal_chickinc_high <- max((HIC_animal_chickinc_low - HIC_base), (HIC_animal_chickinc_high - HIC_base))

#farm size (pig - I)
inputs_tornado <- inputs

inputs_tornado[46,3] <- inputs_tornado[46,17]
HIC_animal_pigfarmi_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[46,3] <- inputs_tornado[46,18]
HIC_animal_pigfarmi_high <- as.numeric(Model(inputs_tornado)[1,1])

HIC_animal_pigfarmi_low <- min((HIC_animal_pigfarmi_low - HIC_base), (HIC_animal_pigfarmi_high - HIC_base))
HIC_animal_pigfarmi_high <- max((HIC_animal_pigfarmi_low - HIC_base), (HIC_animal_pigfarmi_high - HIC_base))

#farm size (pig - S)
inputs_tornado <- inputs

inputs_tornado[47,3] <- inputs_tornado[47,17]
HIC_animal_pigfarms_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[47,3] <- inputs_tornado[47,18]
HIC_animal_pigfarms_high <- as.numeric(Model(inputs_tornado)[1,1])

HIC_animal_pigfarms_low <- min((HIC_animal_pigfarms_low - HIC_base), (HIC_animal_pigfarms_high - HIC_base))
HIC_animal_pigfarms_high <- max((HIC_animal_pigfarms_low - HIC_base), (HIC_animal_pigfarms_high - HIC_base))

#farm size (chicken - I)
inputs_tornado <- inputs

inputs_tornado[44,3] <- inputs_tornado[44,17]
HIC_animal_chickfarmi_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[44,3] <- inputs_tornado[44,18]
HIC_animal_chickfarmi_high <- as.numeric(Model(inputs_tornado)[1,1])

HIC_animal_chickfarmi_low <- min((HIC_animal_chickfarmi_low - HIC_base), (HIC_animal_chickfarmi_high - HIC_base))
HIC_animal_chickfarmi_high <- max((HIC_animal_chickfarmi_low - HIC_base), (HIC_animal_chickfarmi_high - HIC_base))

#farm size (chicken - S)
inputs_tornado <- inputs

inputs_tornado[45,3] <- inputs_tornado[45,17]
HIC_animal_chickfarms_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[45,3] <- inputs_tornado[45,18]
HIC_animal_chickfarms_high <- as.numeric(Model(inputs_tornado)[1,1])

HIC_animal_chickfarms_low <- min((HIC_animal_chickfarms_low - HIC_base), (HIC_animal_chickfarms_high - HIC_base))
HIC_animal_chickfarms_high <- max((HIC_animal_chickfarms_low - HIC_base), (HIC_animal_chickfarms_high - HIC_base))

#number of pigs
inputs_tornado <- inputs

inputs_tornado[42,3] <- inputs_tornado[42,17]
HIC_animal_npigs_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[42,3] <- inputs_tornado[42,18]
HIC_animal_npigs_high <- as.numeric(Model(inputs_tornado)[1,1])

HIC_animal_npigs_low <- min((HIC_animal_npigs_low - HIC_base), (HIC_animal_npigs_high - HIC_base))
HIC_animal_npigs_high <- max((HIC_animal_npigs_low - HIC_base), (HIC_animal_npigs_high - HIC_base))

#number of chickens
inputs_tornado <- inputs

inputs_tornado[43,3] <- inputs_tornado[43,17]
HIC_animal_nchicks_low <- as.numeric(Model(inputs_tornado)[1,1])

inputs_tornado[43,3] <- inputs_tornado[43,18]
HIC_animal_nchicks_high <- as.numeric(Model(inputs_tornado)[1,1])

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

LIC_reg_matrix <- matrix(rep(0), nrow = number_runs, ncol = nrow(inputs)+1)

colnames(LIC_reg_matrix) <- c("Output", rep(0, 68))

for(i in 2:69){
  colnames(LIC_reg_matrix)[i] <- inputs[i-1,1]
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
  
  LIC_reg_matrix[i,1] <- as.data.frame(Model(inputsreg))[1,1]
  
}

write.xlsx(LIC_reg_matrix, "Outputs/reg matrix LIC.xlsx")

### MICI

scenario_income <- "MIC-I"

MICI_reg_matrix <- matrix(rep(0), nrow = number_runs, ncol = nrow(inputs)+1)

colnames(MICI_reg_matrix) <- c("Output", rep(0, 68))

for(i in 2:69){
  colnames(MICI_reg_matrix)[i] <- inputs[i-1,1]
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
  
  MICI_reg_matrix[i,1] <- as.data.frame(Model(inputsreg))[1,1]
  
}

write.xlsx(MICI_reg_matrix, "Outputs/reg matrix MICI.xlsx")

### MICS

scenario_income <- "MIC-S"

MICS_reg_matrix <- matrix(rep(0), nrow = number_runs, ncol = nrow(inputs)+1)

colnames(MICS_reg_matrix) <- c("Output", rep(0, 68))

for(i in 2:69){
  colnames(MICS_reg_matrix)[i] <- inputs[i-1,1]
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
  
  MICS_reg_matrix[i,1] <- as.data.frame(Model(inputsreg))[1,1]
  
}

write.xlsx(MICS_reg_matrix, "Outputs/reg matrix MICS.xlsx")

###HIC

scenario_income <- "HIC"

HIC_reg_matrix <- matrix(rep(0), nrow = number_runs, ncol = nrow(inputs)+1)

colnames(HIC_reg_matrix) <- c("Output", rep(0, 68))

for(i in 2:69){
  colnames(HIC_reg_matrix)[i] <- inputs[i-1,1]
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
  
  HIC_reg_matrix[i,1] <- as.data.frame(Model(inputsreg))[1,1]
  
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
table_3[1,2:9] <- as.numeric(Model(inputs))

scenario_income <- "MIC-S"
table_3[2,2:9] <- as.numeric(Model(inputs))

scenario_income <- "MIC-I"
table_3[3,2:9] <- as.numeric(Model(inputs))

scenario_income <- "HIC"
table_3[4,2:9] <- as.numeric(Model(inputs))

write.xlsx(table_3, "Outputs/Table 3.xlsx")


  

  
  
  